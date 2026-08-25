//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! talkd - local talk daemon
//!
//! This is a **local-only** talk daemon: it serves the BSD `ntalk` control
//! protocol over a Unix-domain datagram socket (default `/var/run/talkd.sock`)
//! rather than UDP port 518. This is a deliberate, documented divergence
//! (audit #TD7): the goal is to make the bundled `talk --local` handshake work
//! end-to-end on one host without exposing a network-facing root daemon. It is
//! therefore NOT interoperable with stock/remote `talk` clients.
//!
//! Privilege model (audit #TD6 — WON'T-FIX, deliberate): to deliver an
//! announcement, talkd must write to the callee's terminal. It performs no
//! privileged operation and is not intended to be started as root, so there is
//! nothing to drop; `setuid`/`setgid` are absent by design rather than by
//! oversight. The BSD `nobody:tty` inetd model exists to contain a *network*
//! ntalkd, and adopting it here would also force the control socket open from
//! 0600 to world-writable (audit #TD5) for no gain on a single-host daemon.
//!
//! Consent is enforced by the recipient's `mesg` setting instead. Unlike
//! `write`, talkd honors `mesg n` **even when running as root**: the
//! announcement is triggered by an unauthenticated datagram, so the recipient's
//! stated wish is the only consent that exists. See `announce_to_tty`, which
//! re-checks `plib::tty::mesg_allowed` rather than relying on `may_write`'s
//! superuser exemption.

use binrw::{binrw, BinReaderExt, BinWrite, Endian};
use clap::Parser;
use gettextrs::gettext;
use plib::syslog::{Facility, Level};
use std::collections::HashMap;
use std::ffi::CString;
use std::fs;
use std::io::{self, Cursor, Write};
use std::net::{Ipv4Addr, SocketAddrV4};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::{FileTypeExt, PermissionsExt};
use std::os::unix::io::AsRawFd;
use std::os::unix::net::UnixDatagram;
use std::path::{Path, PathBuf};
use std::process;
use std::sync::atomic::{AtomicPtr, Ordering};
use std::sync::OnceLock;
use std::time::{Duration, Instant};

#[cfg(target_os = "macos")]
type SaFamily = u16;

#[cfg(target_os = "linux")]
use libc::sa_family_t;
#[cfg(target_os = "linux")]
type SaFamily = sa_family_t;

/// Default socket path for local talkd (proper system location)
pub const DEFAULT_SOCKET_PATH: &str = "/var/run/talkd.sock";

/// Default invitation timeout in seconds
const INVITATION_TIMEOUT_SECS: u64 = 60;

/// Default interval at which an otherwise-idle daemon wakes to expire stale
/// invitations. Without a periodic wake the sweep ran only when a datagram
/// arrived, so an idle daemon held invitations indefinitely past their timeout.
const EXPIRY_TICK_SECS: u64 = 10;

/// Upper bound on simultaneously-held invitations (DoS guard, audit #TD4).
const MAX_INVITATIONS: usize = 1024;

/// Error replies allowed in a burst before throttling engages.
const ERR_REPLY_BURST: u32 = 8;

/// Sustained error replies per second once the burst is spent.
const ERR_REPLY_PER_SEC: u32 = 8;

/// Minimum spacing between aggregate "suppressed N" log lines.
const SUPPRESS_REPORT_SECS: u64 = 60;

/// Protocol version
const TALK_VERSION: u8 = 1;

/// On-the-wire size of a `CTL_RESPONSE`: vers, type, answer, pad (4 bytes),
/// id_num (4), and a 16-byte `Osockaddr`. Stated explicitly rather than as
/// `size_of::<CtlRes>()`, which is the Rust struct's layout and only happens to
/// agree.
const CTL_RES_LEN: usize = 24;

/// talkd - local talk daemon
#[derive(Parser)]
#[command(version, about = gettext("talkd - local talk daemon"))]
struct Args {
    /// Socket path to listen on
    #[arg(short, long, default_value = DEFAULT_SOCKET_PATH)]
    socket: PathBuf,

    /// Run in foreground (don't daemonize)
    #[arg(short, long)]
    foreground: bool,

    /// Seconds an unanswered invitation is retained
    #[arg(long, default_value_t = INVITATION_TIMEOUT_SECS)]
    invite_timeout: u64,

    /// Seconds between idle expiry sweeps (testing aid)
    #[arg(long, default_value_t = EXPIRY_TICK_SECS, hide = true)]
    expiry_tick: u64,
}

// ============================================================================
// Protocol Structures (matching talk.rs)
// ============================================================================

#[derive(Debug, Default, Copy, Clone, PartialEq)]
#[repr(u8)]
enum MessageType {
    LeaveInvite = 0,
    #[default]
    LookUp = 1,
    Delete = 2,
    Announce = 3,
}

impl TryFrom<u8> for MessageType {
    type Error = String;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(MessageType::LeaveInvite),
            1 => Ok(MessageType::LookUp),
            2 => Ok(MessageType::Delete),
            3 => Ok(MessageType::Announce),
            _ => Err(format!("Invalid message type: {}", value)),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(u8)]
#[allow(dead_code)] // Protocol completeness - not all variants used yet
enum Answer {
    Success = 0,
    NotHere = 1,
    Failed = 2,
    MachineUnknown = 3,
    PermissionDenied = 4,
    UnknownRequest = 5,
    BadVersion = 6,
    BadAddr = 7,
    BadCtlAddr = 8,
}

#[derive(Default, Debug, PartialEq, Clone)]
#[binrw]
struct Osockaddr {
    sa_family: SaFamily,
    sa_data: [u8; 14],
}

impl From<&Osockaddr> for SocketAddrV4 {
    fn from(value: &Osockaddr) -> Self {
        let port = u16::from_be_bytes([value.sa_data[0], value.sa_data[1]]);
        let ip = Ipv4Addr::new(
            value.sa_data[2],
            value.sa_data[3],
            value.sa_data[4],
            value.sa_data[5],
        );
        Self::new(ip, port)
    }
}

impl From<&SocketAddrV4> for Osockaddr {
    fn from(value: &SocketAddrV4) -> Self {
        let port: [u8; 2] = value.port().to_be_bytes();
        let octets: [u8; 4] = value.ip().octets();

        let mut result = Self::default();
        result.sa_data[0..2].copy_from_slice(&port);
        result.sa_data[2..6].copy_from_slice(&octets);
        result.sa_data[12..14].copy_from_slice(&[0, 2]);
        result
    }
}

/// Control message from talk client
#[binrw]
#[derive(Clone)]
struct CtlMsg {
    vers: u8,
    r#type: u8,
    answer: u8,
    pad: u8,
    id_num: u32,
    addr: Osockaddr,
    ctl_addr: Osockaddr,
    pid: i32,
    l_name: [i8; 12],
    r_name: [i8; 12],
    r_tty: [i8; 16],
}

impl CtlMsg {
    fn from_bytes(bytes: &[u8]) -> Result<Self, binrw::Error> {
        let mut cursor = Cursor::new(bytes);
        cursor.read_be()
    }

    fn local_name(&self) -> String {
        c_array_to_string(&self.l_name)
    }

    fn remote_name(&self) -> String {
        c_array_to_string(&self.r_name)
    }

    fn remote_tty(&self) -> String {
        c_array_to_string_16(&self.r_tty)
    }
}

/// Control response to talk client
#[binrw]
#[br(big)]
struct CtlRes {
    vers: u8,
    r#type: u8,
    answer: u8,
    pad: u8,
    id_num: u32,
    addr: Osockaddr,
}

impl CtlRes {
    fn new(msg_type: MessageType, answer: Answer, id_num: u32, addr: Osockaddr) -> Self {
        CtlRes {
            vers: TALK_VERSION,
            r#type: msg_type as u8,
            answer: answer as u8,
            pad: 0,
            id_num,
            addr,
        }
    }

    fn to_bytes(&self) -> io::Result<Vec<u8>> {
        let mut bytes = vec![0u8; CTL_RES_LEN];
        let mut cursor = Cursor::new(&mut bytes[..]);
        self.write_options(&mut cursor, Endian::Big, ())
            .map_err(|e| io::Error::other(e.to_string()))?;
        Ok(bytes)
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

fn c_array_to_string(arr: &[i8; 12]) -> String {
    let bytes: Vec<u8> = arr
        .iter()
        .take_while(|&&b| b != 0)
        .map(|&b| b as u8)
        .collect();
    String::from_utf8_lossy(&bytes).into_owned()
}

fn c_array_to_string_16(arr: &[i8; 16]) -> String {
    let bytes: Vec<u8> = arr
        .iter()
        .take_while(|&&b| b != 0)
        .map(|&b| b as u8)
        .collect();
    String::from_utf8_lossy(&bytes).into_owned()
}

// ============================================================================
// Invitation Registry
// ============================================================================

#[derive(Clone)]
#[allow(dead_code)] // Some fields needed for protocol but not used in local mode
struct Invitation {
    id: u32,
    caller: String,
    callee: String,
    caller_tty: String,
    callee_tty: String,
    tcp_addr: Osockaddr,
    ctl_addr: Osockaddr,
    timestamp: Instant,
}

struct InvitationRegistry {
    invitations: HashMap<u32, Invitation>,
    next_id: u32,
    timeout: Duration,
}

impl InvitationRegistry {
    fn new(timeout: Duration) -> Self {
        InvitationRegistry {
            invitations: HashMap::new(),
            next_id: 1,
            timeout,
        }
    }

    /// Drop every invitation past its timeout, returning how many were removed.
    fn cleanup_expired(&mut self) -> usize {
        let before = self.invitations.len();
        let now = Instant::now();
        self.invitations
            .retain(|_, inv| now.duration_since(inv.timestamp) < self.timeout);
        before - self.invitations.len()
    }

    fn find_for_callee(&self, callee: &str, caller: &str) -> Option<&Invitation> {
        self.invitations
            .values()
            .find(|inv| inv.callee == callee && inv.caller == caller)
    }

    #[cfg(test)]
    fn find_by_id(&self, id: u32) -> Option<&Invitation> {
        self.invitations.get(&id)
    }

    /// Insert an invitation, returning its id, or `None` when the table is at
    /// capacity (after an expiry sweep) — a bound against unauthenticated flood.
    fn insert(&mut self, invitation: Invitation) -> Option<u32> {
        if self.invitations.len() >= MAX_INVITATIONS {
            let _ = self.cleanup_expired();
            if self.invitations.len() >= MAX_INVITATIONS {
                return None;
            }
        }
        let id = self.next_id;
        self.next_id = self.next_id.wrapping_add(1);
        self.invitations.insert(id, Invitation { id, ..invitation });
        Some(id)
    }

    /// Refresh an existing (callee, caller) invitation's address and timestamp,
    /// returning its id. Used by LEAVE_INVITE so a re-invite does not hand out a
    /// stale TCP rendezvous address (audit #TD11).
    fn refresh(&mut self, callee: &str, caller: &str, tcp_addr: &Osockaddr) -> Option<u32> {
        let entry = self
            .invitations
            .values_mut()
            .find(|inv| inv.callee == callee && inv.caller == caller)?;
        entry.tcp_addr = tcp_addr.clone();
        entry.timestamp = Instant::now();
        Some(entry.id)
    }

    #[cfg(test)]
    fn delete(&mut self, id: u32) -> bool {
        self.invitations.remove(&id).is_some()
    }

    /// Remove one invitation, identified by the id the client was given.
    ///
    /// The caller name is checked too, so a client cannot cancel an invitation
    /// it does not own by guessing an id. Deleting by name alone (audit #TD13)
    /// let a single datagram cancel every session belonging to a caller, and on
    /// an unauthenticated socket that is a cancel-anyone primitive.
    fn delete_by_id(&mut self, id: u32, caller: &str) -> bool {
        match self.invitations.get(&id) {
            Some(inv) if inv.caller == caller => {
                self.invitations.remove(&id);
                true
            }
            _ => false,
        }
    }
}

// ============================================================================
// Message Handlers
// ============================================================================

fn handle_lookup(registry: &InvitationRegistry, msg: &CtlMsg) -> CtlRes {
    let callee = msg.local_name(); // The one doing lookup is the callee
    let caller = msg.remote_name(); // Looking for invitation from caller

    if let Some(invitation) = registry.find_for_callee(&callee, &caller) {
        CtlRes::new(
            MessageType::LookUp,
            Answer::Success,
            invitation.id,
            invitation.tcp_addr.clone(),
        )
    } else {
        CtlRes::new(
            MessageType::LookUp,
            Answer::NotHere,
            0,
            Osockaddr::default(),
        )
    }
}

/// Verify the callee exists and is logged in, honor their `mesg` permission,
/// and write the talk announcement to their terminal (audit #TD1/#TD2/#TD3).
/// Returns the `Answer` to report to the caller.
fn announce_to_tty(caller: &str, callee: &str, requested_tty: &str) -> Answer {
    // The callee must exist in the password database.
    let c_callee = match CString::new(callee) {
        Ok(c) => c,
        Err(_) => return Answer::Failed,
    };
    if unsafe { libc::getpwnam(c_callee.as_ptr()).is_null() } {
        return Answer::NotHere;
    }

    // The callee must be logged in; pick the requested tty if given, else the
    // first login terminal.
    let want = (!requested_tty.is_empty()).then_some(requested_tty);
    let user_tty = match plib::tty::user_tty(callee, want) {
        Some(t) => t,
        None => return Answer::NotHere,
    };

    // A single open validates the path, lets the kernel enforce write access,
    // and yields the handle we write through -- the permission check and the
    // write cannot end up on different inodes. `open_for_write` also passes
    // O_NOCTTY, without which the first announcement would make the
    // *recipient's* terminal this daemon's controlling terminal.
    let mut f = match plib::tty::open_for_write(&user_tty.path) {
        Ok(f) => f,
        Err(plib::tty::OpenError::PermissionDenied) => return Answer::PermissionDenied,
        Err(plib::tty::OpenError::Io(e)) if e.kind() == io::ErrorKind::PermissionDenied => {
            return Answer::PermissionDenied
        }
        Err(_) => return Answer::Failed,
    };

    // Unlike `write`, talkd must honor `mesg n` even when running as root: the
    // announcement is triggered by an unauthenticated datagram, so the
    // recipient's stated wish is the only consent that exists. `may_write`
    // (applied inside `open_for_write`) exempts the superuser, so re-check
    // `mesg_allowed` on its own here.
    match plib::tty::fstat(f.as_raw_fd()) {
        Ok(st) if plib::tty::mesg_allowed(&st) => {}
        Ok(_) => return Answer::PermissionDenied,
        Err(_) => return Answer::Failed,
    }

    if write_announcement(&mut f, caller).is_err() {
        return Answer::Failed;
    }
    Answer::Success
}

/// Write the talk announcement banner to an already-open recipient terminal.
fn write_announcement(f: &mut fs::File, caller: &str) -> io::Result<()> {
    // SECURITY: `caller` comes from an untrusted datagram and is written to the
    // recipient's terminal. Strip control characters so a hostile name cannot
    // inject terminal escape sequences (cursor/screen manipulation), and bound
    // the length. A real POSIX login name survives this unchanged.
    let safe_caller: String = caller
        .chars()
        .filter(|c| !c.is_control())
        .take(32)
        .collect();

    let banner = format!(
        "\x07\r\nMessage from Talk_Daemon...\r\n\
         talk: connection requested by {safe_caller}.\r\n\
         talk: respond with:  talk {safe_caller}\r\n"
    );
    f.write_all(banner.as_bytes())
}

fn handle_announce(registry: &mut InvitationRegistry, msg: &CtlMsg) -> CtlRes {
    let caller = msg.local_name();
    let callee = msg.remote_name();
    let callee_tty = msg.remote_tty();

    // Notify the callee on their terminal (validating user/login/mesg first).
    let answer = announce_to_tty(&caller, &callee, &callee_tty);
    if answer != Answer::Success {
        return CtlRes::new(MessageType::Announce, answer, 0, Osockaddr::default());
    }

    let invitation = Invitation {
        id: 0, // Will be set by insert
        caller,
        callee,
        caller_tty: String::new(),
        callee_tty,
        tcp_addr: msg.addr.clone(),
        ctl_addr: msg.ctl_addr.clone(),
        timestamp: Instant::now(),
    };

    match registry.insert(invitation) {
        Some(id) => CtlRes::new(
            MessageType::Announce,
            Answer::Success,
            id,
            Osockaddr::default(),
        ),
        None => CtlRes::new(
            MessageType::Announce,
            Answer::Failed,
            0,
            Osockaddr::default(),
        ),
    }
}

fn handle_leave_invite(registry: &mut InvitationRegistry, msg: &CtlMsg) -> CtlRes {
    let caller = msg.local_name();
    let callee = msg.remote_name();
    let callee_tty = msg.remote_tty();

    // If an invitation for this (callee, caller) pair already exists, refresh
    // its address + timestamp rather than returning a stale entry (#TD11).
    if let Some(id) = registry.refresh(&callee, &caller, &msg.addr) {
        return CtlRes::new(
            MessageType::LeaveInvite,
            Answer::Success,
            id,
            Osockaddr::default(),
        );
    }

    let invitation = Invitation {
        id: 0,
        caller,
        callee,
        caller_tty: String::new(),
        callee_tty,
        tcp_addr: msg.addr.clone(),
        ctl_addr: msg.ctl_addr.clone(),
        timestamp: Instant::now(),
    };

    match registry.insert(invitation) {
        Some(id) => CtlRes::new(
            MessageType::LeaveInvite,
            Answer::Success,
            id,
            Osockaddr::default(),
        ),
        None => CtlRes::new(
            MessageType::LeaveInvite,
            Answer::Failed,
            0,
            Osockaddr::default(),
        ),
    }
}

fn handle_delete(registry: &mut InvitationRegistry, msg: &CtlMsg) -> CtlRes {
    let caller = msg.local_name();

    // BSD talkd deletes the single invitation named by id_num. Reporting
    // success for an id that was never ours would tell a client its teardown
    // worked when nothing happened.
    let answer = if registry.delete_by_id(msg.id_num, &caller) {
        Answer::Success
    } else {
        Answer::Failed
    };

    CtlRes::new(
        MessageType::Delete,
        answer,
        msg.id_num,
        Osockaddr::default(),
    )
}

// ============================================================================
// Daemon Main Loop
// ============================================================================

/// Bounds what the daemon emits in response to unauthenticated garbage.
///
/// Every malformed datagram used to produce both a log line and, for a bad
/// version or an unknown type, a reply datagram sent to an address the sender
/// chose. A tight send loop therefore turned the daemon into a syslog flood
/// and a packet reflector. Well-formed requests are never throttled, so
/// legitimate clients are unaffected.
///
/// Deliberately global rather than per-peer: `talk` binds a fresh socket path
/// per request, so a peer-keyed table would never see a repeat key *and* would
/// itself grow without bound — a second denial of service.
struct ErrorLimiter {
    tokens: u32,
    last_refill: Instant,
    suppressed: u64,
    last_report: Instant,
}

impl ErrorLimiter {
    fn new() -> Self {
        let now = Instant::now();
        ErrorLimiter {
            tokens: ERR_REPLY_BURST,
            last_refill: now,
            suppressed: 0,
            last_report: now,
        }
    }

    /// Consume a token. `false` means this reply and its log line are dropped.
    fn allow(&mut self) -> bool {
        let now = Instant::now();
        let elapsed = now.duration_since(self.last_refill).as_secs();
        if elapsed > 0 {
            let refill = elapsed.saturating_mul(ERR_REPLY_PER_SEC as u64);
            self.tokens = self
                .tokens
                .saturating_add(refill.min(u32::MAX as u64) as u32)
                .min(ERR_REPLY_BURST);
            // Advance by whole seconds only, so sub-second remainders are not
            // discarded by repeated calls.
            self.last_refill += Duration::from_secs(elapsed);
        }

        if self.tokens > 0 {
            self.tokens -= 1;
            true
        } else {
            self.suppressed += 1;
            false
        }
    }

    /// Emit at most one aggregate line per `SUPPRESS_REPORT_SECS`, so a flood
    /// leaves a trace without becoming the flood itself.
    fn flush_suppressed(&mut self) {
        if self.suppressed == 0 {
            return;
        }
        let now = Instant::now();
        if now.duration_since(self.last_report) < Duration::from_secs(SUPPRESS_REPORT_SECS) {
            return;
        }
        log_err(&format!(
            "{} {}",
            gettext("suppressed malformed datagrams:"),
            self.suppressed
        ));
        self.suppressed = 0;
        self.last_report = now;
    }
}

/// Whether `e` is a `SO_RCVTIMEO` expiry rather than a real error. Platforms
/// differ: Linux reports `EAGAIN` (`WouldBlock`), others `ETIMEDOUT`.
fn is_recv_timeout(e: &io::Error) -> bool {
    matches!(
        e.kind(),
        io::ErrorKind::WouldBlock | io::ErrorKind::TimedOut
    )
}

/// Expire stale invitations, logging only when something was actually removed.
fn sweep(registry: &mut InvitationRegistry) {
    let n = registry.cleanup_expired();
    if n > 0 {
        log_info(&format!("{} {}", gettext("expired invitations:"), n));
    }
}

fn daemon_loop(socket_path: &Path, invite_timeout: Duration, tick: Duration) -> io::Result<()> {
    // Symlink safety (#TD5): inspect the path without following links. Only
    // unlink a pre-existing path if it is a socket we own; never follow a
    // symlink an attacker may have planted to make us unlink an arbitrary file.
    match fs::symlink_metadata(socket_path) {
        Ok(meta) => {
            if !meta.file_type().is_socket() {
                return Err(io::Error::other(format!(
                    "refusing to replace non-socket path {:?}",
                    socket_path
                )));
            }
            fs::remove_file(socket_path)?;
        }
        Err(e) if e.kind() == io::ErrorKind::NotFound => {}
        Err(e) => return Err(e),
    }

    // Restrict the socket mode to 0600 by tightening the umask across bind.
    let old_umask = unsafe { libc::umask(0o177) };
    let bind_result = UnixDatagram::bind(socket_path);
    unsafe { libc::umask(old_umask) };
    let socket = bind_result?;
    // Belt-and-suspenders: enforce 0600 explicitly.
    let _ = fs::set_permissions(socket_path, fs::Permissions::from_mode(0o600));

    // Publish the socket path for the signal handler only now that the bind has
    // succeeded, so a signal arriving during startup cannot unlink a path this
    // process never created.
    publish_socket_path(socket_path)?;

    // Wake periodically even with no traffic, so invitations expire on time
    // rather than only when the next datagram happens to arrive.
    socket.set_read_timeout(Some(tick))?;

    let mut registry = InvitationRegistry::new(invite_timeout);
    let mut limiter = ErrorLimiter::new();
    let mut buf = [0u8; 1024];

    log_info(&format!("{} {:?}", gettext("listening on"), socket_path));

    loop {
        // Receive message from client
        let (len, client_addr) = match socket.recv_from(&mut buf) {
            Ok(r) => r,
            // The read timeout expired: no traffic. Sweep and wait again. This
            // is the normal idle path and must not be logged.
            Err(e) if is_recv_timeout(&e) => {
                sweep(&mut registry);
                limiter.flush_suppressed();
                continue;
            }
            // Reachable now that the receive has a timeout: a timed-out socket
            // receive is not restarted after a handler runs, even under
            // SA_RESTART.
            Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
            Err(e) => {
                log_err(&format!("{}: {}", gettext("recv error"), e));
                continue;
            }
        };

        // Clean up expired invitations
        sweep(&mut registry);

        // Parse the control message (a short/garbage datagram is dropped, not fatal)
        let msg = match CtlMsg::from_bytes(&buf[..len]) {
            Ok(m) => m,
            Err(e) => {
                if limiter.allow() {
                    log_err(&format!("{}: {}", gettext("failed to parse message"), e));
                }
                continue;
            }
        };

        // Validate version
        if msg.vers != TALK_VERSION {
            if limiter.allow() {
                let response = CtlRes::new(
                    MessageType::try_from(msg.r#type).unwrap_or(MessageType::LookUp),
                    Answer::BadVersion,
                    msg.id_num,
                    Osockaddr::default(),
                );
                if let Ok(bytes) = response.to_bytes() {
                    let _ = socket.send_to_addr(&bytes, &client_addr);
                }
            }
            continue;
        }

        // Handle the message based on type
        let msg_type = match MessageType::try_from(msg.r#type) {
            Ok(t) => t,
            Err(_) => {
                if limiter.allow() {
                    let response = CtlRes::new(
                        MessageType::LookUp,
                        Answer::UnknownRequest,
                        msg.id_num,
                        Osockaddr::default(),
                    );
                    if let Ok(bytes) = response.to_bytes() {
                        let _ = socket.send_to_addr(&bytes, &client_addr);
                    }
                }
                continue;
            }
        };

        let response = match msg_type {
            MessageType::LookUp => handle_lookup(&registry, &msg),
            MessageType::Announce => handle_announce(&mut registry, &msg),
            MessageType::LeaveInvite => handle_leave_invite(&mut registry, &msg),
            MessageType::Delete => handle_delete(&mut registry, &msg),
        };

        // Send response
        if let Ok(bytes) = response.to_bytes() {
            if let Err(e) = socket.send_to_addr(&bytes, &client_addr) {
                log_err(&format!("{}: {}", gettext("failed to send response"), e));
            }
        }
    }
}

/// Operational logging. In foreground mode messages go to stderr; once
/// daemonized (no controlling terminal), they go to syslog (LOG_DAEMON).
fn log_info(msg: &str) {
    if DAEMONIZED.get().copied().unwrap_or(false) {
        syslog_line(Level::Info, msg);
    } else {
        eprintln!("talkd: {}", msg);
    }
}

fn log_err(msg: &str) {
    if DAEMONIZED.get().copied().unwrap_or(false) {
        syslog_line(Level::Err, msg);
    } else {
        eprintln!("talkd: {}", msg);
    }
}

fn syslog_line(level: Level, msg: &str) {
    // `open` is a no-op after the first call, so the identity is established
    // once and every later line reuses it.
    plib::syslog::open("talkd", true, Facility::DAEMON);
    plib::syslog::log(level, msg);
}

// ============================================================================
// Signal Handling
// ============================================================================

/// NUL-terminated socket path published for the signal handler.
///
/// Built and leaked *before* the pointer is stored, so the handler performs
/// only an atomic load — never an allocation, a `PathBuf`, or a `OnceLock`
/// probe. Null means "nothing to unlink yet".
static SOCKET_PATH_C: AtomicPtr<libc::c_char> = AtomicPtr::new(std::ptr::null_mut());

/// Whether the daemon has detached from its controlling terminal (selects the
/// logging sink).
static DAEMONIZED: OnceLock<bool> = OnceLock::new();

/// Async-signal-safe teardown.
///
/// POSIX.1-2024 XSH 2.4.3 lists `unlink()` and `_exit()` as async-signal-safe,
/// and makes every atomic operation other than initialization so. Nothing else
/// happens here. In particular there is no `std::process::exit`, which would
/// run atexit handlers and TLS destructors from a signal context, and no
/// `fs::remove_file`, which allocates a `CString` from the path.
extern "C" fn handle_signal(_sig: libc::c_int) {
    let p = SOCKET_PATH_C.load(Ordering::Acquire);
    if !p.is_null() {
        // SAFETY: `p` was published from a leaked CString and stays valid for
        // the life of the process. `unlink` clobbers errno, which does not
        // matter because the next statement never returns.
        unsafe { libc::unlink(p) };
    }
    unsafe { libc::_exit(0) };
}

/// Install `handler` for `signum` via `sigaction`.
///
/// `SA_RESTART` is deliberately cleared: these handlers never return, so
/// restart semantics are immaterial to them, and a timed-out socket receive is
/// not restarted regardless. Matches the precedent in `cron/crond.rs`.
fn install_signal(signum: libc::c_int, handler: extern "C" fn(libc::c_int)) {
    unsafe {
        let mut act: libc::sigaction = std::mem::zeroed();
        act.sa_sigaction = handler as libc::sighandler_t;
        act.sa_flags = 0;
        libc::sigemptyset(&mut act.sa_mask);
        libc::sigaction(signum, &act, std::ptr::null_mut());
    }
}

/// Install signal dispositions. Called before `bind`, so the default
/// *terminate* disposition is never in force during startup.
fn install_signal_handlers() {
    for sig in [libc::SIGINT, libc::SIGTERM, libc::SIGQUIT, libc::SIGHUP] {
        install_signal(sig, handle_signal);
    }
}

/// Publish the socket path for the handler. Called only after a successful
/// bind, so a signal during startup cannot unlink a path we did not create.
fn publish_socket_path(path: &Path) -> io::Result<()> {
    let c = CString::new(path.as_os_str().as_bytes())
        .map_err(|_| io::Error::other("socket path contains NUL"))?;
    // Leaked on purpose: a signal can arrive at any time, so the pointer must
    // outlive every scope. One allocation for the life of the process.
    SOCKET_PATH_C.store(c.into_raw(), Ordering::Release);
    Ok(())
}

/// Detach from the controlling terminal (audit #TD9): double-fork, `setsid`,
/// `chdir("/")`, and redirect the standard descriptors to `/dev/null`.
fn daemonize() -> io::Result<()> {
    // First fork: the parent exits so the child is not a process-group leader.
    match unsafe { libc::fork() } {
        -1 => return Err(io::Error::last_os_error()),
        0 => {}
        _ => process::exit(0),
    }
    if unsafe { libc::setsid() } < 0 {
        return Err(io::Error::last_os_error());
    }
    // Second fork: ensure we can never reacquire a controlling terminal.
    match unsafe { libc::fork() } {
        -1 => return Err(io::Error::last_os_error()),
        0 => {}
        _ => process::exit(0),
    }
    unsafe {
        libc::chdir(c"/".as_ptr());
        let nullfd = libc::open(c"/dev/null".as_ptr(), libc::O_RDWR);
        if nullfd >= 0 {
            libc::dup2(nullfd, libc::STDIN_FILENO);
            libc::dup2(nullfd, libc::STDOUT_FILENO);
            libc::dup2(nullfd, libc::STDERR_FILENO);
            if nullfd > libc::STDERR_FILENO {
                libc::close(nullfd);
            }
        }
    }
    Ok(())
}

// ============================================================================
// Main
// ============================================================================

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("talkd");

    let args = Args::parse();

    // Detach unless asked to stay in the foreground (#TD9).
    if !args.foreground {
        if let Err(e) = daemonize() {
            plib::diag::error(&format!("{}: {}", gettext("failed to daemonize"), e));
            std::process::exit(1);
        }
        let _ = DAEMONIZED.set(true);
    } else {
        let _ = DAEMONIZED.set(false);
    }

    install_signal_handlers();

    let invite_timeout = Duration::from_secs(args.invite_timeout.max(1));
    let tick = Duration::from_secs(args.expiry_tick.max(1));

    if let Err(e) = daemon_loop(&args.socket, invite_timeout, tick) {
        log_err(&e.to_string());
        // Clean up socket on error
        let _ = fs::remove_file(&args.socket);
        std::process::exit(1);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_invitation_registry_insert_find() {
        let mut registry = InvitationRegistry::new(Duration::from_secs(INVITATION_TIMEOUT_SECS));

        let inv = Invitation {
            id: 0,
            caller: "alice".to_string(),
            callee: "bob".to_string(),
            caller_tty: "pts/0".to_string(),
            callee_tty: "pts/1".to_string(),
            tcp_addr: Osockaddr::default(),
            ctl_addr: Osockaddr::default(),
            timestamp: Instant::now(),
        };

        let id = registry.insert(inv).expect("insert should succeed");
        assert!(registry.find_by_id(id).is_some());
        assert!(registry.find_for_callee("bob", "alice").is_some());
        assert!(registry.find_for_callee("alice", "bob").is_none());
    }

    #[test]
    fn test_invitation_registry_delete() {
        let mut registry = InvitationRegistry::new(Duration::from_secs(INVITATION_TIMEOUT_SECS));

        let inv = Invitation {
            id: 0,
            caller: "alice".to_string(),
            callee: "bob".to_string(),
            caller_tty: String::new(),
            callee_tty: String::new(),
            tcp_addr: Osockaddr::default(),
            ctl_addr: Osockaddr::default(),
            timestamp: Instant::now(),
        };

        let id = registry.insert(inv).expect("insert should succeed");
        assert!(registry.delete(id));
        assert!(registry.find_by_id(id).is_none());
    }

    #[test]
    fn test_message_type_conversion() {
        assert_eq!(MessageType::try_from(0u8), Ok(MessageType::LeaveInvite));
        assert_eq!(MessageType::try_from(1u8), Ok(MessageType::LookUp));
        assert_eq!(MessageType::try_from(2u8), Ok(MessageType::Delete));
        assert_eq!(MessageType::try_from(3u8), Ok(MessageType::Announce));
        assert!(MessageType::try_from(4u8).is_err());
    }

    #[test]
    fn test_c_array_to_string() {
        let arr: [i8; 12] = [
            b'h' as i8, b'e' as i8, b'l' as i8, b'l' as i8, b'o' as i8, 0, 0, 0, 0, 0, 0, 0,
        ];
        assert_eq!(c_array_to_string(&arr), "hello");
    }

    fn sample_invitation(caller: &str, callee: &str) -> Invitation {
        Invitation {
            id: 0,
            caller: caller.to_string(),
            callee: callee.to_string(),
            caller_tty: String::new(),
            callee_tty: String::new(),
            tcp_addr: Osockaddr::default(),
            ctl_addr: Osockaddr::default(),
            timestamp: Instant::now(),
        }
    }

    #[test]
    fn test_invitation_table_is_bounded() {
        // The table must refuse growth past MAX_INVITATIONS (DoS guard, #TD4).
        let mut registry = InvitationRegistry::new(Duration::from_secs(INVITATION_TIMEOUT_SECS));
        for i in 0..MAX_INVITATIONS {
            assert!(registry
                .insert(sample_invitation(&format!("c{i}"), &format!("d{i}")))
                .is_some());
        }
        assert!(
            registry
                .insert(sample_invitation("overflow", "x"))
                .is_none(),
            "insert past the cap must be rejected"
        );
    }

    #[test]
    fn test_leave_invite_refresh_updates_address() {
        // A re-invite for the same (callee, caller) refreshes the stored TCP
        // address instead of leaving a stale one (#TD11).
        let mut registry = InvitationRegistry::new(Duration::from_secs(INVITATION_TIMEOUT_SECS));
        let id = registry
            .insert(sample_invitation("alice", "bob"))
            .expect("insert");

        let new_addr = Osockaddr::from(&SocketAddrV4::new(Ipv4Addr::new(10, 0, 0, 9), 4242));
        let refreshed = registry.refresh("bob", "alice", &new_addr);
        assert_eq!(refreshed, Some(id), "refresh keeps the same id");
        assert_eq!(
            registry.find_by_id(id).unwrap().tcp_addr,
            new_addr,
            "refresh updates the TCP address"
        );
    }

    #[test]
    fn test_caller_name_control_chars_stripped() {
        // The sanitization applied before a tty write must drop control bytes
        // (terminal-escape-injection guard) while preserving a normal name.
        let hostile = "bob\x1b[2J\x07\r\nevil";
        let safe: String = hostile
            .chars()
            .filter(|c| !c.is_control())
            .take(32)
            .collect();
        assert_eq!(safe, "bob[2Jevil");
        assert!(!safe.chars().any(|c| c.is_control()));

        let normal = "alice.dev_1";
        let safe_normal: String = normal
            .chars()
            .filter(|c| !c.is_control())
            .take(32)
            .collect();
        assert_eq!(safe_normal, normal, "a normal login name is unchanged");
    }

    #[test]
    fn test_ctlmsg_wire_round_trip() {
        // A CtlRes serializes to a fixed-size big-endian buffer and a truncated
        // datagram fails to parse (graceful drop, not a panic) (#TD12).
        let res = CtlRes::new(
            MessageType::LookUp,
            Answer::Success,
            0x01020304,
            Osockaddr::default(),
        );
        let bytes = res.to_bytes().expect("serialize");
        assert_eq!(bytes.len(), size_of::<CtlRes>());

        // A short buffer must be rejected by the parser, not panic.
        assert!(CtlMsg::from_bytes(&[0u8; 4]).is_err());
        // A zero-length datagram is also a graceful parse error.
        assert!(CtlMsg::from_bytes(&[]).is_err());
    }

    #[test]
    fn test_delete_is_id_scoped_and_ownership_checked() {
        // Audit #TD13: DELETE must remove exactly the invitation named by
        // id_num, and only for its own caller.
        let mut registry = InvitationRegistry::new(Duration::from_secs(INVITATION_TIMEOUT_SECS));
        let mk = |callee: &str| Invitation {
            id: 0,
            caller: "alice".to_string(),
            callee: callee.to_string(),
            caller_tty: String::new(),
            callee_tty: String::new(),
            tcp_addr: Osockaddr::default(),
            ctl_addr: Osockaddr::default(),
            timestamp: Instant::now(),
        };

        let first = registry.insert(mk("bob")).expect("insert");
        let second = registry.insert(mk("carol")).expect("insert");

        // Deleting one leaves the other alone -- the old delete-by-name removed
        // every invitation belonging to the caller.
        assert!(registry.delete_by_id(first, "alice"));
        assert!(registry.find_by_id(second).is_some());

        // A different caller cannot cancel someone else's invitation.
        assert!(!registry.delete_by_id(second, "mallory"));
        assert!(registry.find_by_id(second).is_some());

        // An unknown id is a failure, not a forged success.
        assert!(!registry.delete_by_id(9999, "alice"));
    }
}
