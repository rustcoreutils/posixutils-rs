//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! talkd - Local talk daemon for testing
//!
//! This is a local-only implementation of the talk daemon that uses Unix
//! domain sockets instead of the network UDP protocol. It maintains an
//! invitation registry and facilitates connections between talk clients.

use binrw::{binrw, BinReaderExt, BinWrite, Endian};
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::collections::HashMap;
use std::fs;
use std::io::{self, Cursor};
use std::mem::size_of;
use std::net::{Ipv4Addr, SocketAddrV4};
use std::os::unix::net::UnixDatagram;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

#[cfg(target_os = "macos")]
type SaFamily = u16;

#[cfg(target_os = "linux")]
use libc::sa_family_t;
#[cfg(target_os = "linux")]
type SaFamily = sa_family_t;

/// Default socket path for local talkd (proper system location)
pub const DEFAULT_SOCKET_PATH: &str = "/var/run/talkd.sock";

/// Invitation timeout in seconds
const INVITATION_TIMEOUT_SECS: u64 = 60;

/// Protocol version
const TALK_VERSION: u8 = 1;

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

#[derive(Default, PartialEq, Clone)]
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
        let mut bytes = vec![0u8; size_of::<CtlRes>()];
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
}

impl InvitationRegistry {
    fn new() -> Self {
        InvitationRegistry {
            invitations: HashMap::new(),
            next_id: 1,
        }
    }

    fn cleanup_expired(&mut self) {
        let timeout = Duration::from_secs(INVITATION_TIMEOUT_SECS);
        let now = Instant::now();
        self.invitations
            .retain(|_, inv| now.duration_since(inv.timestamp) < timeout);
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

    fn insert(&mut self, invitation: Invitation) -> u32 {
        let id = self.next_id;
        self.next_id = self.next_id.wrapping_add(1);
        self.invitations.insert(id, Invitation { id, ..invitation });
        id
    }

    #[cfg(test)]
    fn delete(&mut self, id: u32) -> bool {
        self.invitations.remove(&id).is_some()
    }

    fn delete_by_caller(&mut self, caller: &str) {
        self.invitations.retain(|_, inv| inv.caller != caller);
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

fn handle_announce(registry: &mut InvitationRegistry, msg: &CtlMsg) -> CtlRes {
    // For now, just acknowledge - in a real daemon this would notify the callee
    // In our local testing setup, we just store the invitation
    let caller = msg.local_name();
    let callee = msg.remote_name();
    let callee_tty = msg.remote_tty();

    let invitation = Invitation {
        id: 0, // Will be set by insert
        caller: caller.clone(),
        callee,
        caller_tty: String::new(),
        callee_tty,
        tcp_addr: msg.addr.clone(),
        ctl_addr: msg.ctl_addr.clone(),
        timestamp: Instant::now(),
    };

    let id = registry.insert(invitation);

    CtlRes::new(
        MessageType::Announce,
        Answer::Success,
        id,
        Osockaddr::default(),
    )
}

fn handle_leave_invite(registry: &mut InvitationRegistry, msg: &CtlMsg) -> CtlRes {
    let caller = msg.local_name();
    let callee = msg.remote_name();
    let callee_tty = msg.remote_tty();

    // Check if invitation already exists
    if let Some(existing) = registry.find_for_callee(&callee, &caller) {
        return CtlRes::new(
            MessageType::LeaveInvite,
            Answer::Success,
            existing.id,
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

    let id = registry.insert(invitation);

    CtlRes::new(
        MessageType::LeaveInvite,
        Answer::Success,
        id,
        Osockaddr::default(),
    )
}

fn handle_delete(registry: &mut InvitationRegistry, msg: &CtlMsg) -> CtlRes {
    let caller = msg.local_name();

    // Delete all invitations from this caller
    registry.delete_by_caller(&caller);

    CtlRes::new(
        MessageType::Delete,
        Answer::Success,
        msg.id_num,
        Osockaddr::default(),
    )
}

// ============================================================================
// Daemon Main Loop
// ============================================================================

fn daemon_loop(socket_path: &Path) -> io::Result<()> {
    // Remove existing socket file if present
    if socket_path.exists() {
        fs::remove_file(socket_path)?;
    }

    let socket = UnixDatagram::bind(socket_path)?;
    let mut registry = InvitationRegistry::new();
    let mut buf = [0u8; 1024];

    eprintln!("talkd: listening on {:?}", socket_path);

    loop {
        // Receive message from client
        let (len, client_addr) = match socket.recv_from(&mut buf) {
            Ok(r) => r,
            Err(e) => {
                eprintln!("talkd: recv error: {}", e);
                continue;
            }
        };

        // Clean up expired invitations periodically
        registry.cleanup_expired();

        // Parse the control message
        let msg = match CtlMsg::from_bytes(&buf[..len]) {
            Ok(m) => m,
            Err(e) => {
                eprintln!("talkd: failed to parse message: {}", e);
                continue;
            }
        };

        // Validate version
        if msg.vers != TALK_VERSION {
            let response = CtlRes::new(
                MessageType::try_from(msg.r#type).unwrap_or(MessageType::LookUp),
                Answer::BadVersion,
                msg.id_num,
                Osockaddr::default(),
            );
            if let Ok(bytes) = response.to_bytes() {
                let _ = socket.send_to_addr(&bytes, &client_addr);
            }
            continue;
        }

        // Handle the message based on type
        let msg_type = match MessageType::try_from(msg.r#type) {
            Ok(t) => t,
            Err(_) => {
                let response = CtlRes::new(
                    MessageType::LookUp,
                    Answer::UnknownRequest,
                    msg.id_num,
                    Osockaddr::default(),
                );
                if let Ok(bytes) = response.to_bytes() {
                    let _ = socket.send_to_addr(&bytes, &client_addr);
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
                eprintln!("talkd: failed to send response: {}", e);
            }
        }
    }
}

// ============================================================================
// Signal Handling
// ============================================================================

static mut SOCKET_PATH: Option<PathBuf> = None;

extern "C" fn handle_signal(_sig: libc::c_int) {
    // Clean up socket file on exit
    unsafe {
        if let Some(ref path) = SOCKET_PATH {
            let _ = fs::remove_file(path);
        }
    }
    std::process::exit(0);
}

fn register_signals(socket_path: &Path) {
    unsafe {
        SOCKET_PATH = Some(socket_path.to_path_buf());
        libc::signal(
            libc::SIGINT,
            handle_signal as *const () as libc::sighandler_t,
        );
        libc::signal(
            libc::SIGTERM,
            handle_signal as *const () as libc::sighandler_t,
        );
        libc::signal(
            libc::SIGQUIT,
            handle_signal as *const () as libc::sighandler_t,
        );
    }
}

// ============================================================================
// Main
// ============================================================================

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    register_signals(&args.socket);

    if let Err(e) = daemon_loop(&args.socket) {
        eprintln!("talkd: {}", e);
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
        let mut registry = InvitationRegistry::new();

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

        let id = registry.insert(inv);
        assert!(registry.find_by_id(id).is_some());
        assert!(registry.find_for_callee("bob", "alice").is_some());
        assert!(registry.find_for_callee("alice", "bob").is_none());
    }

    #[test]
    fn test_invitation_registry_delete() {
        let mut registry = InvitationRegistry::new();

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

        let id = registry.insert(inv);
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
}
