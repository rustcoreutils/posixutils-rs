//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Wire-protocol integration tests for `talkd`.
//!
//! The control messages are **hand-rolled here** rather than reused from
//! `talkd.rs`. Partly out of necessity — the protocol types are private to a
//! `[[bin]]` target — but mainly on purpose: a test that encodes with the
//! implementation's own serializer cannot detect a layout bug, because both
//! sides would drift together. An independent encoder is a strictly stronger
//! check of a wire format.
//!
//! Every test self-skips when the `talkd` binary is absent, and all of them
//! serialize on the shared talk/talkd lock.

use std::os::unix::net::UnixDatagram;
use std::path::{Path, PathBuf};
use std::time::Duration;

use crate::common::{
    acquire_lock, drain_stderr, start_talkd_with, stop_talkd, test_socket_path, wait_for_line,
};

// ============================================================================
// Wire format
//
// BSD ntalk CTL_MSG, big-endian, 84 bytes, no padding:
//    0  vers u8    1  type u8    2  answer u8    3  pad u8
//    4  id_num u32
//    8  addr:     sa_family u16 + sa_data [u8; 14]   (16 bytes)
//   24  ctl_addr: sa_family u16 + sa_data [u8; 14]   (16 bytes)
//   40  pid i32
//   44  l_name [12]   56  r_name [12]   68  r_tty [16]
//
// CTL_RESPONSE, big-endian, 24 bytes:
//    0  vers, type, answer, pad     4  id_num u32     8  addr (16 bytes)
// ============================================================================

const CTL_MSG_LEN: usize = 84;
const CTL_RES_LEN: usize = 24;

// Message types.
const T_LEAVE_INVITE: u8 = 0;
const T_LOOK_UP: u8 = 1;
const T_DELETE: u8 = 2;

// Answers.
const A_SUCCESS: u8 = 0;
const A_NOT_HERE: u8 = 1;
const A_UNKNOWN_REQUEST: u8 = 5;
const A_BAD_VERSION: u8 = 6;

const TALK_VERSION: u8 = 1;

/// A fixed-width, NUL-padded name field.
fn name_field(s: &str, len: usize) -> Vec<u8> {
    let mut v = s.as_bytes().to_vec();
    v.truncate(len);
    v.resize(len, 0);
    v
}

/// A `sockaddr_in` in the protocol's 16-byte form.
fn sockaddr(port: u16, ip: [u8; 4]) -> Vec<u8> {
    let mut v = Vec::with_capacity(16);
    v.extend_from_slice(&2u16.to_be_bytes()); // AF_INET
    v.extend_from_slice(&port.to_be_bytes());
    v.extend_from_slice(&ip);
    v.resize(16, 0);
    v
}

struct Msg {
    vers: u8,
    typ: u8,
    id_num: u32,
    addr: Vec<u8>,
    l_name: String,
    r_name: String,
    r_tty: String,
}

impl Msg {
    fn new(typ: u8, l_name: &str, r_name: &str) -> Self {
        Msg {
            vers: TALK_VERSION,
            typ,
            id_num: 0,
            addr: sockaddr(0, [0, 0, 0, 0]),
            l_name: l_name.to_string(),
            r_name: r_name.to_string(),
            r_tty: String::new(),
        }
    }

    fn encode(&self) -> Vec<u8> {
        let mut v = Vec::with_capacity(CTL_MSG_LEN);
        v.push(self.vers);
        v.push(self.typ);
        v.push(0); // answer (unused in a request)
        v.push(0); // pad
        v.extend_from_slice(&self.id_num.to_be_bytes());
        v.extend_from_slice(&self.addr);
        v.extend_from_slice(&sockaddr(0, [0, 0, 0, 0])); // ctl_addr
        v.extend_from_slice(&0i32.to_be_bytes()); // pid
        v.extend_from_slice(&name_field(&self.l_name, 12));
        v.extend_from_slice(&name_field(&self.r_name, 12));
        v.extend_from_slice(&name_field(&self.r_tty, 16));
        assert_eq!(v.len(), CTL_MSG_LEN, "CTL_MSG must be exactly 84 bytes");
        v
    }
}

#[derive(Debug)]
struct Res {
    vers: u8,
    typ: u8,
    answer: u8,
    id_num: u32,
    sa_data: [u8; 14],
}

fn parse_res(b: &[u8]) -> Option<Res> {
    if b.len() < CTL_RES_LEN {
        return None;
    }
    let mut sa_data = [0u8; 14];
    sa_data.copy_from_slice(&b[10..24]);
    Some(Res {
        vers: b[0],
        typ: b[1],
        answer: b[2],
        id_num: u32::from_be_bytes([b[4], b[5], b[6], b[7]]),
        sa_data,
    })
}

/// A client socket. It **must** be bound: talkd replies to the sender's source
/// address, and an unbound Unix datagram socket is unnamed, so the reply would
/// have nowhere to go (see audit #TK22).
struct Client {
    sock: UnixDatagram,
    path: PathBuf,
}

impl Client {
    fn new(tag: &str) -> Self {
        let path = PathBuf::from(format!(
            "/tmp/talkd_client_{}_{}_{}.sock",
            std::process::id(),
            tag,
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let _ = std::fs::remove_file(&path);
        let sock = UnixDatagram::bind(&path).expect("bind client socket");
        sock.set_read_timeout(Some(Duration::from_secs(3))).unwrap();
        Client { sock, path }
    }

    fn send(&self, server: &Path, bytes: &[u8]) {
        self.sock.send_to(bytes, server).expect("send to talkd");
    }

    /// Send without requiring success. Under a deliberate flood the datagram
    /// buffer fills and the kernel refuses the send (`ENOBUFS` on macOS); that
    /// is the OS applying backpressure, which is part of the phenomenon under
    /// test rather than a harness failure.
    fn try_send(&self, server: &Path, bytes: &[u8]) -> bool {
        self.sock.send_to(bytes, server).is_ok()
    }

    /// Send a request and await the reply.
    fn request(&self, server: &Path, msg: &Msg) -> Option<Res> {
        self.send(server, &msg.encode());
        self.recv()
    }

    fn recv(&self) -> Option<Res> {
        let mut buf = [0u8; 256];
        let n = self.sock.recv(&mut buf).ok()?;
        parse_res(&buf[..n])
    }
}

impl Drop for Client {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.path);
    }
}

/// Run `body` against a freshly started talkd, or skip if the binary is absent.
fn with_talkd(extra: &[&str], body: impl FnOnce(&Path)) {
    let _lock = acquire_lock();
    let sock = test_socket_path();
    let Some(child) = start_talkd_with(&sock, extra) else {
        eprintln!("Skipping test: talkd binary not found");
        return;
    };
    if !sock.exists() {
        stop_talkd(child, &sock);
        eprintln!("Skipping test: talkd did not bind its socket");
        return;
    }
    body(&sock);
    stop_talkd(child, &sock);
}

// ============================================================================
// Round-trip tests
// ============================================================================

#[test]
fn test_talkd_lookup_not_here() {
    with_talkd(&[], |sock| {
        let c = Client::new("lookup");
        let res = c
            .request(sock, &Msg::new(T_LOOK_UP, "nobody_here", "nobody_there"))
            .expect("no reply from talkd");
        assert_eq!(res.vers, TALK_VERSION);
        assert_eq!(res.typ, T_LOOK_UP);
        assert_eq!(res.answer, A_NOT_HERE, "unknown pair must answer NotHere");
    });
}

#[test]
fn test_talkd_bad_version() {
    with_talkd(&[], |sock| {
        let c = Client::new("badvers");
        let mut m = Msg::new(T_LOOK_UP, "a", "b");
        m.vers = 99;
        let res = c.request(sock, &m).expect("no reply from talkd");
        assert_eq!(res.answer, A_BAD_VERSION);
    });
}

#[test]
fn test_talkd_unknown_type() {
    with_talkd(&[], |sock| {
        let c = Client::new("unktype");
        let m = Msg::new(42, "a", "b");
        let res = c.request(sock, &m).expect("no reply from talkd");
        assert_eq!(res.answer, A_UNKNOWN_REQUEST);
    });
}

#[test]
fn test_talkd_short_datagram_is_dropped_and_daemon_survives() {
    with_talkd(&[], |sock| {
        let c = Client::new("short");
        // A truncated datagram must be dropped silently, with no reply.
        c.send(sock, &[1u8, 2, 3, 4, 5, 6, 7, 8]);
        assert!(
            c.recv().is_none(),
            "a malformed datagram must not draw a reply"
        );

        // The daemon must still be serving.
        let res = c
            .request(sock, &Msg::new(T_LOOK_UP, "x", "y"))
            .expect("daemon stopped answering after a malformed datagram");
        assert_eq!(res.answer, A_NOT_HERE);
    });
}

#[test]
fn test_talkd_leave_invite_then_lookup() {
    with_talkd(&[], |sock| {
        let c = Client::new("invite");

        // alice leaves an invitation for bob, carrying a rendezvous address.
        let mut inv = Msg::new(T_LEAVE_INVITE, "alice", "bob");
        inv.addr = sockaddr(4242, [127, 0, 0, 1]);
        let left = c.request(sock, &inv).expect("no reply to LEAVE_INVITE");
        assert_eq!(left.answer, A_SUCCESS);
        assert_ne!(left.id_num, 0, "a successful invitation must carry an id");

        // bob looks up alice's invitation and gets the same id and address.
        let found = c
            .request(sock, &Msg::new(T_LOOK_UP, "bob", "alice"))
            .expect("no reply to LOOK_UP");
        assert_eq!(found.answer, A_SUCCESS);
        assert_eq!(found.id_num, left.id_num);
        assert_eq!(
            &found.sa_data[0..6],
            &[0x10, 0x92, 127, 0, 0, 1],
            "the echoed address must be the one left (port 4242 = 0x1092)"
        );
    });
}

#[test]
fn test_talkd_delete_is_id_scoped() {
    // Audit #TD13 at integration level: DELETE must remove the invitation the
    // id names, not merely the first one belonging to that caller.
    with_talkd(&[], |sock| {
        let c = Client::new("delete");

        let mut a = Msg::new(T_LEAVE_INVITE, "alice", "bob");
        a.addr = sockaddr(1111, [127, 0, 0, 1]);
        let ra = c.request(sock, &a).expect("no reply").id_num;

        let mut b = Msg::new(T_LEAVE_INVITE, "alice", "carol");
        b.addr = sockaddr(2222, [127, 0, 0, 1]);
        let rb = c.request(sock, &b).expect("no reply").id_num;
        assert_ne!(ra, rb, "each invitation needs its own id");

        // Delete only the alice->bob invitation.
        let mut del = Msg::new(T_DELETE, "alice", "bob");
        del.id_num = ra;
        assert_eq!(c.request(sock, &del).expect("no reply").answer, A_SUCCESS);

        // bob's is gone...
        assert_eq!(
            c.request(sock, &Msg::new(T_LOOK_UP, "bob", "alice"))
                .expect("no reply")
                .answer,
            A_NOT_HERE
        );
        // ...while carol's survives.
        let carol = c
            .request(sock, &Msg::new(T_LOOK_UP, "carol", "alice"))
            .expect("no reply");
        assert_eq!(
            carol.answer, A_SUCCESS,
            "deleting one invitation must not remove the caller's others"
        );
        assert_eq!(carol.id_num, rb);
    });
}

#[test]
fn test_talkd_error_replies_are_rate_limited() {
    // An unauthenticated sender must not be able to make the daemon reflect a
    // reply datagram per garbage packet.
    with_talkd(&[], |sock| {
        let c = Client::new("flood");
        let mut m = Msg::new(T_LOOK_UP, "a", "b");
        m.vers = 99;
        let bytes = m.encode();

        // Sends are best-effort: a flood fills the datagram buffer and the
        // kernel starts refusing them (ENOBUFS on macOS). Pause periodically so
        // the daemon can drain and a useful number actually lands. The pauses
        // are far too short to refill a meaningful number of tokens.
        const ATTEMPTS: usize = 200;
        let mut sent = 0usize;
        for i in 0..ATTEMPTS {
            if c.try_send(sock, &bytes) {
                sent += 1;
            }
            if i % 20 == 19 {
                std::thread::sleep(Duration::from_millis(5));
            }
        }

        std::thread::sleep(Duration::from_millis(1500));
        c.sock
            .set_read_timeout(Some(Duration::from_millis(200)))
            .unwrap();

        let mut replies = 0usize;
        let mut buf = [0u8; 256];
        while c.sock.recv(&mut buf).is_ok() {
            replies += 1;
        }

        assert!(replies > 0, "the first errors should still be answered");
        // Only meaningful if the kernel let through more than one burst's worth;
        // otherwise the OS did the throttling for us and there is nothing to
        // prove.
        if sent > 16 {
            assert!(
                replies < sent,
                "expected the reply flood to be throttled, got {replies} replies \
                 to {sent} delivered datagrams"
            );
        } else {
            eprintln!("note: only {sent} of {ATTEMPTS} datagrams were accepted by the kernel");
        }
    });
}

#[test]
fn test_talkd_expiry_fires_without_traffic() {
    // The point of this test is the *absence* of traffic. The obvious version --
    // leave an invitation, sleep, then LOOK_UP and assert NotHere -- passes even
    // on the broken code, because the verifying LOOK_UP is itself what triggers
    // the sweep. So the sweep has to be observed in the log with nothing in
    // flight.
    let _lock = acquire_lock();
    let sock = test_socket_path();
    let Some(mut child) = start_talkd_with(&sock, &["--invite-timeout", "2", "--expiry-tick", "1"])
    else {
        eprintln!("Skipping test: talkd binary not found");
        return;
    };
    if !sock.exists() {
        stop_talkd(child, &sock);
        eprintln!("Skipping test: talkd did not bind its socket");
        return;
    }
    let log = drain_stderr(&mut child);

    {
        let c = Client::new("expiry");
        let mut inv = Msg::new(T_LEAVE_INVITE, "alice", "bob");
        inv.addr = sockaddr(4242, [127, 0, 0, 1]);
        assert_eq!(c.request(&sock, &inv).expect("no reply").answer, A_SUCCESS);
    }
    // Nothing is sent from here on.

    let swept = wait_for_line(&log, "expired invitations", Duration::from_secs(10));
    assert!(
        swept,
        "an idle daemon must expire invitations on its own; log was: {:?}",
        log.lock().unwrap()
    );

    // And the logged sweep must reflect real state, not just a log line.
    let c = Client::new("expiry2");
    let after = c
        .request(&sock, &Msg::new(T_LOOK_UP, "bob", "alice"))
        .expect("no reply");
    assert_eq!(after.answer, A_NOT_HERE);

    stop_talkd(child, &sock);
}

#[test]
fn test_talkd_socket_mode_is_private() {
    // Audit #TD5: the control socket is created 0600.
    use std::os::unix::fs::PermissionsExt;
    with_talkd(&[], |sock| {
        let mode = std::fs::metadata(sock).unwrap().permissions().mode() & 0o777;
        assert_eq!(
            mode, 0o600,
            "talkd socket must not be group/world reachable"
        );
    });
}

#[test]
fn test_talkd_unprivileged_start_is_not_an_error() {
    // Audit #TD6 is a WON'T-FIX: talkd performs no privileged operation, so an
    // ordinary user must be able to run it and get service.
    with_talkd(&[], |sock| {
        let c = Client::new("unpriv");
        let res = c
            .request(sock, &Msg::new(T_LOOK_UP, "a", "b"))
            .expect("an unprivileged talkd must still serve requests");
        assert_eq!(res.answer, A_NOT_HERE);
    });
}
