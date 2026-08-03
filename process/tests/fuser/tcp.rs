//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::{fuser_test, parse_posix_pid_list};
use std::{
    io,
    net::{TcpListener, TcpStream},
};

/// Starts a TCP server on a predefined local address and port.
///
/// **Returns:**
/// - An `io::Result` containing the bound `TcpListener` if successful.
fn start_tcp_server() -> io::Result<TcpListener> {
    TcpListener::bind("127.0.0.1:0")
}

/// Waits for the TCP server to be ready by attempting to connect to it.
///
/// **Arguments:**
/// - `port`: The port number where the TCP server is expected to be listening.
fn wait_for_tcp_server(port: u16) {
    let address = format!("127.0.0.1:{}", port);

    loop {
        if let Ok(stream) = TcpStream::connect(&address) {
            stream
                .shutdown(std::net::Shutdown::Both)
                .expect("Failed to close the connection");

            break;
        }

        std::thread::yield_now();
    }
}
/// Tests `fuser` with the TCP server to ensure it can find the process associated with the server.
///
/// **Setup:**
/// - Starts a TCP server and waits for it to become available.
///
/// **Assertions:**
/// - Verifies that the `fuser` command can find the process associated with the TCP server.
#[test]
fn test_fuser_tcp() {
    let server = start_tcp_server().expect("Failed to start TCP server");
    let port = server.local_addr().unwrap().port();
    wait_for_tcp_server(port);

    fuser_test(vec![format!("{}/tcp", port)], "", 0, |_, output| {
        assert_eq!(output.status.code(), Some(0));

        // This test process owns the listening socket, so it must be reported.
        let pids = parse_posix_pid_list(&output.stdout);
        assert!(
            pids.contains(&std::process::id()),
            "fuser must report the process holding the TCP socket ({}), got {:?}",
            std::process::id(),
            pids
        );
    });

    drop(server);
}
