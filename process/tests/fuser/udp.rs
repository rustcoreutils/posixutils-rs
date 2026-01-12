use super::fuser_test;
use std::{io, net::UdpSocket, process::Command};

/// Waits for a UDP server to become available by sending a dummy message to the specified port.
///
/// **Arguments:**
/// - `port`: The port number where the UDP server is expected to be listening.
fn wait_for_udp_server(port: u16) {
    let socket = UdpSocket::bind("127.0.0.1:0").expect("Failed to bind dummy UDP socket");
    let dummy_message = b"ping";

    loop {
        if socket
            .send_to(dummy_message, format!("127.0.0.1:{}", port))
            .is_ok()
        {
            break;
        }
    }
    drop(socket);
}
/// Starts a UDP server listening on a specific port.
///
/// **Returns:**
/// - An `io::Result` containing the bound `UdpSocket` if successful.
fn start_udp_server() -> io::Result<UdpSocket> {
    UdpSocket::bind("127.0.0.1:0")
}

/// Tests `fuser` with the `-u` flag to ensure it outputs the process owner for the UDP server.
///
/// **Setup:**
/// - Starts a UDP server and waits for it to become available.
///
/// **Assertions:**
/// - Verifies that the `fuser` command can find the process associated with the UDP server.
#[test]
fn test_fuser_udp() {
    let server = start_udp_server().expect("Failed to start UDP server");
    let port = server.local_addr().unwrap().port();
    wait_for_udp_server(port);

    fuser_test(vec![format!("{}/udp", port)], "", 0, |_, output| {
        let manual_output = Command::new("fuser")
            .arg(format!("{}/udp", port))
            .output()
            .unwrap();

        assert_eq!(output.status.code(), Some(0));
        assert_eq!(output.stdout, manual_output.stdout);
    });

    drop(server);
}
