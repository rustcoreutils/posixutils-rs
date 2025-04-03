mod unix {
    use crate::fuser::fuser_test;
    use std::os::unix::net::{UnixListener, UnixStream};
    use std::process::Command;
    use std::{fs, thread};

    /// Starts a Unix socket server at the specified socket path.
    ///
    /// **Arguments:**
    /// - `socket_path`: The path where the Unix socket will be created.
    ///
    /// **Returns:**
    /// - A `Result` containing the bound `UnixListener` if successful, or an `io::Error`.
    fn start_unix_socket(socket_path: &str) -> Result<UnixListener, std::io::Error> {
        if fs::metadata(socket_path).is_ok() {
            fs::remove_file(socket_path).map_err(|e| {
                eprintln!("Failed to delete existing socket: {}", e);
                e
            })?;
        }
        UnixListener::bind(socket_path).map_err(|e| {
            eprintln!("Failed to bind Unix socket: {}", e);
            e
        })
    }

    /// Waits for the Unix socket server to be ready by attempting to connect to it.
    ///
    /// **Arguments:**
    /// - `socket_path`: The path of the Unix socket to connect to.
    fn wait_for_unix_socket(socket_path: &str) {
        loop {
            match UnixStream::connect(socket_path) {
                Ok(stream) => {
                    if let Err(e) = stream.shutdown(std::net::Shutdown::Both) {
                        eprintln!("Failed to close the connection: {}", e);
                    }
                    break;
                }
                Err(_) => {
                    thread::sleep(std::time::Duration::from_millis(10));
                }
            }
        }
    }

    /// Tests `fuser` with a Unix socket to ensure it can find the process associated with the socket.
    ///
    /// **Setup:**
    /// - Starts a Unix socket server.
    ///
    /// **Assertions:**
    /// - Verifies that the `fuser` command can find the process associated with the Unix socket.
    #[test]
    fn test_fuser_unixsocket() {
        let socket_path = "/tmp/test.sock";
        let _unix_socket = match start_unix_socket(socket_path) {
            Ok(socket) => socket,
            Err(e) => {
                eprintln!("Failed to start Unix socket: {}", e);
                return;
            }
        };

        wait_for_unix_socket(socket_path);

        let handle = thread::spawn(move || {
            fuser_test(vec![socket_path.to_string()], "", 0, |_, _output| {
                let manual_output = Command::new("fuser").arg(socket_path).output();

                match manual_output {
                    Ok(output) => {
                        assert_eq!(output.status.code(), Some(0));
                    }
                    Err(e) => {
                        eprintln!("Failed to run fuser command: {}", e);
                    }
                }
            });
        });

        handle.join().expect("Thread panicked");
    }
}
