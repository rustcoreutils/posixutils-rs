// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::{error::ErrorKind, Parser};
use gettextrs::gettext;
use thiserror::Error;

use binrw::{binrw, BinReaderExt, BinWrite, Endian};
#[cfg(target_os = "linux")]
use libc::sa_family_t;
use libc::{
    addrinfo, getaddrinfo, gethostname, getpid, getpwuid, getservbyname, getuid, ioctl, signal,
    sockaddr_in, winsize, AF_INET, AI_CANONNAME, SIGINT, SIGPIPE, SIGQUIT, SOCK_DGRAM,
    STDIN_FILENO, STDOUT_FILENO, TIOCGWINSZ,
};

use std::{
    char,
    ffi::{CStr, CString},
    io::{self, Cursor, Error, IsTerminal, Write},
    mem::{size_of, zeroed},
    net::{
        self, AddrParseError, Ipv4Addr, SocketAddr, SocketAddrV4, SocketAddrV6, TcpListener,
        TcpStream, UdpSocket,
    },
    os::fd::AsRawFd,
    os::unix::net::UnixDatagram,
    path::{Path, PathBuf},
    process, ptr,
    sync::atomic::{AtomicBool, Ordering},
    sync::{Arc, LazyLock, Mutex},
    thread,
    time::{Duration, Instant},
};

#[derive(Parser)]
#[command(version, about=gettext("talk - talk to another user"))]
struct Args {
    #[arg(help = gettext("Address to connect or listen to"))]
    address: String,

    #[arg(help = gettext("Terminal name to use (optional)"))]
    ttyname: Option<String>,

    /// Use local talkd daemon via Unix socket instead of network
    #[arg(long)]
    local: bool,
}

/// Default path for local talkd Unix socket (proper system location)
const DEFAULT_LOCAL_SOCKET: &str = "/var/run/talkd.sock";

pub struct State {
    pub msg_bytes1: Vec<u8>,
    pub msg_bytes2: Vec<u8>,
    pub socket: Arc<UdpSocket>,
    pub talkd_addr: SocketAddr,
}

/// A static variable to hold the state of delete invitations on SIGINT signal.
static DELETE_INVITATIONS: LazyLock<Arc<Mutex<Option<State>>>> =
    LazyLock::new(|| Arc::new(Mutex::new(None)));

/// Original terminal attributes captured before entering raw mode, so the
/// signal handler and connection-close paths can restore the terminal on exit
/// (the per-thread `RestoreTermOnDrop` guard does not cover those paths).
static ORIGINAL_TERMIOS: LazyLock<Mutex<Option<libc::termios>>> =
    LazyLock::new(|| Mutex::new(None));

/// The terminal's erase/kill characters, captured from termios before raw mode
/// (spec 116758). Falls back to the POSIX defaults until then.
static KEYMAP: LazyLock<Mutex<KeyMap>> = LazyLock::new(|| Mutex::new(KeyMap::default()));

/// Set by the SIGWINCH handler; the render loops re-query the terminal size and
/// repaint when they see it (audit #TK9).
static RESIZED: AtomicBool = AtomicBool::new(false);

/// Restore the terminal to the attributes captured before raw mode, if any.
fn restore_terminal() {
    if let Ok(guard) = ORIGINAL_TERMIOS.lock() {
        if let Some(termios) = guard.as_ref() {
            unsafe {
                libc::tcsetattr(libc::STDIN_FILENO, libc::TCSANOW, termios);
            }
        }
    }
}

/// The size of the buffer for control message fields like l_name, r_name, and r_tty in CtlMsg.
const BUFFER_SIZE: usize = 12;

//i The maximum size for the buffer to store the hostname, based on typical hostname lengths.
const HOSTNAME_BUFFER_SIZE: usize = 256;

/// The version number for the talk protocol.
const TALK_VERSION: u8 = 1;

#[derive(Debug, Default, Copy, Clone, PartialEq)]
#[binrw]
#[brw(repr(u8))]
/// Represents the types of messages exchanged in the communication.
enum MessageType {
    /// Leave invitation with server.
    LeaveInvite,
    /// Check for invitation by callee.
    #[default]
    LookUp,
    /// Delete invitation by caller.
    Delete,
    /// Announce invitation by caller.
    Announce,
}

impl TryFrom<u8> for MessageType {
    type Error = TalkError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(MessageType::LeaveInvite),
            1 => Ok(MessageType::LookUp),
            2 => Ok(MessageType::Delete),
            3 => Ok(MessageType::Announce),
            _ => Err(TalkError::Other(
                "Not existing MessageType provided".to_string(),
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
#[binrw]
#[brw(repr(u8))]
/// Represents the possible responses from a request.
enum Answer {
    /// Operation completed properly.
    Success,
    /// Callee not logged in.
    NotHere,
    /// Operation failed for unexplained reason.
    Failed,
    /// Caller’s machine name is unknown.
    MachineUnknown,
    /// Callee’s TTY doesn’t permit announce.
    PermissionDenied,
    /// Request has an invalid type value.
    UnknownRequest,
    /// Request has an invalid protocol version.
    BadVersion,
    /// Request has an invalid address value.
    BadAddr,
    /// Request has an invalid control address value.
    BadCtlAddr,
}

impl TryFrom<u8> for Answer {
    type Error = TalkError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Answer::Success),
            1 => Ok(Answer::NotHere),
            2 => Ok(Answer::Failed),
            3 => Ok(Answer::MachineUnknown),
            4 => Ok(Answer::PermissionDenied),
            5 => Ok(Answer::UnknownRequest),
            6 => Ok(Answer::BadVersion),
            7 => Ok(Answer::BadAddr),
            8 => Ok(Answer::BadCtlAddr),
            _ => Err(TalkError::Other("Not existing Answer provided".to_string())),
        }
    }
}

struct StateLogger {
    value: String,
}

impl StateLogger {
    fn new(initial_value: &str) -> Self {
        StateLogger {
            value: initial_value.to_string(),
        }
    }

    fn set_state(&mut self, new_value: &str) {
        if self.value != new_value {
            println!("{}", new_value);
            self.value = new_value.to_string();
        }
    }
}

struct RestoreTermOnDrop {
    original_termios: libc::termios,
}

impl Drop for RestoreTermOnDrop {
    fn drop(&mut self) {
        // Restore the original terminal attributes
        unsafe {
            libc::tcsetattr(libc::STDIN_FILENO, libc::TCSANOW, &self.original_termios);
        }
    }
}

#[derive(Debug, Error)]
pub enum TalkError {
    #[error("Usage: talk user [ttyname]")]
    InvalidArguments,
    #[error("Not a TTY")]
    NotTty,
    #[error("Failed to resolve addresses: {0}")]
    AddressResolutionFailed(String),
    #[error("I/O error: {0}")]
    IoError(#[from] io::Error),
    #[error("An error occurred: {0}")]
    Other(String),
}

#[cfg(target_os = "macos")]
type SaFamily = u16;

#[cfg(target_os = "linux")]
type SaFamily = sa_family_t;

#[derive(PartialEq)]
#[binrw]
/// Socket address structure representing a network address.
pub struct Osockaddr {
    /// Address family (e.g., IPv4, IPv6).
    pub sa_family: SaFamily,
    /// Address data, including the port and IP address.
    pub sa_data: [u8; 14],
}

#[allow(clippy::derivable_impls)]
impl Default for Osockaddr {
    fn default() -> Self {
        Osockaddr {
            sa_family: 0, // TODO use enum libc::AF_UNSPEC as u16
            sa_data: [0; 14],
        }
    }
}

impl From<&Osockaddr> for SocketAddrV4 {
    fn from(value: &Osockaddr) -> Self {
        // Extract the port
        let port = u16::from_be_bytes([value.sa_data[0], value.sa_data[1]]);

        // Extract the IP address
        let ip = Ipv4Addr::new(
            value.sa_data[2],
            value.sa_data[3],
            value.sa_data[4],
            value.sa_data[5],
        );

        Self::new(ip, port)
    }
}

impl From<&SocketAddr> for Osockaddr {
    fn from(value: &SocketAddr) -> Self {
        match value {
            SocketAddr::V4(v) => Self::from(v),
            SocketAddr::V6(v) => Self::from(v),
        }
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

impl From<&SocketAddrV6> for Osockaddr {
    fn from(_value: &SocketAddrV6) -> Self {
        // The talk wire protocol carries only IPv4 (sockaddr_in); an IPv6
        // address has no representation here. Return an empty address rather
        // than panicking (the connection attempt will fail downstream).
        Osockaddr::default()
    }
}

#[binrw]
/// Control message structure used for communication in the talk protocol.
struct CtlMsg {
    /// Version of the message.
    vers: u8,
    /// Type of the message.
    r#type: u8,
    /// Answer code (success or failure).
    answer: u8,
    /// Padding for alignment.
    pad: u8,
    /// Identifier number for the message.
    id_num: u32,
    /// Socket address of the recipient.
    addr: Osockaddr,
    /// Control socket address.
    ctl_addr: Osockaddr,
    /// Process ID of the sender.
    pid: i32,
    /// Local user name.
    l_name: [i8; 12],
    /// Remote user name.
    r_name: [i8; 12],
    /// Remote terminal name.
    r_tty: [i8; 16],
}

impl Default for CtlMsg {
    fn default() -> Self {
        CtlMsg {
            vers: 1,
            r#type: MessageType::default() as u8,
            answer: Answer::Success as u8,
            pad: 0,
            id_num: 0,
            addr: Osockaddr::default(),
            ctl_addr: Osockaddr::default(),
            pid: 0,
            l_name: string_to_c_string(""),
            r_name: string_to_c_string(""),
            r_tty: [0; 16],
        }
    }
}

impl CtlMsg {
    // Converts the CtlMsg structure into a vector of bytes for network transmission
    fn to_bytes(&self) -> io::Result<Vec<u8>> {
        let mut bytes = vec![0u8; size_of::<CtlMsg>()];
        let mut cursor = Cursor::new(&mut bytes[..]);
        self.write_options(&mut cursor, Endian::Big, ()).unwrap();
        Ok(bytes)
    }

    // create control sockaddr data from a SocketAddr
    pub fn create_ctl_addr(&self, addr: SocketAddr) -> [u8; 14] {
        let mut ctl_addr: [u8; 14] = [0; 14];
        if let net::IpAddr::V4(ipv4) = addr.ip() {
            let ip_bytes = ipv4.octets();

            let port_bytes = addr.port().to_be_bytes();
            ctl_addr[0..2].copy_from_slice(&port_bytes);

            ctl_addr[2..6].copy_from_slice(&ip_bytes);
        }

        ctl_addr
    }
}

#[binrw]
#[br(big)]
/// Control response structure used for communication with the daemon.
pub struct CtlRes {
    /// Version of the control protocol.
    pub vers: u8,

    /// Type of message being sent/received.
    r#type: MessageType,

    /// Response to the control message.
    answer: Answer,

    /// Padding byte to maintain alignment.
    pub pad: u8,
    /// Unique identifier number for the invitation.
    pub id_num: u32,

    /// Socket address associated with the response.
    pub addr: Osockaddr,
}

impl Default for CtlRes {
    fn default() -> Self {
        CtlRes {
            vers: 0,
            r#type: MessageType::default(),
            answer: Answer::Failed,
            pad: 0,
            id_num: 0,
            addr: Osockaddr::default(),
        }
    }
}

impl CtlRes {
    // Converts a byte slice into a CtlRes struct, ensuring correct parsing of each field.
    fn from_bytes(bytes: &[u8]) -> Result<Self, binrw::Error> {
        let mut cursor = Cursor::new(bytes);
        cursor.read_be()
    }
}

fn talk(args: Args) -> Result<(), TalkError> {
    let mut msg = CtlMsg::default();
    let mut res = CtlRes::default();

    // Retrieve the local and remote machine names
    let (my_machine_name, his_machine_name) =
        get_names(&mut msg, &args.address, &args.ttyname).map_err(TalkError::IoError)?;

    // Get the local and remote addresses, and the daemon port number
    let (my_machine_addr, his_machine_addr, daemon_port) =
        get_addrs(&mut msg, &my_machine_name, &his_machine_name).map_err(TalkError::IoError)?;

    let (width, height) = get_terminal_size();

    let mut logger = StateLogger::new("No connection yet.");

    check_if_tty()?;
    // Open control socket
    let (ctl_addr, socket) = open_ctl(my_machine_addr).map_err(TalkError::IoError)?;

    let ctl_addr_data = msg.create_ctl_addr(ctl_addr);
    msg.ctl_addr.sa_data = ctl_addr_data;

    logger.set_state("[Checking for invitation on caller's machine]");

    let talkd_addr: SocketAddr = format!("{}:{}", his_machine_addr, daemon_port)
        .parse()
        .map_err(|e: AddrParseError| TalkError::AddressResolutionFailed(e.to_string()))?;

    // Look for an invitation from the daemon
    look_for_invite(daemon_port, his_machine_addr, &mut msg, &socket, &mut res)?;

    // Set the invitation ID number and send a delete request for the old invitation
    if res.answer == Answer::Success {
        handle_existing_invitation(width, height, &mut res)?;
    } else {
        logger.set_state("[Waiting to connect with caller]");
        handle_new_invitation(
            talkd_addr,
            daemon_port,
            his_machine_addr,
            &mut msg,
            socket,
            &mut res,
            my_machine_addr,
            &mut logger,
        )?;
    }

    Ok(())
}

/// Main entry point for local mode talk using Unix domain sockets.
///
/// This function handles talk sessions using a local talkd daemon
/// via Unix domain sockets instead of the network UDP protocol.
fn talk_local(args: Args) -> Result<(), TalkError> {
    let mut msg = CtlMsg::default();
    let mut res = CtlRes::default();

    let socket_path = PathBuf::from(get_local_socket_path());

    // Verify the daemon socket exists
    if !socket_path.exists() {
        eprintln!(
            "Local talkd socket not found at {:?}. Is talkd running?",
            socket_path
        );
        eprintln!("Hint: Start talkd with: talkd --socket {:?}", socket_path);
        return Err(TalkError::Other("talkd not running".to_string()));
    }

    // Get local machine info for message setup
    let my_name = get_current_user_name().map_err(TalkError::IoError)?;
    let my_machine_name = get_local_machine_name().map_err(TalkError::IoError)?;

    // Parse the address to get the target user
    let (his_name, _his_machine_name) = parse_address(&args.address, &my_machine_name);

    // Set up the control message
    msg.vers = TALK_VERSION;
    msg.addr.sa_family = AF_INET as SaFamily;
    msg.ctl_addr.sa_family = AF_INET as SaFamily;
    msg.l_name = string_to_c_string(&my_name);
    msg.r_name = string_to_c_string(&his_name);
    msg.r_tty = tty_to_c_string(args.ttyname.as_ref().unwrap_or(&"".to_string()));
    msg.pid = unsafe { getpid() };

    let (width, height) = get_terminal_size();

    let mut logger = StateLogger::new("No connection yet.");

    check_if_tty()?;

    // For local mode, we still need a TCP listener for peer-to-peer connection
    let my_machine_addr = Ipv4Addr::LOCALHOST;
    let (socket_addr, listener) = open_socket(my_machine_addr).map_err(TalkError::IoError)?;

    // Set up the TCP address in the message
    let tcp_data = Osockaddr::from(&socket_addr).sa_data;
    msg.addr.sa_data = tcp_data;

    // Also set ctl_addr (for local mode, use loopback)
    let ctl_socket = UdpSocket::bind((my_machine_addr, 0)).map_err(TalkError::IoError)?;
    let ctl_addr = ctl_socket.local_addr().map_err(TalkError::IoError)?;
    let ctl_addr_data = msg.create_ctl_addr(ctl_addr);
    msg.ctl_addr.sa_data = ctl_addr_data;

    logger.set_state("[Checking for invitation on local daemon]");

    // Look for an existing invitation
    look_for_invite_local(&socket_path, &mut msg, &mut res)?;

    if res.answer == Answer::Success {
        // Found an existing invitation - connect to it
        handle_existing_invitation(width, height, &mut res)?;
    } else {
        // No invitation found - create one and wait
        logger.set_state("[Waiting for your party to respond]");

        // Send announce message
        announce_local(&socket_path, &mut msg, &mut res)?;
        let remote_id = res.id_num;

        // Leave an invitation for the other party
        leave_invite_local(&socket_path, &mut msg, &mut res)?;
        let local_id = res.id_num;

        logger.set_state("[Ringing your party]");

        // Wait for incoming connection
        for stream in listener.incoming() {
            match stream {
                Ok(client_stream) => {
                    // Clean up invitations
                    msg.id_num = local_id;
                    let _ = send_delete_local(&socket_path, &mut msg, &mut res);
                    msg.id_num = remote_id;
                    let _ = send_delete_local(&socket_path, &mut msg, &mut res);

                    if let Err(e) = handle_client(client_stream) {
                        eprintln!("Failed to handle client: {}", e);
                    }
                    break;
                }
                Err(e) => eprintln!("Failed to accept connection: {}", e),
            }
        }
    }

    Ok(())
}

/// Checks if the standard input is a TTY (terminal).
///
/// # Returns
///
/// A `Result` indicating success or a `TalkError` if the input is not a TTY.
fn check_if_tty() -> Result<(), TalkError> {
    if !io::stdin().is_terminal() {
        plib::diag::error(&gettext("standard input is not a terminal"));
        return Err(TalkError::NotTty);
    }
    // Spec STDOUT: if standard output is not a terminal, talk exits non-zero.
    if !io::stdout().is_terminal() {
        plib::diag::error(&gettext("standard output is not a terminal"));
        return Err(TalkError::NotTty);
    }
    Ok(())
}

/// Handles an existing invitation by establishing a TCP connection and managing user input.
///
/// # Arguments
///
/// * `width` - The width of the terminal for drawing purposes.
/// * `height` - The height of the terminal for positioning.
/// * `res` - A mutable reference to the control response containing the address.
///
/// # Returns
///
/// A `Result` indicating success or a `TalkError`.
fn handle_existing_invitation(width: u16, height: u16, res: &mut CtlRes) -> Result<(), TalkError> {
    let tcp_addr = SocketAddrV4::from(&res.addr);

    // Establish a TCP connection to the `tcp_addr`. Map any IO errors to `TalkError::IoError`.
    let stream = TcpStream::connect(tcp_addr).map_err(TalkError::IoError)?;

    let write_stream = stream.try_clone().map_err(TalkError::IoError)?;
    let read_stream = stream.try_clone().map_err(TalkError::IoError)?;

    let split_row = height / 2;
    // Top half is the local user's, bottom half the peer's.
    let local_window = Arc::new(Mutex::new(Window::new(
        1,
        split_row.saturating_sub(1),
        width,
    )));
    let peer_window = Arc::new(Mutex::new(Window::new(
        split_row + 1,
        height.saturating_sub(split_row),
        width,
    )));

    // Spawn a thread to handle incoming data from the TCP read stream and update the terminal accordingly.
    spawn_input_thread(read_stream, split_row, width, Arc::clone(&peer_window))?;

    // Handle user input from stdin, writing it to the TCP write stream and updating the terminal's top line.
    handle_stdin_input(write_stream, split_row, local_window).map_err(TalkError::IoError)?;

    Ok(())
}

/// Spawns a thread to handle input from a TCP stream and update the terminal interface.
///
/// # Arguments
///
/// * `read_stream` - The TCP stream to read input from.
/// * `split_row` - The row in the terminal where the split occurs.
/// * `width` - The width of the terminal for drawing purposes.
/// * `top_line` - An `Arc` of a `Mutex` that tracks the top line position in the terminal.
/// * `bottom_line` - An `Arc` of a `Mutex` that tracks the bottom line position in the terminal.
///
/// # Returns
///
/// A `Result` indicating success or a `TalkError`.
fn spawn_input_thread(
    read_stream: TcpStream,
    split_row: u16,
    width: u16,
    peer_window: Arc<Mutex<Window>>,
) -> Result<(), TalkError> {
    thread::spawn(move || {
        // Set terminal to raw mode
        let stdin_fd = libc::STDIN_FILENO;
        let mut termios = std::mem::MaybeUninit::<libc::termios>::uninit();

        // Get the current terminal attributes
        if unsafe { libc::tcgetattr(stdin_fd, termios.as_mut_ptr()) } != 0 {
            eprintln!("Failed to get terminal attributes");
            return;
        }
        let mut termios = unsafe { termios.assume_init() };

        // Save the original terminal attributes to restore later (both via the
        // local Drop guard and the global the signal handler consults).
        let original_termios = termios;
        if let Ok(mut guard) = ORIGINAL_TERMIOS.lock() {
            *guard = Some(original_termios);
        }

        // The erase/kill characters must come from the terminal's own settings
        // (spec 116758), so capture them before we change anything.
        if let Ok(mut guard) = KEYMAP.lock() {
            *guard = KeyMap::from_termios(&original_termios);
        }

        // Set raw mode flags. ECHO must go too: talk draws every character
        // itself, so leaving the driver's echo on shows each keystroke twice
        // (audit #TK15) -- the original comment claimed this but only cleared
        // ICANON.
        termios.c_lflag &= !(libc::ICANON | libc::ECHO);
        termios.c_cc[libc::VMIN] = 1; // Minimum number of characters to read
        termios.c_cc[libc::VTIME] = 0; // No timeout, read immediately

        // Apply the raw mode settings
        if unsafe { libc::tcsetattr(stdin_fd, libc::TCSANOW, &termios) } != 0 {
            eprintln!("Failed to set terminal to raw mode");
            return;
        }

        // Initialize terminal drawing
        let mut handle = match draw_terminal(split_row, width) {
            Ok(handle) => handle,
            Err(e) => {
                eprintln!("Failed to draw terminal: {}", e);
                return; // Exit thread on failure
            }
        };

        let mut buffer = [0; 128];
        // Bytes of a multi-byte character split across two recv() calls
        // (audit #TK10).
        let mut pending: Vec<u8> = Vec::new();

        let _restore = RestoreTermOnDrop { original_termios };

        loop {
            // Receive data from the TCP stream
            let result = unsafe {
                libc::recv(
                    read_stream.as_raw_fd(),
                    buffer.as_mut_ptr() as *mut libc::c_void,
                    buffer.len(),
                    0,
                )
            };

            match result {
                r if r < 0 => {
                    eprintln!("Error reading from stream: {}", io::Error::last_os_error());
                    break;
                }
                0 => {
                    handle_connection_close();
                    break;
                }
                nbytes => {
                    let nbytes = nbytes as usize;
                    pending.extend_from_slice(&buffer[..nbytes]);
                    let text = decode_pending(&mut pending);

                    let keys = KEYMAP.lock().map(|k| *k).unwrap_or_default();
                    let mut win = match peer_window.lock() {
                        Ok(w) => w,
                        Err(_) => break,
                    };
                    if RESIZED.swap(false, Ordering::SeqCst) {
                        let (w, h) = get_terminal_size();
                        let split = h / 2;
                        win.resize(split + 1, h.saturating_sub(split), w);
                        let _ = win.repaint(&mut handle);
                    }
                    for c in text.chars() {
                        // The peer's stream is display-only: nothing it sends is
                        // echoed back, so the transmit half of `apply` is dropped.
                        if let Err(e) = apply(c, &keys, &mut win, &mut handle) {
                            eprintln!("Error rendering peer input: {}", e);
                            break;
                        }
                    }
                }
            }
        }
    });
    Ok(())
}

// ============================================================================
// Split-screen renderer
// ============================================================================

/// One half of talk's split screen.
///
/// Both directions render the same way — the local user's typing into the top
/// window, the peer's into the bottom — so they share this one implementation
/// rather than the two divergent copies the code used to carry. Keeping it free
/// of sockets, threads and `io::stdout()` is what makes the render path
/// testable: a caller can drive it against a `Vec<u8>` and assert the exact
/// bytes emitted.
///
/// Row and column are tracked here rather than recomputed from a text buffer,
/// which is what audit #TK16 was about: the column used to come from a snapshot
/// of the sender's buffer that the peer thread could never see updated.
struct Window {
    /// First terminal row this window occupies (1-based).
    origin: u16,
    /// Usable rows, at least 1.
    height: u16,
    /// Terminal width in columns, at least 1.
    width: u16,
    /// Cursor row within the window, 0-based.
    row: u16,
    /// Cursor column, 0-based.
    col: u16,
    /// Characters on the current line, kept for backspace redraw.
    line: String,
}

impl Window {
    fn new(origin: u16, height: u16, width: u16) -> Self {
        Window {
            origin: origin.max(1),
            height: height.max(1),
            width: width.max(1),
            row: 0,
            col: 0,
            line: String::new(),
        }
    }

    /// Absolute terminal row of the cursor (1-based).
    fn cursor_row(&self) -> u16 {
        self.origin + self.row
    }

    /// Start a fresh line, wrapping back to the top of the window when full.
    fn newline<W: Write>(&mut self, out: &mut W) -> io::Result<()> {
        self.line.clear();
        self.col = 0;
        self.row += 1;
        if self.row >= self.height {
            self.row = 0;
        }
        // Explicit positioning rather than a bare "\n": with ICANON/ONLCR
        // cleared a newline moves down without returning to column 0
        // (audit #TK17).
        write!(out, "\x1b[{};1H\x1b[K", self.cursor_row())?;
        out.flush()
    }

    /// Append one character, wrapping at the window width (audit #TK14).
    fn put<W: Write>(&mut self, c: char, out: &mut W) -> io::Result<()> {
        if self.col >= self.width {
            self.newline(out)?;
        }
        self.line.push(c);
        self.col += 1;
        write!(out, "{}", c)?;
        out.flush()
    }

    /// Erase the character before the cursor.
    fn erase<W: Write>(&mut self, out: &mut W) -> io::Result<()> {
        if self.line.pop().is_none() {
            return Ok(());
        }
        self.col = self.col.saturating_sub(1);
        // Redraw the line rather than emitting a destructive backspace, so the
        // window stays consistent after a wrap.
        write!(out, "\x1b[{};1H\x1b[K{}", self.cursor_row(), self.line)?;
        out.flush()
    }

    /// Discard the whole current line (the termios KILL character).
    fn kill<W: Write>(&mut self, out: &mut W) -> io::Result<()> {
        self.line.clear();
        self.col = 0;
        write!(out, "\x1b[{};1H\x1b[K", self.cursor_row())?;
        out.flush()
    }

    /// Repaint this window's current line, used for Ctrl-L refresh and after a
    /// resize.
    fn repaint<W: Write>(&mut self, out: &mut W) -> io::Result<()> {
        write!(out, "\x1b[{};1H\x1b[K{}", self.cursor_row(), self.line)?;
        out.flush()
    }

    /// Re-apply a new terminal geometry, clamping the cursor into range.
    fn resize(&mut self, origin: u16, height: u16, width: u16) {
        self.origin = origin.max(1);
        self.height = height.max(1);
        self.width = width.max(1);
        if self.row >= self.height {
            self.row = self.height - 1;
        }
        if self.col > self.width {
            self.col = self.width;
        }
    }
}

/// The terminal's erase and kill characters.
///
/// Spec 116758: "Typing the erase and kill characters shall affect the sender's
/// terminal in the manner described by the termios interface". They therefore
/// come from the terminal's own settings rather than being hardcoded, which is
/// what audit #TK7 was about.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct KeyMap {
    erase: u8,
    kill: u8,
}

impl Default for KeyMap {
    /// POSIX defaults when no termios is available: DEL and ^U.
    fn default() -> Self {
        KeyMap {
            erase: 0x7f,
            kill: 0x15,
        }
    }
}

impl KeyMap {
    fn from_termios(t: &libc::termios) -> Self {
        KeyMap {
            erase: t.c_cc[libc::VERASE],
            kill: t.c_cc[libc::VKILL],
        }
    }
}

/// What the character-processing rules (spec 116755-116770) say to do with one
/// character.
#[derive(Debug, PartialEq, Eq)]
enum Action {
    /// 116756: alert the recipient's terminal.
    Alert,
    /// 116757: refresh the sender's screen regions. Local only — never sent.
    Refresh,
    /// 116758: erase the preceding character.
    Erase,
    /// 116758: discard the current line.
    Kill,
    /// End of line.
    Newline,
    /// 116764: an LC_CTYPE `print` or `space` character, sent verbatim.
    Send(char),
    /// 116769: any other non-printable, rendered as an implementation-defined
    /// printable sequence. Uses the `^X` convention `write`(1) already uses
    /// (see `render_line` in `users/write.rs`).
    Caret(char),
}

/// Classify one character according to the spec's rules.
fn classify(c: char, keys: &KeyMap) -> Action {
    // erase/kill are matched first: they are configurable and may well be
    // control characters that the rules below would otherwise claim.
    if let Ok(byte) = u8::try_from(c as u32) {
        if byte == keys.erase {
            return Action::Erase;
        }
        if byte == keys.kill {
            return Action::Kill;
        }
    }

    match c {
        '\x07' => Action::Alert,
        '\x0c' => Action::Refresh,
        '\n' | '\r' => Action::Newline,
        // <tab> is in the `space` class but not `print`.
        '\t' => Action::Send(c),
        // Within ASCII, LC_CTYPE decides: anything not `print` is a control
        // character and gets a printable stand-in (116769).
        _ if c.is_ascii() && !plib::locale::isprint(c) => Action::Caret(c),
        // Everything else passes through as its own bytes. Non-ASCII cannot be
        // usefully re-encoded here -- RATIONALE 116855-116857 notes the two
        // parties may not even share a language -- so it is the recipient's
        // terminal that interprets it. This matches `write`'s `render_line`.
        _ => Action::Send(c),
    }
}

/// The two printable characters that stand in for a control character.
fn caret_pair(c: char) -> (char, char) {
    let byte = c as u32;
    if byte == 0x7f {
        ('^', '?')
    } else {
        ('^', char::from_u32(byte + 64).unwrap_or('?'))
    }
}

/// Decode as much valid UTF-8 as `pending` currently holds, leaving any
/// trailing partial sequence in place for the next read.
///
/// `recv` splits wherever it likes, so a multi-byte character can straddle two
/// chunks. Decoding each chunk in isolation used to drop the *whole* chunk on
/// such a boundary (audit #TK10); this keeps the tail instead. Genuinely
/// invalid bytes are consumed as U+FFFD so a corrupt stream cannot wedge the
/// decoder.
fn decode_pending(pending: &mut Vec<u8>) -> String {
    let mut out = String::new();
    loop {
        match std::str::from_utf8(pending) {
            Ok(s) => {
                out.push_str(s);
                pending.clear();
                return out;
            }
            Err(e) => {
                let good = e.valid_up_to();
                out.push_str(unsafe { std::str::from_utf8_unchecked(&pending[..good]) });
                match e.error_len() {
                    // Invalid sequence: replace it and keep going.
                    Some(bad) => {
                        out.push('\u{fffd}');
                        pending.drain(..good + bad);
                    }
                    // Truncated but possibly valid: wait for more bytes.
                    None => {
                        pending.drain(..good);
                        return out;
                    }
                }
            }
        }
    }
}

/// Apply one character to a window, returning what should be transmitted.
///
/// This is the single render path for both directions: the local user's typing
/// and the peer's incoming text differ only in which window they land in and
/// whether the result is sent onward. Keeping it free of sockets and
/// `io::stdout()` is what makes it testable.
///
/// Returns the bytes to forward to the other side, which is empty for purely
/// local effects such as Ctrl-L refresh (spec 116757).
fn apply<W: Write>(c: char, keys: &KeyMap, win: &mut Window, out: &mut W) -> io::Result<Vec<u8>> {
    match classify(c, keys) {
        Action::Alert => {
            // 116756: alerts the *recipient*, so it is forwarded, not drawn.
            Ok(vec![0x07])
        }
        Action::Refresh => {
            win.repaint(out)?;
            Ok(Vec::new())
        }
        Action::Erase => {
            win.erase(out)?;
            Ok(vec![keys.erase])
        }
        Action::Kill => {
            win.kill(out)?;
            Ok(vec![keys.kill])
        }
        Action::Newline => {
            win.newline(out)?;
            Ok(vec![b'\n'])
        }
        Action::Send(ch) => {
            win.put(ch, out)?;
            let mut buf = [0u8; 4];
            Ok(ch.encode_utf8(&mut buf).as_bytes().to_vec())
        }
        Action::Caret(ch) => {
            // 116769: send a printable stand-in rather than the raw control
            // character, which historical implementations refuse for security
            // reasons (RATIONALE 116866-116869).
            let (a, b) = caret_pair(ch);
            win.put(a, out)?;
            win.put(b, out)?;
            let mut buf = [0u8; 8];
            let mut bytes = a.encode_utf8(&mut buf).as_bytes().to_vec();
            let mut tail = [0u8; 4];
            bytes.extend_from_slice(b.encode_utf8(&mut tail).as_bytes());
            Ok(bytes)
        }
    }
}
/// Read the local user's keystrokes, echo them into the top window, and send
/// them to the peer.
///
/// Bytes are accumulated and decoded incrementally, so a multi-byte character
/// typed (or pasted) as several reads is delivered as one character rather than
/// being mangled into Latin-1 (audit #TK10).
fn handle_stdin_input(
    write_stream: TcpStream,
    split_row: u16,
    local_window: Arc<Mutex<Window>>,
) -> Result<(), io::Error> {
    let mut buffer: [u8; 1] = [0; 1];
    let mut pending: Vec<u8> = Vec::new();

    loop {
        let result =
            unsafe { libc::read(STDIN_FILENO, buffer.as_mut_ptr() as *mut libc::c_void, 1) };

        match result.cmp(&0) {
            std::cmp::Ordering::Less => {
                eprintln!("Error reading from stdin: {}", io::Error::last_os_error());
                break;
            }
            // 116760: end-of-file terminates the local utility.
            std::cmp::Ordering::Equal => break,
            _ => {}
        }

        pending.push(buffer[0]);
        let text = decode_pending(&mut pending);
        if text.is_empty() {
            continue;
        }

        let keys = KEYMAP.lock().map(|k| *k).unwrap_or_default();
        let mut win = match local_window.lock() {
            Ok(w) => w,
            Err(_) => break,
        };

        // The local echo is part of the on-screen UI and belongs on the
        // terminal (spec STDERR is "None").
        let stdout = io::stdout();
        let mut out = stdout.lock();

        if RESIZED.swap(false, Ordering::SeqCst) {
            let (w, h) = get_terminal_size();
            win.resize(1, (h / 2).saturating_sub(1), w);
            let _ = win.repaint(&mut out);
        }

        for c in text.chars() {
            let outgoing = apply(c, &keys, &mut win, &mut out)?;
            if !outgoing.is_empty() {
                send_bytes(&write_stream, &outgoing)?;
            }
        }
        let _ = split_row; // geometry is carried by the window itself
    }

    Ok(())
}

/// Sends a single byte over the specified TCP stream.
///
/// # Arguments
///
/// * `write_stream` - A reference to the TCP stream to send the byte through.
/// * `byte` - The byte to be sent.
///
/// # Returns
///
/// A `Result` indicating success or an `io::Error` if sending the byte fails.
fn send_bytes(write_stream: &TcpStream, bytes: &[u8]) -> Result<(), io::Error> {
    if bytes.is_empty() {
        return Ok(());
    }

    let result = unsafe {
        libc::send(
            write_stream.as_raw_fd(),
            bytes.as_ptr() as *const libc::c_void,
            bytes.len(),
            0,
        )
    };

    if result < 0 {
        return Err(io::Error::last_os_error());
    }

    Ok(())
}

/// Handles a new invitation by setting up a TCP socket, notifying the daemon,
/// and managing incoming connections.
///
/// # Arguments
///
/// * `talkd_addr` - The socket address of the talk daemon.
/// * `daemon_port` - The port number for the talk daemon.
/// * `his_machine_addr` - The IPv4 address of the remote machine.
/// * `msg` - A mutable reference to the control message (`CtlMsg`).
/// * `socket` - An `Arc` of the UDP socket used for communication.
/// * `res` - A mutable reference to the control response (`CtlRes`).
/// * `my_machine_addr` - The IPv4 address of the local machine.
/// * `logger` - A mutable reference to the state logger for logging state changes.
///
/// # Returns
///
/// A `Result` indicating success or a `TalkError`.
#[allow(clippy::too_many_arguments)]
fn handle_new_invitation(
    talkd_addr: SocketAddr,
    daemon_port: u16,
    his_machine_addr: Ipv4Addr,
    msg: &mut CtlMsg,
    socket: Arc<UdpSocket>,
    res: &mut CtlRes,
    my_machine_addr: Ipv4Addr,
    logger: &mut StateLogger,
) -> Result<(), TalkError> {
    let (socket_addr, listener) = open_socket(my_machine_addr).map_err(TalkError::IoError)?;

    logger.set_state("[Service connection established.]");

    // Create the socket address data and set it in the `msg`.
    let tcp_data = Osockaddr::from(&socket_addr).sa_data;

    logger.set_state("[Waiting for your party to respond]");

    msg.addr.sa_data = tcp_data;

    // Send the announce message to the daemon, informing it of the new invitation.
    announce(daemon_port, his_machine_addr, msg, &socket, res)?;
    let remote_id = res.id_num;

    // Send the leave invitation message to clear the previous invite state.
    leave_invite(daemon_port, my_machine_addr, msg, &socket, res)?;
    let local_id = res.id_num;

    msg.r#type = MessageType::Delete as u8;
    msg.id_num = local_id;
    let msg_bytes1 = msg
        .to_bytes()
        .map_err(|e| TalkError::Other(e.to_string()))?;

    msg.r#type = MessageType::Delete as u8;
    msg.id_num = remote_id;
    let msg_bytes2 = msg
        .to_bytes()
        .map_err(|e| TalkError::Other(e.to_string()))?;

    let clone_socket = Arc::clone(&socket);

    *DELETE_INVITATIONS.lock().unwrap() = Some(State {
        msg_bytes1,
        msg_bytes2,
        socket: clone_socket,
        talkd_addr,
    });

    // Start listening for incoming TCP connections.
    for stream in listener.incoming() {
        match stream {
            Ok(client_stream) => {
                msg.id_num = local_id;
                send_delete(daemon_port, his_machine_addr, msg, &socket, res)?;
                msg.id_num = remote_id;
                send_delete(daemon_port, his_machine_addr, msg, &socket, res)?;

                if let Err(e) = handle_client(client_stream) {
                    eprintln!("Failed to handle client: {}", e);
                }
            }
            Err(e) => eprintln!("Failed to accept connection: {}", e),
        }
    }

    Ok(())
}

/// Retrieves the current user's login name.
///
/// # Returns
///
/// A `Result` containing the login name as a `String` on success,
/// or an `io::Error` if the user cannot be found.
fn get_current_user_name() -> Result<String, io::Error> {
    unsafe {
        let login_name = libc::getlogin();
        if !login_name.is_null() {
            Ok(CStr::from_ptr(login_name).to_string_lossy().into_owned())
        } else {
            let pw = getpwuid(getuid());
            // If no user information is found, return an error.
            if pw.is_null() {
                Err(io::Error::new(
                    io::ErrorKind::NotFound,
                    "You don't exist. Go away.",
                ))
            } else {
                // Convert the pw_name (user name) from the passwd struct to a Rust String.
                Ok(CStr::from_ptr((*pw).pw_name).to_string_lossy().into_owned())
            }
        }
    }
}

/// Retrieves the local machine's hostname.
///
/// # Returns
///
/// A `Result` containing the hostname as a `String` on success, or an `io::Error` on failure.
fn get_local_machine_name() -> Result<String, io::Error> {
    let mut buffer = vec![0; HOSTNAME_BUFFER_SIZE];
    let result = unsafe { gethostname(buffer.as_mut_ptr(), buffer.len()) };

    if result == 0 {
        let c_str = unsafe { CStr::from_ptr(buffer.as_ptr()) };
        Ok(c_str.to_string_lossy().into_owned())
    } else {
        Err(Error::other("Cannot get local hostname"))
    }
}

/// Parses a user-provided address, which can be in the format "user@host" or "host:user".
/// If no delimiter is found, returns the address as the user and the local machine name as the host.
///
/// # Arguments
///
/// * `address` - A string slice representing the user-provided address.
/// * `my_machine_name` - A string slice representing the local machine name.
///
/// # Returns
///
/// A tuple containing the user name and the host name.
fn parse_address(address: &str, my_machine_name: &str) -> (String, String) {
    // Historical BSD `talk` resolves the address by delimiter *precedence*,
    // not by whichever delimiter appears first:
    //
    //   index('@')  ->  user@host
    //   rindex('!') ->  host!user
    //   rindex(':') ->  host:user
    //   rindex('.') ->  host.user
    //
    // Scanning for the leftmost of the whole set instead (audit #TK19) splits
    // `first.last@host` at the dot -- never reaching the `@` branch -- and
    // splits `host.example.com:user` at its first label. Taking `@` first, and
    // the *last* occurrence of the others, fixes both: a dotted hostname keeps
    // its labels, and a dotted user name still yields the `@` form.
    if let Some(index) = address.find('@') {
        let (user, rest) = address.split_at(index);
        let host = rest.get(1..).unwrap_or_default();
        return (user.to_string(), host.to_string());
    }

    for delimiter in ['!', ':', '.'] {
        if let Some(index) = address.rfind(delimiter) {
            let (host, rest) = address.split_at(index);
            let user = rest.get(1..).unwrap_or_default();
            return (user.to_string(), host.to_string());
        }
    }

    (address.to_string(), my_machine_name.to_string())
}

/// Retrieves and sets the names for both local and remote users, updating the control message accordingly.
///
/// # Arguments
///
/// * `msg` - A mutable reference to a `CtlMsg` structure that will be updated with user and machine names.
/// * `address` - A string representing the remote user's address.
/// * `ttyname` - An optional string containing the terminal name.
///
/// # Returns
///
/// Returns a tuple containing the local machine name and the remote machine name.
///
/// # Errors
///
/// Returns an error if retrieving user or machine names fails.
fn get_names(
    msg: &mut CtlMsg,
    address: &str,
    ttyname: &Option<String>,
) -> Result<(String, String), io::Error> {
    let my_name = get_current_user_name()?;
    let my_machine_name = get_local_machine_name()?;

    let (his_name, his_machine_name) = parse_address(address, &my_machine_name);
    msg.vers = TALK_VERSION;
    msg.addr.sa_family = AF_INET as SaFamily;
    msg.ctl_addr.sa_family = AF_INET as SaFamily;
    msg.l_name = string_to_c_string(&my_name);
    msg.r_name = string_to_c_string(&his_name);
    msg.r_tty = tty_to_c_string(ttyname.as_ref().unwrap_or(&"".to_string()));

    Ok((my_machine_name, his_machine_name))
}

/// Copy `s` into a fixed-size NUL-terminated C buffer. An interior NUL ends the
/// copy (treated as the terminator); longer strings are truncated. Never panics.
fn copy_to_c_buffer<const N: usize>(s: &str) -> [i8; N] {
    let mut buffer = [0i8; N];
    for (i, &byte) in s
        .as_bytes()
        .iter()
        .take_while(|&&b| b != 0)
        .take(N - 1)
        .enumerate()
    {
        buffer[i] = byte as i8;
    }
    buffer
}

/// Converts a Rust string to a C-style string and stores it in a fixed-size buffer.
fn string_to_c_string(s: &str) -> [i8; BUFFER_SIZE] {
    copy_to_c_buffer(s)
}

/// Converts a Rust string to a C-style string for terminal names.
fn tty_to_c_string(s: &str) -> [i8; 16] {
    copy_to_c_buffer(s)
}

/// Resolves the IP addresses for both the local and remote machines, and retrieves the service port for communication.
///
/// # Arguments
///
/// * `msg` - A mutable reference to a `CtlMsg` structure, which will have its `pid` field set.
/// * `my_machine_name` - A string representing the local machine's hostname.
/// * `his_machine_name` - A string representing the remote machine's hostname.
///
/// # Returns
///
/// Returns a tuple containing the local machine's IP address, the remote machine's IP address, and the service port for the "ntalk" service.
///
/// # Errors
///
/// Returns an error if address resolution or service lookup fails.
fn get_addrs(
    msg: &mut CtlMsg,
    my_machine_name: &str,
    his_machine_name: &str,
) -> Result<(Ipv4Addr, Ipv4Addr, u16), std::io::Error> {
    let lhost = CString::new(my_machine_name)?;
    let rhost = CString::new(his_machine_name)?;
    let service = CString::new("ntalk")?;
    let protocol = CString::new("udp")?;

    msg.pid = unsafe { getpid() };

    let hints = addrinfo {
        ai_family: AF_INET, // IPv4 only
        ai_socktype: SOCK_DGRAM,
        ai_flags: AI_CANONNAME,
        ai_protocol: 0,
        ai_addrlen: 0,
        ai_canonname: ptr::null_mut(),
        ai_addr: ptr::null_mut(),
        ai_next: ptr::null_mut(),
    };

    let my_machine_addr = resolve_address(&lhost, &hints, my_machine_name)?;

    // If the remote machine is different from the local one, resolve its IP address as well.
    let his_machine_addr = if rhost != lhost {
        resolve_address(&rhost, &hints, his_machine_name)?
    } else {
        my_machine_addr
    };

    // Retrieve the service port for the "ntalk" service using the UDP protocol.
    let daemon_port = get_service_port(&service, &protocol)?;

    Ok((my_machine_addr, his_machine_addr, daemon_port))
}

/// Resolves the IP address for a given host using the specified hints for address resolution.
///
/// # Arguments
///
/// * `host` - A `CString` representing the hostname to resolve.
/// * `hints` - A reference to an `addrinfo` structure providing hints for the resolution process.
/// * `host_name` - A string representing the hostname (for error reporting).
///
/// # Returns
///
/// Returns `Result<Ipv4Addr, io::Error>` containing the resolved IPv4 address on success, or an error if resolution fails.
fn resolve_address(
    host: &CString,
    hints: &addrinfo,
    host_name: &str,
) -> Result<Ipv4Addr, io::Error> {
    let mut res: *mut addrinfo = ptr::null_mut();
    let err = unsafe { getaddrinfo(host.as_ptr(), ptr::null(), hints, &mut res) };

    if err != 0 {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!(
                "Error resolving address for {}: {}",
                host_name,
                unsafe { CStr::from_ptr(libc::gai_strerror(err)) }.to_string_lossy()
            ),
        ));
    }

    let mut addr = Ipv4Addr::UNSPECIFIED;
    let mut ai = res;
    while !ai.is_null() {
        let ai_ref = unsafe { &*ai };
        if ai_ref.ai_family == AF_INET {
            let sockaddr: &sockaddr_in = unsafe { &*(ai_ref.ai_addr as *const sockaddr_in) };
            addr = Ipv4Addr::from(u32::from_be(sockaddr.sin_addr.s_addr));
            break;
        }
        ai = ai_ref.ai_next;
    }

    if addr == Ipv4Addr::UNSPECIFIED {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("Address not found for {}", host_name),
        ));
    }

    Ok(addr)
}

/// Retrieves the port number for a given service and protocol by querying the system's service database.
///
/// # Arguments
///
/// * `service` - A `CString` representing the name of the service (e.g., "talk").
/// * `protocol` - A `CString` representing the protocol (e.g., "udp" or "tcp").
///
/// # Returns
///
/// Returns `Result<u16, io::Error>` containing the port number on success, or an error if the service is not found.
fn get_service_port(service: &CString, protocol: &CString) -> Result<u16, io::Error> {
    // Get the service by name
    let talkd_service = unsafe { getservbyname(service.as_ptr(), protocol.as_ptr()) };

    // Check if the service was found
    if talkd_service.is_null() {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!(
                "Service {} with protocol {} not registered.",
                service.to_str().unwrap_or("invalid service"),
                protocol.to_str().unwrap_or("invalid protocol")
            ),
        ));
    }

    // Safely get the port number from the service
    let port = unsafe { (*talkd_service).s_port };
    Ok(u16::from_be(port as u16))
}

/// Handles the client's connection by spawning a read thread and managing user input from stdin.
///
/// # Arguments
///
/// * `stream` - The `TcpStream` representing the client's connection.
///
/// # Returns
///
/// Returns `Result<(), io::Error>` if the operation completes successfully or an error occurs during execution.
fn handle_client(stream: TcpStream) -> Result<(), io::Error> {
    let write_stream = stream.try_clone()?;

    let (width, height) = get_terminal_size();
    let split_row = height / 2;
    let local_window = Arc::new(Mutex::new(Window::new(
        1,
        split_row.saturating_sub(1),
        width,
    )));
    let peer_window = Arc::new(Mutex::new(Window::new(
        split_row + 1,
        height.saturating_sub(split_row),
        width,
    )));

    // Spawn a separate thread to handle reading from the stream.
    spawn_input_thread(stream, split_row, width, Arc::clone(&peer_window)).unwrap();

    // Handle user input from stdin and send it to the write stream.
    handle_stdin_input(write_stream, split_row, local_window)?;

    Ok(())
}

/// Draws the terminal interface with a split line separating the top and bottom sections.
///
/// # Arguments
///
/// * `split_row` - The row number where the split line will be drawn.
/// * `width` - The width of the terminal in characters.
///
/// # Returns
///
/// Returns a `StdoutLock` wrapped in a `Result` if successful.
/// Any `io::Error` encountered during drawing will be returned.
///
/// # Errors
///
/// Returns an `io::Error` if there is an issue with writing to the terminal or flushing output.
fn draw_terminal(split_row: u16, width: u16) -> io::Result<io::StdoutLock<'static>> {
    let stdout = io::stdout();
    let mut handle = stdout.lock();

    // Clear terminal screen
    print!("\x1B[2J\x1B[H");
    io::stdout().flush().ok();

    // Display the connection message at the top of the terminal.
    write!(handle, "[Connection established]")?;
    // Move the cursor to the split row and draw the split line.
    write!(handle, "\x1b[{};0H", split_row)?;
    // Draw the horizontal split line (─) across the width of the terminal.
    let width = usize::from(width.saturating_sub(2));
    writeln!(handle, "└{:─<width$}┘", "", width = width)?;
    // Move the cursor back to the top-left corner and then down by one line.
    write!(handle, "\x1b[1;H")?;
    write!(handle, "\x1B[1B")?;

    handle.flush()?;

    Ok(handle)
}

/// Opens a TCP socket bound to the provided IPv4 address.
/// This socket is used for sending and receiving messages between talk users.
///
/// # Arguments
///
/// * `my_machine_addr` - The IPv4 address of the machine where the socket will be bound.
///
/// # Returns
///
/// Returns a tuple containing the `SocketAddr` and the `TcpListener` on success.
/// Returns an `io::Error` if there is an error in binding the socket.
fn open_socket(my_machine_addr: Ipv4Addr) -> Result<(SocketAddr, TcpListener), io::Error> {
    let listener = TcpListener::bind((my_machine_addr, 0))?;
    let addr = listener.local_addr()?;

    Ok((addr, listener))
}

/// Opens a UDP socket bound to the provided IPv4 address.
/// The socket is used for checking access to the connection.
///
/// # Arguments
///
/// * `my_machine_addr` - The IPv4 address of the machine where the socket will be bound.
///
/// # Returns
///
/// Returns a tuple containing the `SocketAddr` and a reference-counted `UdpSocket` on success.
/// Returns an `io::Error` if there is an error in binding the socket or setting non-blocking mode.
fn open_ctl(my_machine_addr: Ipv4Addr) -> Result<(SocketAddr, Arc<UdpSocket>), io::Error> {
    let socket = UdpSocket::bind((my_machine_addr, 0))?;
    // socket.set_nonblocking(true)?;
    let addr = socket.local_addr()?;

    Ok((addr, Arc::new(socket)))
}

/// Handles sending a message (`CtlMsg`) to the daemon and receiving a response.
/// Internally calls the `request` function for communication.
///
/// # Arguments
///
/// * `daemon_port` - The port number of the talk daemon.
/// * `his_machine_addr` - The IPv4 address of the remote user or localhost.
/// * `msg` - A mutable reference to the control message (`CtlMsg`) to be sent.
/// * `socket` - The UDP socket used to send and receive messages.
/// * `res` - A mutable reference where the received response (`CtlRes`) will be stored.
/// * `msg_type` - The type of message being sent (e.g., Delete, LookUp).
///
fn handle_invite(
    daemon_port: u16,
    his_machine_addr: Ipv4Addr,
    msg: &mut CtlMsg,
    socket: &UdpSocket,
    res: &mut CtlRes,
    msg_type: MessageType,
) -> Result<(), TalkError> {
    request(daemon_port, his_machine_addr, msg, msg_type, socket, res)
}

/// Sends a LookUp message to the talk daemon to search for an invite.
///
/// # Arguments
///
/// * `daemon_port` - The port number of the talk daemon.
/// * `his_machine_addr` - The IPv4 address of the remote user or localhost.
/// * `msg` - A mutable reference to the control message (`CtlMsg`) to be sent.
/// * `socket` - The UDP socket used to send and receive messages.
/// * `res` - A mutable reference where the received response (`CtlRes`) will be stored.
///
fn look_for_invite(
    daemon_port: u16,
    his_machine_addr: Ipv4Addr,
    msg: &mut CtlMsg,
    socket: &UdpSocket,
    res: &mut CtlRes,
) -> Result<(), TalkError> {
    handle_invite(
        daemon_port,
        his_machine_addr,
        msg,
        socket,
        res,
        MessageType::LookUp,
    )
}

/// Sends a LeaveInvite message to the talk daemon, indicating that the user is leaving the invite.
///
/// # Arguments
///
/// * `daemon_port` - The port number of the talk daemon.
/// * `his_machine_addr` - The IPv4 address of the remote user or localhost.
/// * `msg` - A mutable reference to the control message (`CtlMsg`) to be sent.
/// * `socket` - The UDP socket used to send and receive messages.
/// * `res` - A mutable reference where the received response (`CtlRes`) will be stored.
///
fn leave_invite(
    daemon_port: u16,
    his_machine_addr: Ipv4Addr,
    msg: &mut CtlMsg,
    socket: &UdpSocket,
    res: &mut CtlRes,
) -> Result<(), TalkError> {
    handle_invite(
        daemon_port,
        his_machine_addr,
        msg,
        socket,
        res,
        MessageType::LeaveInvite,
    )
}

/// Sends an Announce message to the talk daemon, announcing an invite.
///
/// # Arguments
///
/// * `daemon_port` - The port number of the talk daemon.
/// * `his_machine_addr` - The IPv4 address of the remote user or localhost.
/// * `msg` - A mutable reference to the control message (`CtlMsg`) to be sent.
/// * `socket` - The UDP socket used to send and receive messages.
/// * `res` - A mutable reference where the received response (`CtlRes`) will be stored.
///
fn announce(
    daemon_port: u16,
    his_machine_addr: Ipv4Addr,
    msg: &mut CtlMsg,
    socket: &UdpSocket,
    res: &mut CtlRes,
) -> Result<(), TalkError> {
    handle_invite(
        daemon_port,
        his_machine_addr,
        msg,
        socket,
        res,
        MessageType::Announce,
    )?;
    check_announce_answer(res)
}

/// Translate a daemon ANNOUNCE response into an error for any non-success
/// answer, so the caller stops instead of ringing forever (spec 123069: write
/// shall fail when the user lacks the appropriate privileges; talk likewise).
fn check_announce_answer(res: &CtlRes) -> Result<(), TalkError> {
    match res.answer {
        Answer::Success => Ok(()),
        Answer::NotHere => Err(TalkError::Other(gettext(
            "the addressed user is not logged on",
        ))),
        Answer::PermissionDenied => Err(TalkError::Other(gettext(
            "the addressed user's terminal does not permit messages",
        ))),
        ref other => Err(TalkError::Other(format!(
            "{}: {:?}",
            gettext("the talk daemon refused the request"),
            other
        ))),
    }
}

/// Sends a delete request (`CtlMsg`) to the talk daemon over a UDP socket and waits for a response.
/// This function is a wrapper around `handle_invite`, specifically for handling delete messages.
///
/// # Arguments
///
/// * `daemon_port` - The port number of the talk daemon.
/// * `his_machine_addr` - The IPv4 address of the remote user or localhost.
/// * `msg` - A mutable reference to the control message (`CtlMsg`) to be sent.
/// * `socket` - The UDP socket used to send and receive messages.
/// * `res` - A mutable reference where the received response (`CtlRes`) will be stored.
///
fn send_delete(
    daemon_port: u16,
    his_machine_addr: Ipv4Addr,
    msg: &mut CtlMsg,
    socket: &UdpSocket,
    res: &mut CtlRes,
) -> Result<(), TalkError> {
    handle_invite(
        daemon_port,
        his_machine_addr,
        msg,
        socket,
        res,
        MessageType::Delete,
    )
}

// ============================================================================
// Local Mode Helper Functions (Unix domain socket)
// ============================================================================

/// Sends a LookUp message to the local talkd daemon via Unix socket.
fn look_for_invite_local(
    socket_path: &Path,
    msg: &mut CtlMsg,
    res: &mut CtlRes,
) -> Result<(), TalkError> {
    request_local(socket_path, msg, MessageType::LookUp, res)
}

/// Sends a LeaveInvite message to the local talkd daemon via Unix socket.
fn leave_invite_local(
    socket_path: &Path,
    msg: &mut CtlMsg,
    res: &mut CtlRes,
) -> Result<(), TalkError> {
    request_local(socket_path, msg, MessageType::LeaveInvite, res)
}

/// Sends an Announce message to the local talkd daemon via Unix socket.
fn announce_local(socket_path: &Path, msg: &mut CtlMsg, res: &mut CtlRes) -> Result<(), TalkError> {
    request_local(socket_path, msg, MessageType::Announce, res)?;
    check_announce_answer(res)
}

/// Sends a Delete message to the local talkd daemon via Unix socket.
fn send_delete_local(
    socket_path: &Path,
    msg: &mut CtlMsg,
    res: &mut CtlRes,
) -> Result<(), TalkError> {
    request_local(socket_path, msg, MessageType::Delete, res)
}

/// Sends a control message (`CtlMsg`) to the talk daemon over a UDP socket and waits for a response.
///
/// This function retries sending the message if it encounters a `WouldBlock` error and will continue
/// trying until a response is received or an error occurs.
///
/// # Arguments
///
/// * `daemon_port` - The port number of the talk daemon.
/// * `his_machine_addr` - The IPv4 address of the remote user or localhost.
/// * `msg` - A mutable reference to the control message (`CtlMsg`) to be sent.
/// * `msg_type` - The type of the message, used to set the message type in `msg`.
/// * `socket` - The UDP socket used to send and receive messages.
/// * `res` - A mutable reference where the received response (`CtlRes`) will be stored.
///
/// # Returns
///
/// Returns `Ok(())` if the message is successfully sent and a valid response is received.
/// Returns a `TalkError` if an error occurs during sending, receiving, or message parsing.
///
/// # Errors
///
/// * `TalkError::AddressResolutionFailed` - If the talk daemon's address cannot be resolved.
/// * `TalkError::Other` - If the message serialization or response deserialization fails.
/// * `TalkError::IoError` - If a network I/O error occurs.
fn request(
    daemon_port: u16,
    his_machine_addr: Ipv4Addr,
    msg: &mut CtlMsg,
    msg_type: MessageType,
    socket: &UdpSocket,
    res: &mut CtlRes,
) -> Result<(), TalkError> {
    let talkd_addr: SocketAddr = format!("{}:{}", his_machine_addr, daemon_port)
        .parse()
        .map_err(|e: AddrParseError| TalkError::AddressResolutionFailed(e.to_string()))?;
    msg.r#type = msg_type as u8;
    let msg_bytes = msg
        .to_bytes()
        .map_err(|e| TalkError::Other(e.to_string()))?;

    let start_time = Instant::now();
    let timeout = Duration::from_secs(5);

    loop {
        // Try sending the message bytes to the talk daemon
        match socket.send_to(&msg_bytes, talkd_addr) {
            Ok(_) => {
                let mut buf = [0; 1024];
                // Try to receive a response from the talk daemon
                match socket.recv_from(&mut buf) {
                    Ok((amt, _)) => {
                        let ctl_res = CtlRes::from_bytes(&buf[..amt])
                            .map_err(|e| TalkError::Other(e.to_string()))?;
                        *res = ctl_res;
                        break;
                    }
                    Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                        // If we would block, check for timeout and continue
                        if start_time.elapsed() >= timeout {
                            eprintln!("Please check talk daemon status. Cannot connect to it!");
                            restore_terminal();
                            process::exit(128);
                        }

                        std::thread::sleep(Duration::from_millis(10));
                        continue;
                    }
                    Err(e) => {
                        eprintln!("Error receiving message: {}", e);
                        return Err(TalkError::IoError(e));
                    }
                }
            }
            Err(e) => {
                eprintln!("Error sending message: {}", e);
                return Err(TalkError::IoError(e));
            }
        }
    }
    Ok(())
}

/// Sends a control message to the local talkd daemon via Unix domain socket.
///
/// # Arguments
///
/// * `socket_path` - Path to the Unix socket
/// * `msg` - The control message to send
/// * `msg_type` - The type of message to send
/// * `res` - Mutable reference to store the response
///
/// # Returns
///
/// A `Result` indicating success or a `TalkError`.
fn request_local(
    socket_path: &Path,
    msg: &mut CtlMsg,
    msg_type: MessageType,
    res: &mut CtlRes,
) -> Result<(), TalkError> {
    let socket = UnixDatagram::unbound().map_err(TalkError::IoError)?;

    msg.r#type = msg_type as u8;
    let msg_bytes = msg
        .to_bytes()
        .map_err(|e| TalkError::Other(e.to_string()))?;

    let start_time = Instant::now();
    let timeout = Duration::from_secs(5);

    // Set socket timeout
    socket
        .set_read_timeout(Some(Duration::from_millis(100)))
        .map_err(TalkError::IoError)?;

    loop {
        match socket.send_to(&msg_bytes, socket_path) {
            Ok(_) => {
                let mut buf = [0; 1024];
                match socket.recv(&mut buf) {
                    Ok(amt) => {
                        let ctl_res = CtlRes::from_bytes(&buf[..amt])
                            .map_err(|e| TalkError::Other(e.to_string()))?;
                        *res = ctl_res;
                        return Ok(());
                    }
                    Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                        if start_time.elapsed() >= timeout {
                            eprintln!("Local talkd daemon not responding. Is it running?");
                            restore_terminal();
                            process::exit(128);
                        }
                        std::thread::sleep(Duration::from_millis(10));
                        continue;
                    }
                    Err(e) => {
                        return Err(TalkError::IoError(e));
                    }
                }
            }
            Err(ref e) if e.kind() == io::ErrorKind::ConnectionRefused => {
                eprintln!(
                    "Cannot connect to local talkd. Is it running at {:?}?",
                    socket_path
                );
                restore_terminal();
                process::exit(128);
            }
            Err(e) => {
                return Err(TalkError::IoError(e));
            }
        }
    }
}

/// Gets the local talkd socket path from environment or uses default.
fn get_local_socket_path() -> String {
    std::env::var("TALKD_SOCKET").unwrap_or_else(|_| DEFAULT_LOCAL_SOCKET.to_string())
}

/// Registers signal handlers for specific signals (SIGINT, SIGQUIT, SIGPIPE).
/// The signal handler function is `handle_signals`.
///
/// # Errors
///
/// Logs an error message if registering a signal handler fails.
pub fn register_signals() {
    unsafe {
        // List of signals to register handlers for
        let signals = &[SIGINT, SIGQUIT, SIGPIPE];

        // Register the `handle_signals` function for each signal
        for &sig in signals {
            if signal(sig, handle_signals as *const () as usize) == libc::SIG_ERR {
                eprintln!("Failed to register signal handler for signal {}", sig);
            }
        }

        // Window resize (audit #TK9). Separate from the list above because the
        // handler must not terminate: it only records that the geometry
        // changed, and the render loops re-query and repaint when they notice.
        if signal(libc::SIGWINCH, handle_winch as *const () as usize) == libc::SIG_ERR {
            eprintln!("Failed to register signal handler for SIGWINCH");
        }
    }
}

/// SIGWINCH handler: record the resize and return.
///
/// Async-signal-safe by construction -- a single atomic store, nothing else.
/// The terminal is re-measured and repainted by whichever render loop observes
/// the flag.
extern "C" fn handle_winch(_signal_code: i32) {
    RESIZED.store(true, Ordering::SeqCst);
}

/// Handles incoming signals by setting the interrupt flag and exiting the process.
///
/// # Arguments
///
/// * `signal_code` - The signal code received (e.g., from `SIGINT`).
///
/// # Errors
///
/// Logs an error message if sending or receiving messages fails.
pub fn handle_signals(signal_code: libc::c_int) {
    // Restore the terminal (we may be in raw mode) and clear the screen.
    restore_terminal();
    clear_terminal();
    eprintln!("Connection closed, exiting...");

    // Lock the DELETE_INVITATIONS mutex and check for an existing invitation
    if let Ok(guard) = DELETE_INVITATIONS.lock() {
        if let Some(state) = guard.as_ref() {
            // Handle the deletion of invitations
            handle_delete_invitations(&state.socket, &state.msg_bytes1, &state.talkd_addr);
            handle_delete_invitations(&state.socket, &state.msg_bytes2, &state.talkd_addr);
        }
    }

    // POSIX: a SIGINT shall terminate talk with a zero exit status. Other
    // trapped signals use the conventional 128+signal status.
    let code = if signal_code == SIGINT {
        0
    } else {
        128 + signal_code
    };
    std::process::exit(code);
}

/// Clears the terminal screen by sending escape sequences.
///
/// # Errors
///
/// This function does not return errors, but prints to standard error if the terminal cannot be cleared.
fn clear_terminal() {
    // clear screeen
    eprint!("\x1B[2J\x1B[H");
}

/// Handles sending a delete request and waiting for the response from the talk daemon.
///
/// # Arguments
///
/// * `socket` - A reference to the socket used for communication.
/// * `msg_bytes` - The bytes of the message to send.
/// * `talkd_addr` - The address of the talk daemon to which the message is sent.
///
/// # Errors
///
/// Logs an error message if sending or receiving messages fails.
fn handle_delete_invitations(socket: &UdpSocket, msg_bytes: &[u8], talkd_addr: &SocketAddr) {
    loop {
        if let Err(e) = socket.send_to(msg_bytes, talkd_addr) {
            eprintln!("Error sending message: {}", e);
            break;
        }

        let mut buf = [0; 1024];
        match socket.recv_from(&mut buf) {
            Ok((amt, _)) => {
                if let Ok(ctl_res) =
                    CtlRes::from_bytes(&buf[..amt]).map_err(|e| TalkError::Other(e.to_string()))
                {
                    if ctl_res.r#type == MessageType::Delete {
                        break;
                    }
                }
            }
            Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                std::thread::sleep(Duration::from_millis(10));
            }
            Err(e) => {
                eprintln!("Error receiving message: {}", e);
                break;
            }
        }
    }
}

/// Clears the terminal screen and exits the program.
///
/// This function prints a message indicating that the connection is closed,
/// clears the terminal, and terminates the process with exit code `3`.
fn handle_connection_close() {
    // The peer closed the connection: restore the terminal, clear the screen,
    // and exit cleanly (the conversation ended normally).
    restore_terminal();
    clear_terminal();
    io::stdout().flush().ok();
    eprintln!("Connection closed, exiting...");
    process::exit(0);
}

/// Retrieves the terminal size (width and height in characters) via an `ioctl` system call.
///
/// # Returns
///
/// A tuple `(width, height)`. Defaults to `(80, 24)` if the size cannot be retrieved.
///
/// # Errors
///
/// Prints an error message if the `ioctl` call fails.
fn get_terminal_size() -> (u16, u16) {
    let mut size: winsize = unsafe { zeroed() };

    unsafe {
        if ioctl(STDOUT_FILENO, TIOCGWINSZ, &mut size) == -1 {
            eprintln!("Failed to get terminal size");
            // Default fallback size
            return (80, 24);
        }
    }

    (size.ws_col, size.ws_row)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("talk");

    let args = Args::try_parse().unwrap_or_else(|err| {
        if err.kind() == ErrorKind::DisplayHelp || err.kind() == ErrorKind::DisplayVersion {
            // Print help or version message
            eprintln!("{}", err);
        } else {
            // Print custom error message
            eprintln!("Error parsing arguments: {}", err);
        }

        // Exit with a non-zero status code
        std::process::exit(1);
    });

    let mut exit_code = 0;

    register_signals();

    let result = if args.local {
        talk_local(args)
    } else {
        talk(args)
    };

    if let Err(err) = result {
        exit_code = 1;
        plib::diag::error(&err.to_string());
    }

    process::exit(exit_code)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_osock_addr_from_socket_addr_v4() {
        let socket = SocketAddrV4::new(Ipv4Addr::new(127, 0, 0, 1), 600);
        let value = Osockaddr::from(&socket);
        assert_eq!(value.sa_family, libc::AF_UNSPEC as u16);
        assert_eq!(value.sa_data, [2, 88, 127, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 2]);
    }

    /// Pin the on-the-wire layout of CtlMsg.
    ///
    /// The talk protocol's control message is a fixed 84-byte big-endian
    /// structure that a remote talkd parses positionally, so the encoding
    /// binrw generates is part of this program's external contract rather than
    /// an implementation detail. This asserts each field's offset and byte
    /// order so that a binrw upgrade cannot silently change the format.
    #[test]
    fn test_ctl_msg_wire_layout() {
        let msg = CtlMsg {
            vers: 1,
            r#type: MessageType::Announce as u8,
            answer: Answer::NotHere as u8,
            pad: 0,
            id_num: 0x0102_0304,
            addr: Osockaddr {
                sa_family: 0x0a0b,
                sa_data: [0x11; 14],
            },
            ctl_addr: Osockaddr {
                sa_family: 0x0c0d,
                sa_data: [0x22; 14],
            },
            pid: 0x0506_0708,
            l_name: string_to_c_string("local"),
            r_name: string_to_c_string("remote"),
            r_tty: [0; 16],
        };

        let bytes = msg.to_bytes().unwrap();
        assert_eq!(bytes.len(), 84, "CtlMsg is a fixed 84-byte structure");

        assert_eq!(&bytes[0..4], &[1, 3, 1, 0], "vers/type/answer/pad");
        assert_eq!(&bytes[4..8], &0x0102_0304u32.to_be_bytes(), "id_num is BE");

        assert_eq!(&bytes[8..10], &0x0a0bu16.to_be_bytes(), "addr.sa_family");
        assert_eq!(&bytes[10..24], &[0x11; 14], "addr.sa_data");
        assert_eq!(
            &bytes[24..26],
            &0x0c0du16.to_be_bytes(),
            "ctl_addr.sa_family"
        );
        assert_eq!(&bytes[26..40], &[0x22; 14], "ctl_addr.sa_data");

        assert_eq!(&bytes[40..44], &0x0506_0708u32.to_be_bytes(), "pid is BE");
        assert_eq!(&bytes[44..49], b"local", "l_name");
        assert_eq!(&bytes[56..62], b"remote", "r_name");
        assert_eq!(&bytes[68..84], &[0; 16], "r_tty");
    }

    /// CtlRes decodes from the 24-byte big-endian response the daemon sends.
    #[test]
    fn test_ctl_res_decodes_daemon_response() {
        let mut bytes = [0u8; 24];
        bytes[0] = 1; // vers
        bytes[1] = MessageType::Delete as u8;
        bytes[2] = Answer::Success as u8;
        bytes[3] = 0; // pad
        bytes[4..8].copy_from_slice(&0xdead_beefu32.to_be_bytes());
        bytes[8..10].copy_from_slice(&2u16.to_be_bytes()); // sa_family
        bytes[10..24].copy_from_slice(&[0x33; 14]);

        let res = CtlRes::from_bytes(&bytes).expect("decode CtlRes");
        assert_eq!(res.vers, 1);
        assert_eq!(res.r#type, MessageType::Delete);
        assert_eq!(res.answer, Answer::Success);
        assert_eq!(res.id_num, 0xdead_beef);
        assert_eq!(res.addr.sa_family, 2);
        assert_eq!(res.addr.sa_data, [0x33; 14]);
    }

    // ========================================================================
    // Split-screen renderer (audit #TK7, #TK14, #TK15-#TK18)
    //
    // The render path takes any `W: Write`, so these drive it against a Vec<u8>
    // and assert the exact bytes emitted -- no socket, tty, or thread.
    // ========================================================================

    fn render(chars: &str, win: &mut Window) -> (String, Vec<u8>) {
        let keys = KeyMap::default();
        let mut out: Vec<u8> = Vec::new();
        let mut sent: Vec<u8> = Vec::new();
        for c in chars.chars() {
            sent.extend(apply(c, &keys, win, &mut out).unwrap());
        }
        (String::from_utf8_lossy(&out).into_owned(), sent)
    }

    // ========================================================================
    // Address parsing (audit #TK19)
    // ========================================================================

    #[test]
    fn test_parse_address_bsd_precedence() {
        let me = "myhost";
        let cases: &[(&str, &str, &str)] = &[
            // (input, expected user, expected host)
            ("alice", "alice", "myhost"),
            ("alice@host", "alice", "host"),
            ("alice@host.example.com", "alice", "host.example.com"),
            // '@' wins over a dot appearing earlier: this is the case the old
            // leftmost-of-any-delimiter scan got wrong.
            ("first.last@host", "first.last", "host"),
            // Non-'@' forms are host-first, and use the *last* delimiter so a
            // dotted hostname keeps its labels.
            ("host!alice", "alice", "host"),
            ("host:alice", "alice", "host"),
            ("host.alice", "alice", "host"),
            ("ns1.example.com!alice", "alice", "ns1.example.com"),
            ("host.example.com:alice", "alice", "host.example.com"),
            ("a.b.c.alice", "alice", "a.b.c"),
        ];
        for (input, want_user, want_host) in cases {
            let (user, host) = parse_address(input, me);
            assert_eq!(
                (user.as_str(), host.as_str()),
                (*want_user, *want_host),
                "parsing {:?}",
                input
            );
        }
    }

    #[test]
    fn test_classify_follows_spec_rules() {
        let keys = KeyMap::default();
        // 116756 / 116757: alert and Ctrl-L are special.
        assert_eq!(classify('\x07', &keys), Action::Alert);
        assert_eq!(classify('\x0c', &keys), Action::Refresh);
        // 116758: erase and kill come from termios, not hardcoded.
        assert_eq!(classify('\x7f', &keys), Action::Erase);
        assert_eq!(classify('\x15', &keys), Action::Kill);
        // 116764: print and space classes are sent verbatim.
        assert_eq!(classify('a', &keys), Action::Send('a'));
        assert_eq!(classify(' ', &keys), Action::Send(' '));
        assert_eq!(classify('\t', &keys), Action::Send('\t'));
        // 116769: other non-printables become printable stand-ins.
        assert_eq!(classify('\x01', &keys), Action::Caret('\x01'));
        assert_eq!(classify('\n', &keys), Action::Newline);
    }

    #[test]
    fn test_erase_and_kill_honor_termios() {
        // A terminal configured with ^H as erase must get erase semantics for
        // 0x08, and 0x7f then becomes an ordinary non-printable.
        let keys = KeyMap {
            erase: 0x08,
            kill: 0x18,
        };
        assert_eq!(classify('\x08', &keys), Action::Erase);
        assert_eq!(classify('\x18', &keys), Action::Kill);
        assert_eq!(classify('\x7f', &keys), Action::Caret('\x7f'));
    }

    #[test]
    fn test_renderer_emits_cr_positioning_not_bare_newline() {
        // #TK17: with ICANON cleared a bare "\n" never returns to column 0, so
        // the renderer must position the cursor explicitly.
        let mut win = Window::new(1, 10, 20);
        let (out, sent) = render("ab\n", &mut win);
        assert!(
            !out.contains('\n') || out.contains("\x1b["),
            "expected explicit cursor positioning, got {:?}",
            out
        );
        assert!(
            out.contains("\u{1b}[2;1H"),
            "newline should move to row 2 col 1: {:?}",
            out
        );
        // A newline is still what goes on the wire.
        assert_eq!(sent, b"ab\n");
    }

    #[test]
    fn test_renderer_wraps_at_window_width() {
        // #TK14: long lines wrap at the terminal width instead of running off
        // the edge or being truncated at a byte count.
        let mut win = Window::new(1, 10, 4);
        let (out, _) = render("abcdef", &mut win);
        // After 4 columns the renderer moves to the next row.
        assert!(
            out.contains("\u{1b}[2;1H"),
            "expected a wrap to row 2: {:?}",
            out
        );
        assert_eq!(win.row, 1, "cursor should be on the second row");
    }

    #[test]
    fn test_renderer_erase_redraws_line() {
        let mut win = Window::new(1, 10, 20);
        let (_, _) = render("abc", &mut win);
        assert_eq!(win.line, "abc");
        let (out, sent) = render("\x7f", &mut win);
        assert_eq!(win.line, "ab");
        assert!(out.contains("ab"), "line should be redrawn: {:?}", out);
        assert_eq!(sent, vec![0x7f]);
    }

    #[test]
    fn test_renderer_kill_clears_line() {
        let mut win = Window::new(1, 10, 20);
        render("hello", &mut win);
        render("\x15", &mut win);
        assert_eq!(win.line, "");
        assert_eq!(win.col, 0);
    }

    #[test]
    fn test_refresh_is_local_only_and_alert_is_forwarded() {
        let mut win = Window::new(1, 10, 20);
        // Ctrl-L repaints locally and sends nothing (116757).
        let (_, sent) = render("\x0c", &mut win);
        assert!(sent.is_empty(), "Ctrl-L must not be transmitted");
        // The alert goes to the peer (116756).
        let (_, sent) = render("\x07", &mut win);
        assert_eq!(sent, vec![0x07]);
    }

    #[test]
    fn test_non_printable_sent_as_printable_sequence() {
        // 116769 + RATIONALE 116866-116869: raw control characters are not
        // transmitted; a printable stand-in is.
        let mut win = Window::new(1, 10, 20);
        let (out, sent) = render("\x01", &mut win);
        assert!(out.contains("^A"), "expected caret rendering: {:?}", out);
        assert_eq!(sent, b"^A");
    }

    #[test]
    fn test_multibyte_character_survives_round_trip() {
        // #TK10: non-ASCII must reach the peer as its own bytes, not narrowed
        // to one. It passes through regardless of the sender's LC_CTYPE, since
        // only the recipient's terminal can interpret it (RATIONALE
        // 116855-116857).
        let mut win = Window::new(1, 10, 20);
        let (_, sent) = render("é", &mut win);
        assert_eq!(sent, "é".as_bytes());
    }

    #[test]
    fn test_tiny_terminal_does_not_panic() {
        // #TK18: a terminal shorter than the old `split_row - 2` guard used to
        // underflow. Every dimension here is degenerate.
        let mut win = Window::new(1, 1, 1);
        render("abc\n\x7f\x15", &mut win);
        let mut win = Window::new(0, 0, 0);
        render("x\n", &mut win);
    }

    #[test]
    fn test_decode_pending_handles_split_multibyte() {
        // #TK10: a character split across two reads must not be dropped.
        let mut pending = Vec::new();
        pending.extend_from_slice(&"é".as_bytes()[..1]);
        assert_eq!(decode_pending(&mut pending), "");
        pending.extend_from_slice(&"é".as_bytes()[1..]);
        assert_eq!(decode_pending(&mut pending), "é");
        assert!(pending.is_empty());

        // Invalid bytes are consumed rather than wedging the decoder.
        let mut pending = vec![0xff, b'a'];
        assert_eq!(decode_pending(&mut pending), "\u{fffd}a");
        assert!(pending.is_empty());
    }
}
