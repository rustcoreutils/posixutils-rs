extern crate clap;
extern crate libc;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use libc::fstat;
use plib::PROJECT_NAME;
use std::{
    collections::BTreeMap,
    env,
    ffi::{CStr, CString},
    fmt,
    fs::{self, File},
    io::{self, BufRead, Error, ErrorKind, Write},
    net::{IpAddr, Ipv4Addr, UdpSocket},
    os::unix::io::AsRawFd,
    path::{Component, Path, PathBuf},
    sync::mpsc,
    thread,
    time::Duration,
};

const PROC_PATH: &'static str = "/proc";
const PROC_MOUNTS: &'static str = "/proc/mounts";
const NAME_FIELD: usize = 20;

#[derive(Clone, Default, PartialEq)]
enum ProcType {
    #[default]
    Normal = 0,
    Mount = 1,
    Knfsd = 2,
    Swap = 3,
}

#[derive(Clone, Default, PartialEq)]
enum NameSpace {
    #[default]
    File = 0,
    Tcp = 1,
    Udp = 2,
}

#[derive(Clone, Default, PartialEq)]
enum Access {
    Cwd = 1,
    Exe = 2,
    #[default]
    File = 4,
    Root = 8,
    Mmap = 16,
    Filewr = 32,
}

#[derive(Clone)]
struct IpConnections {
    names: Names,
    lcl_port: u64,
    rmt_port: u64,
    rmt_addr: IpAddr,
    next: Option<Box<IpConnections>>,
}

impl Default for IpConnections {
    fn default() -> Self {
        IpConnections {
            names: Names::default(),
            lcl_port: 0,
            rmt_port: 0,
            rmt_addr: IpAddr::V4(Ipv4Addr::UNSPECIFIED),
            next: None,
        }
    }
}

impl IpConnections {
    fn new(names: Names, lcl_port: u64, rmt_port: u64, rmt_addr: IpAddr) -> Self {
        IpConnections {
            names,
            lcl_port,
            rmt_port,
            rmt_addr,
            next: None,
        }
    }

    fn iter(&self) -> IpConnectionsIterator {
        IpConnectionsIterator {
            current: Some(self),
        }
    }
}

struct IpConnectionsIterator<'a> {
    current: Option<&'a IpConnections>,
}

impl<'a> Iterator for IpConnectionsIterator<'a> {
    type Item = &'a IpConnections;

    fn next(&mut self) -> Option<Self::Item> {
        self.current.map(|node| {
            self.current = node.next.as_deref();
            node
        })
    }
}

#[derive(Clone, Default)]
struct Procs {
    pid: i32,
    uid: u32,
    access: Access,
    proc_type: ProcType,
    username: Option<i8>,
    command: String,
}

impl Procs {
    fn new(pid: i32, uid: u32, access: Access, proc_type: ProcType, command: String) -> Self {
        Self {
            pid,
            uid,
            access,
            proc_type,
            username: None,
            command,
        }
    }
}

#[derive(Default, Clone)]
struct UnixSocketList {
    name: String,
    device_id: u64,
    inode: u64,
    net_inode: u64,
    next: Option<Box<UnixSocketList>>,
}

impl UnixSocketList {
    fn new(name: String, device_id: u64, inode: u64, net_inode: u64) -> Self {
        UnixSocketList {
            name,
            device_id,
            inode,
            net_inode,
            next: None,
        }
    }

    fn add_socket(&mut self, name: String, device_id: u64, inode: u64, net_inode: u64) {
        let new_node = Box::new(UnixSocketList {
            name,
            device_id,
            net_inode,
            inode,
            next: self.next.take(),
        });

        self.next = Some(new_node);
    }

    fn iter(&self) -> UnixSocketListIterator {
        UnixSocketListIterator {
            current: Some(self),
        }
    }
}

struct UnixSocketListIterator<'a> {
    current: Option<&'a UnixSocketList>,
}

impl<'a> Iterator for UnixSocketListIterator<'a> {
    type Item = &'a UnixSocketList;

    fn next(&mut self) -> Option<Self::Item> {
        self.current.map(|node| {
            self.current = node.next.as_deref();
            node
        })
    }
}

#[derive(Default)]
struct InodeList {
    name: Names,
    device_id: u64,
    inode: u64,
    next: Option<Box<InodeList>>,
}

impl InodeList {
    fn new(name: Names, device_id: u64, inode: u64) -> Self {
        InodeList {
            name,
            device_id,
            inode,
            next: None,
        }
    }

    fn iter(&self) -> InodeListIterator {
        InodeListIterator {
            current: Some(self),
        }
    }
}

struct InodeListIterator<'a> {
    current: Option<&'a InodeList>,
}

impl<'a> Iterator for InodeListIterator<'a> {
    type Item = &'a InodeList;

    fn next(&mut self) -> Option<Self::Item> {
        self.current.map(|node| {
            self.current = node.next.as_deref();
            node
        })
    }
}

#[derive(Default, Clone)]
struct MountList {
    mountpoints: Vec<PathBuf>,
}

struct LibcStat {
    inner: libc::stat,
}

impl Default for LibcStat {
    fn default() -> Self {
        LibcStat {
            inner: unsafe { std::mem::zeroed() },
        }
    }
}

impl Clone for LibcStat {
    fn clone(&self) -> Self {
        LibcStat { inner: self.inner }
    }
}

#[derive(Clone, Default)]
struct Names {
    filename: PathBuf,
    name_space: NameSpace,
    matched_procs: Vec<Procs>,
    st: LibcStat,
}

impl Names {
    fn new(
        filename: PathBuf,
        name_space: NameSpace,
        st: LibcStat,
        matched_procs: Vec<Procs>,
    ) -> Self {
        Names {
            filename,
            name_space,
            st,
            matched_procs,
        }
    }

    fn add_procs(&mut self, proc: Procs) {
        let exists = self
            .matched_procs
            .iter()
            .any(|p| p.access == proc.access && p.pid == proc.pid);

        if !exists {
            self.matched_procs.push(proc);
        }
    }
}

#[derive(Default)]
struct DeviceList {
    name: Names,
    device_id: u64,
    next: Option<Box<DeviceList>>,
}

impl DeviceList {
    fn new(name: Names, device_id: u64) -> Self {
        DeviceList {
            name,
            device_id,
            next: None,
        }
    }
    fn iter(&self) -> DeviceListIterator {
        DeviceListIterator {
            current: Some(self),
        }
    }
}

struct DeviceListIterator<'a> {
    current: Option<&'a DeviceList>,
}

impl<'a> Iterator for DeviceListIterator<'a> {
    type Item = &'a DeviceList;

    fn next(&mut self) -> Option<Self::Item> {
        self.current.map(|node| {
            self.current = node.next.as_deref();
            node
        })
    }
}

/// fuser - list process IDs of all processes that have one or more files open
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// The file is treated as a mount point and the utility shall report on any files open in the file system.
    #[arg(short = 'c')]
    mount: bool,
    /// The report shall be only for the named files.
    #[arg(short = 'f')]
    named_files: bool,
    /// The user name, in parentheses, associated with each process ID written to standard output shall be written to standard error.
    #[arg(short = 'u')]
    user: bool,

    #[arg(required = true, name = "FILE", num_args(0..))]
    /// A pathname on which the file or file system is to be reported.
    file: Vec<PathBuf>,
}
use clap::CommandFactory;
fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let Args {
        mount, user, file, ..
    } = Args::try_parse().unwrap_or_else(|err| match err.kind() {
        clap::error::ErrorKind::DisplayHelp | clap::error::ErrorKind::DisplayVersion => {
            print!("{err}");
            std::process::exit(1);
        }
        _ => {
            let mut stdout = std::io::stdout();
            let mut cmd = Args::command();
            eprintln!("No process specification given");
            cmd.write_help(&mut stdout).unwrap();
            std::process::exit(1);
        }
    });

    let (
        mut names,
        mut unix_socket_list,
        mut mount_list,
        mut device_list,
        mut inode_list,
        mut need_check_map,
    ) = init_defaults(file);

    fill_unix_cache(&mut unix_socket_list)?;

    let net_dev = find_net_dev()?;

    for name in names.iter_mut() {
        name.name_space = determine_namespace(&name.filename);

        match name.name_space {
            NameSpace::File => handle_file_namespace(
                name,
                mount,
                &mut mount_list,
                &mut inode_list,
                &mut device_list,
                &mut need_check_map,
            )?,
            NameSpace::Tcp => {
                if !mount {
                    handle_tcp_namespace(name, &mut inode_list, net_dev)?
                }
            }
            NameSpace::Udp => {
                if !mount {
                    handle_udp_namespace(name, &mut inode_list, net_dev)?
                }
            }
        }

        if (scan_procs(
            need_check_map,
            name,
            &inode_list,
            &device_list,
            &unix_socket_list,
            net_dev,
        ))
        .is_err()
        {
            std::process::exit(1);
        }

        print_matches(name, user)?;
    }

    std::process::exit(0)
}

/// Initializes and returns default values.
///
/// # Arguments
///
/// * `file` - A vector of `PathBuf` representing the file paths used to initialize `Names` objects.
///
/// # Returns
///
/// Returns a tuple containing:
///
/// * Default-initialized `Vec<Names>`, UnixSocketList`, `MountList`, `DeviceList`, `InodeList`, `IpConnections` (TCP), and `IpConnections` (UDP).
/// * A boolean value set to `false`, indicating the initial state.
fn init_defaults(
    files: Vec<PathBuf>,
) -> (
    Vec<Names>,
    UnixSocketList,
    MountList,
    DeviceList,
    InodeList,
    bool,
) {
    let names_vec = files
        .iter()
        .map(|path| {
            Names::new(
                path.clone(),
                NameSpace::default(),
                LibcStat::default(),
                vec![],
            )
        })
        .collect();

    (
        names_vec,
        UnixSocketList::default(),
        MountList::default(),
        DeviceList::default(),
        InodeList::default(),
        false,
    )
}

/// Determines the `NameSpace` based on the presence of "tcp" or "udp" in the path string.
///
/// # Arguments
///
/// * `filename` -`PathBuf`.
///
/// # Returns
///
/// Namespace type
fn determine_namespace(filename: &Path) -> NameSpace {
    filename
        .to_str()
        .and_then(|name_str| {
            let parts: Vec<&str> = name_str.split('/').collect();
            if parts.len() == 2 && parts[0].parse::<u16>().is_ok() {
                match parts[1] {
                    "tcp" => Some(NameSpace::Tcp),
                    "udp" => Some(NameSpace::Udp),
                    _ => None,
                }
            } else {
                None
            }
        })
        .unwrap_or_else(NameSpace::default)
}

/// Processes file namespaces by expanding paths and updating lists based on the mount flag.
///
/// # Arguments
///
/// * `names` - A mutable reference to a `Names` object containing the file path.
/// * `mount` - A boolean indicating whether to handle mount information or not.
/// * `mount_list` - A mutable reference to a `MountList` for reading mount points.
/// * `inode_list` - A mutable reference to an `InodeList` for updating inode information.
/// * `device_list` - A mutable reference to a `DeviceList` for updating device information.
///
/// # Errors
///
/// Returns an error if path expansion, file status retrieval, or `/proc/mounts` reading fails.
///
/// # Returns
///
/// Returns `Ok(())` on success.
fn handle_file_namespace(
    names: &mut Names,
    mount: bool,
    mount_list: &mut MountList,
    inode_list: &mut InodeList,
    device_list: &mut DeviceList,
    need_check_map: &mut bool,
) -> Result<(), std::io::Error> {
    names.filename = expand_path(&names.filename)?;
    let st = timeout(&names.filename.to_string_lossy(), 5)?;
    read_proc_mounts(mount_list)?;

    if mount {
        *device_list = DeviceList::new(names.clone(), st.st_dev);
        *need_check_map = true;
    } else {
        let st = stat(&names.filename.to_string_lossy())?;
        *inode_list = InodeList::new(names.clone(), st.st_dev, st.st_ino);
    }
    Ok(())
}

/// Handles TCP namespace processing by updating connection and inode lists.
///
/// # Arguments
///
/// * `names` - A mutable reference to a `Names` object containing the file path.
/// * `tcp_connection_list` - A mutable reference to an `IpConnections` object for TCP connections.
/// * `inode_list` - A mutable reference to an `InodeList` for updating inode information.
/// * `net_dev` - A `u64` representing the network device identifier.
///
/// # Errors
///
/// Returns an error if parsing TCP connections or finding network sockets fails.
///
/// # Returns
///
/// Returns `Ok(())` on success.
fn handle_tcp_namespace(
    names: &mut Names,
    inode_list: &mut InodeList,
    net_dev: u64,
) -> Result<(), std::io::Error> {
    let tcp_connection_list = parse_inet(names)?;
    *inode_list = find_net_sockets(&tcp_connection_list, "tcp", net_dev)?;
    Ok(())
}

/// Handles UDP namespace processing by updating connection and inode lists.
///
/// # Arguments
///
/// * `names` - A mutable reference to a `Names` object containing the file path.
/// * `udp_connection_list` - A mutable reference to an `IpConnections` object for UDP connections.
/// * `inode_list` - A mutable reference to an `InodeList` for updating inode information.
/// * `net_dev` - A `u64` representing the network device identifier.
///
/// # Errors
///
/// Returns an error if parsing UDP connections or finding network sockets fails.
///
/// # Returns
///
/// Returns `Ok(())` on success.
fn handle_udp_namespace(
    names: &mut Names,
    inode_list: &mut InodeList,
    net_dev: u64,
) -> Result<(), std::io::Error> {
    let udp_connection_list = parse_inet(names)?;
    *inode_list = find_net_sockets(&udp_connection_list, "udp", net_dev)?;
    Ok(())
}

/// Prints process matches for a given `Names` object to `stderr` and `stdout`.
///
/// # Arguments
///
/// * `name` - A mutable reference to a `Names` object containing matched processes.
/// * `user` - A boolean indicating whether to display the process owner name.
///
/// # Errors
///
/// Returns an error if flushing output to `stderr` or `stdout` fails.
///
/// # Returns
///
/// Returns `Ok(())` on success.
fn print_matches(name: &mut Names, user: bool) -> Result<(), io::Error> {
    let mut proc_map: BTreeMap<i32, (String, u32)> = BTreeMap::new();
    let mut name_has_procs = false;
    let mut len = name.filename.to_string_lossy().len() + 1;

    eprint!("{}:", name.filename.display());
    while len < NAME_FIELD {
        len += 1;
        eprint!(" ");
    }
    io::stderr().flush()?;

    for procs in name.matched_procs.iter() {
        if procs.proc_type == ProcType::Normal {
            name_has_procs = true;
        }

        let entry = proc_map
            .entry(procs.pid)
            .or_insert((String::new(), procs.uid));

        match procs.access {
            Access::Root => entry.0.push('r'),
            Access::Cwd => entry.0.push('c'),
            Access::Exe => entry.0.push('e'),
            Access::Mmap => entry.0.push('m'),
            _ => (),
        }
    }

    if !name_has_procs {
        // exit if no processes matched
        return Ok(());
    }

    for (pid, (access, uid)) in proc_map {
        let width = if pid.to_string().len() > 4 { " " } else { "  " };

        print!("{}{}", width, pid);
        io::stdout().flush()?;

        eprint!("{}", access);
        if user {
            let owner = unsafe {
                let pw_entry = libc::getpwuid(uid);
                if pw_entry.is_null() {
                    "unknownr"
                } else {
                    CStr::from_ptr((*pw_entry).pw_name)
                        .to_str()
                        .unwrap_or("invalid_string")
                }
            };
            eprint!("({})", owner);
        }
        io::stderr().flush()?;
    }

    eprintln!();
    Ok(())
}

/// Scans the `/proc` directory for process information and checks various access types.
///
/// # Arguments
///
/// * `names` - A mutable reference to a `Names` object for storing matched processes.
/// * `inode_list` - A reference to an `InodeList` for updating inode information.
/// * `device_list` - A reference to a `DeviceList` for updating device information.
/// * `unix_socket_list` - A reference to a `UnixSocketList` for checking Unix sockets.
/// * `net_dev` - A `u64` representing the network device identifier.
///
/// # Errors
///
/// Returns an error if reading directory entries, accessing process stats, or checking access types fails.
///
/// # Returns
///
/// Returns `Ok(())` on success.
fn scan_procs(
    need_check_map: bool,
    names: &mut Names,
    inode_list: &InodeList,
    device_list: &DeviceList,
    unix_socket_list: &UnixSocketList,
    net_dev: u64,
) -> Result<(), io::Error> {
    let my_pid = std::process::id() as i32;
    let dir_entries = fs::read_dir(PROC_PATH)?;

    for entry in dir_entries {
        let entry = entry?;
        let filename = entry
            .file_name()
            .into_string()
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Invalid file name"))?;

        if let Ok(pid) = filename.parse::<i32>() {
            // Skip the current process
            if pid == my_pid {
                continue;
            }

            let cwd_stat = match get_pid_stat(pid, "/cwd") {
                Ok(stat) => stat,
                Err(_) => continue,
            };

            let exe_stat = match get_pid_stat(pid, "/exe") {
                Ok(stat) => stat,
                Err(_) => continue,
            };
            let root_stat = match get_pid_stat(pid, "/root") {
                Ok(stat) => stat,
                Err(_) => continue,
            };

            let st = timeout(&entry.path().to_string_lossy(), 5)?;
            let uid = st.st_uid;

            check_root_access(names, pid, uid, &root_stat, device_list, inode_list)?;
            check_cwd_access(names, pid, uid, &cwd_stat, device_list, inode_list)?;
            check_exe_access(names, pid, uid, &exe_stat, device_list, inode_list)?;

            if need_check_map {
                check_map(names, pid, "maps", device_list, uid, Access::Mmap)?;
            }

            check_dir(
                names,
                pid,
                "fd",
                device_list,
                inode_list,
                uid,
                Access::File,
                unix_socket_list,
                net_dev,
            )?;
        }
    }

    Ok(())
}

/// Checks if a process has access to the root directory and updates the `Names` object if it does.
///
/// # Arguments
///
/// * `names` - A mutable reference to a `Names` object for adding process information.
/// * `pid` - The process ID to check.
/// * `uid` - The user ID of the process.
/// * `root_stat` - A reference to a `libc::stat` structure containing root directory information.
/// * `device_list` - A reference to a `DeviceList` for checking device IDs.
/// * `inode_list` - A reference to an `InodeList` for checking inode information.
///
/// # Errors
///
/// Returns an error if adding process information fails.
///
/// # Returns
///
/// Returns `Ok(())` on success.
fn check_root_access(
    names: &mut Names,
    pid: i32,
    uid: u32,
    root_stat: &libc::stat,
    device_list: &DeviceList,
    inode_list: &InodeList,
) -> Result<(), io::Error> {
    if device_list
        .iter()
        .any(|device| device.device_id == root_stat.st_dev)
    {
        add_process(names, pid, uid, Access::Root, ProcType::Normal, None);
        return Ok(());
    }
    if inode_list
        .iter()
        .any(|inode| inode.device_id == root_stat.st_dev && inode.inode == root_stat.st_ino)
    {
        add_process(names, pid, uid, Access::Root, ProcType::Normal, None);
        return Ok(());
    }

    Ok(())
}

/// Checks if a process has access to the current working directory and updates the `Names` object if it does.
///
/// # Arguments
///
/// * `names` - A mutable reference to a `Names` object for adding process information.
/// * `pid` - The process ID to check.
/// * `uid` - The user ID of the process.
/// * `cwd_stat` - A reference to a `libc::stat` structure containing current working directory information.
/// * `device_list` - A reference to a `DeviceList` for checking device IDs.
/// * `inode_list` - A reference to an `InodeList` for checking inode information.
///
/// # Errors
///
/// Returns an error if adding process information fails.
///
/// # Returns
///
/// Returns `Ok(())` on success.
fn check_cwd_access(
    names: &mut Names,
    pid: i32,
    uid: u32,
    cwd_stat: &libc::stat,
    device_list: &DeviceList,
    inode_list: &InodeList,
) -> Result<(), std::io::Error> {
    if device_list
        .iter()
        .any(|device| device.device_id == cwd_stat.st_dev)
    {
        add_process(names, pid, uid, Access::Cwd, ProcType::Normal, None);
        return Ok(());
    }
    if inode_list
        .iter()
        .any(|inode| inode.device_id == cwd_stat.st_dev && inode.inode == cwd_stat.st_ino)
    {
        add_process(names, pid, uid, Access::Cwd, ProcType::Normal, None);
        return Ok(());
    }

    Ok(())
}

/// Checks if a process has access to the executable file and updates the `Names` object if it does.
///
/// # Arguments
///
/// * `names` - A mutable reference to a `Names` object for adding process information.
/// * `pid` - The process ID to check.
/// * `uid` - The user ID of the process.
/// * `exe_stat` - A reference to a `libc::stat` structure containing executable file information.
/// * `device_list` - A reference to a `DeviceList` for checking device IDs.
/// * `inode_list` - A reference to an `InodeList` for checking inode information.
///
/// # Errors
///
/// Returns an error if adding process information fails.
///
/// # Returns
///
/// Returns `Ok(())` on success.
fn check_exe_access(
    names: &mut Names,
    pid: i32,
    uid: u32,
    exe_stat: &libc::stat,
    device_list: &DeviceList,
    inode_list: &InodeList,
) -> Result<(), io::Error> {
    if device_list
        .iter()
        .any(|device| device.device_id == exe_stat.st_dev)
    {
        add_process(names, pid, uid, Access::Exe, ProcType::Normal, None);
        return Ok(());
    }
    if inode_list
        .iter()
        .any(|inode| inode.device_id == exe_stat.st_dev && inode.inode == exe_stat.st_ino)
    {
        add_process(names, pid, uid, Access::Exe, ProcType::Normal, None);
        return Ok(());
    }

    Ok(())
}

/// Adds a new process to the `Names` object with specified access and process type.
fn add_process(
    names: &mut Names,
    pid: i32,
    uid: u32,
    access: Access,
    proc_type: ProcType,
    command: Option<String>,
) {
    let proc = Procs::new(pid, uid, access, proc_type, command.unwrap_or_default());
    names.add_procs(proc);
}

/// Checks a directory within a process's `/proc` entry for matching devices and inodes,
/// and updates the `Names` object with relevant process information.
///
/// # Arguments
///
/// * `names` - A mutable reference to a `Names` object for adding process information.
/// * `pid` - The process ID whose directory is being checked.
/// * `dirname` - The name of the directory to check (e.g., "fd").
/// * `device_list` - A reference to a `DeviceList` for checking device IDs.
/// * `inode_list` - A reference to an `InodeList` for checking inode information.
/// * `uid` - The user ID of the process.
/// * `access` - The type of access to assign (e.g., File, Filewr).
/// * `unix_socket_list` - A reference to a `UnixSocketList` for checking Unix sockets.
/// * `net_dev` - A `u64` representing the network device identifier.
///
/// # Errors
///
/// Returns an error if reading directory entries or accessing file stats fails.
///
/// # Returns
///
/// Returns `Ok(())` on success.
fn check_dir(
    names: &mut Names,
    pid: i32,
    dirname: &str,
    device_list: &DeviceList,
    inode_list: &InodeList,
    uid: u32,
    access: Access,
    unix_socket_list: &UnixSocketList,
    net_dev: u64,
) -> Result<(), io::Error> {
    let dir_path = format!("/proc/{}/{}", pid, dirname);
    let dir_entries = fs::read_dir(&dir_path)?;
    for entry in dir_entries {
        let entry = entry?;
        let path = entry.path();
        let path_str = path.to_string_lossy();

        let mut stat = match timeout(&path_str, 5) {
            Ok(stat) => stat,
            Err(_) => continue,
        };

        if stat.st_dev == net_dev {
            if let Some(unix_socket) = unix_socket_list
                .iter()
                .find(|sock| sock.net_inode == stat.st_ino)
            {
                stat.st_dev = unix_socket.device_id;
                stat.st_ino = unix_socket.inode;
            }
        }

        let new_access = match access {
            Access::File => Access::Filewr,
            _ => access.clone(),
        };
        if device_list
            .iter()
            .any(|dev| dev.name.filename != PathBuf::from("") && stat.st_dev == dev.device_id)
            || inode_list.iter().any(|inode| inode.inode == stat.st_ino)
        {
            add_process(names, pid, uid, new_access, ProcType::Normal, None);
        }
    }

    Ok(())
}

/// Checks the memory map of a process for matching devices and updates the `Names` object.
///
/// # Arguments
///
/// * `names` - A mutable reference to a `Names` object for adding process information.
/// * `pid` - The process ID whose memory map is being checked.
/// * `filename` - The name of the file containing the memory map (e.g., "maps").
/// * `device_list` - A reference to a `DeviceList` for checking device IDs.
/// * `uid` - The user ID of the process.
/// * `access` - The type of access to assign (e.g., Mmap).
///
/// # Errors
///
/// Returns an error if opening the file or reading lines fails.
///
/// # Returns
///
/// Returns `Ok(())` on success.
fn check_map(
    names: &mut Names,
    pid: i32,
    filename: &str,
    device_list: &DeviceList,
    uid: u32,
    access: Access,
) -> Result<(), io::Error> {
    let already_exists = names.matched_procs.iter().any(|p| p.pid == pid);

    if already_exists {
        return Ok(());
    }

    let pathname = format!("/proc/{}/{}", pid, filename);
    let file = File::open(&pathname)?;
    let reader = io::BufReader::new(file);

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() >= 5 {
            let dev_info: Vec<&str> = parts[3].split(':').collect();
            if dev_info.len() == 2 {
                let tmp_maj = match u32::from_str_radix(dev_info[0], 16) {
                    Ok(value) => value,
                    Err(_) => continue,
                };

                let tmp_min = match u32::from_str_radix(dev_info[1], 16) {
                    Ok(value) => value,
                    Err(_) => continue,
                };

                let device = tmp_maj * 256 + tmp_min;
                let device_u64 = device as u64;

                if device_list
                    .iter()
                    .any(|device| device.device_id == device_u64)
                {
                    add_process(names, pid, uid, access.clone(), ProcType::Normal, None);
                }
            }
        }
    }
    Ok(())
}

/// get stat of current /proc/{pid}/{filename}
fn get_pid_stat(pid: i32, filename: &str) -> Result<libc::stat, io::Error> {
    let path = format!("{}/{}{}", PROC_PATH, pid, filename);
    timeout(&path, 5)
}

/// Fills the `unix_socket_list` with information from the `/proc/net/unix` file.
///
/// # Arguments
///
/// * `unix_socket_list` - A mutable reference to a `UnixSocketList` to be populated with socket information.
///
/// # Errors
///
/// Returns an error if opening the file or reading lines fails.
///
/// # Returns
///
/// Returns `Ok(())` on success.
fn fill_unix_cache(unix_socket_list: &mut UnixSocketList) -> Result<(), io::Error> {
    let file = File::open("/proc/net/unix")?;
    let reader = io::BufReader::new(file);

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split_whitespace().collect();

        if let (Some(net_inode_str), Some(scanned_path)) = (parts.get(6), parts.get(7)) {
            let net_inode = net_inode_str.parse().unwrap_or(0);
            let path = normalize_path(scanned_path);

            match timeout(&path, 5) {
                Ok(stat) => UnixSocketList::add_socket(
                    unix_socket_list,
                    scanned_path.to_string(),
                    stat.st_dev,
                    stat.st_ino,
                    net_inode,
                ),
                Err(_) => continue,
            }
        }
    }
    Ok(())
}

/// Reads the `/proc/mounts` file and updates the `mount_list` with mount points.
///
/// # Arguments
///
/// * `mount_list` - A mutable reference to a `MountList` for storing the mount points.
///
/// # Errors
///
/// Returns an error if opening the file or reading lines fails.
///
/// # Returns
///
/// Returns `Ok(mount_list)` on success, with the `mount_list` updated.
fn read_proc_mounts(mount_list: &mut MountList) -> io::Result<&mut MountList> {
    let file = File::open(PROC_MOUNTS)?;
    let reader = io::BufReader::new(file);

    for line in reader.lines() {
        let line = line?;
        let parts: Vec<&str> = line.split_whitespace().collect();
        let mountpoint = PathBuf::from(parts[1].trim());
        mount_list.mountpoints.push(mountpoint);
    }

    Ok(mount_list)
}

/// Normalizes a file path by removing the leading '@' character if present.
fn normalize_path(scanned_path: &str) -> String {
    if let Some(path) = scanned_path.strip_prefix('@') {
        path.to_string()
    } else {
        scanned_path.to_string()
    }
}

/// Parses network socket information from the `filename` field of the `Names` struct
/// and returns an `IpConnections` instance.
///
/// # Arguments
///
/// * `names` - A mutable reference to a `Names` struct containing the filename to parse.
/// * `ip_list` - A mutable reference to an `IpConnections` instance to be populated.
///
/// # Errors
///
/// Returns an error if the filename format is invalid or if parsing fails.
///
/// # Returns
///
/// Returns an `IpConnections` instance populated with parsed network information.
fn parse_inet(names: &mut Names) -> Result<IpConnections, io::Error> {
    let filename_str = names.filename.to_string_lossy();
    let parts: Vec<&str> = filename_str.split('/').collect();

    if parts.len() < 2 {
        return Err(Error::new(
            ErrorKind::InvalidInput,
            "Invalid filename format",
        ));
    }

    let hostspec = parts[0];
    let host_parts: Vec<&str> = hostspec.split(',').collect();

    let lcl_port_str = host_parts
        .first()
        .ok_or_else(|| Error::new(ErrorKind::InvalidInput, "Local port is missing"))?;
    let rmt_addr_str = host_parts.get(1).cloned();
    let rmt_port_str = host_parts.get(2).cloned();

    let lcl_port = lcl_port_str
        .parse::<u64>()
        .map_err(|_| Error::new(ErrorKind::InvalidInput, "Invalid local port format"))?;

    let rmt_port = rmt_port_str
        .as_ref()
        .map(|s| {
            s.parse::<u64>()
                .map_err(|_| Error::new(ErrorKind::InvalidInput, "Invalid remote port format"))
        })
        .transpose()?
        .unwrap_or(0);

    let rmt_addr = match rmt_addr_str {
        Some(addr_str) => match addr_str.parse::<IpAddr>() {
            Ok(addr) => addr,
            Err(_) => {
                eprintln!("Warning: Invalid remote address {}", addr_str);
                IpAddr::V4(Ipv4Addr::UNSPECIFIED) // Default value if address parsing fails
            }
        },
        None => IpAddr::V4(Ipv4Addr::UNSPECIFIED), // Default value if address is not provided
    };

    Ok(IpConnections::new(
        names.clone(),
        lcl_port,
        rmt_port,
        rmt_addr,
    ))
}
/// Retrieves the device identifier of the network interface associated with a UDP socket.
///
/// # Errors
///
/// Returns an error if binding the socket or retrieving the device identifier fails.
///
/// # Returns
///
/// Returns the device identifier (`u64`) of the network interface.
fn find_net_dev() -> Result<u64, io::Error> {
    let socket = UdpSocket::bind("0.0.0.0:0")?;
    let fd = socket.as_raw_fd();
    let mut stat_buf = unsafe { std::mem::zeroed() };

    unsafe {
        if fstat(fd, &mut stat_buf) != 0 {
            return Err(io::Error::last_os_error());
        }
    }

    Ok(stat_buf.st_dev as u64)
}

/// Finds network sockets based on the given protocol and updates the `InodeList`
/// with the relevant inode information if a matching connection is found.
///
/// # Arguments
///
/// * `inode_list` - A mutable reference to the `InodeList` that will be updated.
/// * `connections_list` - A reference to the `IpConnections` that will be used to match connections.
/// * `protocol` - A `&str` representing the protocol (e.g., "tcp", "udp") to look for in `/proc/net`.
/// * `net_dev` - A `u64` representing the network device identifier.
///
/// # Errors
///
/// Returns an `io::Error` if there is an issue opening or reading the file at `/proc/net/{protocol}`, or
/// if parsing the net sockets fails.
///
/// # Returns
///
/// Returns an `InodeList` containing the updated information if a matching connection is found.
/// Returns an `io::Error` with `ErrorKind::ConnectionRefused` if can't parse sockets.

fn find_net_sockets(
    connections_list: &IpConnections,
    protocol: &str,
    net_dev: u64,
) -> Result<InodeList, io::Error> {
    let pathname = format!("/proc/net/{}", protocol);

    let file = File::open(&pathname)?;
    let reader = io::BufReader::new(file);

    for line in reader.lines() {
        let line = line?;

        let parts: Vec<&str> = line.split_whitespace().collect();
        let parse_hex_port = |port_str: &str| -> Option<u64> {
            port_str
                .split(':')
                .nth(1)
                .and_then(|s| u64::from_str_radix(s, 16).ok())
        };

        let loc_port = parts.get(1).and_then(|&s| parse_hex_port(s));
        let rmt_port = parts.get(2).and_then(|&s| parse_hex_port(s));
        let scanned_inode = parts.get(9).and_then(|&s| s.parse::<u64>().ok());

        if let Some(scanned_inode) = scanned_inode {
            for connection in connections_list.iter() {
                let loc_port = loc_port.unwrap_or(0);
                let rmt_port = rmt_port.unwrap_or(0);
                let rmt_addr = parse_ipv4_addr(parts[2].split(':').next().unwrap_or(""))
                    .unwrap_or(Ipv4Addr::UNSPECIFIED);

                if (connection.lcl_port == 0 || connection.lcl_port == loc_port)
                    && (connection.rmt_port == 0 || connection.rmt_port == rmt_port)
                    && (connection.rmt_addr == Ipv4Addr::UNSPECIFIED
                        || connection.rmt_addr == rmt_addr)
                {
                    return Ok(InodeList::new(
                        connection.names.clone(),
                        net_dev,
                        scanned_inode,
                    ));
                }
            }
        }
    }

    Err(Error::new(
        ErrorKind::ConnectionRefused,
        "Cannot parse net sockets",
    ))
}

/// Parses a hexadecimal string representation of an IPv4 address.
fn parse_ipv4_addr(addr: &str) -> Option<Ipv4Addr> {
    if addr.len() == 8 {
        let octets = [
            u8::from_str_radix(&addr[0..2], 16).ok()?,
            u8::from_str_radix(&addr[2..4], 16).ok()?,
            u8::from_str_radix(&addr[4..6], 16).ok()?,
            u8::from_str_radix(&addr[6..8], 16).ok()?,
        ];
        Some(Ipv4Addr::from(octets))
    } else {
        None
    }
}

/// Retrieves the status of a file given its filename.
fn stat(filename_str: &str) -> io::Result<libc::stat> {
    let filename = CString::new(filename_str)?;

    unsafe {
        let mut st: libc::stat = std::mem::zeroed();
        let rc = libc::stat(filename.as_ptr(), &mut st);
        if rc == 0 {
            Ok(st)
        } else {
            Err(io::Error::last_os_error())
        }
    }
}

/// Execute stat() system call with timeout to avoid deadlock
/// on network based file systems.
fn timeout(path: &str, seconds: u32) -> Result<libc::stat, io::Error> {
    let (tx, rx) = mpsc::channel();

    thread::scope(|s| {
        s.spawn(|| {
            if let Err(e) = tx.send(stat(path)) {
                eprintln!("Failed to send result through channel: {}", e);
            }
        });
    });

    match rx.recv_timeout(Duration::from_secs(seconds.into())) {
        Ok(stat) => stat,
        Err(mpsc::RecvTimeoutError::Timeout) => Err(io::Error::new(
            io::ErrorKind::TimedOut,
            "Operation timed out",
        )),
        Err(mpsc::RecvTimeoutError::Disconnected) => {
            Err(io::Error::new(io::ErrorKind::Other, "Channel disconnected"))
        }
    }
}

/// This function handles relative paths by resolving them against the current working directory to absolute path
///
/// # Arguments
///
/// * `path` - [str](std::str) that represents the file path.
///
/// # Errors
///
/// Returns an error if passed invalid input.
///
/// # Returns
///
/// Returns PathBuf real_path.
pub fn expand_path(path: &PathBuf) -> Result<PathBuf, io::Error> {
    let mut real_path = if path.starts_with(Path::new("/")) {
        PathBuf::from("/")
    } else {
        env::current_dir()?
    };

    for component in Path::new(path).components() {
        match component {
            Component::CurDir => {
                // Ignore '.'
            }
            Component::ParentDir => {
                // Handle '..' by moving up one directory level if possible
                real_path.pop();
            }
            Component::Normal(name) => {
                // Append directory or file name
                real_path.push(name);
            }
            Component::RootDir | Component::Prefix(_) => {
                // Handle root directory or prefix
                real_path = PathBuf::from(component.as_os_str());
            }
        }
    }

    if real_path.as_os_str() != "/" && real_path.as_os_str().to_string_lossy().ends_with('/') {
        real_path.pop();
    }

    Ok(real_path)
}
