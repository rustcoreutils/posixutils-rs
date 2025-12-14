# UUCP - Unix-to-Unix Copy

This directory contains a POSIX-compliant implementation of the UUCP utilities
using SSH as the transport layer.

## Overview

UUCP (Unix-to-Unix Copy) is a suite of utilities for transferring files and
executing commands between Unix systems. Originally developed at AT&T Bell
Laboratories in 1976-1977, UUCP was designed for communication over dial-up
telephone lines and serial connections, predating the modern Internet.

This implementation provides the three POSIX-specified UUCP utilities:

- **uucp** - Copy files between systems
- **uux** - Execute commands on remote systems
- **uustat** - Query and manage the job queue

## Historical Context

### Traditional UUCP

Traditional UUCP implementations used specialized protocols optimized for
unreliable serial connections:

| Protocol | Description |
|----------|-------------|
| **g** | Original packet-based protocol with sliding window (1-7) and error correction. Packet sizes 64-4096 bytes. Supported by all implementations. |
| **f** | Seven-bit ASCII protocol for text-only connections. Efficient for text, poor for binary data. |
| **t** | TCP-optimized protocol with no error checking (assumes reliable transport). 512-byte command blocks, 1024-byte data blocks. |
| **e** | Similar to t protocol, sends file sizes as ASCII before transmission. |
| **i** | Bidirectional sliding-window protocol supporting simultaneous transfers. |

Traditional UUCP also used a complex spool file system:
- `C.*` files - Command files specifying transfers
- `D.*` files - Data files for spooled content
- `X.*` files - Execution request files for remote commands

The handshake process involved hostname exchange, protocol negotiation, and
authentication via the `Systems` (or `L.sys`) configuration file.

### Why SSH?

Traditional UUCP protocols are obsolete:

1. **No modern infrastructure** - Dial-up modems and UUCP-over-TCP services
   are effectively extinct
2. **Security concerns** - Traditional UUCP authentication was weak
3. **Complexity** - The g protocol's error correction adds overhead on
   reliable networks
4. **Maintenance burden** - Implementing and maintaining legacy protocols
   provides no practical benefit

SSH provides:
- Strong authentication and encryption
- Reliable, connection-oriented transport
- Universal availability on Unix systems
- Zero configuration for systems with existing SSH access

## Implementation Architecture

### Transport Layer

All network operations use SSH via the system's `ssh` command:

```
┌─────────────────────────────────────────────────────────────┐
│                     UUCP Utilities                          │
│              (uucp, uux, uustat)                             │
├─────────────────────────────────────────────────────────────┤
│                     common.rs                               │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │ssh_send_file│  │ssh_fetch_file│ │  ssh_exec   │         │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘         │
├─────────┼────────────────┼────────────────┼─────────────────┤
│         │                │                │                 │
│         ▼                ▼                ▼                 │
│    ssh host 'cat > path'  ssh host 'cat path'  ssh host cmd │
│                                                             │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
                     System SSH Client
                              │
                              ▼
                      Remote SSH Server
```

### File Transfer (uucp)

File transfers are implemented as:

- **Local to remote**: `ssh host 'cat > /path/to/dest' < local_file`
- **Remote to local**: `ssh host 'cat /path/to/src' > local_file`
- **Remote to remote**: Fetch to local temp file, then send to destination

Directory creation uses `ssh host 'mkdir -p /path/to/dir'` when needed.

### Remote Execution (uux)

Commands are executed via `ssh host 'sh -c "command"'` with:

- Working directory created in `/tmp/uux_<jobid>/`
- Input files staged to execution host before running
- Standard input piped through SSH when `-p` flag is used
- Cleanup of working directory after execution

### Job Queue (uustat)

Jobs are stored in a spool directory with a simplified format:

```
$UUCP_SPOOL/
└── <system>/
    └── J.<jobid>
```

Job files contain:
```
id=<job_id>
user=<username>
system=<target_system>
command=<uucp|uux>
request=<description>
```

This simplified format replaces the traditional C.*/D.*/X.* file hierarchy.

## Path Notation

The standard UUCP path notation is supported:

```
system!path          Remote path on system
~/path               Public directory (/var/spool/uucppublic/path)
~user/path           User's home directory
/absolute/path       Absolute path
relative/path        Relative to current directory
```

**Note**: Multi-hop routing (`system1!system2!path`) is not supported and
will produce an error. This was a feature for store-and-forward networks
that has no modern equivalent.

## Configuration

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `UUCP_SPOOL` | Spool directory location | `/var/spool/uucp` or `~/.uucp/spool` |

### SSH Configuration

The utilities use the system SSH client with `BatchMode=yes`. Remote systems
must be accessible via SSH without interactive authentication (use ssh-agent,
SSH keys, or ControlMaster).

To configure SSH access for a UUCP remote system:

```bash
# In ~/.ssh/config
Host uucp-remote
    HostName remote.example.com
    User uucp
    IdentityFile ~/.ssh/uucp_key
    BatchMode yes
```

Then use:
```bash
uucp localfile.txt uucp-remote!/path/to/dest.txt
```

## Usage Examples

### File Transfer (uucp)

```bash
# Copy local file to remote system
uucp myfile.txt remotehost!/tmp/myfile.txt

# Copy from remote to local
uucp remotehost!/var/log/syslog ./remote-syslog.txt

# Copy to remote public directory
uucp data.txt remotehost!~/shared/data.txt

# Multiple files to remote directory
uucp file1.txt file2.txt remotehost!/backup/

# Queue job without executing (-r flag)
uucp -r largefile.dat remotehost!/data/

# Print job ID
uucp -j localfile.txt remotehost!/dest/
```

### Remote Execution (uux)

```bash
# Execute command on remote system
uux "remotehost!who"

# Pipe stdin to remote command
echo "Hello" | uux -p "remotehost!cat > /tmp/greeting.txt"

# Remote command with output redirection
uux "remotehost!date > /tmp/timestamp.txt"

# Local command execution
uux "!echo local command"

# Print job ID
uux -j "remotehost!uptime"
```

### Job Management (uustat)

```bash
# List your queued jobs
uustat

# Show queue summary by system
uustat -q

# Filter by system
uustat -s remotehost

# Filter by user
uustat -u alice

# Kill a queued job
uustat -k <jobid>

# Rejuvenate (touch) a job
uustat -r <jobid>
```

## POSIX Compliance

This implementation conforms to IEEE Std 1003.1-2024 (POSIX.1-2024) with the
following notes:

### Fully Implemented

- All command-line options for uucp, uux, and uustat
- Path notation (system!path, tilde expansion)
- Job queue management (queue, kill, rejuvenate)
- User and system filtering
- Mail notifications (-m, -n flags)
- Disallowed redirection operators in uux (>>, <<, >|, >&)

### Known Limitations

| Feature | Status | Notes |
|---------|--------|-------|
| Wildcard expansion | Warning | `system!*.txt` patterns not expanded |
| Multi-hop routing | Error | `a!b!c!path` not supported |
| Cross-system output routing | Warning | uux output to different host than execution |
| Grade/priority (-g) | Ignored | All jobs have equal priority |

These limitations reflect the minimal viable implementation approach
appropriate for UUCP's legacy status.

## Testing

Manual test scripts are provided in `test_scripts/`:

```bash
cd uucp/test_scripts
./run_tests.sh
```

The test suite:
- Creates an isolated SSH environment (does not modify ~/.ssh)
- Runs a local sshd on a high port with generated keys
- Tests all utilities with real SSH operations
- Cleans up automatically (use `--keep` to preserve test artifacts)

## Differences from Traditional UUCP

| Aspect | Traditional UUCP | This Implementation |
|--------|-----------------|---------------------|
| Transport | Dial-up modems, serial lines, TCP | SSH |
| Protocol | g, f, t, e, i protocols | SSH channel |
| Authentication | Systems/L.sys file | SSH keys/config |
| Error correction | g protocol windowing | TCP + SSH |
| Encryption | None | SSH encryption |
| Spool format | C.*/D.*/X.* files | J.* files only |
| Configuration | Multiple config files | SSH config only |
| Daemon | uucico polling daemon | None (immediate or queued) |

## Security Considerations

- Commands are passed through SSH, inheriting SSH's security model
- Shell metacharacters in paths are escaped using single-quote wrapping
- Job ownership is enforced for kill/rejuvenate operations
- The `-r` (queue only) mode stores job metadata but not credentials

## Files

```
uucp/
├── Cargo.toml      # Crate manifest
├── common.rs       # Shared library (SSH functions, job management)
├── uucp.rs         # File transfer utility
├── uux.rs          # Remote execution utility
├── uustat.rs       # Job queue management utility
├── README.md       # This file
└── test_scripts/   # Manual test suite
    ├── run_tests.sh
    ├── test_uucp.sh
    ├── test_uux.sh
    └── test_uustat.sh
```

## References

- [POSIX.1-2024 uucp specification](https://pubs.opengroup.org/onlinepubs/9799919799/)
- [Taylor UUCP documentation](https://www.airs.com/ian/uucp-doc/)
- [UUCP Internals FAQ](https://opennet.ru/docs/FAQ/network/Mail/uucp-internals.html)
