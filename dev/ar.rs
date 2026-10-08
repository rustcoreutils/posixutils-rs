//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod armap;

use armap::member_symbols;
use clap::{Parser, Subcommand};
use gettextrs::gettext;
use plib::diag;
use std::ffi::{OsStr, OsString};
use std::io::{stdout, Write};
use std::path::Path;

#[derive(clap::Args)]
#[group(required = false, multiple = false)]
struct InsertArgs {
    #[arg(short = 'a', help = gettext("Insert the files after the specified member"))]
    insert_after: bool,

    #[arg(short = 'b', short_alias = 'i', help = gettext("Insert the files before the specified member"))]
    insert_before: bool,
}

#[derive(clap::Args)]
struct DeleteArgs {
    #[arg(short = 'v', help = gettext("Give verbose output"))]
    verbose: bool,

    archive: OsString,
    files: Vec<OsString>,
}

#[derive(clap::Args)]
struct MoveArgs {
    #[arg(short = 'v', help = gettext("Give verbose output"))]
    verbose: bool,

    #[command(flatten)]
    insert_args: InsertArgs,

    files: Vec<OsString>,
}

#[derive(clap::Args)]
struct PrintArgs {
    #[arg(short = 'v', help = gettext("Give verbose output"))]
    verbose: bool,

    #[arg(short = 's', help = gettext("Force regeneration of the archive's symbol table"))]
    regenerate_symbol_table: bool,

    archive: OsString,
    files: Vec<OsString>,
}

#[derive(clap::Args)]
struct QuickAppendArgs {
    #[arg(short = 'c', help = gettext("Suppress archive creation diagnostics"))]
    no_create_message: bool,

    // Accepted for `ar rcs` / `ar qs`: the symbol table is always written.
    #[arg(short = 's', help = gettext("Write the archive's symbol table (always done)"))]
    symbol_table: bool,

    #[arg(short = 'v', help = gettext("Give verbose output"))]
    verbose: bool,

    archive: String,
    files: Vec<String>,
}

#[derive(clap::Args)]
struct ReplaceArgs {
    #[arg(short = 'c', help = gettext("Suppress archive creation diagnostics"))]
    no_create_message: bool,

    // Accepted for `ar rcs` / `ar qs`: the symbol table is always written.
    #[arg(short = 's', help = gettext("Write the archive's symbol table (always done)"))]
    symbol_table: bool,

    #[arg(short = 'u', help = gettext("Update older files in the archive"))]
    update_if_not_newer: bool,

    #[arg(short = 'v', help = gettext("Give verbose output"))]
    verbose: bool,

    #[command(flatten)]
    insert_args: InsertArgs,

    files: Vec<OsString>,
}

#[derive(clap::Args)]
struct ListArgs {
    #[arg(short = 'v', help = gettext("Give verbose output"))]
    verbose: bool,

    #[arg(short = 's', help = gettext("Force regeneration of the archive's symbol table"))]
    regenerate_symbol_table: bool,

    archive: OsString,
    files: Vec<OsString>,
}

#[derive(clap::Args)]
struct ExtractArgs {
    #[arg(short = 'v', help = gettext("Give verbose output"))]
    verbose: bool,

    #[arg(short = 's', help = gettext("Force regeneration of the archive's symbol table"))]
    regenerate_symbol_table: bool,

    #[arg(short = 'C', help = gettext("Do not replace existing files"))]
    dont_replace_files: bool,

    #[arg(short = 'T', help = gettext("Allow truncation of file names from the archive"))]
    allow_truncation: bool,

    archive: OsString,
    files: Vec<OsString>,
}

#[derive(Subcommand)]
enum Commands {
    #[command(name = "-d", about = gettext("Delete one or more files from the archive"))]
    Delete(DeleteArgs),
    #[command(name = "-m", about = gettext("Move named files within the archive"))]
    Move(MoveArgs),
    #[command(name = "-p", about = gettext("Print the contents of the files in the archive"))]
    Print(PrintArgs),
    #[command(name = "-q", about = gettext("Append files to the archive without checking for duplicates"))]
    QuickAppend(QuickAppendArgs),
    #[command(name = "-r", about = gettext("Replace or add files to the archive"))]
    Replace(ReplaceArgs),
    #[command(name = "-t", about = gettext("List the contents of the archive"))]
    List(ListArgs),
    #[command(name = "-x", about = gettext("Extract files from the archive"))]
    Extract(ExtractArgs),
}

/// ar - create and maintain library archives
#[derive(Parser)]
#[command(version, about = gettext("ar - create and maintain library archives"))]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

const MEMBER_HEADER_SIZE: u64 = 60;
const DATE_FORMAT: &str = "%b %e %H:%M %Y";

type ArResult<T> = Result<T, Box<dyn std::error::Error>>;

#[derive(Default)]
struct ArchiveMember {
    name: OsString,
    date: u64,
    uid: u64,
    gid: u64,
    mode: u64,
    size: u64,
    data: Vec<u8>,
    symbols: Vec<String>,
    symbol_bytes: u64,
}

impl ArchiveMember {
    fn read(file_path: &Path) -> ArResult<Self> {
        if !file_path.exists() {
            return Err(format!(
                "{}: {}",
                file_path.display(),
                gettext("No such file or directory")
            )
            .into());
        }

        if !file_path.is_file() {
            return Err(format!("{}: {}", file_path.display(), gettext("Is a directory")).into());
        }

        let file_metadata = file_path.metadata()?;
        // we already checked that the path is to a file so unwrap is safe
        let name = file_path.file_name().unwrap().to_os_string();

        let (uid, gid, mode) = owner_and_mode(&file_metadata);
        let data = std::fs::read(file_path)?;
        let symbols = member_symbols(&data);
        let symbol_bytes = symbols.iter().map(|s| s.len() as u64 + 1).sum::<u64>();

        // The archive date field is the member's mtime as Unix epoch seconds
        // (#A1). The previous `t.elapsed()` stored the file's *age*, producing
        // dates near 1970-01-01 on `ar -tv` and breaking `ar -ru`.
        let date = file_metadata
            .modified()
            .ok()
            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
            .map(|d| d.as_secs())
            .unwrap_or_default();

        Ok(ArchiveMember {
            name,
            date,
            uid,
            gid,
            mode,
            size: file_metadata.len(),
            data,
            symbols,
            symbol_bytes,
        })
    }

    fn write<W: Write>(&self, writer: &mut W, long_name_offset: Option<usize>) -> ArResult<()> {
        // format definition taken from: https://en.wikipedia.org/wiki/Ar_(Unix)

        // Since we are using the System V (or GNU) archive format, the data section
        // needs to be 2 byte aligned, if it isn't we add a newline as filler.
        //
        // #A13: the header `size` field is the *payload* size. The alignment pad
        // written below is not part of the member and must not be counted, or
        // every reader hands the pad byte back as data -- `ar -x` then writes a
        // file one byte too long, and each rewrite appends another newline.
        writer.write_all(&format_name_for_header(&self.name, long_name_offset)?)?;
        writer.write_all(&pad_metadata_with_spaces::<12>(self.date.to_string())?)?;
        writer.write_all(&pad_metadata_with_spaces::<6>(self.uid.to_string())?)?;
        writer.write_all(&pad_metadata_with_spaces::<6>(self.gid.to_string())?)?;
        writer.write_all(&pad_metadata_with_spaces::<8>(format!("{:o}", self.mode))?)?;
        writer.write_all(&pad_metadata_with_spaces::<10>(self.size.to_string())?)?;
        writer.write_all(&object::archive::TERMINATOR)?;
        writer.write_all(&self.data)?;
        if !self.data.len().is_multiple_of(2) {
            writer.write_all(b"\n")?;
        }

        Ok(())
    }
}

enum InsertPosition {
    After(usize),
    Before(usize),
    End,
}

#[derive(Default)]
struct Archive {
    members: Vec<ArchiveMember>,
    symbol_count: u64,
    symbol_bytes: u64,
    archive_size: u64,
}

impl Archive {
    fn read_from_file(path: &Path) -> ArResult<Self> {
        if !path.exists() {
            return Err(format!(
                "{}: {}",
                path.display(),
                gettext("No such file or directory")
            )
            .into());
        }

        if !path.is_file() {
            return Err(format!("{}: {}", path.display(), gettext("Is a directory")).into());
        }

        let file_data = std::fs::read(path)?;
        let parsed_archive = object::read::archive::ArchiveFile::parse(&*file_data)?;
        let mut members = Vec::new();
        let mut archive_symbol_count = 0;
        let mut archive_symbol_bytes = 0;
        let mut archive_size = 0;

        for member in parsed_archive.members() {
            let member = member.map_err(|_| gettext("invalid archive format"))?;

            let data = member.data(&*file_data)?;
            let name = name_from_bytes(member.name());
            let symbols = member_symbols(data);

            archive_symbol_count += symbols.len() as u64;
            let symbol_bytes = member_symbol_bytes(&symbols);
            archive_symbol_bytes += symbol_bytes;
            archive_size += MEMBER_HEADER_SIZE + data.len() as u64;

            members.push(ArchiveMember {
                name,
                date: member.date().ok_or(gettext("invalid archive format"))?,
                uid: member.uid().ok_or(gettext("invalid archive format"))?,
                gid: member.gid().ok_or(gettext("invalid archive format"))?,
                mode: member.mode().ok_or(gettext("invalid archive format"))?,
                size: data.len() as u64,
                data: data.to_vec(),
                symbols,
                symbol_bytes,
            });
        }
        Ok(Archive {
            members,
            symbol_count: archive_symbol_count,
            symbol_bytes: archive_symbol_bytes,
            archive_size,
        })
    }

    fn write<W: Write>(&self, writer: &mut W) -> ArResult<()> {
        writer.write_all(&object::archive::MAGIC)?;

        // Build the System V "//" long-name string table for any member name
        // longer than 15 bytes (#A6). Shared with `strip` via plib so both
        // tools emit the same layout (#ST10).
        let mut names = plib::archive::NameTable::new();
        for m in &self.members {
            names.push(m.name.as_encoded_bytes());
        }

        // Member offsets recorded in the symbol table must account for the
        // "//" member sitting between it and the first file member.
        self.write_symbol_table(writer, names.member_bytes())?;
        names.write(writer)?;
        for (i, member) in self.members.iter().enumerate() {
            member.write(writer, names.offset(i))?;
        }
        Ok(())
    }

    fn write_symbol_table<W: Write>(&self, writer: &mut W, prefix_bytes: u64) -> ArResult<()> {
        let members: Vec<plib::archive::MemberInfo> = self
            .members
            .iter()
            .map(|m| plib::archive::MemberInfo {
                size: m.size,
                symbols: m.symbols.clone(),
            })
            .collect();
        plib::archive::write_sysv_symtab(writer, &members, prefix_bytes)?;
        Ok(())
    }

    fn insert(&mut self, members: Vec<ArchiveMember>, position: InsertPosition) {
        let added_bytes = members
            .iter()
            .map(|m| MEMBER_HEADER_SIZE + m.size)
            .sum::<u64>();
        self.archive_size += added_bytes;
        self.symbol_count += members.iter().map(|m| m.symbols.len() as u64).sum::<u64>();
        self.symbol_bytes += members.iter().map(|m| m.symbol_bytes).sum::<u64>();
        let added_members = members.len() as u64;
        match position {
            InsertPosition::After(index) => {
                // we need to reverse the members to insert them in the order they were given
                let mut reversed = members;
                reversed.reverse();
                self.members.extend(reversed);
                self.members[index + 1..].rotate_right(added_members as usize);
            }
            InsertPosition::Before(index) => {
                self.members.extend(members);
                self.members[index..].rotate_right(added_members as usize);
            }
            InsertPosition::End => {
                self.members.extend(members);
            }
        }
    }

    fn move_to_end(&mut self, index: usize) {
        self.members[index..].rotate_left(1);
    }

    fn move_before(&mut self, index: usize, target: usize) {
        if index > target {
            self.members[target..=index].rotate_right(1);
        } else {
            self.members[index..target].rotate_left(1);
        }
    }

    fn move_after(&mut self, index: usize, target: usize) {
        if index > target {
            self.members[target + 1..=index].rotate_right(1);
        } else {
            self.members[index..=target].rotate_left(1);
        }
    }

    fn replace(&mut self, pos: usize, member: ArchiveMember) {
        let old_member = &self.members[pos];
        self.symbol_bytes -= old_member.symbol_bytes;
        self.symbol_count -= old_member.symbols.len() as u64;
        self.archive_size -= MEMBER_HEADER_SIZE + old_member.size;

        self.symbol_bytes += member.symbol_bytes;
        self.symbol_count += member.symbols.len() as u64;
        self.archive_size += MEMBER_HEADER_SIZE + member.size;

        self.members[pos] = member;
    }

    fn delete(&mut self, pos: usize) {
        let member = &self.members[pos];
        self.symbol_bytes -= member.symbol_bytes;
        self.symbol_count -= member.symbols.len() as u64;
        self.archive_size -= MEMBER_HEADER_SIZE + member.size;
        self.members.remove(pos);
    }

    fn member_index(&self, name: &OsStr) -> Option<usize> {
        // POSIX 84379-84380: the comparison of a file operand to archive
        // member names uses the LAST pathname component of the operand (#A2),
        // so `ar -d arc sub/foo.o` matches the member `foo.o`.
        let basename = Path::new(name).file_name().unwrap_or(name);
        self.members
            .iter()
            .position(|m| m.name.as_os_str() == basename)
    }

    fn get_member(&self, index: usize) -> &ArchiveMember {
        &self.members[index]
    }
}

/// Generates a byte array of length N, from the input string padding it with spaces.
/// If the input string is longer than N, an error is returned.
fn pad_metadata_with_spaces<const N: usize>(s: String) -> ArResult<[u8; N]> {
    if s.len() > N {
        return Err(gettext("file metadata cannot fit into archive format").into());
    }
    let mut result = [b' '; N];
    for (i, byte) in s.as_bytes().iter().enumerate() {
        result[i] = *byte;
    }
    Ok(result)
}

/// Generates a byte array of length 16, from the input OsStr padding it with spaces.
/// We use the System V (or GNU) archive format, which requires the name to be a maximum
/// of 15 bytes, followed by a '/' character and space padding.
fn format_name_for_header(name: &OsStr, long_name_offset: Option<usize>) -> ArResult<[u8; 16]> {
    Ok(plib::archive::format_name_field(
        name.as_encoded_bytes(),
        long_name_offset,
    )?)
}

/// A member name from the archive's bytes.
#[cfg(unix)]
fn name_from_bytes(bytes: &[u8]) -> OsString {
    use std::os::unix::ffi::OsStringExt;
    OsString::from_vec(bytes.to_vec())
}

/// A member name from the archive's bytes, which a Windows name can hold
/// only as text: bytes that are not UTF-8 become U+FFFD.
#[cfg(windows)]
fn name_from_bytes(bytes: &[u8]) -> OsString {
    OsString::from(String::from_utf8_lossy(bytes).into_owned())
}

/// The user ID, group ID and mode an archive records for a file.
#[cfg(unix)]
fn owner_and_mode(meta: &std::fs::Metadata) -> (u64, u64, u64) {
    use std::os::unix::fs::MetadataExt;
    (meta.uid() as u64, meta.gid() as u64, meta.mode() as u64)
}

/// Windows has no user or group IDs: a member records 0 for both, and the
/// mode of a regular file whose permissions are its read-only attribute.
#[cfg(windows)]
fn owner_and_mode(meta: &std::fs::Metadata) -> (u64, u64, u64) {
    const S_IFREG: u32 = 0o100000;
    let mode = S_IFREG | plib::perm::mode_of(&meta.permissions());
    (0, 0, mode as u64)
}

fn member_symbol_bytes(member_symbols: &[String]) -> u64 {
    // we add 1 for the null terminator that is required for each symbol
    // in the archives symbol table
    member_symbols.iter().map(|s| s.len() as u64 + 1).sum()
}

fn delete_cmd(args: DeleteArgs) -> ArResult<()> {
    let archive_path = Path::new(&args.archive);
    let mut archive = Archive::read_from_file(archive_path)?;
    for file in &args.files {
        if let Some(index) = archive.member_index(OsStr::new(&file)) {
            if args.verbose {
                println!("d - {}", file.to_string_lossy());
            }
            archive.delete(index);
        }
    }
    let mut buf = Vec::new();
    archive.write(&mut buf)?;
    plib::io::write_atomic(archive_path, &buf)?;
    Ok(())
}

fn move_cmd(args: MoveArgs) -> ArResult<()> {
    if args.insert_args.insert_after || args.insert_args.insert_before {
        if args.files.len() < 2 {
            return Err(gettext("missing archive operand").into());
        }

        let posname = &args.files[0];
        let archive_path = Path::new(&args.files[1]);
        let mut archive = Archive::read_from_file(archive_path)?;

        if archive.member_index(posname).is_none() {
            return Err(format!(
                "{}: {}",
                posname.to_string_lossy(),
                gettext("No such file or directory")
            )
            .into());
        }
        for file in args.files.iter().skip(2) {
            let target = archive.member_index(posname).unwrap();
            let index = archive.member_index(file);
            if let Some(index) = index {
                if args.verbose {
                    println!("m - {}", file.to_string_lossy());
                }
                if args.insert_args.insert_after {
                    archive.move_after(index, target);
                } else if args.insert_args.insert_before {
                    archive.move_before(index, target);
                }
            } else {
                return Err(format!(
                    "{} {} {}",
                    gettext("no entry"),
                    file.to_string_lossy(),
                    gettext("in archive")
                )
                .into());
            }
        }

        let mut buf = Vec::new();
        archive.write(&mut buf)?;
        plib::io::write_atomic(archive_path, &buf)?;
    } else {
        let archive_path = Path::new(&args.files[0]);
        let mut archive = Archive::read_from_file(archive_path)?;

        for file in args.files.iter().skip(1) {
            let index = archive.member_index(file);
            if let Some(index) = index {
                if args.verbose {
                    println!("m - {}", file.to_string_lossy());
                }
                archive.move_to_end(index);
            } else {
                return Err(format!(
                    "{} {} {}",
                    gettext("no entry"),
                    file.to_string_lossy(),
                    gettext("in archive")
                )
                .into());
            }
        }
        let mut buf = Vec::new();
        archive.write(&mut buf)?;
        plib::io::write_atomic(archive_path, &buf)?;
    }
    Ok(())
}

fn print_cmd(args: PrintArgs) -> ArResult<()> {
    let archive_path = Path::new(&args.archive);
    let archive = Archive::read_from_file(archive_path)?;

    if args.files.is_empty() {
        for member in &archive.members {
            if args.verbose {
                print!("\n<{}>\n\n", member.name.to_string_lossy());
            }
            stdout().write_all(&member.data)?;
        }
        return Ok(());
    } else {
        for file in &args.files {
            if let Some(index) = archive.member_index(file) {
                let member = archive.get_member(index);
                if args.verbose {
                    // POSIX STDOUT 84476-84479 (#A8): when file operands are
                    // given, the prefix is the operand, not the member name.
                    print!("\n<{}>\n\n", file.to_string_lossy());
                }
                stdout().write_all(&member.data)?;
            } else {
                diag::error(&format!(
                    "{}: {}",
                    file.to_string_lossy(),
                    gettext("No such file or directory")
                ));
            }
        }
    }
    if args.regenerate_symbol_table {
        let mut buf = Vec::new();
        archive.write(&mut buf)?;
        plib::io::write_atomic(archive_path, &buf)?;
    }

    Ok(())
}

fn quick_append_cmd(args: QuickAppendArgs) -> ArResult<()> {
    // the verbose flag is not specified to do anything for this command

    let archive_path = Path::new(&args.archive);
    let mut archive = if archive_path.exists() {
        Archive::read_from_file(archive_path)?
    } else {
        if !args.no_create_message {
            eprintln!("ar: {} {}", gettext("creating"), archive_path.display());
        }
        Archive::default()
    };

    let mut members = Vec::new();
    for file in args.files {
        members.push(ArchiveMember::read(Path::new(&file))?);
    }

    archive.insert(members, InsertPosition::End);

    let mut buf = Vec::new();
    archive.write(&mut buf)?;
    plib::io::write_atomic(archive_path, &buf)?;

    Ok(())
}

fn replace_cmd(args: ReplaceArgs) -> ArResult<()> {
    let special_insert_position = args.insert_args.insert_before || args.insert_args.insert_after;

    let archive_path = if special_insert_position {
        if args.files.len() < 2 {
            return Err(gettext("missing archive operand").into());
        }
        Path::new(&args.files[1])
    } else {
        if args.files.is_empty() {
            return Err(gettext("missing archive operand").into());
        }
        Path::new(&args.files[0])
    };

    // #A11: distinguish "no file operands" from "missing archive". POSIX leaves
    // `-r` with no files (on an existing archive) undefined; we reject it with a
    // clear message and non-zero exit rather than the misleading
    // "missing archive operand" or a silent no-op.
    let files_start = special_insert_position as usize + 1;
    if args.files.len() <= files_start {
        return Err(gettext("no file operands specified").into());
    }

    let mut archive = if archive_path.exists() {
        Archive::read_from_file(archive_path)?
    } else {
        if !args.no_create_message {
            eprintln!("ar: {} {}", gettext("creating"), archive_path.display());
        }
        Archive::default()
    };
    let mut to_be_added = Vec::new();
    for file in args.files.iter().skip(special_insert_position as usize + 1) {
        let member = ArchiveMember::read(Path::new(&file))?;
        let file_name = Path::new(file).file_name().unwrap();
        if let Some(index) = archive.member_index(file_name) {
            if args.update_if_not_newer {
                let current_member = archive.get_member(index);
                if current_member.date > member.date {
                    continue;
                }
            }
            if args.verbose {
                println!("r - {}", file.to_string_lossy());
            }
            archive.replace(index, member);
        } else {
            if args.verbose {
                println!("a - {}", file.to_string_lossy());
            }
            to_be_added.push(member);
        }
    }

    let insert_position = if special_insert_position {
        let posname = &args.files[0];
        if let Some(position) = archive.member_index(posname) {
            if args.insert_args.insert_after {
                InsertPosition::After(position)
            } else {
                InsertPosition::Before(position)
            }
        } else {
            InsertPosition::End
        }
    } else {
        InsertPosition::End
    };

    archive.insert(to_be_added, insert_position);
    let mut buf = Vec::new();
    archive.write(&mut buf)?;
    plib::io::write_atomic(archive_path, &buf)?;

    Ok(())
}

fn format_mode(mode: u64) -> String {
    let types = ["---", "--x", "-w-", "-wx", "r--", "r-x", "rw-", "rwx"];

    let mut s = format!(
        "{}{}{}",
        types[((mode >> 6) & 7) as usize],
        types[((mode >> 3) & 7) as usize],
        types[(mode & 7) as usize]
    )
    .into_bytes();

    // setuid / setgid / sticky bits, rendered in the exec positions like ls
    // (#A9): lowercase when the exec bit is also set, uppercase otherwise.
    if mode & 0o4000 != 0 {
        s[2] = if s[2] == b'x' { b's' } else { b'S' };
    }
    if mode & 0o2000 != 0 {
        s[5] = if s[5] == b'x' { b's' } else { b'S' };
    }
    if mode & 0o1000 != 0 {
        s[8] = if s[8] == b'x' { b't' } else { b'T' };
    }

    String::from_utf8(s).unwrap()
}

fn list_member(member: &ArchiveMember, verbose: bool) {
    // Honor LC_TIME and TZ via libc strftime; chrono's format is locale-blind.
    let date =
        plib::locale::strftime(DATE_FORMAT, member.date as i64).unwrap_or_else(|_| String::new());
    if verbose {
        println!(
            "{} {}/{} {} {} {}",
            format_mode(member.mode),
            member.uid,
            member.gid,
            member.size,
            date,
            member.name.to_string_lossy()
        );
    } else {
        println!("{}", member.name.to_string_lossy());
    }
}

fn list_cmd(args: ListArgs) -> ArResult<()> {
    let archive_path = Path::new(&args.archive);
    let archive = Archive::read_from_file(archive_path)?;

    if args.files.is_empty() {
        for member in &archive.members {
            list_member(member, args.verbose);
        }
    } else {
        for file in args.files {
            if let Some(index) = archive.member_index(&file) {
                list_member(archive.get_member(index), args.verbose);
            } else {
                return Err(format!(
                    "{}: {}",
                    file.to_string_lossy(),
                    gettext("No such file or directory")
                )
                .into());
            }
        }
    }

    if args.regenerate_symbol_table {
        let mut buf = Vec::new();
        archive.write(&mut buf)?;
        plib::io::write_atomic(archive_path, &buf)?;
    }
    Ok(())
}

/// Largest filename (in bytes) the current directory's filesystem accepts.
#[cfg(unix)]
fn name_max_for_cwd() -> usize {
    let dot = std::ffi::CString::new(".").unwrap();
    let v = unsafe { libc::pathconf(dot.as_ptr(), libc::_PC_NAME_MAX) };
    if v > 0 {
        v as usize
    } else {
        255
    }
}

/// Largest filename an NTFS directory accepts, in UTF-16 units; Windows has
/// no `pathconf`.
#[cfg(windows)]
fn name_max_for_cwd() -> usize {
    255
}

/// What NAME_MAX counts, for the diagnostic.
#[cfg(unix)]
const NAME_MAX_UNIT_SUFFIX: &str = "bytes); use -T to allow truncation";
#[cfg(windows)]
const NAME_MAX_UNIT_SUFFIX: &str = "characters); use -T to allow truncation";

/// A file name's length as NAME_MAX counts it: bytes.
#[cfg(unix)]
fn name_len(name: &OsStr) -> usize {
    name.as_encoded_bytes().len()
}

/// A file name's length as NTFS counts it: UTF-16 units.
#[cfg(windows)]
fn name_len(name: &OsStr) -> usize {
    use std::os::windows::ffi::OsStrExt;
    name.encode_wide().count()
}

/// The first `max` bytes of `name`.
#[cfg(unix)]
fn truncate_name(name: &OsStr, max: usize) -> OsString {
    name_from_bytes(&name.as_encoded_bytes()[..max])
}

/// The longest run of whole characters from the start of `name` that fits in
/// `max` UTF-16 units.
#[cfg(windows)]
fn truncate_name(name: &OsStr, max: usize) -> OsString {
    let mut used = 0;
    name.to_string_lossy()
        .chars()
        .take_while(|c| {
            used += c.len_utf16();
            used <= max
        })
        .collect::<String>()
        .into()
}

/// Whether `name` names a file directly in the current directory: a single
/// path component, not `.` or `..`, so that a crafted archive cannot have a
/// member written through `../`, an absolute path or a subdirectory.
fn is_plain_file_name(name: &OsStr) -> bool {
    let mut parts = Path::new(name).components();
    matches!(
        (parts.next(), parts.next()),
        (Some(std::path::Component::Normal(_)), None)
    ) && !names_something_else(name)
}

/// Unix has no file names that mean something other than a file.
#[cfg(unix)]
fn names_something_else(_name: &OsStr) -> bool {
    false
}

/// Whether a Windows file name means something other than a file in the
/// directory: `file:stream` names an alternate data stream of `file`, and a
/// device name (`CON`, `NUL`, `COM1`...) opens the device, whatever extension
/// follows it and whatever dots and spaces end it.
#[cfg(windows)]
fn names_something_else(name: &OsStr) -> bool {
    const DEVICES: [&str; 8] = [
        "CON", "PRN", "AUX", "NUL", "CONIN$", "CONOUT$", "COM", "LPT",
    ];
    let bytes = name.as_encoded_bytes();
    if bytes.contains(&b':') {
        return true;
    }
    let stem = bytes.split(|&b| b == b'.').next().unwrap_or_default();
    let stem = String::from_utf8_lossy(stem)
        .trim_end_matches(' ')
        .to_ascii_uppercase();
    DEVICES.iter().any(|&device| match device {
        // COM1-COM9 and LPT1-LPT9, the superscript digits included.
        "COM" | "LPT" => stem.strip_prefix(device).is_some_and(|digit| {
            matches!(
                digit,
                "1" | "2"
                    | "3"
                    | "4"
                    | "5"
                    | "6"
                    | "7"
                    | "8"
                    | "9"
                    | "\u{b9}"
                    | "\u{b2}"
                    | "\u{b3}"
            )
        }),
        _ => stem == device,
    })
}

fn extract_member(
    member: &ArchiveMember,
    dont_replace: bool,
    verbose: bool,
    allow_truncation: bool,
) -> ArResult<()> {
    // POSIX 84418-84421 (#A4): extracting a name longer than NAME_MAX is an
    // error by default; -T allows the name to be truncated to fit.
    let name_max = name_max_for_cwd();
    let out_name: OsString = if name_len(&member.name) > name_max {
        if !allow_truncation {
            return Err(format!(
                "{}: {} {} {}",
                member.name.to_string_lossy(),
                gettext("file name too long (limit"),
                name_max,
                gettext(NAME_MAX_UNIT_SUFFIX)
            )
            .into());
        }
        truncate_name(&member.name, name_max)
    } else {
        member.name.clone()
    };

    if !is_plain_file_name(&out_name) {
        return Err(format!(
            "{}: {}",
            member.name.to_string_lossy(),
            gettext("member name is not a file name in the current directory")
        )
        .into());
    }

    let file_path = Path::new(&out_name);
    if file_path.exists() && dont_replace {
        return Ok(());
    }
    if verbose {
        println!("x - {}", out_name.to_string_lossy());
    }
    let mut out_file = std::fs::File::create(file_path)?;
    out_file.write_all(&member.data)?;
    Ok(())
}

fn extract_cmd(args: ExtractArgs) -> ArResult<()> {
    let archive_path = Path::new(&args.archive);
    let archive = Archive::read_from_file(archive_path)?;

    if args.files.is_empty() {
        for member in &archive.members {
            extract_member(
                member,
                args.dont_replace_files,
                args.verbose,
                args.allow_truncation,
            )?;
        }
    } else {
        for file in args.files {
            if let Some(index) = archive.member_index(&file) {
                extract_member(
                    archive.get_member(index),
                    args.dont_replace_files,
                    args.verbose,
                    args.allow_truncation,
                )?;
            } else {
                return Err(format!(
                    "{}: {}",
                    file.to_string_lossy(),
                    gettext("No such file or directory")
                )
                .into());
            }
        }
    }

    if args.regenerate_symbol_table {
        let mut buf = Vec::new();
        archive.write(&mut buf)?;
        plib::io::write_atomic(archive_path, &buf)?;
    }

    Ok(())
}

/// The seven mode letters; one of these is the "key" that selects the operation.
const MODE_LETTERS: &[u8] = b"dmpqrtx";

/// Split a bundled key token such as `-rv`/`-tv`/`-dv` into separate `-r -v`
/// tokens before clap sees them (#A3). XBD 12.2 requires grouped single-char
/// options to be equivalent to separate ones, but the mode flags are clap
/// subcommands, so a literal `-rv` token would not match any subcommand. Only
/// the first argument (the ar key) is rewritten; `-a`/`-b`/`-i` posname
/// operands remain separate tokens and are untouched.
///
/// The key may also be given in the traditional form without the leading
/// '-' (`ar cr lib.a x.o`, `ar rcs ...`), as Makefiles, libtool and automake's
/// archiver probe write it; it means the same letters with a '-'.
fn canonicalize_args(mut args: Vec<OsString>) -> Vec<OsString> {
    if args.len() < 2 {
        return args;
    }
    let bytes = args[1].as_encoded_bytes();
    let letters = match bytes.first() {
        // Need "-" + at least two letters; leave "-d", "--", "--long" to clap.
        Some(b'-') if bytes.len() > 2 && bytes[1] != b'-' => &bytes[1..],
        Some(b'-') | None => return args,
        Some(_) => bytes,
    };
    if !letters.iter().all(u8::is_ascii_alphabetic) {
        return args;
    }
    let Some(mode_pos) = letters.iter().position(|c| MODE_LETTERS.contains(c)) else {
        return args;
    };
    let mut replacement = vec![OsString::from(format!("-{}", letters[mode_pos] as char))];
    for (i, c) in letters.iter().enumerate() {
        if i != mode_pos {
            replacement.push(OsString::from(format!("-{}", *c as char)));
        }
    }
    args.splice(1..2, replacement);
    args
}

fn main() {
    diag::init_locale("ar");
    let args = Args::parse_from(canonicalize_args(std::env::args_os().collect()));
    let result = match args.command {
        Commands::Delete(args) => delete_cmd(args),
        Commands::Move(args) => move_cmd(args),
        Commands::Print(args) => print_cmd(args),
        Commands::QuickAppend(args) => quick_append_cmd(args),
        Commands::Replace(args) => replace_cmd(args),
        Commands::List(args) => list_cmd(args),
        Commands::Extract(args) => extract_cmd(args),
    };
    if let Err(err) = result {
        diag::error(&format!("{}", err));
    }
    std::process::exit(diag::exit_status());
}
