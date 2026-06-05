//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::{Parser, Subcommand};
use gettextrs::gettext;
use object::{Object, ObjectSymbol, SymbolKind};
use plib::diag;
use std::ffi::{OsStr, OsString};
use std::io::{stdout, Write};
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::os::unix::fs::MetadataExt;
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

    #[arg(short = 'v', help = gettext("Give verbose output"))]
    verbose: bool,

    archive: String,
    files: Vec<String>,
}

#[derive(clap::Args)]
struct ReplaceArgs {
    #[arg(short = 'c', help = gettext("Suppress archive creation diagnostics"))]
    no_create_message: bool,

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
            return Err(format!("{}: No such file or directory", file_path.display()).into());
        }

        if !file_path.is_file() {
            return Err(format!("{}: Is a directory", file_path.display()).into());
        }

        let file_metadata = file_path.metadata()?;
        // we already checked that the path is to a file so unwrap is safe
        let name = file_path.file_name().unwrap().to_os_string();

        let data = std::fs::read(file_path)?;
        let symbols = read_member_symbols(&data);
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
            uid: file_metadata.uid() as u64,
            gid: file_metadata.gid() as u64,
            mode: file_metadata.mode() as u64,
            size: file_metadata.len(),
            data,
            symbols,
            symbol_bytes,
        })
    }

    fn write<W: Write>(&self, writer: &mut W) -> ArResult<()> {
        // format definition taken from: https://en.wikipedia.org/wiki/Ar_(Unix)

        // Since we are using the System V (or GNU) archive format, the data section
        // needs to be 2 byte aligned, if it isn't we add a newline as filler
        let size = self.size + (self.data.len() % 2) as u64;

        writer.write_all(&format_name_for_header(&self.name)?)?;
        writer.write_all(&pad_metadata_with_spaces::<12>(self.date.to_string())?)?;
        writer.write_all(&pad_metadata_with_spaces::<6>(self.uid.to_string())?)?;
        writer.write_all(&pad_metadata_with_spaces::<6>(self.gid.to_string())?)?;
        writer.write_all(&pad_metadata_with_spaces::<8>(format!("{:o}", self.mode))?)?;
        writer.write_all(&pad_metadata_with_spaces::<10>(size.to_string())?)?;
        writer.write_all(&object::archive::TERMINATOR)?;
        writer.write_all(&self.data)?;
        if self.data.len() % 2 != 0 {
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
            return Err(format!("{}: No such file or directory", path.display()).into());
        }

        if !path.is_file() {
            return Err(format!("{}: Is a directory", path.display()).into());
        }

        let file_data = std::fs::read(path)?;
        let parsed_archive = object::read::archive::ArchiveFile::parse(&*file_data)?;
        let mut members = Vec::new();
        let mut archive_symbol_count = 0;
        let mut archive_symbol_bytes = 0;
        let mut archive_size = 0;

        for member in parsed_archive.members() {
            let member = member.map_err(|_| "invalid archive format")?;

            let data = member.data(&*file_data)?;
            let name = OsString::from_vec(member.name().to_vec());
            let symbols = read_member_symbols(data);

            archive_symbol_count += symbols.len() as u64;
            let symbol_bytes = member_symbol_bytes(&symbols);
            archive_symbol_bytes += symbol_bytes;
            archive_size += MEMBER_HEADER_SIZE + data.len() as u64;

            members.push(ArchiveMember {
                name,
                date: member.date().ok_or("invalid archive format")?,
                uid: member.uid().ok_or("invalid archive format")?,
                gid: member.gid().ok_or("invalid archive format")?,
                mode: member.mode().ok_or("invalid archive format")?,
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
        self.write_symbol_table(writer)?;
        for member in &self.members {
            member.write(writer)?;
        }
        Ok(())
    }

    fn write_symbol_table<W: Write>(&self, writer: &mut W) -> ArResult<()> {
        let members: Vec<plib::archive::MemberInfo> = self
            .members
            .iter()
            .map(|m| plib::archive::MemberInfo {
                size: m.size,
                symbols: m.symbols.clone(),
            })
            .collect();
        plib::archive::write_sysv_symbol_table(writer, &members)?;
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
        return Err("file metadata cannot fit into archive format".into());
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
fn format_name_for_header(name: &OsStr) -> ArResult<[u8; 16]> {
    if name.len() > 15 {
        return Err(format!("{}: file name is too long", name.to_string_lossy()).into());
    }
    let mut result = [b' '; 16];
    for (i, byte) in name.as_bytes().iter().enumerate() {
        result[i] = *byte;
    }
    result[name.len()] = b'/';
    Ok(result)
}

fn member_symbol_bytes(member_symbols: &[String]) -> u64 {
    // we add 1 for the null terminator that is required for each symbol
    // in the archives symbol table
    member_symbols.iter().map(|s| s.len() as u64 + 1).sum()
}

fn read_member_symbols(member_data: &[u8]) -> Vec<String> {
    if let Ok(object_file) = object::read::File::parse(member_data) {
        let symbols = object_file
            .symbols()
            .filter(|s| {
                s.kind() == SymbolKind::Text
                    || s.kind() == SymbolKind::Data
                    || s.kind() == SymbolKind::Tls
            })
            .map(|s| s.name().unwrap().to_string())
            .collect();
        symbols
    } else {
        Vec::new()
    }
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
            return Err("missing archive operand".into());
        }

        let posname = &args.files[0];
        let archive_path = Path::new(&args.files[1]);
        let mut archive = Archive::read_from_file(archive_path)?;

        if archive.member_index(posname).is_none() {
            return Err(format!("{}: No such file or directory", posname.to_string_lossy()).into());
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
                return Err(format!("no entry {} in archive", file.to_string_lossy()).into());
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
                return Err(format!("no entry {} in archive", file.to_string_lossy()).into());
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
                    print!("\n<{}>\n\n", member.name.to_string_lossy());
                }
                stdout().write_all(&member.data)?;
            } else {
                diag::error(&format!(
                    "{}: No such file or directory",
                    file.to_string_lossy()
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
            eprintln!("ar: creating {}", archive_path.display());
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
            return Err("missing archive operand".into());
        }
        Path::new(&args.files[1])
    } else {
        if args.files.is_empty() {
            return Err("missing archive operand".into());
        }
        Path::new(&args.files[0])
    };

    let mut archive = if archive_path.exists() {
        Archive::read_from_file(archive_path)?
    } else {
        if !args.no_create_message {
            eprintln!("ar: creating {}", archive_path.display());
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

    let user = types[((mode >> 6) & 7) as usize];
    let group = types[((mode >> 3) & 7) as usize];
    let others = types[(mode & 7) as usize];

    format!("{}{}{}", user, group, others)
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
                return Err(
                    format!("{}: No such file or directory", archive_path.display()).into(),
                );
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
fn name_max_for_cwd() -> usize {
    let dot = std::ffi::CString::new(".").unwrap();
    let v = unsafe { libc::pathconf(dot.as_ptr(), libc::_PC_NAME_MAX) };
    if v > 0 {
        v as usize
    } else {
        255
    }
}

fn extract_member(
    member: &ArchiveMember,
    dont_replace: bool,
    verbose: bool,
    allow_truncation: bool,
) -> ArResult<()> {
    // POSIX 84418-84421 (#A4): extracting a name longer than NAME_MAX is an
    // error by default; -T allows the name to be truncated to fit.
    let name_bytes = member.name.as_bytes();
    let name_max = name_max_for_cwd();
    let out_name: OsString = if name_bytes.len() > name_max {
        if !allow_truncation {
            return Err(format!(
                "{}: file name too long (limit {} bytes); use -T to allow truncation",
                member.name.to_string_lossy(),
                name_max
            )
            .into());
        }
        OsString::from_vec(name_bytes[..name_max].to_vec())
    } else {
        member.name.clone()
    };

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
                return Err(
                    format!("{}: No such file or directory", file.to_string_lossy()).into(),
                );
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
/// the first option-shaped argument (the ar key) is rewritten; `-a`/`-b`/`-i`
/// posname operands remain separate tokens and are untouched.
fn canonicalize_args(mut args: Vec<OsString>) -> Vec<OsString> {
    if args.len() < 2 {
        return args;
    }
    let bytes = args[1].as_bytes();
    // Need "-" + at least two letters; leave "-d", "--", "--long" to clap.
    if bytes.len() <= 2 || bytes[0] != b'-' || bytes[1] == b'-' {
        return args;
    }
    let letters = &bytes[1..];
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
