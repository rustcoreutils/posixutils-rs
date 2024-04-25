//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate clap;

use chrono::DateTime;
use clap::{Parser, Subcommand};
use object::{Object, ObjectSymbol, SymbolKind};
use std::ffi::{OsStr, OsString};
use std::io::{stdout, Write};
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::os::unix::fs::MetadataExt;
use std::path::Path;

#[derive(clap::Args, Debug)]
#[group(required = false, multiple = false)]
struct InsertArgs {
    /// Insert the files after the specified member
    #[arg(short = 'a')]
    insert_after: bool,

    /// Insert the files before the specified member
    #[arg(short = 'b', short_alias = 'i')]
    insert_before: bool,
}

#[derive(clap::Args, Debug)]
struct DeleteArgs {
    /// Give verbose output
    #[arg(short = 'v')]
    verbose: bool,

    archive: OsString,
    files: Vec<OsString>,
}

#[derive(clap::Args, Debug)]
struct MoveArgs {
    #[command(flatten)]
    insert_args: InsertArgs,

    files: Vec<OsString>,
}

#[derive(clap::Args, Debug)]
struct PrintArgs {
    /// Give verbose output
    #[arg(short = 'v')]
    verbose: bool,

    /// Force regeneration of the archive's symbol table
    #[arg(short = 's')]
    regenerate_symbol_table: bool,

    archive: OsString,
    files: Vec<OsString>,
}

#[derive(clap::Args, Debug)]
struct QuickAppendArgs {
    /// Suppress archive creation diagnostics
    #[arg(short = 'c')]
    no_create_message: bool,

    /// Give verbose output
    #[arg(short = 'v')]
    verbose: bool,

    archive: String,
    files: Vec<String>,
}

#[derive(clap::Args, Debug)]
struct ReplaceArgs {
    /// Suppress archive creation diagnostics
    #[arg(short = 'c')]
    no_create_message: bool,

    /// Update older files in the archive
    #[arg(short = 'u')]
    update_if_not_newer: bool,

    /// Give verbose output
    #[arg(short = 'v')]
    verbose: bool,

    #[command(flatten)]
    insert_args: InsertArgs,

    files: Vec<OsString>,
}

#[derive(clap::Args, Debug)]
struct ListArgs {
    /// Give verbose output
    #[arg(short = 'v')]
    verbose: bool,

    /// Force regeneration of the archive's symbol table
    #[arg(short = 's')]
    regenerate_symbol_table: bool,

    archive: OsString,
    files: Vec<OsString>,
}

#[derive(clap::Args, Debug)]
struct ExtractArgs {
    /// Give verbose output
    #[arg(short = 'v')]
    verbose: bool,

    /// Force regeneration of the archive's symbol table
    #[arg(short = 's')]
    regenerate_symbol_table: bool,

    /// Do not replace existing files
    #[arg(short = 'C')]
    dont_replace_files: bool,

    /// Allow truncation of file names from the archive
    #[arg(short = 'T')]
    allow_truncation: bool,

    archive: OsString,
    files: Vec<OsString>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Delete one or more files from the archive
    #[command(name = "-d")]
    Delete(DeleteArgs),
    /// Move named files within the archive
    #[command(name = "-m")]
    Move(MoveArgs),
    /// Print the contents of the files in the archive
    #[command(name = "-p")]
    Print(PrintArgs),
    /// Append files to the archive without checking for duplicates
    #[command(name = "-q")]
    QuickAppend(QuickAppendArgs),
    /// Replace or add files to the archive
    #[command(name = "-r")]
    Replace(ReplaceArgs),
    /// List the contents of the archive
    #[command(name = "-t")]
    List(ListArgs),
    /// Extract files from the archive
    #[command(name = "-x")]
    Extract(ExtractArgs),
}

/// ar - create and maintain library archives
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
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
            return Err(format!("ar: {}: No such file or directory", file_path.display()).into());
        }

        if !file_path.is_file() {
            return Err(format!("ar: {}: Is a directory", file_path.display()).into());
        }

        let file_metadata = file_path.metadata()?;
        // we already checked that the path is to a file so unwrap is safe
        let name = file_path.file_name().unwrap().to_os_string();

        let data = std::fs::read(file_path)?;
        let symbols = read_member_symbols(&data);
        let symbol_bytes = symbols.iter().map(|s| s.len() as u64 + 1).sum::<u64>();

        let date = file_metadata
            .modified()
            .ok()
            .map(|t| t.elapsed().ok().map(|d| d.as_secs()).unwrap_or_default())
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
            writer.write_all(&[b'\n'])?;
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
            return Err(format!("ar: {}: No such file or directory", path.display()).into());
        }

        if !path.is_file() {
            return Err(format!("ar: {}: Is a directory", path.display()).into());
        }

        let file_data = std::fs::read(path)?;
        let parsed_archive = object::read::archive::ArchiveFile::parse(&*file_data)?;
        let mut members = Vec::new();
        let mut archive_symbol_count = 0;
        let mut archive_symbol_bytes = 0;
        let mut archive_size = 0;

        for member in parsed_archive.members() {
            let member = member.map_err(|_| "ar: invalid archive format")?;

            let data = member.data(&*file_data)?;
            let name = OsString::from_vec(member.name().to_vec());
            let symbols = read_member_symbols(data);

            archive_symbol_count += symbols.len() as u64;
            let symbol_bytes = member_symbol_bytes(&symbols);
            archive_symbol_bytes += symbol_bytes;
            archive_size += MEMBER_HEADER_SIZE + data.len() as u64;

            members.push(ArchiveMember {
                name,
                date: member.date().ok_or("ar: invalid archive format")?,
                uid: member.uid().ok_or("ar: invalid archive format")?,
                gid: member.gid().ok_or("ar: invalid archive format")?,
                mode: member.mode().ok_or("ar: invalid archive format")?,
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
        // format definition taken from: https://en.wikipedia.org/wiki/Ar_(Unix)

        // The symbol table is made up of the following:
        // - a big endian i32 for the number of symbols
        // - for each symbol, an i32 offset to the archive member where the symbol is defined
        // - for each symbol, the symbol name followed by a null terminator
        let mut symbol_table_size = (4 + self.symbol_count * 4 + self.symbol_bytes) as u32;

        // data section needs to be 2 byte aligned
        if symbol_table_size % 2 != 0 {
            symbol_table_size += 1;
        }
        let mut table_offsets = Vec::with_capacity(self.symbol_count as usize * 4);
        let mut table_symbols = Vec::with_capacity(self.symbol_bytes as usize);
        let mut total_offset =
            object::archive::MAGIC.len() as u32 + MEMBER_HEADER_SIZE as u32 + symbol_table_size;

        for member in &self.members {
            for symbol in &member.symbols {
                table_offsets.extend_from_slice(&total_offset.to_be_bytes());
                table_symbols.extend(symbol.as_bytes());
                table_symbols.push(b'\0');
            }
            total_offset += (MEMBER_HEADER_SIZE + member.size) as u32;
        }

        let mut symbol_table = Vec::with_capacity(symbol_table_size as usize);
        symbol_table.extend(&(self.symbol_count as u32).to_be_bytes());
        symbol_table.extend(&table_offsets);
        symbol_table.extend(&table_symbols);
        // to remain 2 byte aligned, the symbol table is padded with '\0' instead of '\n'
        if symbol_table.len() % 2 != 0 {
            symbol_table.push(b'\0');
        }

        writer.write_all(&pad_metadata_with_spaces::<16>("/".to_string())?)?;
        writer.write_all(&pad_metadata_with_spaces::<12>("0".to_string())?)?;
        writer.write_all(&pad_metadata_with_spaces::<6>("0".to_string())?)?;
        writer.write_all(&pad_metadata_with_spaces::<6>("0".to_string())?)?;
        writer.write_all(&pad_metadata_with_spaces::<8>("0".to_string())?)?;
        writer.write_all(&pad_metadata_with_spaces::<10>(
            symbol_table_size.to_string(),
        )?)?;
        writer.write_all(&object::archive::TERMINATOR)?;
        writer.write_all(&symbol_table)?;

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
        self.members.iter().position(|m| m.name == name)
    }

    fn get_member(&self, index: usize) -> &ArchiveMember {
        &self.members[index]
    }
}

/// Generates a byte array of length N, from the input string padding it with spaces.
/// If the input string is longer than N, an error is returned.
fn pad_metadata_with_spaces<const N: usize>(s: String) -> ArResult<[u8; N]> {
    if s.len() > N {
        return Err("ar: file metadata cannot fit into archive format".into());
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
        return Err(format!("ar: {}: file name is too long", name.to_string_lossy()).into());
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
    let mut out_file = std::fs::File::create(archive_path)?;
    archive.write(&mut out_file)?;
    Ok(())
}

fn move_cmd(args: MoveArgs) -> ArResult<()> {
    if args.insert_args.insert_after || args.insert_args.insert_before {
        if args.files.len() < 2 {
            return Err("ar: missing archive operand".into());
        }

        let posname = &args.files[0];
        let archive_path = Path::new(&args.files[1]);
        let mut archive = Archive::read_from_file(archive_path)?;

        if archive.member_index(posname).is_none() {
            return Err(format!(
                "ar: {}: No such file or directory",
                posname.to_string_lossy()
            )
            .into());
        }
        for file in args.files.iter().skip(2) {
            let target = archive.member_index(posname).unwrap();
            let index = archive.member_index(file);
            if let Some(index) = index {
                if args.insert_args.insert_after {
                    archive.move_after(index, target);
                } else if args.insert_args.insert_before {
                    archive.move_before(index, target);
                }
            } else {
                return Err(format!("ar: no entry {} in archive", file.to_string_lossy()).into());
            }
        }

        let mut out_file = std::fs::File::create(archive_path)?;
        archive.write(&mut out_file)?;
    } else {
        let archive_path = Path::new(&args.files[0]);
        let mut archive = Archive::read_from_file(archive_path)?;

        for file in args.files.iter().skip(1) {
            let index = archive.member_index(file);
            if let Some(index) = index {
                archive.move_to_end(index);
            } else {
                return Err(format!("ar: no entry {} in archive", file.to_string_lossy()).into());
            }
        }
        let mut out_file = std::fs::File::create(archive_path)?;
        archive.write(&mut out_file)?;
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
                eprintln!("ar: {}: No such file or directory", file.to_string_lossy());
            }
        }
    }
    if args.regenerate_symbol_table {
        let mut out_file = std::fs::File::create(archive_path)?;
        archive.write(&mut out_file)?;
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

    let mut out_file = std::fs::File::create(archive_path)?;
    archive.write(&mut out_file)?;

    Ok(())
}

fn replace_cmd(args: ReplaceArgs) -> ArResult<()> {
    let special_insert_position = args.insert_args.insert_before || args.insert_args.insert_after;

    let archive_path = if special_insert_position {
        if args.files.len() < 2 {
            return Err("ar: missing archive operand".into());
        }
        Path::new(&args.files[1])
    } else {
        if args.files.is_empty() {
            return Err("ar: missing archive operand".into());
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
    let mut out_file = std::fs::File::create(archive_path)?;
    archive.write(&mut out_file)?;

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
    let date = DateTime::from_timestamp(member.date as i64, 0).unwrap();
    if verbose {
        println!(
            "{} {}/{} {} {} {}",
            format_mode(member.mode),
            member.uid,
            member.gid,
            member.size,
            date.format(DATE_FORMAT),
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
                    format!("ar: {}: No such file or directory", archive_path.display()).into(),
                );
            }
        }
    }

    if args.regenerate_symbol_table {
        let mut out_file = std::fs::File::create(archive_path)?;
        archive.write(&mut out_file)?;
    }
    Ok(())
}

fn extract_member(member: &ArchiveMember, dont_replace: bool, verbose: bool) -> ArResult<()> {
    let file_path = Path::new(&member.name);
    if file_path.exists() && dont_replace {
        return Ok(());
    }
    if verbose {
        println!("x - {}", member.name.to_string_lossy());
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
            extract_member(member, args.dont_replace_files, args.verbose)?;
        }
    } else {
        for file in args.files {
            if let Some(index) = archive.member_index(&file) {
                extract_member(
                    archive.get_member(index),
                    args.dont_replace_files,
                    args.verbose,
                )?;
            } else {
                return Err(
                    format!("ar: {}: No such file or directory", file.to_string_lossy()).into(),
                );
            }
        }
    }

    if args.regenerate_symbol_table {
        let mut out_file = std::fs::File::create(archive_path)?;
        archive.write(&mut out_file)?;
    }

    Ok(())
}

fn main() -> ArResult<()> {
    let args = Args::parse();
    match args.command {
        Commands::Delete(args) => delete_cmd(args),
        Commands::Move(args) => move_cmd(args),
        Commands::Print(args) => print_cmd(args),
        Commands::QuickAppend(args) => quick_append_cmd(args),
        Commands::Replace(args) => replace_cmd(args),
        Commands::List(args) => list_cmd(args),
        Commands::Extract(args) => extract_cmd(args),
    }
}
