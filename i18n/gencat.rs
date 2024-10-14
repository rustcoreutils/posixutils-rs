use byteorder::{BigEndian, ByteOrder, LittleEndian, NativeEndian, WriteBytesExt};
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::{
    cell::RefCell,
    collections::BTreeMap,
    fmt::Display,
    fs::File,
    io::{self, Cursor, Read, Seek, Write},
    num::ParseIntError,
    path::PathBuf,
    rc::Rc,
};

const NL_SETMAX: u32 = 255; //max set number(the limits.h defines it and is mentioned in POSIX specification)
const NL_SETD: u32 = 1; // the default set number for the messages that are not in any set
const GLIBC_MAGIC: u32 = 0x960408de;

#[cfg(target_os = "macos")]
pub mod osx {
    pub const OSX_MAGIC: &[u8; 8] = b"*nazgul*";
    pub const OSX_MAJOR_VER: i32 = 1;
    pub const OSX_MINOR_VER: i32 = 0;
    pub const OSX_BYTE_ORDER: i32 = 0x01; // denotes BIG ENDIAN for now
    pub const OSX_NOT_INVALID_FLAG: i32 = 0;
    pub const FIRST_SET_OFFSET: i64 = 32;
}

#[derive(Parser)]
#[command(version, about = gettext("gencat - generate a formatted message catalog"))]
struct Args {
    #[arg(help = gettext("A pathname of the formatted message catalog"))]
    catfile: PathBuf,

    #[arg(help = gettext("A pathname of a message text source file"))]
    msgfile: PathBuf,
}

/// In memory representation of a message
#[derive(Debug)]
pub struct Msg {
    /// The message id
    pub msg_id: usize,

    /// The message text
    pub msg: String,

    pub hconst: String,

    /// The offset of message within the buffer or file
    pub offset: i64,

    /// The next message in the set
    pub next: Option<Rc<RefCell<Msg>>>,

    /// The previous message in the set
    pub prev: Option<Rc<RefCell<Msg>>>,
}

/// In memory representation of a set
#[derive(Debug, Clone)]
pub struct Set {
    /// The set identifier
    pub set_id: u32,

    pub hconst: String,

    /// The first message in the set
    pub first_msg: Option<Rc<RefCell<Msg>>>,

    /// The last message in the set
    pub last_msg: Option<Rc<RefCell<Msg>>>,

    /// The next set in the catalog
    pub next: Option<Rc<RefCell<Set>>>,

    /// The previous set in the catalog
    pub prev: Option<Rc<RefCell<Set>>>,
}

impl Set {
    #[cfg(target_os = "macos")]
    fn get_msgs_count(&self) -> i32 {
        let mut count = 0;
        let mut current = self.first_msg.clone();

        while let Some(msg) = current {
            count += 1;
            current = (*msg).borrow().next.clone();
        }

        count
    }
}

/// In memory representation of a catalog
#[derive(Debug)]
pub struct Cat {
    /// The first set in the catalog
    pub first_set: Option<Rc<RefCell<Set>>>,

    /// The last set in the catalog
    pub last_set: Option<Rc<RefCell<Set>>>,
}

impl Cat {
    pub fn find_set(&self, set_id: u32) -> Option<Rc<RefCell<Set>>> {
        self.all_sets()
            .into_iter()
            .find(|set| set.borrow().set_id == set_id)
    }

    pub fn all_sets(&self) -> Vec<Rc<RefCell<Set>>> {
        let mut sets = Vec::new();
        let mut current_set = self.first_set.clone();
        while let Some(set) = current_set {
            sets.push(set.clone());
            current_set = set.borrow().next.clone();
        }
        sets
    }

    pub fn total_messages(&self) -> usize {
        self.all_sets()
            .iter()
            .map(|set| {
                let set = set.borrow();
                let mut count = 0;
                let mut current_msg = set.first_msg.clone();
                while let Some(msg) = current_msg {
                    count += 1;
                    current_msg = msg.borrow().next.clone();
                }
                count
            })
            .sum()
    }

    pub fn total_sets(&self) -> i32 {
        let mut count = 0;
        let mut current_set = self.first_set.clone();

        while let Some(set) = current_set {
            count += 1;
            current_set = (*set).borrow().next.clone();
        }

        count
    }

    pub fn delete_set(&mut self, set_id: u32) {
        let mut current = self.first_set.clone();
        let mut prev: Option<Rc<RefCell<Set>>> = None;

        while let Some(ref set) = current {
            let next = set.borrow().next.clone();
            if set.borrow().set_id == set_id {
                if let Some(prev) = prev.as_ref() {
                    prev.borrow_mut().next = next.clone();
                } else {
                    self.first_set = next.clone();
                }

                if set.borrow().set_id == self.last_set.as_ref().unwrap().borrow().set_id {
                    self.last_set = prev;
                }

                return;
            } else {
                prev = Some(set.clone());
                current = next;
            }
        }

        // if we don't find the set to delete, we'll simply ignore
    }
}

#[derive(Debug)]
pub struct MessageCatalog {
    pub cat: Cat,
}

/// Magic Header structure for the catalog file
#[cfg(target_os = "macos")]
#[derive(bytemuck::NoUninit, Clone, Copy)]
#[repr(C)]
struct CatFileMagicHeader {
    /// Magic cookie "*nazgul*"
    magic: [u8; 8],

    /// Currently : 1
    major_ver: i32,

    /// Currently : 0
    minor_ver: i32,

    /// Informational flags, currently used to set the endianess
    flags: i32,

    /// Number of valid Sets
    num_sets: i32,

    /// Offset of first set on disk
    first_set: i64,
}

#[derive(Debug)]
pub enum ParseError {
    NoSetNumber(usize, String),
    ParseSetNumber(usize, ParseIntError),
    InvalidSetNumber(usize, u32),
    SetNumberNotInAscending(usize, u32, u32),
    InvalidLine(usize, String),
    InvalidQuoteChar(usize, String),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::InvalidLine(line_num, line_content) => {
                write!(f, "Invalid line {line_num} with content {line_content}")
            }
            ParseError::NoSetNumber(line_num, parse_int_error) => {
                write!(
                    f,
                    "No set number available at line {line_num} with content {parse_int_error}"
                )
            }
            ParseError::ParseSetNumber(line_num, line_content) => {
                write!(
                    f,
                    "Unable to parse set number at line {line_num} with content {line_content}"
                )
            }
            ParseError::InvalidSetNumber(line_num, set_num) => {
                write!(f, "Invalid set number {set_num} at line {line_num}")
            }
            ParseError::SetNumberNotInAscending(line_num, prev_set_num, current_set_num) => {
                write!(
                    f,
                    "Set number {current_set_num} is not in ascending order after {prev_set_num} at line {line_num}"
                )
            }
            ParseError::InvalidQuoteChar(line_num, line_content) => {
                write!(
                    f,
                    "Invalid quote character at line {line_num} with content {line_content}"
                )
            }
        }
    }
}

impl std::error::Error for ParseError {}

/// For set if it's $set NUMBER #COMMENT
impl MessageCatalog {
    pub fn new(
        #[cfg_attr(target_os = "macos", allow(unused_variables))] build_default: bool,
    ) -> Self {
        let message_catalog = MessageCatalog {
            cat: Cat {
                first_set: None,
                last_set: None,
            },
        };

        let message_catalog_to_use: MessageCatalog = {
            #[cfg(target_os = "macos")]
            {
                message_catalog
            }

            #[cfg(not(target_os = "macos"))]
            {
                if build_default {
                    let mut message_catalog_mut = message_catalog;

                    message_catalog_mut.add_set(NL_SETD, String::from("Default Set"));

                    message_catalog_mut
                } else {
                    message_catalog
                }
            }
        };

        message_catalog_to_use
    }

    /// Parse the message file and override the catalog file(if it already exists)
    pub fn parse(
        input_path: &PathBuf,
        catfile_catalog: Option<MessageCatalog>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let mut file = plib::io::input_stream(input_path, true)?;

        let mut input = String::new();
        file.read_to_string(&mut input)?;

        let mut catalog = match catfile_catalog {
            Some(catalog) => catalog,
            None => MessageCatalog::new(true),
        };

        let mut current_set = catalog.cat.first_set.clone();
        let mut last_set_id = NL_SETD;

        // $quote CHAR:
        // regarding it, hmm..we can have multiple quotes but only the latest encountered quote character will be used
        // btw this behavour was observed in the GNU implementation that's why we are settling for it
        //
        // Eg
        // $quote "
        // $quote '
        //
        // Here, the quote character will be ' and not " as it was the last encountered quote character
        // also if empty(i.e no character), then it will be unset
        //
        // i guess this will be useful when we have some qutoe character and sets and then we have another quote character
        // and sets
        let mut quote_char: Option<char> = None;

        for (line_num, line) in input.lines().enumerate() {
            let line = line.trim();

            // Skip empty lines and comments
            if line.is_empty() || line.starts_with("$ ") {
                continue;
            }

            if line.starts_with("$quote") {
                if let Some(rem) = line.strip_prefix("$quote") {
                    let c = rem.trim();
                    if c.is_empty() {
                        // unset quote character
                        quote_char = None;
                    } else if c.len() == 1 {
                        quote_char = c.chars().next();
                    } else {
                        return Err(Box::new(ParseError::InvalidQuoteChar(
                            line_num + 1,
                            line.to_string(),
                        )));
                    }
                }
            } else if line.starts_with("$delset") {
                // the gnu implementation seems to just remove those sets from the "array" (both - little endian and big endian)
                // and it preserves those datain the string pool too(weird, they(GNU devs) might need to improve on that)
                // but we are going to remove the set from the catalog itself(check the hexdump btw for $delset)
                //
                // The question remains, are we diverting from the specification?
                // Hmm..no coz the specification doesn't mention anything about the implementation details(it was GNU's choice to do that)
                // and the GNU implementation is just one of the many ways to implement it, and the catopen will interpret it the sameway
                if let Some(rem) = line.strip_prefix("$delset") {
                    let set_id = rem
                        .trim()
                        .parse::<u32>()
                        .map_err(|e| Box::new(ParseError::ParseSetNumber(line_num, e)))?;

                    catalog.cat.delete_set(set_id);
                }
            } else if line.starts_with("$set") {
                if let Some(rem) = line.strip_prefix("$set") {
                    let parts: Vec<&str> = rem.trim().splitn(2, char::is_whitespace).collect();

                    if parts.is_empty() {
                        return Err(Box::new(ParseError::NoSetNumber(
                            line_num,
                            line.to_string(),
                        )));
                    }

                    let set_id = parts[0]
                        .parse::<u32>()
                        .map_err(|e| Box::new(ParseError::ParseSetNumber(line_num, e)))?;

                    if set_id == 0 || set_id > NL_SETMAX {
                        return Err(Box::new(ParseError::InvalidSetNumber(line_num, set_id)));
                    }

                    if set_id < last_set_id {
                        return Err(Box::new(ParseError::SetNumberNotInAscending(
                            line_num + 1,
                            last_set_id,
                            set_id,
                        )));
                    }

                    last_set_id = set_id;

                    if let Some(existing_set) = catalog.cat.find_set(set_id) {
                        // we'll reuse the same set if we collide
                        current_set = Some(existing_set);
                    } else {
                        let comment = parts.get(1).map(|&s| s.to_string()).unwrap_or_default();
                        current_set = Some(catalog.add_set(set_id, comment));
                    }
                }
            } else {
                let parts: Vec<&str> = line.splitn(2, char::is_whitespace).collect();
                if parts.len() != 2 {
                    return Err(Box::new(ParseError::InvalidLine(
                        line_num + 1,
                        line.to_string(),
                    )));
                }
                let msg_id = parts[0].parse::<usize>()?;
                let msg = parts[1].to_string();
                let msg = if let Some(q) = quote_char {
                    if msg.starts_with(q) && msg.ends_with(q) {
                        msg[1..msg.len() - 1].to_string()
                    } else {
                        msg
                    }
                } else {
                    msg
                };

                catalog.add_msg(current_set.as_ref().unwrap(), msg_id, msg);
            }
        }

        Ok(catalog)
    }

    fn add_set(&mut self, set_id: u32, hconst: String) -> Rc<RefCell<Set>> {
        let new_set = Rc::new(RefCell::new(Set {
            set_id,
            hconst,
            first_msg: None,
            last_msg: None,
            next: None,
            prev: None,
        }));

        match self.cat.last_set.take() {
            Some(last_set) => {
                last_set.borrow_mut().next = Some(Rc::clone(&new_set));
                new_set.borrow_mut().prev = Some(Rc::clone(&last_set));
                self.cat.last_set = Some(Rc::clone(&new_set));
            }
            None => {
                self.cat.first_set = Some(Rc::clone(&new_set));
                self.cat.last_set = Some(Rc::clone(&new_set));
            }
        }

        new_set
    }

    fn add_msg(&self, set: &Rc<RefCell<Set>>, msg_id: usize, msg: String) {
        let new_msg = Rc::new(RefCell::new(Msg {
            msg_id,
            msg,
            hconst: String::new(),
            offset: 0,
            next: None,
            prev: None,
        }));

        let mut set = set.borrow_mut();
        match set.last_msg.take() {
            Some(last_msg) => {
                last_msg.borrow_mut().next = Some(Rc::clone(&new_msg));
                new_msg.borrow_mut().prev = Some(last_msg);
                set.last_msg = Some(Rc::clone(&new_msg));
            }
            None => {
                set.first_msg = Some(Rc::clone(&new_msg));
                set.last_msg = Some(Rc::clone(&new_msg));
            }
        }
    }

    #[cfg(not(target_os = "macos"))]
    fn compute_optimal_size(&self) -> (usize, usize) {
        let mut best_total = usize::MAX;
        let mut best_size = usize::MAX;
        let mut best_depth = usize::MAX;

        // starting with a size that would give an average depth of 5
        let mut act_size = 1 + self.cat.total_messages() / 5;

        while act_size <= best_total {
            let mut deep = vec![0; act_size as usize];
            let mut act_depth = 1;

            for set in self.cat.all_sets() {
                let set = set.borrow();
                let mut current_msg = set.first_msg.clone();

                while let Some(msg) = current_msg {
                    let msg = msg.borrow();
                    let idx = (msg.msg_id * set.set_id as usize) % act_size;
                    deep[idx] += 1;

                    if deep[idx] > act_depth {
                        act_depth = deep[idx];

                        if act_depth * act_size > best_total as usize {
                            break;
                        }
                    }
                    current_msg = msg.next.clone();
                }
            }

            if act_depth * act_size <= best_total {
                best_total = act_depth * act_size;
                best_size = act_size;
                best_depth = act_depth;
            }

            act_size += 1;
        }

        if best_size == usize::MAX {
            best_size = 1;
            best_depth = 1;
        }

        (best_size, best_depth)
    }

    #[cfg(not(target_os = "macos"))]
    fn fill_arrays(&self, array: &mut [u32], string_pool: &mut Vec<u8>, best_size: usize) {
        for set in self.cat.all_sets().iter().rev() {
            let set = set.borrow();
            let mut current_msg = set.first_msg.clone();

            while let Some(msg) = current_msg {
                let msg = msg.borrow();
                let mut idx = (((set.set_id + 1) as usize * msg.msg_id) % best_size) * 3;

                while array[idx] != 0 {
                    idx += best_size * 3;
                }

                array[idx] = (set.set_id + 1) as u32;
                array[idx + 1] = msg.msg_id as u32;
                array[idx + 2] = string_pool.len() as u32;

                // add the message to the string pool
                string_pool.extend_from_slice(msg.msg.as_bytes());
                string_pool.push(0); // Null terminator

                current_msg = msg.next.clone();
            }
        }
    }

    /// Read the **GNU based** binary catalog file and build [MessageCatalog]
    #[allow(clippy::needless_range_loop)]
    pub fn read_catfile<T: Read>(
        mut input: T,
    ) -> Result<MessageCatalog, Box<dyn std::error::Error>> {
        let mut catalog = MessageCatalog::new(false);
        let mut set_msg: BTreeMap<u32, BTreeMap<u32, String>> = BTreeMap::new();

        let mut buf: Vec<u8> = Vec::new();
        input.read_to_end(&mut buf)?;
        let mut ptr = 0;

        let header = NativeEndian::read_u32(&buf[ptr..(ptr + 4)]);
        ptr += 4;

        if header != GLIBC_MAGIC {
            panic!("DOESNT MATCH");
        }

        let plane_size = NativeEndian::read_u32(&buf[ptr..(ptr + 4)]);
        ptr += 4;

        let plane_depth = NativeEndian::read_u32(&buf[ptr..(ptr + 4)]);
        ptr += 4;

        // little-endian array
        let array_size = (plane_size * plane_depth * 3) as usize;
        let mut le_array = vec![0u32; array_size];
        for i in 0..array_size {
            le_array[i] = LittleEndian::read_u32(&buf[ptr..(ptr + 4)]);
            ptr += 4;
        }

        let mut be_array = vec![0u32; array_size];
        for i in 0..array_size {
            be_array[i] = BigEndian::read_u32(&buf[ptr..(ptr + 4)]);
            ptr += 4;
        }

        // TODO: probably compare be_array and le_array as they should be the same

        let string_pool = &buf[ptr..];

        // Process the array to reconstruct sets and messages
        for i in (0..array_size).step_by(3) {
            if le_array[i] != 0 {
                // as we already had incremented the set_id by 1 while inserting
                // we will be now reducing it by 1 for now
                let set_id = le_array[i] - 1;
                let msg_id = le_array[i + 1];
                let string_offset = le_array[i + 2] as usize;

                let msg_end = string_pool[string_offset..]
                    .iter()
                    .position(|&c| c == 0)
                    .unwrap_or(string_pool.len() - string_offset)
                    + string_offset;

                let msg = String::from_utf8_lossy(&string_pool[string_offset..msg_end]).to_string();
                set_msg.entry(set_id).or_default().insert(msg_id, msg);
            }
        }

        for (set_id, messages) in set_msg.iter() {
            let set = catalog.add_set(*set_id, String::new());
            for (msg_id, msg) in messages.iter() {
                catalog.add_msg(&set, *msg_id as usize, msg.clone());
            }
        }

        Ok(catalog)
    }

    /// Write to the cat file **for GNU only**
    #[cfg(not(target_os = "macos"))]
    pub fn write_catfile<T: Write + Seek>(
        &self,
        file: &mut T,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let (best_size, best_depth) = self.compute_optimal_size();

        file.write_u32::<NativeEndian>(GLIBC_MAGIC)?;
        file.write_u32::<NativeEndian>(best_size as u32)?;
        file.write_u32::<NativeEndian>(best_depth as u32)?;

        let array_size: u32 = (best_size * best_depth * 3) as u32;
        let mut array = vec![0u32; array_size as usize];
        let mut string_pool = Vec::new();

        self.fill_arrays(&mut array, &mut string_pool, best_size);

        // little-endian array
        for &value in &array {
            file.write_u32::<LittleEndian>(value)?;
        }

        // big-endian array
        for &value in &array {
            file.write_u32::<BigEndian>(value)?;
        }

        // Write string pool
        file.write_all(&string_pool)?;

        Ok(())
    }

    /// Write to the cat file **for OSX only**
    #[cfg(target_os = "macos")]
    pub fn write_catfile<T: Write + Seek>(
        &self,
        file: &mut T,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let header = CatFileMagicHeader {
            magic: *osx::OSX_MAGIC,
            major_ver: osx::OSX_MAJOR_VER.to_be(),
            minor_ver: osx::OSX_MINOR_VER.to_be(),
            flags: osx::OSX_BYTE_ORDER.to_be(),
            num_sets: self.cat.total_sets().to_be(),
            // for now we have set it to 32, but we will change this later on as we lay the first set details
            first_set: osx::FIRST_SET_OFFSET.to_be(),
        };

        file.write_all(bytemuck::bytes_of(&header))?;

        // the position of the first_set value within the file that we need to change later on
        let first_set_pos = file.stream_position()? - 8;

        //file.write_u64::<BigEndian>(0)?;
        let cat = &self.cat;
        let mut current_set = self.cat.first_set.clone();

        while let Some(set) = current_set {
            let set = (*set).borrow();
            let set_pos = file.stream_position()?;

            if set.set_id == 1 {
                file.seek(io::SeekFrom::Start(first_set_pos))?;
                file.write_u64::<BigEndian>(set_pos)?;
                file.seek(io::SeekFrom::Start(set_pos))?;
            }

            file.write_u32::<BigEndian>(set.set_id)?;

            let next_set_offset = file.stream_position()?;
            file.write_i64::<BigEndian>(0)?; // just a placeholder(hmm, but might use it if there's no next set)

            let first_msg_offset_pos = file.stream_position()?;
            file.write_i64::<BigEndian>(0)?; // just a placeholder

            let data_offset_pos = file.stream_position()?;
            file.write_i64::<BigEndian>(0)?; // just a placeholder

            let data_length_pos = file.stream_position()?;
            file.write_i32::<BigEndian>(0)?; // just a placeholder

            let num_msgs = set.get_msgs_count();
            file.write_i32::<BigEndian>(num_msgs)?;
            file.write_i32::<BigEndian>(osx::OSX_NOT_INVALID_FLAG)?;

            // We'll write the string data now
            let data_offset = file.stream_position()?;
            let mut data_length = 0;
            let mut current_msg = set.first_msg.clone();

            while let Some(msg) = current_msg {
                let mut msg = msg.borrow_mut();
                let msg_offset = file.stream_position()? - data_offset;
                msg.offset = msg_offset as i64;
                file.write_all(msg.msg.as_bytes())?;

                // null terminator
                file.write_u8(0)?;
                data_length += msg.msg.len() as i32 + 1;
                current_msg = msg.next.clone();
            }

            let first_msg_offset = file.stream_position()?;
            let mut current_msg = set.first_msg.clone();
            while let Some(msg) = current_msg {
                let msg = (*msg).borrow();

                file.write_i32::<BigEndian>(msg.msg_id as i32)?;
                file.write_i64::<BigEndian>(msg.offset)?;
                file.write_i32::<BigEndian>(osx::OSX_NOT_INVALID_FLAG)?;

                current_msg = msg.next.clone();
            }

            let current_pos = file.stream_position()?;

            // go back and write first msg offset
            file.seek(io::SeekFrom::Start(first_msg_offset_pos))?;
            file.write_i64::<BigEndian>(first_msg_offset as i64)?;

            // go back and write data offset
            file.seek(io::SeekFrom::Start(data_offset_pos))?;
            file.write_i64::<BigEndian>(data_offset as i64)?;

            // go back and write data length
            file.seek(io::SeekFrom::Start(data_length_pos))?;
            file.write_i32::<BigEndian>(data_length)?;

            let last_set = cat.last_set.as_ref().unwrap();
            let last_set = (**last_set).borrow();

            // if not last then we need to write the next set offset
            if set.set_id != last_set.set_id {
                // go back and write next set offset
                file.seek(io::SeekFrom::Start(next_set_offset))?;
                file.write_i64::<BigEndian>(current_pos as i64)?;
            }

            file.seek(io::SeekFrom::Start(current_pos))?;

            current_set = set.next.clone();
        }
        Ok(())
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    // the parsed catalog file, if it exists(which surely doesn't if we provide "-" a stdout)
    // and after we provide a file path, we cannot read the catalog file if the file doesn't exist
    let mut catfile_catalog: Option<MessageCatalog> = None;
    let catfile_path_str = args.catfile.as_os_str();

    if catfile_path_str != "-" && args.catfile.exists() {
        let catfile_catalog_file = File::open(&args.catfile)?;
        catfile_catalog = Some(MessageCatalog::read_catfile(catfile_catalog_file)?);
    }

    match MessageCatalog::parse(&args.msgfile, catfile_catalog) {
        Ok(catalog) => {
            let mut buffer = Cursor::new(Vec::new());
            catalog.write_catfile(&mut buffer)?;

            if catfile_path_str == "-" {
                io::stdout().write_all(buffer.get_ref())?;
            } else {
                let mut file = File::create(&args.catfile)?;
                file.write_all(buffer.get_ref())?;
            }
        }
        Err(err) => {
            exit_code = 1;
            eprintln!("Error: {err}");
        }
    }

    std::process::exit(exit_code)
}
