// RUST-LS
// https://github.com/we-and/rust_ls
// Author: Jean Dumont
// jd@weand.co.uk

extern crate xattr;
use textwrap::{fill, fill_inplace,Options};

use users::{get_group_by_gid, get_user_by_uid};
use xattr::{FileExt, XAttrs};
extern crate atty;
extern crate term_size;

use atty::{is, Stream};
mod dir_entry_data;
use dir_entry_data::DirEntryData;

mod arguments;
use arguments::{get_argument_matches,get_command_settings_from_matches};
mod command_settings;
use command_settings::CommandSettings;

mod display;
use display::{display_entries};

mod utils;
use utils::{sanitize_filename,is_printable};

mod check_type;
use check_type::{is_executable,is_directory,is_fifo,is_symlink};

mod symlink;
use symlink::{*};

mod sort;
use sort::sort_entries;

mod named_direntry_vec;
use named_direntry_vec::NamedDirEntriesVec;

mod permissions;
use permissions::{*};

mod entries;
use entries::{*};

fn main() {
    run();
}
fn run() {
    //read arguments
    let matches = get_argument_matches();
    let path = &matches.value_of("path").unwrap();

    //generate settings from argument list
    let mut command_settings=get_command_settings_from_matches(&matches);
    override_settings(&mut command_settings);

    //list entries in path 
    list_directory(path, &command_settings);
}

fn override_settings(command_settings: &mut CommandSettings) {
    if command_settings.is_f_sort_by_system_order {
        (command_settings).is_a_all_including_current_parent = true;
        (command_settings).is_R_recursive = false;
        (command_settings).is_S_sort_by_filesize = false;
    }
    if command_settings.is_o_hide_group{
        command_settings.is_C_multicolumn_sorted_down=false;
        command_settings.is_x_multicolumn_sorted_across=false;
        command_settings.is_m_stream_output=false;
    }
    if command_settings.is_m_stream_output {
        command_settings.is_l_long = false;
    }
    if command_settings.is_n_numeric_gid_uid{
        command_settings.is_l_long=true;
    }
}


fn list_directory(path: &str, command_settings: &CommandSettings) {
    let mut dirs: Vec<NamedDirEntriesVec> = get_entries(path, command_settings);

    sort_entries(&mut dirs, command_settings);

    if !command_settings.is_R_recursive {
        // Access the only element immutably
        let de = &dirs[0];
        display_entries(&de.entries, command_settings);
    } else {
        // Access all elements immutably
        for dir in dirs {
            if dir.name != "." {
                println!("\n{}:", dir.name);
            }
            display_entries(&dir.entries, command_settings);
        }
    }
}