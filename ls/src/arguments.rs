use crate::command_settings::CommandSettings;

use clap::ArgMatches;
use clap::{App, Arg};


pub fn get_argument_matches() -> ArgMatches{
    return   App::new("Rust ar")
    .version("0.1.0")
    .author("J Dumont")
    .about("Implements an ar command in Rust")
    .arg(Arg::new("path")
        .default_value(".")
        .help("The path to list"))    
    .arg(Arg::new("A")
         .short('A')
         .takes_value(false)
         .help("Write out all directory entries, including those whose names begin with a <period> ( '.' ) but excluding the entries dot and dot-dot (if they exist)."))
    .arg(Arg::new("C")
         .short('C')
         .takes_value(false)
         .help("Write multi-text-column output with entries sorted down the columns, according to the collating sequence. The number of text columns and the column separator characters are unspecified, but should be adapted to the nature of the output device. This option disables long format output."))
    .arg(Arg::new("F")
         .short('F')
         .takes_value(false)
         .help("Do not follow symbolic links named as operands unless the -H or -L options are specified. Write a <slash> ( '/' ) immediately after each pathname that is a directory, an <asterisk> ( '*' ) after each that is executable, a <vertical-line> ( '|' ) after each that is a FIFO, and an at-sign ( '@' ) after each that is a symbolic link. For other file types, other symbols may be written."))
    .arg(Arg::new("H")
         .short('H')
         .takes_value(false)
         .help("Evaluate the file information and file type for symbolic links specified on the command line to be those of the file referenced by the link, and not the link itself; however, ls shall write the name of the link itself and not the file referenced by the link."))
    .arg(Arg::new("L")
         .short('L')
         .takes_value(false)
         .help("Evaluate the file information and file type for all symbolic links (whether named on the command line or encountered in a file hierarchy) to be those of the file referenced by the link, and not the link itself; however, ls shall write the name of the link itself and not the file referenced by the link. When -L is used with -l, write the contents of symbolic links in the long format (see the STDOUT section)."))
    .arg(Arg::new("R")
         .short('R')
         .takes_value(false)
         .help("Recursively list subdirectories encountered. When a symbolic link to a directory is encountered, the directory shall not be recursively listed unless the -L option is specified. The use of -R with -d or -f produces unspecified results."))
    .arg(Arg::new("S")
         .short('S')
         .takes_value(false)
         .help("Sort with the primary key being file size (in decreasing order) and the secondary key being filename in the collating sequence (in increasing order)."))
    .arg(Arg::new("a")
         .short('a')
         .takes_value(false)
         .help("Write out all directory entries, including those whose names begin with a <period> ( '.' )."))
    .arg(Arg::new("c")
         .short('c')
         .takes_value(false)
         .help("Use time of last modification of the file status information (see XBD <sys/stat.h>) instead of last modification of the file itself for sorting ( -t) or writing (-l)."))
    .arg(Arg::new("d")
         .short('d')
         .takes_value(false)
         .help("Do not follow symbolic links named as operands unless the -H or -L options are specified. Do not treat directories differently than other types of files. The use of -d with -R or -f produces unspecified results."))
    .arg(Arg::new("f")
         .short('f')
         .takes_value(false)
         .help("List the entries in directory operands in the order they appear in the directory. The behavior for non-directory operands is unspecified. This option shall turn on -a. When -f is specified, any occurrences of the -r, -S, and -t options shall be ignored and any occurrences of the -A, [XSI] [Option Start] -g, [Option End] -l, -n, [XSI] [Option Start] -o, [Option End] and -s options may be ignored. The use of -f with -R or -d produces unspecified results."))
    .arg(Arg::new("g")
         .short('g')
         .takes_value(false)
         .help("Turn on the -l (ell) option, but disable writing the file's owner name or number. Disable the -C, -m, and -x options."))
    .arg(Arg::new("i")
         .short('i')
         .takes_value(false)
         .help("For each file, write the file's file serial number (see stat() in the System Interfaces volume of POSIX.1-2017)."))
    .arg(Arg::new("k")
         .short('k')
         .takes_value(false)
         .help("Set the block size for the -s option and the per-directory block count written for the -l, -n, -s, [XSI] [Option Start] -g, and -o [Option End] options (see the STDOUT section) to 1024 bytes."))
    .arg(Arg::new("l")
         .short('l')
         .takes_value(false)
         .help("Do not follow symbolic links named as operands unless the -H or -L options are specified. Write out in long format (see the STDOUT section). Disable the -C, -m, and -x options."))
    .arg(Arg::new("m")
         .short('m')
         .takes_value(false)
         .help("Stream output format; list pathnames across the page, separated by a <comma> character followed by a <space> character. Use a <newline> character as the list terminator and after the separator sequence when there is not room on a line for the next list entry. This option disables long format output."))
    .arg(Arg::new("n")
         .short('n')
         .takes_value(false)
         .help("Turn on the -l (ell) option, but when writing the file's owner or group, write the file's numeric UID or GID rather than the user or group name, respectively. Disable the -C, -m, and -x options."))
    .arg(Arg::new("o")
         .short('o')
         .takes_value(false)
         .help("Turn on the -l (ell) option, but disable writing the file's group name or number. Disable the -C, -m, and -x options."))
    .arg(Arg::new("p")
         .short('p')
         .takes_value(false)
         .help("Write a <slash> ( '/' ) after each filename if that file is a directory."))
    .arg(Arg::new("q")
         .short('q')
         .takes_value(false)
         .help("Force each instance of non-printable filename characters and <tab> characters to be written as the <question-mark> ( '?' ) character. Implementations may provide this option by default if the output is to a terminal device."))
    .arg(Arg::new("r")
         .short('r')
         .takes_value(false)
         .help("Reverse the order of the sort to get reverse collating sequence oldest first, or smallest file size first depending on the other options given."))
    .arg(Arg::new("s")
         .short('s')
         .takes_value(false)
         .help("Indicate the total number of file system blocks consumed by each file displayed. If the -k option is also specified, the block size shall be 1024 bytes; otherwise, the block size is implementation-defined."))
    .arg(Arg::new("t")
         .short('t')
         .takes_value(false)
         .help("Sort with the primary key being time modified (most recently modified first) and the secondary key being filename in the collating sequence. For a symbolic link, the time used as the sort key is that of the symbolic link itself, unless ls is evaluating its file information to be that of the file referenced by the link (see the -H and -L options)."))
    .arg(Arg::new("u")
         .short('u')
         .takes_value(false)
         .help("Use time of last access (see XBD <sys/stat.h>) instead of last modification of the file for sorting (-t) or writing (-l).
         "))
         .arg(Arg::new("x")
         .short('x')
         .takes_value(false)
         .help("The same as -C, except that the multi-text-column output is produced with entries sorted across, rather than down, the columns. This option disables long format output.
         "))
         .arg(Arg::new("1")
         .short('1')
         .takes_value(false)
         .help("Force output to be one entry per line. This option does not disable long format output. (Long format output is enabled by [XSI] [Option Start] -g, [Option End] -l (ell), -n, and [XSI] [Option Start] -o; [Option End] and disabled by -C, -m, and -x.)
        "))
    .get_matches();
}
pub fn get_command_settings_from_matches(matches : &ArgMatches) -> CommandSettings{
    //FLAGS
   
    let is_a = matches.is_present("a");
    let is_A = matches.is_present("A");
    let is_l = matches.is_present("l");
    let is_R = matches.is_present("R");
    let is_F = matches.is_present("F");
    let is_d = matches.is_present("d");
    let is_f = matches.is_present("f");
    let is_s = matches.is_present("s");
    let is_i = matches.is_present("i");
    let is_k = matches.is_present("k");
    let is_m = matches.is_present("m");
    let is_g = matches.is_present("g");
    let is_n = matches.is_present("n");
    let is_o = matches.is_present("o");
    let is_p = matches.is_present("p");
    let is_q = matches.is_present("q");
    let is_r = matches.is_present("r");
    let is_s = matches.is_present("s");
    let is_t = matches.is_present("t");
    let is_u = matches.is_present("u");
    let is_x = matches.is_present("x");
    let is_1 = matches.is_present("1");
    let is_S = matches.is_present("S");
    let is_C = matches.is_present("C");
    let is_c = matches.is_present("c");

    //COMMAND_SETTINGS
    return  CommandSettings {
        is_A_all_excluding_current_parent: is_A,
        is_l_long: is_l,
        is_C_multicolumn_sorted_down:is_C,
        is_c_use_time_of_last_modification:is_c,
        is_R_recursive: is_R,
        is_S_sort_by_filesize: is_S,
        is_d: is_d,
        is_s_show_system_blocks: is_s,
        is_f_sort_by_system_order: is_f,
        is_n_numeric_gid_uid: is_n,
        is_k_set_blocksize: is_k,
        is_m_stream_output: is_m,
        is_o_hide_group:is_o,
        is_p_add_slash:is_p,
        is_1_single_line_output:is_1,
        is_q_force_nonprintable_as_questionmarks:is_q,
        is_r_reverse_sort_order:is_r,
        is_u_use_time_of_last_access:is_u,
        is_x_multicolumn_sorted_across: is_x,
        is_t_sort_by_time_modified:is_t,
        is_i_show_inode: is_i,
        is_g_hide_user: is_g,
        is_a_all_including_current_parent: is_a,
        is_F_do_not_follow_symbolic_links: is_F,
    };
}