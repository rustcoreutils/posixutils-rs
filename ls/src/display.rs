use crate::dir_entry_data::DirEntryData;
use crate::command_settings::CommandSettings;
use term_size::dimensions;

use atty::{is, Stream};

pub fn print_total_blocks(entries: &[DirEntryData], ){
    let mut total: u64 = 0;
    for e in entries {
        let b = e.blocks.unwrap_or(0);
        total = total + b;
    }
    println!("total {}", total);
}
pub fn display_entries_long(entries: &[DirEntryData], commandsettings: &CommandSettings) {
    //print total if not d
    if !commandsettings.is_d { 
        print_total_blocks(entries);
    }

    let max_user_length = entries
        .iter()
        .map(|e| e.user_name.as_ref().unwrap_or(&"".to_string()).len())
        .max()
        .unwrap_or(0);
    let max_group_length = entries
        .iter()
        .map(|e| e.group_name.as_ref().unwrap_or(&"".to_string()).len())
        .max()
        .unwrap_or(0);

        let max_userid_length = entries
        .iter()
        .map(|e| format!("{}", e.uid.as_ref().unwrap_or(&0)).len())
        .max()
        .unwrap_or(0);
    let max_nlinks_length = entries
        .iter()
        .map(|e| format!("{}", e.nlinks.as_ref().unwrap_or(&0)).len())
        .max()
        .unwrap_or(0);
    let max_groupid_length = entries
        .iter()
        .map(|e| format!("{}", e.gid.as_ref().unwrap_or(&0)).len())
        .max()
        .unwrap_or(0);

    for e in entries {
        //EXTRA_ATTRIBUTES
        let mut extr_attr = " ";
        let has_attr = e.has_extended_attributes.unwrap_or(false);
        if has_attr {
            extr_attr = "@";
        }

        let mut name = e.name.to_string();

        let header=format!(
            "{}{}{} {:max_nlinks_length$}",
            e.file_type.as_ref().unwrap_or(&"".to_string()),
            e.permissions.as_ref().unwrap_or(&"".to_string()),
            extr_attr,
            e.nlinks.unwrap_or(0),
        );
        let footer=format!("{:>7} {} {}", e.size,
        e.modified_time_str.as_ref().unwrap_or(&"".to_string()),
        name);
        let mut row=header.to_string();
 
        if commandsettings.is_g_hide_user {
        }else{
            if commandsettings.is_n_numeric_gid_uid {
                row = format!("{} {:width2$}",row,e.uid.as_ref().unwrap_or(&0),      width2 = max_userid_length);
            }else{
                row = format!("{} {:width2$}",row,e.user_name.as_ref().unwrap_or(&"".to_string()),      width2 = max_user_length);
            }
        }
        if commandsettings.is_o_hide_group {
            
        }else{
            if commandsettings.is_n_numeric_gid_uid {
                row = format!("{}  {:width2$}",row,e.gid.as_ref().unwrap_or(&0),      width2 = max_groupid_length);
            }else{
                row = format!("{}  {:width2$}",row,e.group_name.as_ref().unwrap_or(&"".to_string()),      width2 = max_group_length);
            }
        }
        row = format!("{}{}",row,footer);
        println!("{}",row);        
    }
}

pub fn display_entries_stream_nontty(entries: &[DirEntryData], command_settings: &CommandSettings) {
    let width = 80;
    let mut file_names = Vec::new();
    for entry in entries {
        file_names.push(entry.name.as_ref());
    }
    let mut output = file_names.join(", ");
    let preserved = preserve_trailing_spaces(&output, width);
    println!("{}", preserved);
}

/// Preserves trailing spaces in wrapped text.
fn preserve_trailing_spaces(text: &str, width: usize) -> String {
    let mut result = String::new();
    let mut line_start = 0;

    while line_start < text.len() {
        let line_end = std::cmp::min(line_start + width, text.len());
        let mut line = &text[line_start..line_end];

        // Check if the cut-off point is within the boundaries of the text
        if line_end < text.len() {
            while !line.ends_with(' ') && !line.is_empty() {
                line = &line[..line.len() - 1];
            }
        }

        result.push_str(line);
        result.push('\n');

        // Move to the start of the next line
        line_start += line.len();
    }

    // Trim the last newline character for formatting purposes
    result.pop();
    result
}


pub fn display_entries_stream(entries: &[DirEntryData], commandsettings: &CommandSettings) {
    let mut file_names:Vec<String> = Vec::new();
    for entry in entries {
        file_names.push(entry.name.to_string());
    }

    if let Some((width, _)) = term_size::dimensions() {
        let mut line = String::new();
        for name in &file_names {
            let new_segment = if line.is_empty() { name.to_string() } else { format!(", {}", name) };
            // Check if adding the new segment would exceed the line width
            if line.len() + new_segment.len() > width {
                line.push_str(", ");

                println!("{}", line);
                line = name.to_string(); // Start a new line
            } else {
                if !line.is_empty() {
                    line.push_str(", ");
                }
                let st=&name.to_string();
                let str=st.as_str();
                line.push_str(str);
            }
        }
        if !line.is_empty() {
            println!("{} ", line); // Print the last line if it's not empty
        }
    } else {
        // Fallback if terminal size cannot be determined
        println!("{}", file_names.join(", "));
    }

    //}
    // Create a single string with names separated by ", "
//    let output = file_names.join(", ");
  //  println!("{}", output);
}

pub fn display_entries_normal(entries: &[DirEntryData], commandsettings: &CommandSettings) {
    if atty::is(Stream::Stdout) {
        if commandsettings.is_m_stream_output {
            display_entries_stream(entries, commandsettings);
        } else if let Some((width, _)) = dimensions() {
            let mut max_len = 0;

            if commandsettings.is_s_show_system_blocks { 
                print_total_blocks(entries);
            }

            
            let max_blocks_length = entries
            .iter()
            .map(|e| format!("{}", e.blocks.as_ref().unwrap_or(&0)).len())
            .max()
            .unwrap_or(0);

            for entry in entries {
                let mut field = "".to_string();
                if commandsettings.is_i_show_inode {
                    //field = format!("{:<8} {}", entry.inode.unwrap(), entry.name);
                    field=entry.inode_and_name.as_ref().unwrap_or(&"".to_string()).to_string();// = field;
                } else if commandsettings.is_s_show_system_blocks {
                    if commandsettings.is_k_set_blocksize {
                        field = format!("{:<8} {}", entry.size_in_blocks.unwrap(), entry.name);
                    } else {
                        field = format!("{:width$} {}", entry.blocks.unwrap(), entry.name,width=max_blocks_length);
                    }
                } else {
                    field = format!("{}", entry.name);
                }

                let len = field.len();
                if len > max_len {
                    max_len = len;
                }
            }
            let columns = width / (max_len + 8); // +8 for padding and tab space
            let rows = (entries.len() + columns - 1) / columns; // Calculate required rows

            //println!("r={} c={}",rows,columns);
            for row in 0..rows {
                for col in 0..columns {
                    if let Some(entry) = entries.get(col * rows + row) {
                        // Calculate correct index for column-first ordering
                        if commandsettings.is_i_show_inode {
                            print!(
                                "{:<width$}",
                                entry.inode_and_name.as_ref().unwrap_or(&"".to_string()).to_string(),
                                
                                width = max_len+6
                            );
                        } else if commandsettings.is_s_show_system_blocks {
                            if commandsettings.is_k_set_blocksize {
                                print!(
                                    "{:<8} {:<width$}",
                                    entry.size_in_blocks.unwrap(),
                                    entry.name,
                                    width = max_len
                                );
                            } else {
                                print!(
                                    "{:width2$} {:<width$}",
                                    entry.blocks.unwrap(),
                                    entry.name,
                                    width2=max_blocks_length,
                                    width = max_len+max_blocks_length
                                );
                            }
                        } else {
                            print!("{:<width$}\t", entry.name, width = max_len);
                        }
                    }
                }
                println!(); // End the line after each row
            }
        } else {
            // Fallback if terminal dimensions can't be fetched
            for entry in entries {
                println!("{}", entry.name);
            }
        }
    } else {// Non-TTY output
        if commandsettings.is_m_stream_output {
           display_entries_stream_nontty(entries, commandsettings);
        } else if commandsettings.is_i_show_inode {
            for entry in entries {
                println!("{:<8} {}", entry.inode.unwrap(), entry.name);
            }
        }else if commandsettings.is_s_show_system_blocks{
            
            let max_blocks_length = entries
            .iter()
            .map(|e| format!("{}", e.blocks.as_ref().unwrap()).len())
            .max()
            .unwrap_or(0);

            
            print_total_blocks(entries);

            for entry in entries {
                println!("{:width$} {}", entry.blocks.unwrap(), entry.name,width=max_blocks_length);
            }
        } else {
            
            for entry in entries {
                println!("{}", entry.name);
            }
        }
    }
}

pub fn display_entries(entries: &[DirEntryData], commandsettings: &CommandSettings) {
    //LONG
    if commandsettings.is_l_long || commandsettings.is_g_hide_user {
        display_entries_long(entries, commandsettings);
    } else {
        display_entries_normal(entries, commandsettings);
    }
}
