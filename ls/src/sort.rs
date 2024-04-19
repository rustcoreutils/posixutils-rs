use crate::command_settings::CommandSettings;
use crate::named_direntry_vec::NamedDirEntriesVec;

pub fn sort_entries(dirs: &mut Vec<NamedDirEntriesVec>, commandsettings: &CommandSettings) {
    if commandsettings.is_f_sort_by_system_order {
        //no sorting, use system order
    } else if commandsettings.is_c_use_time_of_last_modification {
        sort_entries_by_created_time(dirs,commandsettings);
    }  else if commandsettings.is_t_sort_by_time_modified {
        sort_entries_by_modified_time(dirs,commandsettings);
    } else if commandsettings.is_S_sort_by_filesize {        
        sort_entries_by_size(dirs,commandsettings);    
    } else {
        sort_entries_by_name(dirs, commandsettings);
    }
    ///reverse if r
    if commandsettings.is_r_reverse_sort_order{
        for dir in dirs {
            dir.entries.reverse();
        }
    }
}
fn sort_entries_by_created_time(dirs: &mut Vec<NamedDirEntriesVec>, commandsettings: &CommandSettings) {
        // Sort entries alphabetically and case-insensitively within each directory list
        dirs.sort_by_key(|dir| dir.name.to_lowercase());

        // Sort entries alphabetically and case-insensitively within each directory list
        for dir in dirs {
            dir.entries.sort_unstable_by_key(|entry| entry.created_time);
            dir.entries.reverse();
        }

}

fn sort_entries_by_modified_time(dirs: &mut Vec<NamedDirEntriesVec>, commandsettings: &CommandSettings) {
    // Sort entries alphabetically and case-insensitively within each directory list
    dirs.sort_by_key(|dir| dir.name.to_lowercase());

    // Sort entries alphabetically and case-insensitively within each directory list
    for dir in dirs {
        dir.entries.sort_unstable_by_key(|entry| entry.modified_time);
        dir.entries.reverse();
    }

}
fn sort_entries_by_name(dirs: &mut Vec<NamedDirEntriesVec>, commandsettings: &CommandSettings) {
//sort by name
        // Sort entries alphabetically and case-insensitively within each directory list
        dirs.sort_by_key(|dir| dir.name.to_string());

        // Sort entries alphabetically and case-insensitively within each directory list
        for dir in dirs {
            dir.entries.sort_by_key(|entry| entry.name.to_string());
        }
}


fn sort_entries_by_size(dirs: &mut Vec<NamedDirEntriesVec>, commandsettings: &CommandSettings) {
        //sort by size
        // Sort entries alphabetically and case-insensitively within each directory list
        dirs.sort_by_key(|dir| dir.name.to_lowercase());

        for dir in dirs {
            dir.entries.sort_unstable_by(|a, b| {
                let a_size = a.size;
                let b_size = b.size;

                // Primary sort by size (reverse order)
                b_size.cmp(&a_size).then_with(|| {
                    // Secondary sort by filename (normal order)
                    a.name.cmp(&b.name)
                })
            });
        }
        

}