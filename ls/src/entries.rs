use std::fs;
use std::ffi::OsString;
use std::fs::Metadata;
use std::path::PathBuf;
use chrono::{DateTime, Local};
use std::path::Path;
use std::os::unix::fs::MetadataExt;
use std::os::unix::fs::PermissionsExt;

use users::{get_group_by_gid, get_user_by_uid};
use crate::command_settings::CommandSettings;
use crate::named_direntry_vec::NamedDirEntriesVec;
use crate::DirEntryData;
use crate::utils::{*};
use crate::symlink::{*};
use crate::check_type::{*};
use crate::permissions::{*};
use std::time::SystemTime;

use std::fs::DirEntry;
pub 
fn get_entries(path: &str, command_settings: &CommandSettings) -> Vec<NamedDirEntriesVec> {
    let mut direntries: Vec<NamedDirEntriesVec> = Vec::new();

    if command_settings.is_d {
        let mut entries: Vec<DirEntryData> = Vec::new();

        let current_dir_entry = gen_current_direntrydata(command_settings);
        entries.push(current_dir_entry);
        let d = NamedDirEntriesVec {
            name: ".".to_string(),
            entries: entries,
        };
        direntries.push(d);
    } else {
        add_entries(&mut direntries, path, &command_settings);

        //add . and .. if is_all
        if command_settings.is_a_all_including_current_parent {
            add_current_and_parent(&mut direntries, command_settings);
        }
    }
    return direntries;
}
        fn add_current_and_parent(
    entries: &mut Vec<NamedDirEntriesVec>,
    command_settings: &CommandSettings,
) {
    let current_dir_entry = gen_current_direntrydata(command_settings);
    let parent_dir_entry = gen_parent_direntrydata(command_settings);

    entries[0].entries.push(current_dir_entry);
    entries[0].entries.push(parent_dir_entry);
}




pub 
fn get_direntrydata_by_path(path: String, command_settings: &CommandSettings) -> DirEntryData {
    let path = PathBuf::from(path);
    let mut name = "".to_string();
    if let Some(file_name) = path.file_name() {
        if let Some(file_name_str) = file_name.to_str() {
            name = file_name_str.to_string()
        }
    }
    let is_symlink = is_symlink(&path);
    let is_symlink2 = is_symlink2(&path);
  
   
    let metadata = if command_settings.is_F_do_not_follow_symbolic_links {
        fs::symlink_metadata(&path) // Do not follow symbolic links
    } else {
        if is_symlink{
            fs::symlink_metadata(&path) // Do not follow symbolic links
            
        }else{
        fs::metadata(&path) // Follow symbolic links
        }
    };
    
    if let Ok(metadata) = metadata {
         return        getDataFromMetadata(path, command_settings, metadata);
    } else {
       

    let is_dir = is_directory(&path);
    let is_exe = is_executable(&path);
    let is_fifo = is_fifo(&path);
  

     //   println!("Could not read metadata for {}", path.display());
        return DirEntryData {
            permissions: None,
            nlinks: None,
            gid: None,
            created_time: None,
            symlink_target_name: None,
            size_in_blocks: None,
            inode: None,
            user_name: None,
            modified_time:None,
            inode_and_name:None,
            blocks_and_name:None,
            group_name: None,
            has_extended_attributes: None,
            blocks: None,
            file_type: None,
            uid: None,
            modified_time_str: None,
            name: name,
            path: path.display().to_string(),
            is_dir: is_dir,
            is_symlink: None,
            size: 0,
        };
    }
}
pub fn getDataFromMetadata(path:PathBuf, command_settings: &CommandSettings,metadata:Metadata) ->DirEntryData{
    let mut name = "".to_string();
    if let Some(file_name) = path.file_name() {
        if let Some(file_name_str) = file_name.to_str() {
            name = file_name_str.to_string()
        }
    }
    if command_settings.is_q_force_nonprintable_as_questionmarks{
        let os_string = OsString::from(name);
        name=sanitize_filename(os_string);
    }


    let is_dir = is_directory(&path);
    let is_exe = is_executable(&path);
    let is_fifo = is_fifo(&path);
    let is_symlink = is_symlink(&path);
    let is_symlink2 = is_symlink2(&path);
  
    

    let mut symlink_target_name = "".to_string();

   
    let mut size = 0;
    if is_symlink {
        size = get_symlink_size(&path).unwrap();

        match find_symlink_target(&path) {
            Ok(Some(target)) => {
                symlink_target_name = target.display().to_string();
            }
            Ok(None) => println!("Not a symlink"),
            Err(e) => println!("Error: {}", e),
        }
    }

    if !is_symlink {
        size = metadata.len();
    }
    let modified_time = metadata.modified().unwrap();
    let modified_time = modified_time.duration_since(std::time::UNIX_EPOCH).unwrap();

    if command_settings.is_F_do_not_follow_symbolic_links {
        if is_symlink {
        //    println!("SYM {}", is_symlink);
            name = format!("{}@", name)
        } else if is_dir {
            name = format!("{}/", name)
        } else if is_exe {
            name = format!("{}*", name)
        } else if is_fifo {
            name = format!("{}|", name)
        }
    }
    if command_settings.is_l_long {
        if is_symlink {
            name = format!("{} -> {}", name, symlink_target_name)
        }
    }

    let mut file_type = if metadata.file_type().is_dir() {
        "d"
    } else if metadata.file_type().is_file() {
        "-"
    } else if metadata.file_type().is_symlink() {
        "l"
    } else {
        "?"
    };
    if is_symlink {
        file_type = "l";
    }
    let mut permissions = metadata.permissions();
    let mut permissions_str = format_mode(permissions.mode());

    let nlinks = metadata.nlink();
    let uid = metadata.uid();
    let gid = metadata.gid();

    let user_name = get_user_by_uid(uid).map(|u| u.name().to_string_lossy().into_owned());
    let group_name = get_group_by_gid(gid).map(|g| g.name().to_string_lossy().into_owned());

    // let size = metadata.size();

    let modified = DateTime::<Local>::from(metadata.modified().unwrap());
    let mut formatted_time = modified.format("%b %d %H:%M").to_string();
    //println!("issym {} {}",name,is_symlink);
    if is_symlink {
        formatted_time = get_symlink_modified(&path).unwrap();
        permissions_str = get_symlink_permissions(&path).unwrap();
        //println!("issym {} {} per={}",name,is_symlink,permissions_str);
    }

    let size_in_blocks = (metadata.len() as f64 / 1024.0).ceil() as u64;
    let has_extended_attributes = has_extended_attributes(&path);
    let mut blocks = metadata.blocks();
    if is_symlink {
        blocks = get_symlink_blocks(&path).unwrap();
    }

    let mut inode=metadata.ino();
    if is_symlink{
        inode=get_symlink_inode(&path).unwrap();            
    }
    let inode_and_name=format!("{:<8} {}", inode, name.clone().to_string());
    let blocks_and_name=format!("{:<8} {}", blocks, name.clone().to_string());

    if command_settings.is_p_add_slash{
        if is_dir{
            name=format!("{}/",name)
            
        }
    }
    return DirEntryData {
        file_type: Some(file_type.to_string()),
        name: name,
        inode_and_name:Some(inode_and_name ),
        blocks_and_name:Some(blocks_and_name ),
        modified_time : Some(metadata.modified().unwrap_or(SystemTime::UNIX_EPOCH)),
        created_time: Some(metadata.created().unwrap_or(SystemTime::UNIX_EPOCH)),
        has_extended_attributes: Some(has_extended_attributes),
        uid: Some(uid),
        blocks: Some(blocks),
        modified_time_str: Some(formatted_time),
        gid: Some(gid),
        inode: Some(inode),
        size_in_blocks: Some(size_in_blocks),
        user_name: user_name,
        symlink_target_name: Some(symlink_target_name),
        group_name: group_name,
        nlinks: Some(nlinks),
        permissions: Some(permissions_str),
        path: path.display().to_string(),
        is_dir: is_dir,
        is_symlink: Some(is_symlink),
        size: size,
    };
}
pub 
fn add_entries(
    entries_vec: &mut Vec<NamedDirEntriesVec>,
    path: &str,
    command_settings: &CommandSettings,
) {
    let mut direntries_data_vec: Vec<DirEntryData> = Vec::new();
           

    if let Ok(entries) = fs::read_dir(path) {

    
        let collected: Vec<_> = entries.filter_map(Result::ok).collect();
        for entry in collected {
            if should_display(&entry, command_settings) {
                let p = entry.path();
                let name = entry.file_name();
                let pstr = p.to_str().unwrap();
                // println!("Add {}",pstr);

                let data = get_direntrydata(entry, command_settings);
                direntries_data_vec.push(data);
                if command_settings.is_R_recursive && p.is_dir() {
                    // Avoiding infinite loop by not re-listing '.' or '..'
                    if name != "." && name != ".." {
                        let mut dirs: Vec<NamedDirEntriesVec> = Vec::new();
                        add_entries(&mut dirs, pstr, command_settings);
                        for dir in dirs {
                            entries_vec.push(dir);
                        }
                    }
                }
            }
        }
        let d: NamedDirEntriesVec = NamedDirEntriesVec {
            name: path.to_string(),
            entries: direntries_data_vec,
        };

        entries_vec.push(d);
    } else {
     //   eprintln!("Failed to read directory: {}", path);
    }
}



fn gen_current_direntrydata(command_settings: &CommandSettings) -> DirEntryData {
    let mut d = get_direntrydata_by_path(".".to_string(), &command_settings);
    d.name = ".".to_string();
    return d;
}
fn gen_parent_direntrydata(command_settings: &CommandSettings) -> DirEntryData {
    let mut d = get_direntrydata_by_path("..".to_string(), &command_settings);
    d.name = "..".to_string();
    return d;
}

fn should_display(entry: &DirEntry, commandsettings: &CommandSettings) -> bool {
    //    let entryname=entry.file_name();
    //  let name=entryname.to_str().unwrap();
    if commandsettings.is_a_all_including_current_parent {
        //  let show=true;
        // println!("Check {} {}",name,show);
        return true;
    } else if commandsettings.is_A_all_excluding_current_parent {
        return true;
        let show = !entry
            .file_name()
            .to_str()
            .map_or(false, |s| s.starts_with('.'));
        // println!("Check {} {}",name,show);
        return show;
    }
    return !entry
        .file_name()
        .to_str()
        .map_or(false, |s| s.starts_with('.'));
}

fn get_direntrydata(entry: DirEntry, command_settings: &CommandSettings) -> DirEntryData {
    let mut name = entry.file_name().to_str().unwrap().to_string();
    let path = entry.path().display().to_string();
    return get_direntrydata_by_path(path, command_settings);
}

fn has_extended_attributes<P: AsRef<Path>>(path: P) -> bool {
    match xattr::list(path.as_ref()) {
        Ok(mut attrs) => attrs.next().is_some(),
        Err(_) => false,
    }
}