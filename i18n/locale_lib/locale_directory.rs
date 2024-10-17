use std::{fs::read_dir, path::Path};

const LOCALE_DIRECTORIES_PATH: &str = "/usr/lib/locale";

pub fn read_locale_directories() -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let locale_dir = Path::new(LOCALE_DIRECTORIES_PATH);
    let mut locale_names = Vec::new();

    for entry in read_dir(locale_dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            if let Some(name) = path.file_name() {
                if let Some(name_str) = name.to_str() {
                    if verify_locale_directory(&path)? {
                        locale_names.push(name_str.to_string());
                    }
                }
            }
        }
    }
    Ok(locale_names)
}

fn verify_locale_directory(path: &Path) -> Result<bool, Box<dyn std::error::Error>> {
    // on GNU, after creating a directory for a locale in /usr/lib/locale and then ONLY after
    // creating a file LC_IDENTIFICATION(even an empty file) the "locale -a" displayed that directory as a valid "locale"
    // so, it seems GNU just checks the existence of file, so we will also be checking just for the existence of that file
    // TODO: need to check the case for macOS

    let required_files = ["LC_IDENTIFICATION"];
    for file in required_files.iter() {
        if !path.join(file).exists() {
            return Ok(false);
        }
    }

    Ok(true)
}
