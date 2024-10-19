use flate2::read::GzDecoder;
use std::{
    collections::HashSet,
    fs::{read_dir, File},
    io::{BufRead, BufReader},
    path::Path,
};

const CHARMAP_DIRECTORIES_PATH: &str = "/usr/share/i18n/charmaps/";
/// so on GNU they are stored as an archive file (gz), so we will be reading the directory
/// and simply returning the names of the files with ".gz" extension removed
/// TODO: need to check the case for macOS

pub fn read_charmap_names() -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let mut charmap_names = Vec::new();
    let mut file_stems = HashSet::new();
    for entry in read_dir(CHARMAP_DIRECTORIES_PATH)? {
        let entry = entry?;
        if entry.path().is_file() && entry.path().extension().and_then(|s| s.to_str()) == Some("gz")
        {
            if let Some(file_stem) = entry.path().file_stem().and_then(|s| s.to_str()) {
                file_stems.insert(file_stem.to_string());
                let aliases = extract_codeset(&entry.path())?;
                charmap_names.extend(aliases);
            }
        }
    }
    file_stems.extend(charmap_names);
    charmap_names = file_stems.into_iter().collect();
    Ok(charmap_names)
}

fn extract_codeset(path: &Path) -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let file = File::open(path)?;
    let decoder = GzDecoder::new(file);
    let reader = BufReader::new(decoder);

    let mut aliases = Vec::new();
    let mut code_set_name = String::new();
    for line in reader.lines().map_while(Result::ok) {
        if line.starts_with("<code_set_name>") {
            if let Some(name) = line.split_whitespace().nth(1) {
                code_set_name = name.to_string();
            }
        } else if line.contains("CHARMAP") {
            break;
        }
    }

    aliases.push(code_set_name);
    Ok(aliases)
}
