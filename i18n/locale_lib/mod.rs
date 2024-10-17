use charmap_dir::read_charmap_names;
use locale_archive::read_locale_archive;
use locale_dir::read_locale_directories;

pub mod charmap_dir;
pub mod locale_archive;
pub mod locale_dir;

pub fn read_available_locale_names() -> Result<Vec<String>, Box<dyn std::error::Error>> {
    // "C" and "POSIX" locales are always available
    let mut locale_names = vec!["POSIX".to_string(), "C".to_string()];

    locale_names.extend(read_locale_archive()?);
    locale_names.extend(read_locale_directories()?);

    // TODO: explore more on exact sorting used on glibc
    locale_names.sort_by(|a, b| a.to_lowercase().cmp(&b.to_lowercase()));
    Ok(locale_names)
}

pub fn read_available_charmap_names() -> Result<Vec<String>, Box<dyn std::error::Error>> {
    let mut charmap_names = read_charmap_names()?;
    charmap_names.sort();
    Ok(charmap_names)
}
