use locale_archive::read_locale_archive;
use locale_directory::read_locale_directories;

pub mod locale_archive;
pub mod locale_directory;

pub fn read_available_locale_names() -> Result<Vec<String>, Box<dyn std::error::Error>> {
    // "C" and "POSIX" locales are always available
    let mut locale_names = vec!["POSIX".to_string(), "C".to_string()];

    locale_names.extend(read_locale_archive()?);
    locale_names.extend(read_locale_directories()?);

    locale_names.sort_by(|a, b| a.to_lowercase().cmp(&b.to_lowercase()));
    Ok(locale_names)
}
