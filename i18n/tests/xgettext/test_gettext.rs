use gettextrs::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // `gettext()` simultaneously marks a string for translation and translates
    // it at runtime.
    println!("Translated: {}", gettext("Hello, world!"));

    Ok(())
}
