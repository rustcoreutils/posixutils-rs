use gettextrs::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
   // gettext de-duplicates strings, i.e. the same string used multiple times
    // will have a single entry in the PO and MO files. However, the same words
    // might have different meaning depending on the context. To distinguish
    // between different contexts, gettext accepts an additional string:
    println!(
        "Plural with context: {}",
        npgettext("This is the context", "One thing", "Multiple things", 2));

    Ok(())
}
