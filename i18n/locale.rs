use clap::Parser;
use gettextrs::gettext;
use locale_lib::read_available_locale_names;

mod locale_lib;

#[derive(Parser)]
#[command(version, about = gettext("locale - get locale-specific information"))]
struct Args {
    #[arg(short = 'a', help = gettext("Write information about all available public locales"))]
    all_locales: bool,

    #[arg(short, long, help = gettext("Write the names of selected locale categories"))]
    category: bool,

    #[arg(short, long, help = gettext("Write the names and values of selected keywords"))]
    keyword: bool,

    #[arg(short = 'm', long, conflicts_with_all = ["category", "keyword", "name"], help = gettext("Write names of available charmaps"))]
    charmap: bool,

    #[arg(value_name = "NAME", help = gettext("The name of a locale category or keyword"))]
    name: Vec<String>,
}

fn display_all_locale_names() -> Result<(), Box<dyn std::error::Error>> {
    let locale_names = read_available_locale_names()?;
    for locale_name in locale_names {
        println!("{locale_name}");
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    if args.all_locales {
        display_all_locale_names()?;
    }

    Ok(())
}
