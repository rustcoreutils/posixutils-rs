use std::{env, process::exit};

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use locale_lib::{read_available_charmap_names, read_available_locale_names};
use plib::PROJECT_NAME;

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
    read_available_locale_names()?
        .iter()
        .for_each(|name| println!("{name}"));
    Ok(())
}

fn display_all_charmap_names() -> Result<(), Box<dyn std::error::Error>> {
    read_available_charmap_names()?
        .into_iter()
        .filter(|name| !name.is_empty()) // TODO: remove this filter when the bug is fixed
        .for_each(|name| println!("{name}"));
    Ok(())
}

fn display_env_variables() {
    let lc_all = env::var("LC_ALL").unwrap_or_default();
    let lang = env::var("LANG").unwrap_or_default();
    let lc_variables = [
        "LC_CTYPE",
        "LC_COLLATE",
        "LC_MONETARY",
        "LC_NUMERIC",
        "LC_TIME",
        "LC_MESSAGES",
    ];

    println!("LANG={}", lang);

    for &var in &lc_variables {
        let value = std::env::var(var).unwrap_or_default();
        if !lc_all.is_empty() {
            println!("{}=\"{}\"", var, lc_all);
        } else if !value.is_empty() {
            println!("{}={}", var, value);
        } else {
            println!("{}=\"{}\"", var, lang);
        }
    }

    println!("LC_ALL={}", lc_all);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    if args.all_locales {
        display_all_locale_names()?;
        exit(0)
    }

    if args.charmap {
        display_all_charmap_names()?;
        exit(0)
    }

    display_env_variables();

    Ok(())
}
