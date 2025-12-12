use clap::Parser;

use gettextrs::{
    bind_textdomain_codeset, bindtextdomain, gettext, setlocale, textdomain, LocaleCategory,
};

#[derive(Parser)]
#[command(
    version,
    about = gettext("time - time a simple command or give resource usage"),
    help_template = gettext("{about}\n\nUsage: {usage}\n\nArguments:\n{positionals}\n\nOptions:\n{options}"),
    disable_help_flag = true,
    disable_version_flag = true,
)]
struct Args {
    #[arg(
        short,
        long,
        help = gettext("Write timing output to standard error in POSIX format")
    )]
    posix: bool,

    #[arg(help = gettext("The utility to be invoked"))]
    utility: String,

    #[arg(
        name = "ARGUMENT",
        trailing_var_arg = true,
        help = gettext("Arguments for the utility")
    )]
    arguments: Vec<String>,

    #[arg(short, long, help = gettext("Print help"), action = clap::ArgAction::HelpLong)]
    help: Option<bool>,

    #[arg(short = 'V', long, help = gettext("Print version"), action = clap::ArgAction::Version)]
    version: Option<bool>,
}
