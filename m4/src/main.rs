use std::{
    ffi::{OsStr, OsString},
    os::unix::ffi::OsStrExt,
};

use clap::builder::{TypedValueParser, ValueParserFactory};

// TODO: potentially we can use a reference here to avoid allocation
#[derive(Debug, Clone)]
struct ArgumentName(OsString);

impl From<OsString> for ArgumentName {
    fn from(value: OsString) -> Self {
        Self(value)
    }
}

impl From<OsString> for ArgumentValue {
    fn from(value: OsString) -> Self {
        Self(value)
    }
}

// TODO: potentially we can use a reference here to avoid allocation
#[derive(Debug, Clone)]
struct ArgumentValue(OsString);

#[derive(Debug, Clone)]
struct ArgumentDefine {
    name: ArgumentName,
    value: Option<ArgumentValue>,
}

#[derive(Clone)]
struct ArgumentDefineParser;

impl TypedValueParser for ArgumentDefineParser {
    type Value = ArgumentDefine;

    fn parse_ref(
        &self,
        _cmd: &clap::Command,
        _arg: Option<&clap::Arg>,
        value: &std::ffi::OsStr,
    ) -> Result<Self::Value, clap::Error> {
        let value_bytes = value.as_encoded_bytes();
        // TODO: do we need to support stripping whitespace after or before the `=`
        let mut split = value_bytes.splitn(2, |b| *b == b'=');
        // TODO: use error
        let name = OsStr::from_bytes(split.next().unwrap()).to_owned().into();

        let value = match split.next() {
            // TODO: perhaps we should use
            // https://doc.rust-lang.org/std/ffi/struct.OsStr.html#method.from_encoded_bytes_unchecked
            // instead?
            Some(value) => Some(OsStr::from_bytes(value).to_owned().into()),
            None => None,
        };
        Ok(ArgumentDefine { name, value })
    }
}

impl ValueParserFactory for ArgumentDefine {
    type Parser = ArgumentDefineParser;

    fn value_parser() -> Self::Parser {
        ArgumentDefineParser
    }
}

#[derive(Debug)]
struct Args {
    /// Enable line synchronization output for the c99 preprocessor phase (that is, #line
    /// directives).
    line_synchronization: bool,
    /// `name[=val]`
    ///
    /// Define `name` to `val` or to `null` if `=val` is omitted.
    defines: Vec<ArgumentDefine>,
    // Undefine `name`.
    undefines: Vec<ArgumentName>,
}

impl Args {
    pub fn parse() -> Self {
        let matches = clap::command!()
        .arg(clap::Arg::new("line_synchronization")
            .short('s')
            .help("Enable line synchronization output for the c99 preprocessor phase (that is, #line directives)."))
        .arg(clap::Arg::new("defines")
                .short('D')
                .value_name("name=[value]")
                .action(clap::ArgAction::Append)
                .help("Define name to val or to null if = val is omitted"))
        .arg(clap::Arg::new("undefines")
                .short('U')
                .value_name("name")
                .action(clap::ArgAction::Append)
                .help("Undefine name"))
        .get_matches();

        Self {
            line_synchronization: matches.get_flag("line_synchronization"),
            defines: matches
                .get_many::<ArgumentDefine>("defines")
                .unwrap()
                .cloned()
                .collect(),
            undefines: matches
                .get_many::<ArgumentName>("undefines")
                .unwrap()
                .cloned()
                .collect(),
        }
    }
}

fn main() {
    let args = Args::parse();
    dbg!(args);
}
