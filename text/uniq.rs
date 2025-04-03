use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, setlocale, textdomain};
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::path::PathBuf;

/// The uniq utility - filters out duplicate lines in a file
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Count the number of repeated lines
    #[arg(short = 'c')]
    count: bool,

    /// Print only the repeated lines
    #[arg(short = 'd')]
    repeated: bool,

    /// Print only unique lines
    #[arg(short = 'u')]
    unique: bool,

    /// Ignore the first fields fields on each input line
    #[arg(short = 'f')]
    fields: Option<usize>,

    /// Ignore the first chars characters on each input line
    #[arg(short = 's')]
    chars: Option<usize>,

    /// Input file (if not specified, use stdin)
    input_file: Option<PathBuf>,

    /// Output file (if not specified, use stdout)
    output_file: Option<PathBuf>,
}

impl Args {
    /// Validates the arguments to ensure no conflicting options are used together.
    ///
    /// # Errors
    ///
    /// Returns an error if conflicting options are found.
    fn validate_args(&self) -> Result<(), String> {
        // Check if conflicting options are used together
        if self.unique && self.repeated {
            return Err("Options '-u' and '-d' cannot be used together".to_string());
        }
        if self.count && self.repeated {
            return Err("Options '-c' and '-d' cannot be used together".to_string());
        }
        if self.count && self.unique {
            return Err("Options '-c' and '-u' cannot be used together".to_string());
        }
        Ok(())
    }
}

/// Processes the input according to the specified arguments and writes the output.
///
/// # Arguments
///
/// * `args` - A reference to the `Args` struct containing the command line arguments.
///
/// # Errors
///
/// Returns an error if there is an issue reading the input or writing the output.
fn uniq(args: &Args) -> Result<(), Box<dyn std::error::Error>> {
    let input: Box<dyn BufRead> = match &args.input_file {
        Some(file) => {
            if *file == PathBuf::from("-") {
                Box::new(BufReader::new(io::stdin()))
            } else {
                Box::new(BufReader::new(File::open(file)?))
            }
        }
        None => Box::new(BufReader::new(io::stdin())),
    };

    let mut output: Box<dyn Write> = match &args.output_file {
        Some(file) => Box::new(File::create(file)?),
        None => Box::new(io::stdout()),
    };

    let lines: Vec<String> = input.lines().collect::<Result<_, _>>()?;

    let mut last_line: Option<String> = None;
    let mut current_count = 0;

    for line in &lines {
        let processed_line = process_line(line, args.fields, args.chars);

        if let Some(last_line) = &last_line {
            let processed_last_line = process_line(last_line, args.fields, args.chars);
            if processed_line == processed_last_line {
                current_count += 1;
                continue;
            } else {
                output_result(&mut output, last_line, current_count, args)?;
            }
        }
        last_line = Some(line.to_string());
        current_count = 1;
    }

    if let Some(last) = last_line {
        output_result(&mut output, &last, current_count, args)?;
    }
    Ok(())
}

/// Processes a line according to the specified field and character options.
///
/// # Arguments
///
/// * `line` - The line to be processed.
/// * `fields` - The number of fields to skip.
/// * `chars` - The number of characters to skip.
///
/// # Returns
///
/// Returns the processed line as a `String`.
fn process_line(line: &str, fields: Option<usize>, chars: Option<usize>) -> String {
    let mut processed_line = line.to_string();
    if line.is_empty() {
        return line.to_string();
    }
    if let Some(f) = fields {
        if f == 0 {
            processed_line = line.to_string();
        } else {
            let mut field_count = 0;

            let chars = line.chars().skip_while(|c| {
                if c.is_whitespace() {
                    if field_count >= f - 1 {
                        return false;
                    }
                    field_count += 1;
                }
                true
            });
            processed_line = chars.collect::<String>();
        }
    }

    if let Some(c) = chars {
        if c < processed_line.len() {
            processed_line = processed_line[c..].to_string();
        } else {
            processed_line.clear();
        }
    }

    if processed_line.is_empty() {
        line.to_string()
    } else {
        processed_line
    }
}

/// Writes the result to the output according to the specified arguments.
///
/// # Arguments
///
/// * `output` - The output writer.
/// * `line` - The line to be written.
/// * `count` - The count of the line occurrences.
/// * `args` - A reference to the `Args` struct containing the command line arguments.
///
/// # Errors
///
/// Returns an error if there is an issue writing to the output.
fn output_result<W: Write>(
    output: &mut W,
    line: &str,
    count: usize,
    args: &Args,
) -> Result<(), io::Error> {
    if args.count {
        writeln!(output, "{} {}", count, line)?;
    } else if args.repeated && count > 1 {
        writeln!(output, "{}", line)?;
    } else if args.unique && count == 1 {
        writeln!(output, "{}", line)?;
    } else if !args.repeated && !args.unique {
        writeln!(output, "{}", line)?;
    }
    Ok(())
}

/// The main function that initializes the application, parses the arguments, and runs the uniq function.
///
/// # Errors
///
/// Returns an error if there is an issue with the arguments or the uniq function.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    args.validate_args()?;

    let mut exit_code = 0;

    if let Err(err) = uniq(&args) {
        exit_code = 1;
        eprintln!("{}", err);
    }

    std::process::exit(exit_code)
}
