use std::borrow::Cow;

pub struct Error {
    pub kind: ErrorKind,
    context: Vec<Cow<'static, str>>,
    source: Option<Box<dyn std::error::Error + Send + Sync + 'static>>,
}

impl Error {
    pub fn new(kind: ErrorKind) -> Self {
        Self {
            kind,
            context: Vec::new(),
            source: None,
        }
    }

    pub fn add_context<C>(mut self, context: C) -> Self
    where
        C: Into<Cow<'static, str>>,
    {
        self.context.push(context.into());
        self
    }

    pub fn with_source<S: std::error::Error + Send + Sync + 'static>(mut self, source: S) -> Self {
        self.source = Some(Box::new(source));
        self
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Error")
            .field("kind", &self.kind)
            .field(
                "context",
                &self
                    .context
                    .iter()
                    .map(|c| c.to_string())
                    .collect::<Vec<_>>(),
            )
            .field("source", &self.source)
            .finish()
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    Evaluation,
    Parsing,
    Io,
    InvalidDivertNumber(i64),
    NotEnoughArguments,
    UnclosedQuote,
    /// NOTE: this isn't always an "error", if the code is 0, it indicates an intentional,
    /// successful, early program exit, just hijacking the [`Result`] semantics to help enable this
    /// in a purely functional manner.
    Exit(i32),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in self.context.iter().rev() {
            write!(f, "{c}: ")?;
        }

        match self.kind {
            ErrorKind::Evaluation => write!(f, "Error evaluating input"),
            ErrorKind::Parsing => write!(f, "Error parsing input"),
            ErrorKind::Io => write!(f, "Error processing io"),
            ErrorKind::InvalidDivertNumber(i) => write!(f, "Invalid divert number: {i}"),
            ErrorKind::NotEnoughArguments => write!(f, "The macro doesn't have enough arguments"),
            ErrorKind::Exit(code) => write!(f, "Program requested an exit with code {code}"),
            ErrorKind::UnclosedQuote => write!(f, "Unclosed quote"),
        }?;

        if f.alternate() {
            if let Some(source) = &self.source {
                write!(f, "\n\nSource:\n  ")?;
                source.fmt(f)?
            }
        }

        Ok(())
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.source {
            Some(source) => Some(&**source),
            None => None,
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Self::new(ErrorKind::Io).with_source(value)
    }
}

impl From<nom::Err<nom::error::Error<&[u8]>>> for Error {
    fn from(error: nom::Err<nom::error::Error<&[u8]>>) -> Self {
        Self::new(ErrorKind::Parsing).add_context(match error {
            nom::Err::Incomplete(_) => format!("{error}"),
            nom::Err::Error(e) => format!(
                "Error code: {:?}, input: {:?}",
                e.code,
                String::from_utf8(e.input.to_vec()).unwrap_or_else(|_| format!("{:?}", e.input))
            ),
            nom::Err::Failure(e) => format!(
                "Failure code: {:?}, input: {:?}",
                e.code,
                String::from_utf8(e.input.to_vec()).unwrap_or_else(|_| format!("{:?}", e.input))
            ),
        })
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub trait GetExitCode {
    fn get_exit_code(&self) -> i32;
}

impl GetExitCode for Error {
    fn get_exit_code(&self) -> i32 {
        match self.kind {
            ErrorKind::Exit(code) => code,
            _ => 1,
        }
    }
}

impl<T> GetExitCode for Result<T> {
    fn get_exit_code(&self) -> i32 {
        match self {
            Ok(_) => 0,
            Err(error) => error.get_exit_code(),
        }
    }
}

pub trait ResultExt<T> {
    fn add_context<C: Into<Cow<'static, str>>, F: Fn() -> C>(self, context: F) -> Self;
}

impl<T> ResultExt<T> for Result<T> {
    fn add_context<C: Into<Cow<'static, str>>, F: Fn() -> C>(self, context: F) -> Self {
        self.map_err(|error| error.add_context(context()))
    }
}
