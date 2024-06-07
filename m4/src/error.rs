#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Error evaluating input")]
    Evaluation,
    #[error("Error parsing input: {0}")]
    Parsing(String),
    #[error("Error processing io: {0}")]
    Io(#[from] std::io::Error),
    #[error("The macro doesn't have enough arguments")]
    NotEnoughArguments,
    /// NOTE: this isn't always an "error", if the code is 0, it indicates an intentional,
    /// successful, early program exit, just hijacking the [`Result`] semantics to help enable this
    /// in a purely functional manner.
    #[error("Program requested an exit with code {0}")]
    Exit(i32),
}

impl From<nom::Err<nom::error::Error<&[u8]>>> for Error {
    fn from(error: nom::Err<nom::error::Error<&[u8]>>) -> Self {
        Self::Parsing(match error {
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
        match self {
            Error::Exit(code) => *code,
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
