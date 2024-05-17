#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Error evaluating input")]
    Evaluation,
    #[error("Error parsing input: {0}")]
    Parsing(String),
    #[error("Error processing io: {0}")]
    Io(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

pub trait GetExitCode {
    fn get_exit_code(&self) -> u8;
}

impl<T> GetExitCode for Result<T> {
    fn get_exit_code(&self) -> u8 {
        match self {
            Ok(_) => 0,
            Err(_) => 1,
        }
    }
}
