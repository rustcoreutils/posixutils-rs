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
