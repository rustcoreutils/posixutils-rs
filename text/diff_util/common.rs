pub struct FormatOptions {
    pub ignore_trailing_white_spaces: bool,
    pub output_format: OutputFormat,
    pub label1: Option<String>,
    pub label2: Option<String>,
}

#[allow(dead_code)]
pub enum OutputFormat {
    Debug,
    Default,
    Context(usize),
    EditScript,
    ForwardEditScript,
    Unified(usize),
}
