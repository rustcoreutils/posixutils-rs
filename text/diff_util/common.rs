pub struct FormatOptions {
    pub ignore_trailing_white_spaces: bool,
    pub output_format: OutputFormat,
    label1: Option<String>,
    label2: Option<String>,
    pub report_identical_files: bool,
}

impl FormatOptions {
    pub fn try_new(
        ignore_trailing_white_spaces: bool,
        output_format: OutputFormat,
        label1: Option<String>,
        label2: Option<String>,
        report_identical_files: bool,
    ) -> Result<Self, &'static str> {
        if label1.is_none() && label2.is_some() {
            return Err("label1 can not be NONE when label2 is available");
        }

        Ok(Self {
            ignore_trailing_white_spaces,
            output_format,
            label1,
            label2,
            report_identical_files,
        })
    }

    pub fn label1(&self) -> &Option<String> {
        &self.label1
    }

    pub fn label2(&self) -> &Option<String> {
        &self.label2
    }
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
