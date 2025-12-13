pub struct FormatOptions {
    pub ignore_trailing_white_spaces: bool,
    pub output_format: OutputFormat,
    label1: Option<String>,
    label2: Option<String>,
}

impl FormatOptions {
    pub fn try_new(
        ignore_trailing_white_spaces: bool,
        output_format: OutputFormat,
        label1: Option<String>,
        label2: Option<String>,
    ) -> Result<Self, &'static str> {
        if label1.is_none() && label2.is_some() {
            return Err("label1 can not be NONE when label2 is available");
        }

        Ok(Self {
            ignore_trailing_white_spaces,
            output_format,
            label1,
            label2,
        })
    }

    pub fn label1(&self) -> &Option<String> {
        &self.label1
    }

    pub fn label2(&self) -> &Option<String> {
        &self.label2
    }
}

pub enum OutputFormat {
    Default,
    Context(usize),
    EditScript,
    ForwardEditScript,
    Unified(usize),
}
