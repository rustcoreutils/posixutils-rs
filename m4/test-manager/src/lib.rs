use std::io::{Read, Write};

pub struct TestSnapshot {
    pub stdout: String,
    pub stderr: String,
    pub status: i32,
    /// The test should be ignored.
    pub ignore: bool,
    /// An error is expected to occur, the stderr does not need to match exactly because error
    /// messages may differe slightly.
    pub expect_error: bool,
    pub stdout_regex: Option<String>,
}

fn escape_newlines(input: &str) -> String {
    input.replace("\n", "\\n")
}

fn unescape_newlines(input: &str) -> String {
    input.replace("\\n", "\n")
}

impl TestSnapshot {
    pub fn serialize(&self, out: &mut impl Write) {
        write!(out, "stdout=").unwrap();
        out.write_all(escape_newlines(&self.stdout).as_bytes())
            .unwrap();
        write!(out, "\n").unwrap();
        write!(out, "stderr=").unwrap();
        out.write_all(escape_newlines(&self.stderr).as_bytes())
            .unwrap();
        write!(out, "\n").unwrap();
        write!(out, "status={}", self.status).unwrap();
        if self.ignore {
            write!(out, "\n").unwrap();
            write!(out, "ignore=true").unwrap();
        }
        if self.expect_error {
            write!(out, "\n").unwrap();
            write!(out, "expect_error=true").unwrap();
        }
        if let Some(stdout_regex) = &self.stdout_regex {
            write!(out, "\n").unwrap();
            write!(out, "stdout_regex=").unwrap();
            out.write_all(escape_newlines(stdout_regex).as_bytes())
            .unwrap();
        }
    }

    pub fn deserialize(input: &mut impl Read) -> Self {
        let mut stdout: Option<String> = None;
        let mut stderr: Option<String> = None;
        let mut status: Option<i32> = None;
        let mut ignore = false;
        let mut expect_error = false;
        let mut stdout_regex: Option<String> = None;

        let mut buffer: String = String::new();
        input.read_to_string(&mut buffer).unwrap();

        for line in buffer.lines() {
            if line.is_empty() || line.starts_with("#") {
                continue;
            }
            let (name, value) = line.split_once("=").unwrap();
            match name {
                "stdout" => stdout = Some(unescape_newlines(value)),
                "stderr" => stderr = Some(unescape_newlines(value)),
                "status" => status = Some(value.parse().unwrap()),
                "ignore" => ignore = value.parse().unwrap(),
                "expect_error" => expect_error = value.parse().unwrap(),
                "stdout_regex" => stdout_regex = Some(unescape_newlines(value)),
                _ => panic!("Unsupported key {name:?}"),
            }
        }

        Self {
            stdout: stdout.unwrap(),
            stderr: stderr.unwrap(),
            status: status.unwrap(),
            ignore,
            expect_error,
            stdout_regex,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::TestSnapshot;

    #[test]
    fn test_deserialize() {
        let input = r#"stdout=1 2 \n
stderr=i 1 2 i  |i 1 2 i  |
status=0
ignore=true
"#;
        let snapshot = TestSnapshot::deserialize(&mut input.as_bytes());
        assert_eq!(snapshot.stdout, "1 2 \n");
        assert_eq!(snapshot.stderr, "i 1 2 i  |i 1 2 i  |");
        assert_eq!(snapshot.status, 0);
        assert_eq!(snapshot.ignore, true);
    }
}
