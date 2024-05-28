use std::io::{Read, Write};

pub struct TestSnapshot {
    pub stdout: String,
    pub stderr: String,
    pub status: i32,
    pub ignore: bool,
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
            write!(out, "ignore={}", self.ignore).unwrap();
        }
    }

    pub fn deserialize(input: &mut impl Read) -> Self {
        let mut stdout: Option<String> = None;
        let mut stderr: Option<String> = None;
        let mut status: Option<i32> = None;
        let mut ignore = false;

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
                _ => panic!("Unsupported key {name:?}"),
            }
        }

        Self {
            stdout: stdout.unwrap(),
            stderr: stderr.unwrap(),
            status: status.unwrap(),
            ignore,
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
