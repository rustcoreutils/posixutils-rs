use crate::shell::environment::Environment;
use std::collections::VecDeque;
use std::io::Read;
use std::path::Path;

#[derive(Debug, Eq, PartialEq)]
pub enum EndPoint<'s> {
    // one based
    CommandNumber(u32),
    // zero based
    Last(u32),
    String(&'s str),
}

impl<'s> EndPoint<'s> {
    pub fn parse(s: &'s str) -> Self {
        if let Ok(n) = s.parse::<i64>() {
            if n < 0 {
                EndPoint::Last(-n as u32)
            } else {
                EndPoint::CommandNumber(n as u32)
            }
        } else {
            EndPoint::String(s)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Entry {
    command: String,
    command_number: u32,
}

fn list_entries<'a>(entries: impl Iterator<Item = &'a Entry>, include_numbers: bool) -> String {
    use std::fmt::Write;

    let mut result = String::new();
    for entry in entries {
        if include_numbers {
            writeln!(result, "{}\t{}", entry.command_number, entry.command)
                .expect("failed writing to string");
        } else {
            writeln!(result, "\t{}", entry.command).expect("failed writing to string");
        }
    }
    result
}

#[derive(Clone)]
pub struct History {
    entries: VecDeque<Entry>,
    max_size: u32,
    last_command_number: u32,
}

impl History {
    pub fn new(max_size: u32) -> Self {
        Self {
            entries: VecDeque::new(),
            max_size,
            last_command_number: 1,
        }
    }

    pub fn add_entry(&mut self, command: String) {
        if self.entries.len() as u32 >= self.max_size {
            self.entries.pop_front();
        }
        let command_number = self.last_command_number;
        self.last_command_number = (command_number % self.max_size) + 1;
        self.entries.push_back(Entry {
            command,
            command_number,
        });
    }

    pub fn remove_last_entry(&mut self) {
        if self.entries.pop_back().is_some() {
            self.last_command_number = self
                .last_command_number
                .checked_sub(1)
                .unwrap_or(self.max_size);
        }
    }

    fn endpoint_to_index(&self, end_point: EndPoint) -> Result<usize, String> {
        match end_point {
            EndPoint::CommandNumber(n) => {
                if let Some(first) = self.entries.front() {
                    if n <= first.command_number {
                        // we wrapped around, look for the command from the back
                        // TODO: I think this can be done in constant time
                        Ok(self
                            .entries
                            .iter()
                            .rposition(|e| e.command_number == n)
                            .unwrap_or(self.entries.len() - 1))
                    } else {
                        Ok((n - first.command_number) as usize)
                    }
                } else {
                    Ok(0)
                }
            }
            EndPoint::Last(n) => Ok(self.entries.len().saturating_sub(n as usize + 1)),
            EndPoint::String(s) => self
                .entries
                .iter()
                .position(|entry| entry.command.starts_with(s))
                .ok_or(format!("{s} not found")),
        }
    }

    /// range includes the end point
    pub fn list(
        &self,
        start: EndPoint,
        end: EndPoint,
        mut reverse: bool,
        include_command_number: bool,
    ) -> Result<String, String> {
        let mut start_index = self.endpoint_to_index(start)?;
        let mut end_index = self.endpoint_to_index(end)?;
        if start_index > end_index {
            std::mem::swap(&mut start_index, &mut end_index);
            reverse = true;
        }
        if reverse {
            let entries = self
                .entries
                .iter()
                .skip(start_index)
                .take(end_index - start_index + 1)
                .rev();
            Ok(list_entries(entries, include_command_number))
        } else {
            let entries = self
                .entries
                .iter()
                .skip(start_index)
                .take(end_index - start_index + 1);
            Ok(list_entries(entries, include_command_number))
        }
    }

    pub fn get_reverse(&self, index: usize) -> Option<&str> {
        if index >= self.entries.len() {
            return None;
        }
        self.entries
            .get(self.entries.len() - 1 - index)
            .map(|e| e.command.as_str())
    }

    pub fn entries_count(&self) -> usize {
        self.entries.len()
    }
}

fn read_history_from_file(path: &Path, max_entries: u32) -> History {
    match std::fs::File::options()
        .read(true)
        .write(true)
        .create(true)
        .open(path)
    {
        Ok(mut file) => {
            let mut history = History::new(max_entries);
            let mut file_contents = String::new();
            if let Err(err) = file.read_to_string(&mut file_contents) {
                eprintln!(
                    "sh: failed to read history file at {}, details: {err}",
                    path.to_string_lossy()
                );
                return history;
            }
            for line in file_contents.lines() {
                history.add_entry(line.to_string());
            }
            history
        }
        Err(err) => {
            eprintln!(
                "sh: failed to open history file at {}, details: {err}",
                path.to_string_lossy()
            );
            History::new(max_entries)
        }
    }
}

pub fn initialize_history_from_system(env: &Environment) -> History {
    let histsize = if let Some(histsize) = env.get_str_value("HISTSIZE") {
        histsize.parse().unwrap_or_else(|_| {
            eprintln!("sh: invalid HISTSIZE value, using default value 32767");
            32767
        })
    } else {
        32767
    };
    if let Some(histfile) = env.get_str_value("HISTFILE") {
        read_history_from_file(Path::new(histfile), histsize)
    } else if let Some(home) = env.get_str_value("HOME") {
        read_history_from_file(Path::new(&format!("{home}/.sh_history")), histsize)
    } else {
        History::new(histsize)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn history_with_entries(max_size: u32, entries: Vec<&str>) -> History {
        let mut history = History::new(max_size);
        for entry in entries {
            history.add_entry(entry.to_string());
        }
        history
    }

    #[test]
    fn list_previous() {
        let history = history_with_entries(10, vec!["a", "b"]);

        let list = history
            .list(EndPoint::Last(1), EndPoint::Last(1), false, false)
            .unwrap();
        assert_eq!(list, "\ta\n");
        let list = history
            .list(EndPoint::Last(1), EndPoint::Last(1), true, false)
            .unwrap();
        assert_eq!(list, "\ta\n");
        let list = history
            .list(EndPoint::Last(1), EndPoint::Last(1), false, true)
            .unwrap();
        assert_eq!(list, "1\ta\n");
    }

    #[test]
    fn list_previous_n() {
        let history = history_with_entries(10, vec!["a", "b", "c", "d", "e"]);

        let list = history
            .list(EndPoint::Last(4), EndPoint::Last(1), false, false)
            .unwrap();
        assert_eq!(list, "\ta\n\tb\n\tc\n\td\n");
        let list = history
            .list(EndPoint::Last(4), EndPoint::Last(1), true, false)
            .unwrap();
        assert_eq!(list, "\td\n\tc\n\tb\n\ta\n");
        let list = history
            .list(EndPoint::Last(4), EndPoint::Last(1), false, true)
            .unwrap();
        assert_eq!(list, "1\ta\n2\tb\n3\tc\n4\td\n");
    }

    #[test]
    fn list_range_with_one_element() {
        let history = history_with_entries(10, vec!["a", "b", "c", "d", "e"]);

        let list = history
            .list(
                EndPoint::CommandNumber(1),
                EndPoint::CommandNumber(1),
                false,
                false,
            )
            .unwrap();
        assert_eq!(list, "\ta\n");
        let list = history
            .list(
                EndPoint::CommandNumber(1),
                EndPoint::CommandNumber(1),
                true,
                false,
            )
            .unwrap();
        assert_eq!(list, "\ta\n");
        let list = history
            .list(
                EndPoint::CommandNumber(1),
                EndPoint::CommandNumber(1),
                false,
                true,
            )
            .unwrap();
        assert_eq!(list, "1\ta\n");
    }

    #[test]
    fn list_range() {
        let history = history_with_entries(10, vec!["a", "b", "c", "d", "e"]);

        let list = history
            .list(
                EndPoint::CommandNumber(2),
                EndPoint::CommandNumber(4),
                false,
                false,
            )
            .unwrap();
        assert_eq!(list, "\tb\n\tc\n\td\n");
        let list = history
            .list(
                EndPoint::CommandNumber(2),
                EndPoint::CommandNumber(4),
                true,
                false,
            )
            .unwrap();
        assert_eq!(list, "\td\n\tc\n\tb\n");
        let list = history
            .list(
                EndPoint::CommandNumber(2),
                EndPoint::CommandNumber(4),
                false,
                true,
            )
            .unwrap();
        assert_eq!(list, "2\tb\n3\tc\n4\td\n");
    }

    #[test]
    fn list_with_string_range() {
        let history = history_with_entries(10, vec!["a", "b", "c", "d", "e"]);

        let list = history
            .list(EndPoint::String("b"), EndPoint::String("d"), false, false)
            .unwrap();
        assert_eq!(list, "\tb\n\tc\n\td\n");
    }

    #[test]
    fn history_is_fifo() {
        let history = history_with_entries(2, vec!["a", "b", "c"]);
        let list = history
            .list(EndPoint::String("b"), EndPoint::String("c"), false, true)
            .unwrap();
        assert_eq!(list, "2\tb\n1\tc\n")
    }

    #[test]
    fn list_range_with_wrap_around() {
        let history = history_with_entries(3, vec!["a", "b", "c", "d"]);
        let list = history
            .list(
                EndPoint::CommandNumber(2),
                EndPoint::CommandNumber(1),
                false,
                true,
            )
            .unwrap();
        assert_eq!(list, "2\tb\n3\tc\n1\td\n")
    }
}
