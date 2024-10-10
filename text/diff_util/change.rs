#[derive(Clone, Copy, Debug, Default, Hash)]
pub struct ChangeData {
    ln1: usize, // line number in file1
    ln2: usize, // line number in file2
}

impl ChangeData {
    pub fn new(ln1: usize, ln2: usize) -> Self {
        Self { ln1, ln2 }
    }
}

#[derive(Clone, Copy, Debug, Default, Hash)]
pub enum Change {
    #[default]
    None,
    Unchanged(ChangeData),
    Insert(ChangeData),
    Delete(ChangeData),
    Substitute(ChangeData),
}

impl Change {
    pub fn get_ln1(&self) -> usize {
        match self {
            Change::None => panic!("Change::None is not allowed in hunk."),
            Change::Unchanged(data) => data.ln1,
            Change::Insert(data) => data.ln1,
            Change::Delete(data) => data.ln1,
            Change::Substitute(data) => data.ln1,
        }
    }

    pub fn get_ln2(&self) -> usize {
        match self {
            Change::None => panic!("Change::None is not allowed in hunk."),
            Change::Unchanged(data) => data.ln2,
            Change::Insert(data) => data.ln2,
            Change::Delete(data) => data.ln2,
            Change::Substitute(data) => data.ln2,
        }
    }
}
