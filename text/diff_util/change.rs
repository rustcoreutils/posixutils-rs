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

pub struct ChangeContext {
    pub change: Change,
    pub ln1_start: usize,
    pub ln1_end: usize,
    pub hk1_start: usize,
    pub hk1_end: usize,
    pub ln2_start: usize,
    pub ln2_end: usize,
    pub hk2_start: usize,
    pub hk2_end: usize,
}

impl Change {
    pub fn is_none(&self) -> bool {
        *self == Change::None
    }

    pub fn is_unchanged(&self) -> bool {
        *self == Change::Unchanged(Default::default())
    }

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

impl PartialEq for Change {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::None, Self::None) => true,
            (Self::Unchanged(_), Self::Unchanged(_)) => true,
            (Self::Insert(_), Self::Insert(_)) => true,
            (Self::Delete(_), Self::Delete(_)) => true,
            (Self::Substitute(_), Self::Substitute(_)) => true,
            _ => false,
        }
    }
}
