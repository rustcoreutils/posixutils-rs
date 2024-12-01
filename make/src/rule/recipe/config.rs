use std::collections::HashSet;

use super::Prefix;

/// A recipe configuration.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Config {
    /// Whether the errors should be ignored.
    pub ignore: bool,
    /// Whether the recipe should be silent.
    pub silent: bool,
    /// Whether the recipe should be forced to run even with -n, -t options
    pub force_run: bool,
}

#[allow(clippy::derivable_impls)]
impl Default for Config {
    fn default() -> Self {
        Config {
            ignore: false,
            silent: false,
            force_run: false,
        }
    }
}

impl From<HashSet<Prefix>> for Config {
    fn from(prefixes: HashSet<Prefix>) -> Self {
        let mut ignore = false;
        let mut silent = false;
        let mut force_run = false;

        for prefix in prefixes {
            match prefix {
                Prefix::Ignore => ignore = true,
                Prefix::Silent => silent = true,
                Prefix::ForceRun => force_run = true,
            }
        }

        Self {
            ignore,
            silent,
            force_run,
        }
    }
}
