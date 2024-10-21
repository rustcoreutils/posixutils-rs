use context::initialize_context_regex_cache;
use ed::initialize_ed_regex_cache;
use envvars::initialize_env_vars_cache;
use normal::initialize_normal_regex_cache;
use unified::initialize_unified_regex_cache;

pub mod context;
pub mod ed;
pub mod envvars;
pub mod normal;
pub mod unified;

pub fn init() {
    initialize_normal_regex_cache();
    initialize_ed_regex_cache();
    initialize_context_regex_cache();
    initialize_unified_regex_cache();
    initialize_env_vars_cache();
}

pub const NO_NEW_LINE: &str = "\\ No newline at end of file";
pub const NO_NEW_LINE_ED: &str = "diff: f2.txt: No newline at end of file";
pub const UNIFIED_CONTEXT_HEADER_REGEX: &str = r"^(?:\*{3}|\-{3}|\+{3})\s+(?P<path>[^ ]+)\s+(?P<date>((?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)\s+(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s+\d{2}\s+\d{2}:\d{2}:\d{2}\s+\d{4})|(?:\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2}:\d{2}(\.\d+)?\s+[+-]\d{4}))\s*$";
