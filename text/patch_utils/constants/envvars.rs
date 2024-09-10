use std::{collections::HashMap, env, sync::Once};


// Provide a default value for the internationalization variables that are unset or null. (See XBD Internationalization Variables the precedence of internationalization variables used to determine the values of locale categories.)
pub const LANG : &str = "LANG";
// If set to a non-empty string value, override the values of all the other internationalization variables.
pub const LC_ALL : &str = "LC_ALL";
// Determine the locale for the behavior of ranges, equivalence classes, and multi-character collating elements used in the extended regular expression defined for the yesexpr locale keyword in the LC_MESSAGES category.
pub const LC_COLLATE : &str = "LC_COLLATE";
// Determine the locale for the interpretation of sequences of bytes of text data as characters (for example, single-byte as opposed to multi-byte characters in arguments and input files), and the behavior of character classes used in the extended regular expression defined for the yesexpr locale keyword in the LC_MESSAGES category.
pub const LC_CTYPE : &str = "LC_CTYPE";
// Determine the locale used to process affirmative responses, and the locale used to affect the format and contents of diagnostic messages and prompts written to standard error.
pub const LC_MESSAGES : &str = "LC_MESSAGES";
// [XSI] [Option Start] Determine the location of message catalogs for the processing of LC_MESSAGES. [Option End]
pub const NLSPATH : &str = "NLSPATH";
// Determine the locale for recognizing the format of file timestamps written by the diff utility in a context-difference input file.
pub const LC_TIME : &str = "LC_TIME";

const INITIALIZE_ENV_VARS_CACHE_ONCE: Once = Once::new();
static mut ENV_VARS_CACHE: Option<HashMap<&str, Option<String>>> = None;

const ALL_ENV_VARS : [&str; 7] = [
    LANG,
    LC_ALL,
    LC_COLLATE,
    LC_CTYPE,
    LC_MESSAGES,
    NLSPATH,
    LC_TIME,
];


pub fn initialize_env_vars_cache() {
    INITIALIZE_ENV_VARS_CACHE_ONCE.call_once(|| unsafe {
        let mut env_vars_cache = HashMap::new();

        for env_var_name in ALL_ENV_VARS {
            env_vars_cache.insert(
                env_var_name,
                env::var(env_var_name).ok(),
            );
        }
       
        ENV_VARS_CACHE = Some(env_vars_cache);
    });
}