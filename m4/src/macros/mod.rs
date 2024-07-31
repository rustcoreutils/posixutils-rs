mod builtin;
mod eval;
pub mod trace;
mod user_defined;

use std::io::Write;

use builtin::*;
use eval::EvalMacro;
use trace::{TraceoffMacro, TraceonMacro};
use user_defined::UserDefinedMacro;

use crate::{
    lexer::{MacroName, MacroParseConfig},
    state::{StackFrame, State},
    Result,
};

macro_rules! macro_enums {
    (
        $(#[$meta:meta])*
        pub enum BuiltinMacroDefinition {
            $($variant_name:ident($variant_type:ident)),* $(,)?
        }
    ) => {
        $(#[$meta])*
        pub enum BuiltinMacro {
            $($variant_name),*
        }

        pub enum MacroDefinitionImplementation {
            $($variant_name($variant_type)),*,
            UserDefined(UserDefinedMacro),
        }

        impl MacroImplementation for MacroDefinitionImplementation {
            fn evaluate(
                &self,
                state: State,
                stderr: &mut dyn Write,
                f: StackFrame,
            ) -> Result<State> {
                state.trace.trace(&state.output.stack, &f, stderr)?;
                match self {
                    $(Self::$variant_name(d) => d.evaluate(state, stderr, f)),*,
                    Self::UserDefined(d) => d.evaluate(state, stderr, f),
                }
            }
        }

        impl BuiltinMacro {
            pub fn enumerate() -> &'static [Self] {
                &[$(Self::$variant_name),*]
            }

            pub fn implementation(&self) -> MacroDefinitionImplementation {
                match self {
                    $(Self::$variant_name => MacroDefinitionImplementation::$variant_name($variant_type)),*
                }
            }
        }
    };
}

// Maketemp intentionally left out, it's obsolete apparently.
macro_enums!(
    #[derive(Clone, Copy)]
    pub enum BuiltinMacroDefinition {
        Changecom(ChangecomMacro),
        Changequote(ChangequoteMacro),
        Decr(DecrMacro),
        Define(DefineMacro),
        Defn(DefnMacro),
        Divert(DivertMacro),
        Divnum(DivnumMacro),
        Dnl(DnlMacro),
        Dumpdef(DumpdefMacro),
        Errprint(ErrprintMacro),
        Eval(EvalMacro),
        File(FileMacro),
        Ifdef(IfdefMacro),
        Ifelse(IfelseMacro),
        Include(IncludeMacro),
        Incr(IncrMacro),
        Index(IndexMacro),
        Len(LenMacro),
        M4exit(M4exitMacro),
        M4wrap(M4wrapMacro),
        Maketemp(MkstempMacro),
        Mkstemp(MkstempMacro),
        Popdef(PopdefMacro),
        Pushdef(PushdefMacro),
        Shift(ShiftMacro),
        Sinclude(SincludeMacro),
        Substr(SubstrMacro),
        Syscmd(SyscmdMacro),
        Sysval(SysvalMacro),
        Traceoff(TraceoffMacro),
        Traceon(TraceonMacro),
        Translit(TranslitMacro),
        Undefine(UndefineMacro),
        Undivert(UndivertMacro),
    }
);

impl AsRef<[u8]> for BuiltinMacro {
    fn as_ref(&self) -> &'static [u8] {
        use BuiltinMacro::*;
        match self {
            Changecom => b"changecom",
            Changequote => b"changequote",
            Decr => b"decr",
            Define => b"define",
            Defn => b"defn",
            Divert => b"divert",
            Divnum => b"divnum",
            Dnl => b"dnl",
            Dumpdef => b"dumpdef",
            Errprint => b"errprint",
            Eval => b"eval",
            File => b"__file__",
            Ifdef => b"ifdef",
            Ifelse => b"ifelse",
            Include => b"include",
            Incr => b"incr",
            Index => b"index",
            Len => b"len",
            M4exit => b"m4exit",
            M4wrap => b"m4wrap",
            Maketemp => b"maketemp",
            Mkstemp => b"mkstemp",
            Popdef => b"popdef",
            Pushdef => b"pushdef",
            Shift => b"shift",
            Sinclude => b"sinclude",
            Substr => b"substr",
            Syscmd => b"syscmd",
            Sysval => b"sysval",
            Traceoff => b"traceoff",
            Traceon => b"traceon",
            Translit => b"translit",
            Undefine => b"undefine",
            Undivert => b"undivert",
        }
    }
}

impl BuiltinMacro {
    pub fn name(&self) -> MacroName {
        MacroName::try_from_slice(self.as_ref()).expect("Expected valid builtin macro name")
    }

    /// The minimum number of args that this macro requires in order for it to be parsed as a
    /// macro.
    pub fn min_args(&self) -> usize {
        use BuiltinMacro::*;
        match self {
            Changecom => 0,
            Changequote => 0,
            Decr => 1,
            Define => 1,
            Defn => 1,
            Divert => 0,
            Divnum => 0,
            Dnl => 0,
            Dumpdef => 1,
            Errprint => 1,
            Eval => 1,
            File => 0,
            Ifdef => 1,
            Ifelse => 1,
            Include => 1,
            Incr => 1,
            Index => 1,
            Len => 1,
            M4exit => 0,
            M4wrap => 1,
            Maketemp => 1,
            Mkstemp => 1,
            Popdef => 1,
            Pushdef => 1,
            Shift => 1,
            Sinclude => 1,
            Substr => 1,
            Syscmd => 1,
            Sysval => 0,
            Traceoff => 0,
            Traceon => 0,
            Translit => 1,
            Undefine => 1,
            Undivert => 0,
        }
    }

    pub fn parse_config(&self) -> MacroParseConfig {
        MacroParseConfig {
            name: self.name(),
            min_args: self.min_args(),
        }
    }
}

pub(crate) struct MacroDefinition {
    pub parse_config: MacroParseConfig,
    pub implementation: MacroDefinitionImplementation,
}

impl MacroDefinition {
    pub fn new_user_defined(name: MacroName, definition: Vec<u8>) -> Self {
        Self {
            parse_config: MacroParseConfig { name, min_args: 0 },
            implementation: MacroDefinitionImplementation::UserDefined(UserDefinedMacro {
                definition,
            }),
        }
    }
}

impl std::fmt::Debug for MacroDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MacroDefinition")
            .field("parse_config", &self.parse_config)
            .finish()
    }
}

pub trait MacroImplementation {
    fn evaluate(&self, state: State, stderr: &mut dyn Write, frame: StackFrame) -> Result<State>;
}
