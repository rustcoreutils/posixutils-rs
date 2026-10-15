//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod builtin;
mod eval;
mod gnu;
pub mod trace;
mod user_defined;

use std::io::Write;

use builtin::*;
use eval::EvalMacro;
use gnu::*;
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
                mut state: State,
                stderr: &mut dyn Write,
                f: StackFrame,
            ) -> Result<State> {
                let pending = if state.is_traced(&f) {
                    state.trace_call(stderr, &f)?
                } else {
                    None
                };
                let mut state = self.call(state, stderr, f)?;
                if let Some(pending) = pending {
                    state.trace_expansion(stderr, pending)?;
                }
                Ok(state)
            }
        }

        impl MacroDefinitionImplementation {
            /// Run the macro on the arguments in `f`, untraced: `indir` and
            /// `builtin` call macros this way, and GNU m4 traces only the
            /// macro named in the input.
            pub fn call(
                &self,
                mut state: State,
                stderr: &mut dyn Write,
                f: StackFrame,
            ) -> Result<State> {
                let outer_expansion_site = state.input.set_expansion_site(Some(f.location.clone()));
                let builtin = match self {
                    Self::UserDefined(d) => {
                        let state = d.evaluate(state, stderr, f)?;
                        state.input.set_expansion_site(outer_expansion_site);
                        return Ok(state);
                    }
                    _ => self.builtin().expect("not user-defined"),
                };
                let outer_call_site = state.call_site.replace(f.location.clone());
                let mut state = if check_arity(&mut state, stderr, builtin, &f)? {
                    match self {
                        $(Self::$variant_name(d) => d.evaluate(state, stderr, f)),*,
                        Self::UserDefined(_) => unreachable!("handled above"),
                    }?
                } else {
                    state
                };
                state.call_site = outer_call_site;
                state.input.set_expansion_site(outer_expansion_site);
                Ok(state)
            }
        }

        impl MacroDefinitionImplementation {
            /// The built-in this implements, or `None` for a user-defined
            /// macro. A built-in keeps this identity under any name it is
            /// copied to, which is what `defn` and `dumpdef` report.
            pub fn builtin(&self) -> Option<BuiltinMacro> {
                match self {
                    $(Self::$variant_name(_) => Some(BuiltinMacro::$variant_name)),*,
                    Self::UserDefined(_) => None,
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
        Builtin(BuiltinMacroCall),
        Changecom(ChangecomMacro),
        Changequote(ChangequoteMacro),
        Decr(DecrMacro),
        Define(DefineMacro),
        Defn(DefnMacro),
        Divert(DivertMacro),
        Divnum(DivnumMacro),
        Dnl(DnlMacro),
        Dumpdef(DumpdefMacro),
        Debugfile(DebugfileMacro),
        Debugmode(DebugmodeMacro),
        Errprint(ErrprintMacro),
        Eval(EvalMacro),
        File(FileMacro),
        Esyscmd(EsyscmdMacro),
        Ifdef(IfdefMacro),
        Ifelse(IfelseMacro),
        Include(IncludeMacro),
        Incr(IncrMacro),
        Format(FormatMacro),
        Index(IndexMacro),
        Len(LenMacro),
        Line(LineMacro),
        M4exit(M4exitMacro),
        M4wrap(M4wrapMacro),
        Maketemp(MkstempMacro),
        Mkstemp(MkstempMacro),
        Popdef(PopdefMacro),
        Indir(IndirMacro),
        Patsubst(PatsubstMacro),
        Pushdef(PushdefMacro),
        Regexp(RegexpMacro),
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
            Builtin => b"builtin",
            Changecom => b"changecom",
            Debugfile => b"debugfile",
            Debugmode => b"debugmode",
            Esyscmd => b"esyscmd",
            Format => b"format",
            Indir => b"indir",
            Patsubst => b"patsubst",
            Regexp => b"regexp",
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
            Line => b"__line__",
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
    /// The built-in called `name` before any renaming.
    pub fn from_name(name: &[u8]) -> Option<Self> {
        Self::enumerate()
            .iter()
            .copied()
            .find(|b| b.as_ref() == name)
    }

    pub fn name(&self) -> MacroName {
        MacroName::try_from_slice(self.as_ref()).expect("Expected valid builtin macro name")
    }

    /// The minimum number of args that this macro requires in order for it to be parsed as a
    /// macro.
    pub fn min_args(&self) -> usize {
        use BuiltinMacro::*;
        match self {
            Builtin => 1,
            Changecom => 0,
            Debugfile => 0,
            Debugmode => 0,
            Esyscmd => 1,
            Format => 1,
            Indir => 1,
            Patsubst => 1,
            Regexp => 1,
            Changequote => 0,
            Decr => 1,
            Define => 1,
            Defn => 1,
            Divert => 0,
            Divnum => 0,
            Dnl => 0,
            Dumpdef => 0,
            Errprint => 1,
            Eval => 1,
            File => 0,
            Ifdef => 1,
            Ifelse => 1,
            Include => 1,
            Incr => 1,
            Index => 1,
            Len => 1,
            Line => 0,
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

    /// How many arguments the built-in takes, least and most, as GNU m4
    /// counts them. Fewer is diagnosed and the call expands to nothing;
    /// more is diagnosed and the rest ignored.
    fn arity(&self) -> (usize, Option<usize>) {
        use BuiltinMacro::*;
        match self {
            Divnum | Dnl | File | Line | Sysval => (0, Some(0)),
            Changecom | Changequote => (0, Some(2)),
            Debugfile | Debugmode => (0, Some(1)),
            Esyscmd => (1, Some(1)),
            Builtin | Format | Indir => (1, None),
            Patsubst | Regexp => (2, Some(3)),
            Divert | M4exit => (0, Some(1)),
            Dumpdef | Traceoff | Traceon | Undivert => (0, None),
            // Handles its own short argument lists.
            Ifelse => (0, None),
            Decr | Incr | Include | Len | Maketemp | Mkstemp | Sinclude | Syscmd => (1, Some(1)),
            Define | Pushdef => (1, Some(2)),
            Defn | Errprint | M4wrap | Popdef | Shift | Undefine => (1, None),
            Eval => (1, Some(3)),
            Ifdef => (2, Some(3)),
            Index => (2, Some(2)),
            Substr | Translit => (2, Some(3)),
        }
    }

    /// Whether a built-in (`defn` of one) passes through this macro's
    /// arguments: only those that define or call macros take one. Every
    /// other macro, user-defined ones included, sees it as empty text, as in
    /// GNU m4.
    pub fn takes_builtin_args(&self) -> bool {
        use BuiltinMacro::*;
        matches!(self, Builtin | Define | Indir | Pushdef)
    }

    /// Whether a call with too few arguments still runs, after the
    /// diagnostic: GNU m4 makes `index(abc)` 0, and `substr(abc)` and
    /// `translit(abc)` abc.
    fn runs_when_short(&self) -> bool {
        matches!(
            self,
            BuiltinMacro::Index
                | BuiltinMacro::Patsubst
                | BuiltinMacro::Regexp
                | BuiltinMacro::Substr
                | BuiltinMacro::Translit
        )
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
    /// `builtin`, under the name `name`.
    pub fn new_builtin(name: MacroName, builtin: BuiltinMacro) -> Self {
        Self {
            parse_config: MacroParseConfig {
                name,
                min_args: builtin.min_args(),
            },
            implementation: builtin.implementation(),
        }
    }

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

/// Diagnose a call to `builtin` with arguments outside its
/// [`BuiltinMacro::arity`], in GNU m4's words, naming it as it was called.
/// Returns whether the built-in should run.
fn check_arity(
    state: &mut State,
    stderr: &mut dyn Write,
    builtin: BuiltinMacro,
    frame: &StackFrame,
) -> Result<bool> {
    let (min, max) = builtin.arity();
    let n_args = frame.args.len();
    let name = &frame.definition.parse_config.name;
    if n_args < min {
        state.emit_warning(
            stderr,
            format_args!("too few arguments to builtin `{name}'"),
        )?;
        return Ok(builtin.runs_when_short());
    }
    if max.is_some_and(|max| n_args > max) {
        state.emit_warning(
            stderr,
            format_args!("excess arguments to builtin `{name}' ignored"),
        )?;
    }
    Ok(true)
}

pub trait MacroImplementation {
    fn evaluate(&self, state: State, stderr: &mut dyn Write, frame: StackFrame) -> Result<State>;
}
