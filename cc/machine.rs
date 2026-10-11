//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! gcc's x86-64 `-m` options that shape code without naming an instruction
//! set (those are `target::X86Isa`'s): the code model, the stack's
//! alignment, the variadic call's `%al`, the Spectre and straight-line
//! speculation mitigations, and where the stack protector's guard lives.
//! What a kernel build passes, and what a kernel needs honoured.

use std::fmt;

/// `-mcmodel=`: where code and data may lie.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum CodeModel {
    /// The low 2 GB: an absolute address fits a zero-extended 32-bit
    /// immediate (`movl $sym, %eax`, `R_X86_64_32`).
    #[default]
    Small,
    /// The top 2 GB, where a kernel lives: an absolute address fits a
    /// sign-extended 32-bit immediate (`movq $sym, %rax`, `R_X86_64_32S`)
    /// and nothing else.
    Kernel,
}

/// How an indirect branch (`-mindirect-branch=`) or a return
/// (`-mfunction-return=`) is made.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum Thunk {
    /// As it is: `call *%rax`, `ret`.
    #[default]
    Keep,
    /// Through a retpoline the unit defines, once per register, in a comdat
    /// section of its own (`__x86_indirect_thunk_rax`,
    /// `__x86_return_thunk`).
    Thunk,
    /// Through a retpoline written out at the branch.
    Inline,
    /// Through the retpoline `__x86_indirect_thunk_<reg>` or
    /// `__x86_return_thunk`, which something else defines: the kernel.
    Extern,
}

/// `indirect_branch(...)` and `function_return(...)`: one function's own
/// [`Thunk`] choices, where it makes them.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct BranchAttrs {
    pub indirect: Option<Thunk>,
    pub ret: Option<Thunk>,
}

impl BranchAttrs {
    /// `other`'s choices over these, where it makes them.
    pub fn merge(&mut self, other: Self) {
        self.indirect = other.indirect.or(self.indirect);
        self.ret = other.ret.or(self.ret);
    }
}

impl Thunk {
    /// The choice `name` names, as the options and attributes spell it.
    pub fn from_name(name: &str) -> Option<Self> {
        Self::NAMES
            .iter()
            .find(|(n, _)| *n == name)
            .map(|(_, t)| *t)
    }

    const NAMES: [(&'static str, Thunk); 4] = [
        ("keep", Thunk::Keep),
        ("thunk", Thunk::Thunk),
        ("thunk-extern", Thunk::Extern),
        ("thunk-inline", Thunk::Inline),
    ];

    fn name(self) -> &'static str {
        Self::NAMES
            .iter()
            .find(|(_, t)| *t == self)
            .map_or("keep", |(n, _)| n)
    }
}

/// `-mharden-sls=`: the instructions followed by an `int3`, so that a
/// processor speculating straight past them stops.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Sls {
    /// After every `ret`.
    pub ret: bool,
    /// After every `jmp *%reg`.
    pub indirect_jmp: bool,
}

/// A segment register the stack protector's guard may be read through.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SegReg {
    Fs,
    Gs,
}

impl fmt::Display for SegReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            SegReg::Fs => "%fs",
            SegReg::Gs => "%gs",
        })
    }
}

/// Where the stack protector reads its guard.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StackGuard {
    /// At a fixed offset from a segment base: glibc's `%fs:40`, a kernel's
    /// per-CPU `%gs:40`.
    Tls { seg: SegReg, offset: i32 },
    /// At a symbol, through a segment register:
    /// `-mstack-protector-guard-symbol=`, which gcc 13 reads as
    /// `%gs:sym(%rip)`.
    TlsSymbol { seg: SegReg, symbol: String },
    /// In the global `__stack_chk_guard`.
    Global,
}

/// The guard's offset when no option gives one: glibc's
/// `tcbhead_t.stack_guard`, which kernels keep at the same place.
const DEFAULT_GUARD_OFFSET: i32 = 40;

/// The x86-64 machine options; see the module documentation.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct X86Options {
    pub code_model: CodeModel,
    /// `-mskip-rax-setup`: a variadic call does not set `%al` when SSE is
    /// off, there being no vector register for it to count.
    pub skip_rax_setup: bool,
    /// `-mpreferred-stack-boundary=3`: the stack is 8-byte aligned at a
    /// call, not 16, so a function can assume no more of its own.
    pub stack_boundary_8: bool,
    /// `-mno-fp-ret-in-387`: a `long double` is not returned in `%st(0)`.
    pub no_fp_ret_in_387: bool,
    /// `-mindirect-branch=`.
    pub indirect_branch: Thunk,
    /// `-mindirect-branch-cs-prefix`: a call or jump to an indirect thunk
    /// for `%r8`..`%r15` carries a `cs` prefix, making it six bytes, which
    /// a kernel can patch into `lfence; call *%r11` in place.
    pub indirect_branch_cs_prefix: bool,
    /// `-mfunction-return=`.
    pub function_return: Thunk,
    /// `-mharden-sls=`.
    pub sls: Sls,
    /// `-mstack-protector-guard=`.
    guard_global: Option<bool>,
    /// `-mstack-protector-guard-reg=`.
    guard_reg: Option<SegReg>,
    /// `-mstack-protector-guard-offset=`.
    guard_offset: Option<i32>,
    /// `-mstack-protector-guard-symbol=`.
    guard_symbol: Option<String>,
}

/// gcc's refusal of a value an enumerated option does not take: the error,
/// then the note listing the values it does.
fn bad_choice(stem: &str, value: &str, valid: &[&str]) -> Vec<String> {
    vec![
        format!("error: unrecognized argument in option '{stem}{value}'"),
        format!("note: valid arguments to '{stem}' are: {}", valid.join(" ")),
    ]
}

/// gcc's refusal of each `-mtls-dialect=` value the architecture has no
/// such dialect for, the other architecture's included: x86-64's gcc takes
/// `gnu` (`__tls_get_addr`, its default) and `gnu2` (TLS descriptors),
/// aarch64's `desc` (descriptors, its default) and `trad`. Whether c17
/// generates a dialect the architecture does have is the caller's question.
pub fn check_tls_dialect(flags: &[String], arch: crate::target::Arch) -> Result<(), Vec<String>> {
    const STEM: &str = "-mtls-dialect=";
    let valid: &[&str] = match arch {
        crate::target::Arch::X86_64 => &["gnu", "gnu2"],
        crate::target::Arch::Aarch64 => &["desc", "trad"],
    };
    for flag in flags {
        if let Some(value) = flag.strip_prefix(STEM) {
            if !valid.contains(&value) {
                return Err(bad_choice(STEM, value, valid));
            }
        }
    }
    Ok(())
}

/// A C integer constant as gcc's `strtol(value, 0, 0)` reads it, whole.
fn parse_c_integer(value: &str) -> Option<i64> {
    let (negative, digits) = match value.strip_prefix('-') {
        Some(rest) => (true, rest),
        None => (false, value.strip_prefix('+').unwrap_or(value)),
    };
    let magnitude = if let Some(hex) = digits
        .strip_prefix("0x")
        .or_else(|| digits.strip_prefix("0X"))
    {
        i64::from_str_radix(hex, 16).ok()?
    } else if digits.len() > 1 && digits.starts_with('0') {
        i64::from_str_radix(&digits[1..], 8).ok()?
    } else {
        digits.parse().ok()?
    };
    Some(if negative { -magnitude } else { magnitude })
}

impl X86Options {
    /// Whether `flag` is one of these options, whatever its value; a value
    /// the option does not take is refused by [`Self::from_flags`].
    pub fn owns(flag: &str) -> bool {
        const PLAIN: &[&str] = &[
            "-mred-zone",
            "-mno-red-zone",
            "-mskip-rax-setup",
            "-mno-skip-rax-setup",
            "-mfp-ret-in-387",
            "-mno-fp-ret-in-387",
            "-mindirect-branch-register",
            "-mno-indirect-branch-register",
            "-mindirect-branch-cs-prefix",
            "-mno-indirect-branch-cs-prefix",
        ];
        const STEMS: &[&str] = &[
            "-mcmodel=",
            "-mpreferred-stack-boundary=",
            "-mindirect-branch=",
            "-mfunction-return=",
            "-mharden-sls=",
            "-mstack-protector-guard=",
            "-mstack-protector-guard-reg=",
            "-mstack-protector-guard-offset=",
            "-mstack-protector-guard-symbol=",
        ];
        PLAIN.contains(&flag) || STEMS.iter().any(|s| flag.starts_with(s))
    }

    /// The options `flags` -- the `-m` options, in order, the last of each
    /// winning -- ask for, or gcc's refusal: its error lines, without the
    /// program name. `darwin` refuses what Mach-O has no form for, which
    /// gcc, having no Darwin x86-64 port in common use, never meets: the
    /// kernel code model and the thunks' comdat sections.
    pub fn from_flags(flags: &[String], darwin: bool) -> Result<Self, Vec<String>> {
        let mut o = Self::default();
        let unsupported = |flag: &str| vec![format!("unsupported machine flag: {flag}")];
        for flag in flags {
            let flag = flag.as_str();
            match flag {
                // c17 never stores below `%rsp`: every frame is allocated
                // before its slots are written, leaf function or not.
                "-mred-zone" | "-mno-red-zone" => {}
                "-mskip-rax-setup" => o.skip_rax_setup = true,
                "-mno-skip-rax-setup" => o.skip_rax_setup = false,
                "-mfp-ret-in-387" => o.no_fp_ret_in_387 = false,
                "-mno-fp-ret-in-387" => o.no_fp_ret_in_387 = true,
                // c17 calls through a register always: what these ask of
                // the code is already so.
                "-mindirect-branch-register" | "-mno-indirect-branch-register" => {}
                "-mindirect-branch-cs-prefix" => o.indirect_branch_cs_prefix = true,
                "-mno-indirect-branch-cs-prefix" => o.indirect_branch_cs_prefix = false,
                _ => {}
            }
            if let Some(model) = flag.strip_prefix("-mcmodel=") {
                o.code_model = match model {
                    "small" => CodeModel::Small,
                    "kernel" if !darwin => CodeModel::Kernel,
                    "32" => {
                        return Err(vec![
                            "error: code model '32' not supported in the 64 bit mode".to_string(),
                        ])
                    }
                    "kernel" | "medium" | "large" => return Err(unsupported(flag)),
                    _ => {
                        return Err(bad_choice(
                            "-mcmodel=",
                            model,
                            &["32", "kernel", "large", "medium", "small"],
                        ))
                    }
                };
            } else if let Some(n) = flag.strip_prefix("-mpreferred-stack-boundary=") {
                let Ok(n) = n.parse::<u32>() else {
                    return Err(vec![
                        "error: argument to '-mpreferred-stack-boundary=' should be a \
                         non-negative integer"
                            .to_string(),
                    ]);
                };
                o.stack_boundary_8 = match n {
                    3 => true,
                    4 => false,
                    5..=12 => return Err(unsupported(flag)),
                    _ => {
                        return Err(vec![format!(
                            "error: '-mpreferred-stack-boundary={n}' is not between 3 and 12"
                        )])
                    }
                };
            } else if let Some(value) = flag.strip_prefix("-mindirect-branch=") {
                o.indirect_branch = Self::thunk("-mindirect-branch=", value)?;
            } else if let Some(value) = flag.strip_prefix("-mfunction-return=") {
                o.function_return = Self::thunk("-mfunction-return=", value)?;
            } else if let Some(value) = flag.strip_prefix("-mharden-sls=") {
                o.sls = match value {
                    "none" => Sls::default(),
                    "all" => Sls {
                        ret: true,
                        indirect_jmp: true,
                    },
                    "return" => Sls {
                        ret: true,
                        indirect_jmp: false,
                    },
                    "indirect-jmp" => Sls {
                        ret: false,
                        indirect_jmp: true,
                    },
                    _ => {
                        return Err(bad_choice(
                            "-mharden-sls=",
                            value,
                            &["all", "indirect-jmp", "none", "return"],
                        ))
                    }
                };
            } else if let Some(value) = flag.strip_prefix("-mstack-protector-guard=") {
                o.guard_global = Some(match value {
                    "tls" => false,
                    "global" => true,
                    _ => {
                        return Err(bad_choice(
                            "-mstack-protector-guard=",
                            value,
                            &["global", "tls"],
                        ))
                    }
                });
            } else if let Some(value) = flag.strip_prefix("-mstack-protector-guard-reg=") {
                o.guard_reg = Some(match value {
                    "fs" => SegReg::Fs,
                    "gs" => SegReg::Gs,
                    _ => {
                        return Err(vec![format!(
                            "error: '{value}' is not a valid base register in \
                             '-mstack-protector-guard-reg='"
                        )])
                    }
                });
            } else if let Some(value) = flag.strip_prefix("-mstack-protector-guard-offset=") {
                let Some(n) = parse_c_integer(value) else {
                    return Err(vec![format!(
                        "error: '{value}' is not a valid number in \
                         '-mstack-protector-guard-offset='"
                    )]);
                };
                let Ok(n) = i32::try_from(n) else {
                    return Err(vec![format!(
                        "error: '{value}' is not a valid offset in \
                         '-mstack-protector-guard-offset='"
                    )]);
                };
                o.guard_offset = Some(n);
            } else if let Some(value) = flag.strip_prefix("-mstack-protector-guard-symbol=") {
                if value.is_empty() {
                    return Err(vec![
                        "error: missing argument to '-mstack-protector-guard-symbol='".to_string(),
                    ]);
                }
                o.guard_symbol = Some(value.to_string());
            }
        }
        if darwin {
            for (stem, thunk) in [
                ("-mindirect-branch=", o.indirect_branch),
                ("-mfunction-return=", o.function_return),
            ] {
                if thunk != Thunk::Keep {
                    return Err(unsupported(&format!("{stem}{}", thunk.name())));
                }
            }
        }
        Ok(o)
    }

    /// One of `-mindirect-branch=` and `-mfunction-return=`'s values.
    fn thunk(stem: &str, value: &str) -> Result<Thunk, Vec<String>> {
        Thunk::from_name(value).ok_or_else(|| {
            let names: Vec<&str> = Thunk::NAMES.iter().map(|(n, _)| *n).collect();
            bad_choice(stem, value, &names)
        })
    }

    /// The options' quarrels with the rest of the command line, as gcc
    /// states them: the kernel code model cannot be position-independent,
    /// and a retpoline the unit writes itself defeats a shadow stack
    /// (`-fcf-protection=return`), whose return addresses its `ret` does
    /// not follow -- an external one is the kernel's to make agree.
    pub fn conflicts(&self, pic: bool, cf_return: bool) -> Vec<String> {
        let mut errors = Vec::new();
        if self.code_model == CodeModel::Kernel && pic {
            errors.push("error: code model kernel does not support PIC mode".to_string());
        }
        for (name, thunk) in [
            ("-mindirect-branch", self.indirect_branch),
            ("-mfunction-return", self.function_return),
        ] {
            if cf_return && matches!(thunk, Thunk::Thunk | Thunk::Inline) {
                errors.push(format!(
                    "error: '{name}' and '-fcf-protection' are not compatible"
                ));
            }
        }
        errors
    }

    /// Where the stack protector reads its guard on a target whose own is
    /// `%fs:40` (`tls_default`) or `__stack_chk_guard`: as the options say,
    /// and by default `%gs` rather than `%fs` for the kernel code model, as
    /// gcc does.
    pub fn stack_guard(&self, tls_default: bool) -> StackGuard {
        let global = self.guard_global.unwrap_or(!tls_default);
        if global {
            return StackGuard::Global;
        }
        let seg = self.guard_reg.unwrap_or(match self.code_model {
            CodeModel::Kernel => SegReg::Gs,
            CodeModel::Small => SegReg::Fs,
        });
        match &self.guard_symbol {
            Some(symbol) => StackGuard::TlsSymbol {
                seg,
                symbol: symbol.clone(),
            },
            None => StackGuard::Tls {
                seg,
                offset: self.guard_offset.unwrap_or(DEFAULT_GUARD_OFFSET),
            },
        }
    }

    /// The alignment, in bytes, a function may count on `%rsp` having at
    /// its entry, before the call pushed the return address.
    pub fn incoming_stack_align(&self) -> i32 {
        if self.stack_boundary_8 {
            8
        } else {
            16
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(flags: &[&str]) -> Result<X86Options, Vec<String>> {
        let flags: Vec<String> = flags.iter().map(|f| f.to_string()).collect();
        X86Options::from_flags(&flags, false)
    }

    /// Each architecture takes its own two dialects and refuses the
    /// other's, listing its own.
    #[test]
    fn tls_dialect_per_architecture() {
        use crate::target::Arch;
        let check = |flag: &str, arch| check_tls_dialect(&[flag.to_string()], arch);
        for flag in ["-mtls-dialect=gnu", "-mtls-dialect=gnu2", "-mfpmath=sse"] {
            assert_eq!(check(flag, Arch::X86_64), Ok(()), "{flag}");
        }
        for flag in ["-mtls-dialect=desc", "-mtls-dialect=trad"] {
            assert_eq!(check(flag, Arch::Aarch64), Ok(()), "{flag}");
        }
        assert_eq!(
            check("-mtls-dialect=desc", Arch::X86_64),
            Err(vec![
                "error: unrecognized argument in option '-mtls-dialect=desc'".to_string(),
                "note: valid arguments to '-mtls-dialect=' are: gnu gnu2".to_string(),
            ])
        );
        assert_eq!(
            check("-mtls-dialect=gnu2", Arch::Aarch64),
            Err(vec![
                "error: unrecognized argument in option '-mtls-dialect=gnu2'".to_string(),
                "note: valid arguments to '-mtls-dialect=' are: desc trad".to_string(),
            ])
        );
    }

    #[test]
    fn last_value_wins() {
        let o = parse(&[
            "-mindirect-branch=thunk",
            "-mindirect-branch=thunk-extern",
            "-mharden-sls=all",
            "-mharden-sls=return",
        ])
        .unwrap();
        assert_eq!(o.indirect_branch, Thunk::Extern);
        assert_eq!(
            o.sls,
            Sls {
                ret: true,
                indirect_jmp: false
            }
        );
    }

    #[test]
    fn guard_follows_code_model_and_options() {
        let tls = |seg, offset| StackGuard::Tls { seg, offset };
        assert_eq!(parse(&[]).unwrap().stack_guard(true), tls(SegReg::Fs, 40));
        assert_eq!(parse(&[]).unwrap().stack_guard(false), StackGuard::Global);
        let kernel = parse(&["-mcmodel=kernel"]).unwrap();
        assert_eq!(kernel.stack_guard(true), tls(SegReg::Gs, 40));
        let o = parse(&[
            "-mcmodel=kernel",
            "-mstack-protector-guard-reg=fs",
            "-mstack-protector-guard-offset=0x28",
        ])
        .unwrap();
        assert_eq!(o.stack_guard(true), tls(SegReg::Fs, 40));
        let o = parse(&["-mstack-protector-guard-offset=-8"]).unwrap();
        assert_eq!(o.stack_guard(true), tls(SegReg::Fs, -8));
        let o = parse(&["-mstack-protector-guard-symbol=g", "-mcmodel=kernel"]).unwrap();
        assert_eq!(
            o.stack_guard(true),
            StackGuard::TlsSymbol {
                seg: SegReg::Gs,
                symbol: "g".to_string()
            }
        );
        let o = parse(&["-mstack-protector-guard=global", "-mcmodel=kernel"]).unwrap();
        assert_eq!(o.stack_guard(true), StackGuard::Global);
    }

    #[test]
    fn refusals_are_gccs() {
        assert_eq!(
            parse(&["-mstack-protector-guard-offset=0x80000000"]).unwrap_err(),
            vec!["error: '0x80000000' is not a valid offset in '-mstack-protector-guard-offset='"]
        );
        assert_eq!(
            parse(&["-mstack-protector-guard-offset=abc"]).unwrap_err(),
            vec!["error: 'abc' is not a valid number in '-mstack-protector-guard-offset='"]
        );
        assert_eq!(
            parse(&["-mpreferred-stack-boundary=2"]).unwrap_err(),
            vec!["error: '-mpreferred-stack-boundary=2' is not between 3 and 12"]
        );
        assert_eq!(
            parse(&["-mharden-sls=x"]).unwrap_err(),
            vec![
                "error: unrecognized argument in option '-mharden-sls=x'",
                "note: valid arguments to '-mharden-sls=' are: all indirect-jmp none return",
            ]
        );
        assert_eq!(
            parse(&["-mcmodel=large"]).unwrap_err(),
            vec!["unsupported machine flag: -mcmodel=large"]
        );
    }
}
