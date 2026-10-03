//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86-64 CPU detection: `__builtin_cpu_init`, `__builtin_cpu_supports` and
// `__builtin_cpu_is`. They read the runtime library's `__cpu_model` (and
// `__cpu_features2`), which `__cpu_indicator_init` fills in -- libgcc's and
// compiler-rt's agree on the layout, which is an ABI:
//
//   struct { unsigned vendor, type, subtype, features[1]; } __cpu_model;
//   unsigned __cpu_features2[];
//
// The name tables are gcc 13's: every name it accepts (it rejects every
// other token of cc1's strings and of its -march= roster), with the field and
// value or bit read off what `gcc -O2 -S` generates for each, so the same
// program asks the same question whichever compiler built it.
//

use super::ast::{BinaryOp, Expr, ExprKind};
use super::parser::{ParseResult, Parser};
use crate::diag;
use crate::kw;
use crate::symbol::{Linkage, Namespace, Symbol};
use crate::token::lexer::Position;
use crate::types::{Type, TypeModifiers};
use gettextrs::gettext;

/// A word of the runtime's CPU model: a `__cpu_model` field, or an element
/// of `__cpu_features2`.
#[derive(Clone, Copy, Debug, PartialEq)]
enum CpuWord {
    Vendor,
    Type,
    Subtype,
    Features,
    Features2(u32),
}

/// Every feature name, in the order of its bit: the first 32 are the bits of
/// `__cpu_model.features[0]`, and each 32 after that a `__cpu_features2`
/// word.
const FEATURES: &[&str] = &[
    "cmov",
    "mmx",
    "popcnt",
    "sse",
    "sse2",
    "sse3",
    "ssse3",
    "sse4.1",
    "sse4.2",
    "avx",
    "avx2",
    "sse4a",
    "fma4",
    "xop",
    "fma",
    "avx512f",
    "bmi",
    "bmi2",
    "aes",
    "pclmul",
    "avx512vl",
    "avx512bw",
    "avx512dq",
    "avx512cd",
    "avx512er",
    "avx512pf",
    "avx512vbmi",
    "avx512ifma",
    "avx5124vnniw",
    "avx5124fmaps",
    "avx512vpopcntdq",
    "avx512vbmi2",
    "gfni",
    "vpclmulqdq",
    "avx512vnni",
    "avx512bitalg",
    "avx512bf16",
    "avx512vp2intersect",
    "3dnow",
    "3dnowp",
    "adx",
    "abm",
    "cldemote",
    "clflushopt",
    "clwb",
    "clzero",
    "cmpxchg16b",
    "cmpxchg8b",
    "enqcmd",
    "f16c",
    "fsgsbase",
    "fxsave",
    "hle",
    "ibt",
    "lahf_lm",
    "lm",
    "lwp",
    "lzcnt",
    "movbe",
    "movdir64b",
    "movdiri",
    "mwaitx",
    "osxsave",
    "pconfig",
    "pku",
    "prefetchwt1",
    "prfchw",
    "ptwrite",
    "rdpid",
    "rdrnd",
    "rdseed",
    "rtm",
    "serialize",
    "sgx",
    "sha",
    "shstk",
    "tbm",
    "tsxldtrk",
    "vaes",
    "waitpkg",
    "wbnoinvd",
    "xsave",
    "xsavec",
    "xsaveopt",
    "xsaves",
    "amx-tile",
    "amx-int8",
    "amx-bf16",
    "uintr",
    "hreset",
    "kl",
    "aeskle",
    "widekl",
    "avxvnni",
    "avx512fp16",
    "x86-64",
    "x86-64-v2",
    "x86-64-v3",
    "x86-64-v4",
    "avxifma",
    "avxvnniint8",
    "avxneconvert",
    "cmpccxadd",
    "amx-fp16",
    "prefetchi",
    "raoint",
    "amx-complex",
];

/// Every CPU name, with the word it is read from and the value it must have.
#[rustfmt::skip]
const CPUS: &[(&str, CpuWord, u32)] = {
    use CpuWord::{Subtype as S, Type as T, Vendor as V};
    &[
        ("intel", V, 1), ("amd", V, 2),
        ("atom", T, 1), ("bonnell", T, 1), ("core2", T, 2), ("corei7", T, 3),
        ("amdfam10h", T, 4), ("amdfam15h", T, 5),
        ("slm", T, 6), ("silvermont", T, 6), ("knl", T, 7), ("bdver1", T, 7), ("bdver2", T, 8),
        ("btver2", T, 9), ("amdfam17h", T, 10), ("knm", T, 11), ("goldmont", T, 12),
        ("goldmont-plus", T, 13), ("tremont", T, 14), ("amdfam19h", T, 15),
        ("sierraforest", T, 17), ("grandridge", T, 18), ("shanghai", T, 5), ("istanbul", T, 6),
        ("nehalem", S, 1), ("westmere", S, 2), ("sandybridge", S, 3), ("barcelona", S, 4),
        ("btver1", S, 7), ("bdver3", S, 9), ("bdver4", S, 10), ("znver1", S, 11),
        ("ivybridge", S, 12), ("haswell", S, 13), ("broadwell", S, 14), ("skylake", S, 15),
        ("skylake-avx512", S, 16), ("cannonlake", S, 17), ("icelake-client", S, 18),
        ("icelake-server", S, 19), ("znver2", S, 20), ("cascadelake", S, 21),
        ("tigerlake", S, 22), ("cooperlake", S, 23), ("sapphirerapids", S, 24),
        ("emeraldrapids", S, 24), ("alderlake", S, 25), ("raptorlake", S, 25),
        ("gracemont", S, 25), ("meteorlake", S, 25), ("znver3", S, 26), ("rocketlake", S, 27),
        ("lujiazui", S, 28), ("znver4", S, 29), ("graniterapids", S, 30),
        ("graniterapids-d", S, 31),
    ]
};

/// The test `__builtin_cpu_supports(name)` (when `supports`) or
/// `__builtin_cpu_is(name)` makes: the word read, how it is tested, and the
/// mask or value it is tested against. `None` for a name gcc rejects.
fn cpu_test(supports: bool, name: &str) -> Option<(CpuWord, BinaryOp, u32)> {
    if supports {
        FEATURES.iter().position(|f| *f == name).map(|bit| {
            let bit = bit as u32;
            let word = if bit < 32 {
                CpuWord::Features
            } else {
                CpuWord::Features2(bit / 32 - 1)
            };
            (word, BinaryOp::BitAnd, 1u32 << (bit % 32))
        })
    } else {
        CPUS.iter()
            .find(|c| c.0 == name)
            .map(|&(_, word, value)| (word, BinaryOp::Eq, value))
    }
}

impl Parser<'_> {
    /// `__builtin_cpu_init`, `__builtin_cpu_supports` and `__builtin_cpu_is`.
    pub(super) fn parse_cpu_builtin(
        &mut self,
        name_id: crate::strings::StringId,
        pos: Position,
    ) -> Option<ParseResult<Expr>> {
        match name_id {
            kw::BUILTIN_CPU_INIT => Some((|| {
                self.expect_special(b'(')?;
                self.expect_special(b')')?;
                let call = self.libm_call(
                    kw::CPU_INDICATOR_INIT,
                    self.types.int_id,
                    &[],
                    Vec::new(),
                    pos,
                );
                let void = self.types.void_id;
                Ok(Self::typed_expr(
                    ExprKind::Cast {
                        cast_type: void,
                        expr: Box::new(call),
                    },
                    void,
                    pos,
                ))
            })()),
            kw::BUILTIN_CPU_SUPPORTS | kw::BUILTIN_CPU_IS => Some((|| {
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                let ExprKind::StringLit(bytes) = &arg.kind else {
                    diag::error(
                        arg.pos,
                        &gettext("parameter to builtin must be a string constant or literal"),
                    );
                    return Ok(Self::typed_expr(
                        ExprKind::IntLit(0),
                        self.types.int_id,
                        pos,
                    ));
                };
                let name: String = bytes.chars().collect();
                let test = cpu_test(name_id == kw::BUILTIN_CPU_SUPPORTS, &name);
                let Some((word, op, operand)) = test else {
                    diag::error_args(arg.pos, "parameter to builtin not valid: {0}", &[&name]);
                    return Ok(Self::typed_expr(
                        ExprKind::IntLit(0),
                        self.types.int_id,
                        pos,
                    ));
                };
                let read = self.cpu_word(word, pos);
                let uint = self.types.uint_id;
                let constant = Self::typed_expr(ExprKind::IntLit(i64::from(operand)), uint, pos);
                let tested = Self::typed_expr(
                    ExprKind::Binary {
                        op,
                        left: Box::new(read),
                        right: Box::new(constant),
                    },
                    if op == BinaryOp::Eq {
                        self.types.int_id
                    } else {
                        uint
                    },
                    pos,
                );
                if op == BinaryOp::Eq {
                    return Ok(tested);
                }
                let zero = Self::typed_expr(ExprKind::IntLit(0), uint, pos);
                Ok(Self::typed_expr(
                    ExprKind::Binary {
                        op: BinaryOp::Ne,
                        left: Box::new(tested),
                        right: Box::new(zero),
                    },
                    self.types.int_id,
                    pos,
                ))
            })()),
            _ => None,
        }
    }

    /// An expression reading `word` of the runtime's CPU model: an element of
    /// an `extern unsigned int` array declared, unseen by the program, where
    /// the call is -- it names the runtime library's object, as an `extern`
    /// declaration in a block would.
    fn cpu_word(&mut self, word: CpuWord, pos: Position) -> Expr {
        let (name, index, len) = match word {
            CpuWord::Vendor => (kw::CPU_MODEL, 0, 4),
            CpuWord::Type => (kw::CPU_MODEL, 1, 4),
            CpuWord::Subtype => (kw::CPU_MODEL, 2, 4),
            CpuWord::Features => (kw::CPU_MODEL, 3, 4),
            CpuWord::Features2(i) => (kw::CPU_FEATURES2, i, 4),
        };
        let uint = self.types.uint_id;
        let mut array = Type::array(uint, len);
        array.modifiers |= TypeModifiers::EXTERN;
        let array = self.types.intern(array);
        let symbol = match self.symbols.lookup_id(name, Namespace::Ordinary) {
            Some(id) if self.symbols.get(id).linkage != Linkage::None => id,
            _ => {
                let mut sym = Symbol::variable(name, array, self.symbols.depth())
                    .with_linkage(Linkage::External);
                // A repeat in this scope is the same declaration, not a
                // redefinition.
                sym.defined = false;
                self.symbols.declare(sym).unwrap_or_else(|_| {
                    self.symbols
                        .lookup_id(name, Namespace::Ordinary)
                        .expect("declared")
                })
            }
        };
        let typ = self.symbols.get(symbol).typ;
        let base = Self::typed_expr(ExprKind::Ident(symbol), typ, pos);
        let idx = Self::typed_expr(ExprKind::IntLit(i64::from(index)), self.types.int_id, pos);
        Self::typed_expr(
            ExprKind::Index {
                array: Box::new(base),
                index: Box::new(idx),
            },
            uint,
            pos,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every name gcc 13 accepts for `__builtin_cpu_supports`, in the order
    /// of the bit `gcc -O2 -S` tests for it. Found by offering gcc every
    /// token of cc1's strings, the -march= roster and the manual's lists;
    /// it rejects everything else.
    const GCC_FEATURES: &[&str] = &[
        "cmov",
        "mmx",
        "popcnt",
        "sse",
        "sse2",
        "sse3",
        "ssse3",
        "sse4.1",
        "sse4.2",
        "avx",
        "avx2",
        "sse4a",
        "fma4",
        "xop",
        "fma",
        "avx512f",
        "bmi",
        "bmi2",
        "aes",
        "pclmul",
        "avx512vl",
        "avx512bw",
        "avx512dq",
        "avx512cd",
        "avx512er",
        "avx512pf",
        "avx512vbmi",
        "avx512ifma",
        "avx5124vnniw",
        "avx5124fmaps",
        "avx512vpopcntdq",
        "avx512vbmi2",
        "gfni",
        "vpclmulqdq",
        "avx512vnni",
        "avx512bitalg",
        "avx512bf16",
        "avx512vp2intersect",
        "3dnow",
        "3dnowp",
        "adx",
        "abm",
        "cldemote",
        "clflushopt",
        "clwb",
        "clzero",
        "cmpxchg16b",
        "cmpxchg8b",
        "enqcmd",
        "f16c",
        "fsgsbase",
        "fxsave",
        "hle",
        "ibt",
        "lahf_lm",
        "lm",
        "lwp",
        "lzcnt",
        "movbe",
        "movdir64b",
        "movdiri",
        "mwaitx",
        "osxsave",
        "pconfig",
        "pku",
        "prefetchwt1",
        "prfchw",
        "ptwrite",
        "rdpid",
        "rdrnd",
        "rdseed",
        "rtm",
        "serialize",
        "sgx",
        "sha",
        "shstk",
        "tbm",
        "tsxldtrk",
        "vaes",
        "waitpkg",
        "wbnoinvd",
        "xsave",
        "xsavec",
        "xsaveopt",
        "xsaves",
        "amx-tile",
        "amx-int8",
        "amx-bf16",
        "uintr",
        "hreset",
        "kl",
        "aeskle",
        "widekl",
        "avxvnni",
        "avx512fp16",
        "x86-64",
        "x86-64-v2",
        "x86-64-v3",
        "x86-64-v4",
        "avxifma",
        "avxvnniint8",
        "avxneconvert",
        "cmpccxadd",
        "amx-fp16",
        "prefetchi",
        "raoint",
        "amx-complex",
    ];

    /// Every name gcc 13 accepts for `__builtin_cpu_is`, with the
    /// `__cpu_model` field and value its `gcc -O2 -S` compares. (gcc reads
    /// `shanghai` and `istanbul` from the type field, though libgcc stores
    /// them as subtypes; c17 asks what gcc asks.)
    #[rustfmt::skip]
    const GCC_CPUS: &[(&str, CpuWord, u32)] = {
        use CpuWord::{Subtype as S, Type as T, Vendor as V};
        &[
            ("intel", V, 1), ("amd", V, 2), ("atom", T, 1), ("bonnell", T, 1), ("core2", T, 2),
            ("corei7", T, 3), ("amdfam10h", T, 4), ("amdfam15h", T, 5), ("shanghai", T, 5),
            ("istanbul", T, 6), ("silvermont", T, 6), ("slm", T, 6), ("bdver1", T, 7),
            ("knl", T, 7), ("bdver2", T, 8), ("btver2", T, 9), ("amdfam17h", T, 10),
            ("knm", T, 11), ("goldmont", T, 12), ("goldmont-plus", T, 13), ("tremont", T, 14),
            ("amdfam19h", T, 15), ("sierraforest", T, 17), ("grandridge", T, 18),
            ("nehalem", S, 1), ("westmere", S, 2), ("sandybridge", S, 3), ("barcelona", S, 4),
            ("btver1", S, 7), ("bdver3", S, 9), ("bdver4", S, 10), ("znver1", S, 11),
            ("ivybridge", S, 12), ("haswell", S, 13), ("broadwell", S, 14), ("skylake", S, 15),
            ("skylake-avx512", S, 16), ("cannonlake", S, 17), ("icelake-client", S, 18),
            ("icelake-server", S, 19), ("znver2", S, 20), ("cascadelake", S, 21),
            ("tigerlake", S, 22), ("cooperlake", S, 23), ("emeraldrapids", S, 24),
            ("sapphirerapids", S, 24), ("alderlake", S, 25), ("gracemont", S, 25),
            ("meteorlake", S, 25), ("raptorlake", S, 25), ("znver3", S, 26),
            ("rocketlake", S, 27), ("lujiazui", S, 28), ("znver4", S, 29),
            ("graniterapids", S, 30), ("graniterapids-d", S, 31),
        ]
    };

    /// Every feature gcc 13 knows tests the bit gcc's code tests, and the
    /// table names nothing gcc rejects.
    #[test]
    fn cpu_supports_matches_gcc() {
        for (bit, name) in GCC_FEATURES.iter().enumerate() {
            let bit = bit as u32;
            let word = match bit / 32 {
                0 => CpuWord::Features,
                n => CpuWord::Features2(n - 1),
            };
            assert_eq!(
                cpu_test(true, name),
                Some((word, BinaryOp::BitAnd, 1 << (bit % 32))),
                "__builtin_cpu_supports(\"{name}\")"
            );
        }
        assert_eq!(FEATURES.len(), GCC_FEATURES.len());
        assert_eq!(cpu_test(true, ""), None);
        assert_eq!(cpu_test(true, "nosuch"), None);
    }

    /// Every CPU gcc 13 knows compares the field and value gcc's code
    /// compares, and the table names nothing gcc rejects.
    #[test]
    fn cpu_is_matches_gcc() {
        for &(name, word, value) in GCC_CPUS {
            assert_eq!(
                cpu_test(false, name),
                Some((word, BinaryOp::Eq, value)),
                "__builtin_cpu_is(\"{name}\")"
            );
        }
        assert_eq!(CPUS.len(), GCC_CPUS.len());
        assert_eq!(cpu_test(false, "nosuch"), None);
    }
}
