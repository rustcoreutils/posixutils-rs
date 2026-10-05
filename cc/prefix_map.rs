//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's path prefix maps: `-fdebug-prefix-map=OLD=NEW` rewrites the paths
// debug information records, `-fmacro-prefix-map=OLD=NEW` the ones
// `__FILE__` and `__BASE_FILE__` expand to, and `-ffile-prefix-map=OLD=NEW`
// both. Reproducible builds pass them to keep the build directory out of
// the objects.
//

use std::borrow::Cow;

/// An ordered list of `OLD=NEW` path rewrites, applied by gcc's rule.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct PrefixMap {
    /// `(old, new)`, in command-line order.
    entries: Vec<(String, String)>,
}

impl PrefixMap {
    /// Add a rewrite after every one already given.
    pub fn push(&mut self, old: &str, new: &str) {
        self.entries.push((old.to_string(), new.to_string()));
    }

    /// `path` with its prefix rewritten.
    ///
    /// gcc's rule: the *last* option whose `OLD` is a prefix of the path
    /// wins -- not the longest -- and the test is a plain byte-prefix
    /// comparison with no regard for directory boundaries, so `OLD=/tm`
    /// rewrites `/tmp/x` too. A relative path is matched as it stands.
    pub fn apply<'p>(&self, path: &'p str) -> Cow<'p, str> {
        for (old, new) in self.entries.iter().rev() {
            if let Some(rest) = path.strip_prefix(old.as_str()) {
                return Cow::Owned(format!("{new}{rest}"));
            }
        }
        Cow::Borrowed(path)
    }
}

/// Which list a prefix-map option feeds.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MapKind {
    /// `-fdebug-prefix-map`: debug information.
    Debug,
    /// `-fmacro-prefix-map`: `__FILE__` and `__BASE_FILE__`.
    Macro,
    /// `-ffile-prefix-map`: both.
    File,
}

/// One prefix-map option as given.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapOption {
    pub kind: MapKind,
    pub old: String,
    pub new: String,
}

impl MapOption {
    /// Read a gcc spelling. `None` when `arg` is not a prefix-map option at
    /// all; `Err` with gcc's diagnostic when it is one with a bad argument.
    ///
    /// The argument splits at its *last* `=`, as gcc's does, so an `OLD`
    /// may contain `=` but a `NEW` may not.
    pub fn parse(arg: &str) -> Option<Result<MapOption, String>> {
        const SPELLINGS: [(&str, MapKind); 3] = [
            ("-fdebug-prefix-map", MapKind::Debug),
            ("-fmacro-prefix-map", MapKind::Macro),
            ("-ffile-prefix-map", MapKind::File),
        ];
        let (name, kind, value) = SPELLINGS.iter().find_map(|&(name, kind)| {
            let value = arg.strip_prefix(name)?.strip_prefix('=')?;
            Some((name, kind, value))
        })?;
        if value.is_empty() {
            return Some(Err(format!("missing argument to '{name}='")));
        }
        Some(match value.rsplit_once('=') {
            Some((old, new)) => Ok(MapOption {
                kind,
                old: old.to_string(),
                new: new.to_string(),
            }),
            None => Err(format!("invalid argument '{value}' to '{name}'")),
        })
    }
}

/// The two maps the options build: the debug map for codegen, the macro map
/// for the preprocessor.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct PrefixMaps {
    pub debug: PrefixMap,
    pub macros: PrefixMap,
}

impl PrefixMaps {
    /// Build both maps from the options in command-line order. A
    /// `-ffile-prefix-map` takes its place in each list, so it competes with
    /// the specific options by position.
    pub fn from_options<'o>(options: impl IntoIterator<Item = &'o MapOption>) -> Self {
        let mut maps = PrefixMaps::default();
        for o in options {
            if o.kind != MapKind::Macro {
                maps.debug.push(&o.old, &o.new);
            }
            if o.kind != MapKind::Debug {
                maps.macros.push(&o.old, &o.new);
            }
        }
        maps
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn map(pairs: &[(&str, &str)]) -> PrefixMap {
        let mut m = PrefixMap::default();
        for (old, new) in pairs {
            m.push(old, new);
        }
        m
    }

    #[test]
    fn empty_map_and_no_match_borrow_the_path() {
        let m = PrefixMap::default();
        assert!(matches!(m.apply("/a/b.c"), Cow::Borrowed("/a/b.c")));
        let m = map(&[("/x", "/y")]);
        assert!(matches!(m.apply("/a/b.c"), Cow::Borrowed("/a/b.c")));
    }

    #[test]
    fn last_match_wins_not_longest() {
        let m = map(&[("/tmp/build", "/LONG"), ("/tmp", "/SHORT")]);
        assert_eq!(m.apply("/tmp/build/t.c"), "/SHORT/build/t.c");
        let m = map(&[("/tmp", "/SHORT"), ("/tmp/build", "/LONG")]);
        assert_eq!(m.apply("/tmp/build/t.c"), "/LONG/t.c");
        // An earlier entry still applies when a later one does not match.
        let m = map(&[("/tmp", "/T"), ("/usr", "/U")]);
        assert_eq!(m.apply("/tmp/t.c"), "/T/t.c");
    }

    #[test]
    fn prefix_is_bytes_not_path_components() {
        let m = map(&[("/tm", "/ZZ")]);
        assert_eq!(m.apply("/tmp/t.c"), "/ZZp/t.c");
        let m = map(&[("t", "Q")]);
        assert_eq!(m.apply("t.c"), "Q.c");
        // The whole path, and an empty OLD, which matches everything.
        let m = map(&[("/a/b.c", "x.c")]);
        assert_eq!(m.apply("/a/b.c"), "x.c");
        let m = map(&[("", "/E")]);
        assert_eq!(m.apply("t.c"), "/Et.c");
        // An empty NEW deletes the prefix.
        let m = map(&[("/a/", "")]);
        assert_eq!(m.apply("/a/b.c"), "b.c");
    }

    #[test]
    fn parse_splits_at_the_last_equals() {
        let o = MapOption::parse("-fdebug-prefix-map=/d=x=/A")
            .unwrap()
            .unwrap();
        assert_eq!(
            o,
            MapOption {
                kind: MapKind::Debug,
                old: "/d=x".into(),
                new: "/A".into()
            }
        );
        let o = MapOption::parse("-ffile-prefix-map==.").unwrap().unwrap();
        assert_eq!(
            (o.kind, o.old.as_str(), o.new.as_str()),
            (MapKind::File, "", ".")
        );
        let o = MapOption::parse("-fmacro-prefix-map=a=").unwrap().unwrap();
        assert_eq!(
            (o.kind, o.old.as_str(), o.new.as_str()),
            (MapKind::Macro, "a", "")
        );
    }

    #[test]
    fn parse_errors_and_non_options() {
        assert_eq!(
            MapOption::parse("-fdebug-prefix-map=noeq"),
            Some(Err("invalid argument 'noeq' to '-fdebug-prefix-map'".into()))
        );
        assert_eq!(
            MapOption::parse("-fmacro-prefix-map="),
            Some(Err("missing argument to '-fmacro-prefix-map='".into()))
        );
        assert_eq!(MapOption::parse("-fdebug-prefix-mapx=a=b"), None);
        assert_eq!(MapOption::parse("-fprofile-prefix-map=a=b"), None);
        assert_eq!(MapOption::parse("-fdebug-prefix-map"), None);
    }

    #[test]
    fn file_option_feeds_both_lists_in_order() {
        let opts: Vec<MapOption> = [
            "-ffile-prefix-map=/b=.",
            "-fdebug-prefix-map=/b=/D",
            "-fmacro-prefix-map=/m=/M",
        ]
        .iter()
        .map(|a| MapOption::parse(a).unwrap().unwrap())
        .collect();
        let maps = PrefixMaps::from_options(&opts);
        assert_eq!(maps.debug.apply("/b/t.c"), "/D/t.c");
        assert_eq!(maps.macros.apply("/b/t.c"), "./t.c");
        assert_eq!(maps.debug.apply("/m/t.c"), "/m/t.c");
        assert_eq!(maps.macros.apply("/m/t.c"), "/M/t.c");
    }
}
