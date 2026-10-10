//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! NFSv4-style ACLs: what a Linux NFSv4 mount (`system.nfs4_acl`) and macOS (extended ACLs)
//! hold, and the text libarchive and star put in a pax archive (`SCHILY.acl.ace`).
//!
//! Permission and flag bits are the NFSv4 protocol's (RFC 7530 section 6.2.1), the values the
//! `system.nfs4_acl` XDR carries; the text spells them with libarchive's letters.

use super::invalid;
use std::io;

/// The most entries an NFSv4 ACL read from text has: more than any filesystem keeps (macOS
/// 128, ZFS 1024).
pub const MAX_ACES: usize = 1024;

/// Who an ACE is about.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Who {
    /// `owner@`: the file's owner.
    Owner,
    /// `group@`: the file's owning group.
    OwningGroup,
    /// `everyone@`.
    Everyone,
    /// A user, named by uid -- or, where no uid could be found for it, by the name it came with.
    User(Named),
    /// A group, likewise.
    Group(Named),
}

/// The user or group an ACE names.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Named {
    Id(u32),
    /// A name no local id was found for (`alice@example.org`, say), kept as it came.
    Name(String),
}

/// What an ACE does with the access it names.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AceKind {
    Allow,
    Deny,
    Audit,
    Alarm,
}

/// One entry of an NFSv4 ACL.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ace {
    pub who: Who,
    /// The `ACE4_*` access mask bits (`READ_DATA` ...).
    pub perms: u32,
    /// The `ACE4_*` flag bits (`FILE_INHERIT` ...), never `IDENTIFIER_GROUP`, which `who` says.
    pub flags: u32,
    pub kind: AceKind,
}

/// An NFSv4-style ACL: its entries, in the order they are evaluated.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Nfs4Acl {
    pub aces: Vec<Ace>,
}

// The access mask bits.
pub const READ_DATA: u32 = 0x0000_0001;
pub const WRITE_DATA: u32 = 0x0000_0002;
pub const APPEND_DATA: u32 = 0x0000_0004;
pub const READ_NAMED_ATTRS: u32 = 0x0000_0008;
pub const WRITE_NAMED_ATTRS: u32 = 0x0000_0010;
pub const EXECUTE: u32 = 0x0000_0020;
pub const DELETE_CHILD: u32 = 0x0000_0040;
pub const READ_ATTRIBUTES: u32 = 0x0000_0080;
pub const WRITE_ATTRIBUTES: u32 = 0x0000_0100;
pub const DELETE: u32 = 0x0001_0000;
pub const READ_ACL: u32 = 0x0002_0000;
pub const WRITE_ACL: u32 = 0x0004_0000;
pub const WRITE_OWNER: u32 = 0x0008_0000;
pub const SYNCHRONIZE: u32 = 0x0010_0000;

// The flag bits.
pub const FILE_INHERIT: u32 = 0x01;
pub const DIRECTORY_INHERIT: u32 = 0x02;
pub const NO_PROPAGATE_INHERIT: u32 = 0x04;
pub const INHERIT_ONLY: u32 = 0x08;
pub const SUCCESSFUL_ACCESS: u32 = 0x10;
pub const FAILED_ACCESS: u32 = 0x20;
pub const INHERITED: u32 = 0x80;
/// The flag that makes a named ACE a group's; `Who` holds it.
const IDENTIFIER_GROUP: u32 = 0x40;

/// The permission letters, in the order libarchive writes them.
const PERM_LETTERS: [(u32, char); 14] = [
    (READ_DATA, 'r'),
    (WRITE_DATA, 'w'),
    (EXECUTE, 'x'),
    (APPEND_DATA, 'p'),
    (DELETE, 'd'),
    (DELETE_CHILD, 'D'),
    (READ_ATTRIBUTES, 'a'),
    (WRITE_ATTRIBUTES, 'A'),
    (READ_NAMED_ATTRS, 'R'),
    (WRITE_NAMED_ATTRS, 'W'),
    (READ_ACL, 'c'),
    (WRITE_ACL, 'C'),
    (WRITE_OWNER, 'o'),
    (SYNCHRONIZE, 's'),
];

/// The flag letters, likewise.
const FLAG_LETTERS: [(u32, char); 7] = [
    (FILE_INHERIT, 'f'),
    (DIRECTORY_INHERIT, 'd'),
    (INHERIT_ONLY, 'i'),
    (NO_PROPAGATE_INHERIT, 'n'),
    (SUCCESSFUL_ACCESS, 'S'),
    (FAILED_ACCESS, 'F'),
    (INHERITED, 'I'),
];

/// Every access mask bit there is a letter for, and every flag bit.
const ALL_PERMS: u32 = all_bits(&PERM_LETTERS);
const ALL_FLAGS: u32 = all_bits(&FLAG_LETTERS);

const fn all_bits(letters: &[(u32, char)]) -> u32 {
    let mut bits = 0;
    let mut i = 0;
    while i < letters.len() {
        bits |= letters[i].0;
        i += 1;
    }
    bits
}

impl AceKind {
    /// The `ACE4_*_ACE_TYPE` value.
    fn wire(self) -> u32 {
        match self {
            AceKind::Allow => 0,
            AceKind::Deny => 1,
            AceKind::Audit => 2,
            AceKind::Alarm => 3,
        }
    }

    fn from_wire(kind: u32) -> Option<AceKind> {
        [
            AceKind::Allow,
            AceKind::Deny,
            AceKind::Audit,
            AceKind::Alarm,
        ]
        .into_iter()
        .find(|k| k.wire() == kind)
    }

    fn word(self) -> &'static str {
        match self {
            AceKind::Allow => "allow",
            AceKind::Deny => "deny",
            AceKind::Audit => "audit",
            AceKind::Alarm => "alarm",
        }
    }
}

/// The letters of the bits `bits` has, in `letters`' order (libarchive's compact style).
fn letters_text(bits: u32, letters: &[(u32, char)]) -> String {
    letters
        .iter()
        .filter(|&&(bit, _)| bits & bit != 0)
        .map(|&(_, c)| c)
        .collect()
}

/// The bits the letters `text` spell, `-` standing for none (the long style); `None` for any
/// other character.
fn letters_bits(text: &str, letters: &[(u32, char)]) -> Option<u32> {
    let mut bits = 0;
    for c in text.chars() {
        if c != '-' {
            bits |= letters.iter().find(|&&(_, l)| l == c)?.0;
        }
    }
    Some(bits)
}

/// Whether `name` can stand in the text form as it is: not empty, and holding no separator,
/// white space -- which libarchive's reader would split it at -- or other control character.
fn usable_name(name: &str) -> bool {
    !name.is_empty()
        && !name.contains([',', ':', '#'])
        && !name.contains(|c: char| c.is_whitespace() || c.is_control())
}

/// The name of the user `uid` (`group`: the group `gid`), where it has one the text form can
/// carry.
fn id_name(id: u32, group: bool) -> Option<String> {
    let name = if group {
        crate::group::lookup_by_gid(id)
            .ok()
            .flatten()
            .map(|g| g.name)
    } else {
        crate::user::lookup_by_uid(id)
            .ok()
            .flatten()
            .map(|u| u.name)
    };
    name?.into_string().ok().filter(|n| usable_name(n))
}

/// The id of the user (`group`: the group) called `name`.
fn name_id(name: &str, group: bool) -> Option<u32> {
    if group {
        crate::group::lookup_by_name(name)
            .ok()
            .flatten()
            .map(|g| g.gid)
    } else {
        crate::user::lookup_by_name(name)
            .ok()
            .flatten()
            .map(|u| u.uid)
    }
}

/// `ACL_UNDEFINED_ID`, which names nobody.
const UNDEFINED_ID: u32 = u32::MAX;

/// A decimal id: digits only, as libarchive reads one, and not the id that names nobody.
fn parse_id(text: &str) -> Option<u32> {
    if text.is_empty() || !text.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    text.parse::<u32>().ok().filter(|&id| id != UNDEFINED_ID)
}

impl Nfs4Acl {
    /// Whether it says only what a mode can, as gnulib's `acl_nfs4_nontrivial` has it: at most
    /// six entries, each an allow or a deny for `owner@`, `group@` or `everyone@` -- at most one
    /// of each kind for each -- with no flag.
    pub fn is_trivial(&self) -> bool {
        let mut seen = Vec::with_capacity(self.aces.len());
        self.aces.len() <= 6
            && self.aces.iter().all(|ace| {
                let special = matches!(ace.who, Who::Owner | Who::OwningGroup | Who::Everyone);
                let kind = matches!(ace.kind, AceKind::Allow | AceKind::Deny);
                let key = (ace.who.clone(), ace.kind);
                let fresh = !seen.contains(&key);
                seen.push(key);
                special && kind && ace.flags == 0 && fresh
            })
    }

    /// This ACL followed by the entries libarchive adds to one read on macOS -- where a file's
    /// ACL holds no `owner@`, `group@` or `everyone@` entry -- to say what the mode `mode` does,
    /// so that a reader elsewhere does not take the ACL for all there is.
    pub fn with_mode_aces(&self, mode: u32) -> Nfs4Acl {
        const R: u32 = READ_DATA;
        const W: u32 = WRITE_DATA | APPEND_DATA;
        const X: u32 = EXECUTE;
        const PUBLIC: u32 = READ_ATTRIBUTES | READ_NAMED_ATTRS | READ_ACL | SYNCHRONIZE;
        const OWNER: u32 = PUBLIC | WRITE_ATTRIBUTES | WRITE_NAMED_ATTRS | WRITE_ACL | WRITE_OWNER;
        // In libarchive's order: an allow and a deny for the owner, a deny for the owning
        // group, then the owner's, the group's and everyone's allows.
        let mut perms = [0, 0, 0, OWNER, PUBLIC, PUBLIC];
        let set = |bit: u32| mode & bit != 0;
        for (shift, bits) in [(2, R), (1, W), (0, X)] {
            let (owner, group, other) = (set(0o100 << shift), set(0o10 << shift), set(1 << shift));
            if other {
                perms[5] |= bits;
            }
            if group {
                perms[4] |= bits;
            } else if other {
                perms[2] |= bits;
            }
            if owner {
                perms[3] |= bits;
                if !group && other {
                    perms[0] |= bits;
                }
            } else if group || other {
                perms[1] |= bits;
            }
        }
        let who = [
            Who::Owner,
            Who::Owner,
            Who::OwningGroup,
            Who::Owner,
            Who::OwningGroup,
            Who::Everyone,
        ];
        let kind = [
            AceKind::Allow,
            AceKind::Deny,
            AceKind::Deny,
            AceKind::Allow,
            AceKind::Allow,
            AceKind::Allow,
        ];
        let mut acl = self.clone();
        for i in 0..perms.len() {
            if perms[i] != 0 {
                acl.aces.push(Ace {
                    who: who[i].clone(),
                    perms: perms[i],
                    flags: 0,
                    kind: kind[i],
                });
            }
        }
        acl
    }

    /// Whether it says exactly what the mode `mode` does, so that dropping it -- where it
    /// cannot be held, or need not be -- loses nothing: only allow and deny entries for
    /// `owner@`, `group@` and `everyone@`, with no flag, granting each of the owner, the owning
    /// group and others the read, write and execute permission the mode does and no other,
    /// evaluated as NFSv4 does -- the first entry that applies to a principal and names a
    /// permission decides it, and one that none names is denied. Stricter than gnulib's
    /// `is_trivial`, which `ls` keeps: an `everyone@` deny of what the mode grants others is
    /// not trivial here. What libarchive adds to say a mode (`with_mode_aces`) is.
    pub fn says_only_mode(&self, mode: u32) -> bool {
        let granted = |shift: u32, bit: u32| {
            let applies = |who: &Who| match who {
                Who::Everyone => true,
                Who::Owner => shift == 6,
                Who::OwningGroup => shift == 3,
                _ => false,
            };
            self.aces
                .iter()
                .find(|ace| applies(&ace.who) && ace.perms & bit != 0)
                .is_some_and(|ace| ace.kind == AceKind::Allow)
        };
        let plain = self.aces.iter().all(|ace| {
            matches!(ace.who, Who::Owner | Who::OwningGroup | Who::Everyone)
                && matches!(ace.kind, AceKind::Allow | AceKind::Deny)
                && ace.flags == 0
        });
        plain
            && [6, 3, 0].into_iter().all(|shift| {
                let bits = (mode >> shift) & 7;
                [(4, READ_DATA), (2, WRITE_DATA), (1, EXECUTE)]
                    .into_iter()
                    .all(|(m, bit)| (bits & m != 0) == granted(shift, bit))
            })
    }

    /// Whether it may let anyone but the owner change the file or its access: an allow entry
    /// for a named user or group, `group@` or `everyone@` granting WRITE_DATA, APPEND_DATA,
    /// WRITE_ACL or WRITE_OWNER. A set-user-ID or set-group-ID file is not given that.
    pub fn lets_others_change(&self) -> bool {
        const CHANGE: u32 = WRITE_DATA | APPEND_DATA | WRITE_ACL | WRITE_OWNER;
        self.aces.iter().any(|ace| {
            ace.kind == AceKind::Allow && ace.who != Who::Owner && ace.perms & CHANGE != 0
        })
    }

    /// Whether its `owner@`, `group@` and `everyone@` entries can be left out -- as macOS,
    /// which has no place for them, must -- with the mode `mode` saying all they do: each an
    /// allow with no flag, or a deny with no flag, after every entry naming a user or group,
    /// of no more than read, write, append and execute, which the mode withholds from every
    /// class it covers (`everyone@`: all three). Leaving out any other -- a deny the mode does
    /// not repeat, one before a named entry it overrides, an audit or alarm entry, an
    /// inheritance flag -- loses what it says.
    pub fn specials_said_by_mode(&self, mode: u32) -> bool {
        const RWX: u32 = READ_DATA | WRITE_DATA | APPEND_DATA | EXECUTE;
        let last_named = self
            .aces
            .iter()
            .rposition(|ace| matches!(ace.who, Who::User(_) | Who::Group(_)));
        let mode_bits = |perms: u32| {
            let bit = |mask: u32, b: u32| if perms & mask != 0 { b } else { 0 };
            bit(READ_DATA, 4) | bit(WRITE_DATA | APPEND_DATA, 2) | bit(EXECUTE, 1)
        };
        self.aces.iter().enumerate().all(|(i, ace)| {
            let shifts: &[u32] = match ace.who {
                Who::Owner => &[6],
                Who::OwningGroup => &[3],
                Who::Everyone => &[6, 3, 0],
                Who::User(_) | Who::Group(_) => return true,
            };
            if ace.flags != 0 {
                return false;
            }
            match ace.kind {
                AceKind::Allow => true,
                AceKind::Deny => {
                    last_named.is_none_or(|n| i > n)
                        && ace.perms & !RWX == 0
                        && shifts
                            .iter()
                            .all(|&s| mode_bits(ace.perms) & (mode >> s) & 7 == 0)
                }
                AceKind::Audit | AceKind::Alarm => false,
            }
        })
    }

    /// The text form libarchive writes in a pax archive (`SCHILY.acl.ace`, its
    /// `ARCHIVE_ENTRY_ACL_STYLE_EXTRA_ID | SEPARATOR_COMMA | COMPACT` style): entries separated
    /// by commas, each `who:perms:flags:type`, a named user or group with its name and, as a
    /// last field, its id -- `owner@:rwxpaARWcCos::allow,user:alice:raRcs::allow:1000`. A user
    /// or group with no name the form can hold is named by its number; one kept by name alone
    /// has no id field. A name that cannot stand in the form fails it.
    pub fn to_ace_text(&self) -> io::Result<String> {
        let mut out = Vec::with_capacity(self.aces.len());
        for ace in &self.aces {
            let perms = letters_text(ace.perms, &PERM_LETTERS);
            let flags = letters_text(ace.flags, &FLAG_LETTERS);
            let tail = format!("{perms}:{flags}:{}", ace.kind.word());
            let named = |tag: &str, named: &Named, group: bool| match named {
                Named::Id(id) => {
                    let name = id_name(*id, group).unwrap_or_else(|| id.to_string());
                    Ok(format!("{tag}:{name}:{tail}:{id}"))
                }
                Named::Name(name) if usable_name(name) => Ok(format!("{tag}:{name}:{tail}")),
                Named::Name(_) => Err(invalid("ACL names a user or group the text cannot hold")),
            };
            out.push(match &ace.who {
                Who::Owner => format!("owner@:{tail}"),
                Who::OwningGroup => format!("group@:{tail}"),
                Who::Everyone => format!("everyone@:{tail}"),
                Who::User(n) => named("user", n, false)?,
                Who::Group(n) => named("group", n, true)?,
            });
        }
        Ok(out.join(","))
    }

    /// Parse the text form (`to_ace_text`): entries separated by commas or newlines, the
    /// permissions and flags in compact or long (`-` padded) style. A user or group is named
    /// as star and libarchive name one: by its name, looked up here, else by the id field
    /// after it, else by the name read as a number; one that is none of those is kept by name.
    /// Anything else -- an unknown letter, type or tag, a field too many or too few, an id that
    /// is not a number, no entry at all, more than `MAX_ACES` entries -- fails the whole ACL.
    pub fn from_ace_text(text: &str) -> io::Result<Nfs4Acl> {
        let mut aces = Vec::new();
        for entry in text.split([',', '\n']).map(str::trim) {
            if entry.is_empty() {
                continue;
            }
            // Refused before the entry's name is looked up.
            if aces.len() == MAX_ACES {
                return Err(invalid("ACL has too many entries"));
            }
            aces.push(parse_ace(entry)?);
        }
        if aces.is_empty() {
            return Err(invalid("invalid ACL text"));
        }
        Ok(Nfs4Acl { aces })
    }

    /// Parse the `system.nfs4_acl` XDR the Linux NFS client hands out (RFC 7530's `fattr4_acl`,
    /// big-endian): the number of entries, then each one's type, flags, access mask and who --
    /// a string (length, bytes, zero padding to four), `OWNER@`, `GROUP@`, `EVERYONE@`, a
    /// number, or `name@domain`. A who with the IDENTIFIER_GROUP flag is a group. A name is
    /// never mapped to a local id: it is kept as it came, and written back so. A type, flag or permission bit the text form cannot hold, a torn entry or bytes
    /// past the last fail it.
    pub fn parse_xdr(mut xdr: &[u8]) -> io::Result<Nfs4Acl> {
        let bad = || invalid("invalid NFSv4 ACL");
        let word = |xdr: &mut &[u8]| -> io::Result<u32> {
            let (w, rest) = xdr.split_first_chunk::<4>().ok_or_else(bad)?;
            *xdr = rest;
            Ok(u32::from_be_bytes(*w))
        };
        let count = word(&mut xdr)?;
        // An entry takes at least sixteen bytes: never allocate for more than are there.
        let count = usize::try_from(count).map_err(|_| bad())?;
        if count > xdr.len() / 16 {
            return Err(bad());
        }
        let mut aces = Vec::with_capacity(count);
        for _ in 0..count {
            let (kind, flags) = (word(&mut xdr)?, word(&mut xdr)?);
            let (perms, len) = (word(&mut xdr)?, word(&mut xdr)?);
            let len = usize::try_from(len).map_err(|_| bad())?;
            let padded = len.checked_next_multiple_of(4).ok_or_else(bad)?;
            let who = xdr.get(..len).ok_or_else(bad)?;
            xdr = xdr.get(padded..).ok_or_else(bad)?;
            let kind = AceKind::from_wire(kind).ok_or_else(bad)?;
            if flags & !(ALL_FLAGS | IDENTIFIER_GROUP) != 0 || perms & !ALL_PERMS != 0 {
                return Err(bad());
            }
            let who = std::str::from_utf8(who).map_err(|_| bad())?;
            aces.push(Ace {
                who: xdr_who(who, flags & IDENTIFIER_GROUP != 0).ok_or_else(bad)?,
                perms,
                flags: flags & !IDENTIFIER_GROUP,
                kind,
            });
        }
        if !xdr.is_empty() {
            return Err(bad());
        }
        Ok(Nfs4Acl { aces })
    }

    /// The `system.nfs4_acl` XDR (`parse_xdr`). A user or group is named by its number, which
    /// the Linux NFS server and client take where no name mapping is set up (the default for
    /// `sec=sys`); one kept by name goes as that name.
    pub fn to_xdr(&self) -> Vec<u8> {
        let mut out = Vec::new();
        out.extend((self.aces.len() as u32).to_be_bytes());
        for ace in &self.aces {
            let (who, group) = match &ace.who {
                Who::Owner => ("OWNER@".to_string(), false),
                Who::OwningGroup => ("GROUP@".to_string(), true),
                Who::Everyone => ("EVERYONE@".to_string(), false),
                Who::User(n) => (named_text(n), false),
                Who::Group(n) => (named_text(n), true),
            };
            let flags = ace.flags | if group { IDENTIFIER_GROUP } else { 0 };
            for word in [ace.kind.wire(), flags, ace.perms, who.len() as u32] {
                out.extend(word.to_be_bytes());
            }
            out.extend(who.as_bytes());
            out.resize(out.len().next_multiple_of(4), 0);
        }
        out
    }
}

fn named_text(named: &Named) -> String {
    match named {
        Named::Id(id) => id.to_string(),
        Named::Name(name) => name.clone(),
    }
}

/// Who the XDR who string `who` is; `group`: it has the IDENTIFIER_GROUP flag. Only a number
/// is taken for an id: a name -- `alice@example.org`, or `root` with a domain not this host's
/// -- is kept as it came, never looked up here, since the same name in another NFSv4 domain
/// is someone else. `None` for an empty one, or a name the text form could not hold.
fn xdr_who(who: &str, group: bool) -> Option<Who> {
    let named = match who {
        "OWNER@" => return Some(Who::Owner),
        "GROUP@" => return Some(Who::OwningGroup),
        "EVERYONE@" => return Some(Who::Everyone),
        _ => match parse_id(who) {
            Some(id) => Named::Id(id),
            None if usable_name(who) => Named::Name(who.to_string()),
            None => return None,
        },
    };
    Some(if group {
        Who::Group(named)
    } else {
        Who::User(named)
    })
}

/// Parse one entry of the text form (`Nfs4Acl::from_ace_text`).
fn parse_ace(entry: &str) -> io::Result<Ace> {
    let bad = || invalid("invalid ACL text");
    let fields: Vec<&str> = entry.split(':').collect();
    let (who, rest) = match fields[..] {
        ["owner@", ref rest @ ..] => (Who::Owner, rest),
        ["group@", ref rest @ ..] => (Who::OwningGroup, rest),
        ["everyone@", ref rest @ ..] => (Who::Everyone, rest),
        [tag @ ("user" | "group"), name, ref rest @ ..] => {
            let group = tag == "group";
            // The id is the last field, after the type.
            let id = match rest.len() {
                3 => None,
                4 => Some(parse_id(rest[3]).ok_or_else(bad)?),
                _ => return Err(bad()),
            };
            let named = named_by(name, id, group).ok_or_else(bad)?;
            let who = if group {
                Who::Group(named)
            } else {
                Who::User(named)
            };
            (who, &rest[..3])
        }
        _ => return Err(bad()),
    };
    let [perms, flags, kind] = rest else {
        return Err(bad());
    };
    let kind = match *kind {
        "allow" => AceKind::Allow,
        "deny" => AceKind::Deny,
        "audit" => AceKind::Audit,
        "alarm" => AceKind::Alarm,
        _ => return Err(bad()),
    };
    Ok(Ace {
        who,
        perms: letters_bits(perms, &PERM_LETTERS).ok_or_else(bad)?,
        flags: letters_bits(flags, &FLAG_LETTERS).ok_or_else(bad)?,
        kind,
    })
}

/// The user (`group`: group) the text names `name`, with the id field `id`: the name looked
/// up, else the id, else the name read as a number, else the name kept. `None` for no name
/// and no id, or a name the text form could not have held (`usable_name`).
fn named_by(name: &str, id: Option<u32>, group: bool) -> Option<Named> {
    if name.is_empty() {
        return id.map(Named::Id);
    }
    if !usable_name(name) {
        return None;
    }
    let found = name_id(name, group).or(id).or_else(|| parse_id(name));
    Some(match found {
        Some(id) => Named::Id(id),
        None => Named::Name(name.to_string()),
    })
}

/// Whether the `system.nfs4_acl` XDR `xdr` says only what a mode can, by gnulib's
/// `acl_nfs4_nontrivial`: at most six entries, each an ALLOW or DENY for `OWNER@`, `GROUP@` or
/// `EVERYONE@` -- at most one of each type for each -- with no flag but IDENTIFIER_GROUP.
/// Anything not read for certain is not trivial.
pub(super) fn nfs4_is_trivial(xdr: &[u8]) -> bool {
    nfs4_trivial(xdr).unwrap_or(false)
}

/// `nfs4_is_trivial`, `None` for XDR that ends early.
fn nfs4_trivial(mut xdr: &[u8]) -> Option<bool> {
    const ALLOW: u32 = 0;
    const DENY: u32 = 1;
    fn word(xdr: &mut &[u8]) -> Option<u32> {
        let (w, rest) = xdr.split_first_chunk::<4>()?;
        *xdr = rest;
        Some(u32::from_be_bytes(*w))
    }
    let count = word(&mut xdr)?;
    if count > 6 {
        return Some(false);
    }
    let mut seen = 0u32;
    for _ in 0..count {
        let (kind, flag, _mask) = (word(&mut xdr)?, word(&mut xdr)?, word(&mut xdr)?);
        let len = usize::try_from(word(&mut xdr)?).ok()?;
        if !matches!(kind, ALLOW | DENY) || flag & !IDENTIFIER_GROUP != 0 {
            return Some(false);
        }
        let who = xdr.get(..len)?;
        xdr = xdr.get(len.div_ceil(4) * 4..)?;
        let who_slot = match who {
            b"OWNER@" => 0,
            b"GROUP@" => 2,
            b"EVERYONE@" => 4,
            _ => return Some(false),
        };
        let bit = 1 << (who_slot | kind);
        if seen & bit != 0 {
            return Some(false);
        }
        seen |= bit;
    }
    Some(true)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Ids nobody has, so that they are written as numbers.
    const UID: u32 = 3_999_999_001;
    const GID: u32 = 3_999_999_002;

    fn ace(who: Who, perms: u32, flags: u32, kind: AceKind) -> Ace {
        Ace {
            who,
            perms,
            flags,
            kind,
        }
    }

    /// libarchive's compact text reads to the ACEs it spells and is written back the same.
    #[test]
    fn libarchive_text_round_trips() {
        let text = format!(
            "owner@:rwxpaARWcCos::allow,user:{UID}:raRcs::allow:{UID},\
             group:{GID}:wpdD:fdinSFI:deny:{GID},group@:rxaRcs::allow,everyone@:raRcs::allow"
        );
        let acl = Nfs4Acl::from_ace_text(&text).unwrap();
        let public = READ_ATTRIBUTES | READ_NAMED_ATTRS | READ_ACL | SYNCHRONIZE;
        let owner = READ_DATA
            | WRITE_DATA
            | EXECUTE
            | APPEND_DATA
            | public
            | WRITE_ATTRIBUTES
            | WRITE_NAMED_ATTRS
            | WRITE_ACL
            | WRITE_OWNER;
        let all_flags = FILE_INHERIT
            | DIRECTORY_INHERIT
            | INHERIT_ONLY
            | NO_PROPAGATE_INHERIT
            | SUCCESSFUL_ACCESS
            | FAILED_ACCESS
            | INHERITED;
        assert_eq!(
            acl.aces,
            [
                ace(Who::Owner, owner, 0, AceKind::Allow),
                ace(
                    Who::User(Named::Id(UID)),
                    READ_DATA | public,
                    0,
                    AceKind::Allow
                ),
                ace(
                    Who::Group(Named::Id(GID)),
                    WRITE_DATA | APPEND_DATA | DELETE | DELETE_CHILD,
                    all_flags,
                    AceKind::Deny
                ),
                ace(
                    Who::OwningGroup,
                    READ_DATA | EXECUTE | public,
                    0,
                    AceKind::Allow
                ),
                ace(Who::Everyone, READ_DATA | public, 0, AceKind::Allow),
            ]
        );
        assert_eq!(acl.to_ace_text().unwrap(), text);
        assert!(!acl.is_trivial());
    }

    /// The long style, `-` padded, and newlines between entries read as the compact style
    /// does; so do audit and alarm entries.
    #[test]
    fn long_style_and_other_kinds_read() {
        let long = format!(
            "owner@:rwxp--aARWcCos:-------:allow\n user:{UID}:r-----a-R-c--s::allow:{UID}\n\
             everyone@:--------------:------I:audit,group@:r:S:alarm,"
        );
        let acl = Nfs4Acl::from_ace_text(&long).unwrap();
        assert_eq!(
            acl.to_ace_text().unwrap(),
            format!(
                "owner@:rwxpaARWcCos::allow,user:{UID}:raRcs::allow:{UID},\
                 everyone@::I:audit,group@:r:S:alarm"
            )
        );
    }

    /// A user is found by name, else by the id field, else by the name read as a number; one
    /// that is none of those is kept by name, written back with no id field. A user with a
    /// name is written by it, the id after.
    #[test]
    fn names_and_ids() {
        let acl = Nfs4Acl::from_ace_text("user:no-such-user-x:r::allow:4321").unwrap();
        assert_eq!(acl.aces[0].who, Who::User(Named::Id(4321)));
        let acl = Nfs4Acl::from_ace_text(&format!("group:{GID}:r::allow")).unwrap();
        assert_eq!(acl.aces[0].who, Who::Group(Named::Id(GID)));
        let acl = Nfs4Acl::from_ace_text(&format!("group::r::allow:{GID}")).unwrap();
        assert_eq!(acl.aces[0].who, Who::Group(Named::Id(GID)));
        let kept = "user:no-such-user-x@example.org:r::deny";
        let acl = Nfs4Acl::from_ace_text(kept).unwrap();
        let name = Named::Name("no-such-user-x@example.org".to_string());
        assert_eq!(acl.aces[0].who, Who::User(name));
        assert_eq!(acl.to_ace_text().unwrap(), kept);

        if let Some(root) = crate::user::lookup_by_uid(0).unwrap() {
            let root = root.name.into_string().unwrap();
            let acl = Nfs4Acl::from_ace_text(&format!("user:{root}:r::allow:77")).unwrap();
            assert_eq!(acl.aces[0].who, Who::User(Named::Id(0)));
            assert_eq!(
                acl.to_ace_text().unwrap(),
                format!("user:{root}:r::allow:0")
            );
        }

        // A name the text cannot hold.
        let bad = Nfs4Acl {
            aces: vec![ace(
                Who::User(Named::Name("a,b".to_string())),
                READ_DATA,
                0,
                AceKind::Allow,
            )],
        };
        assert!(bad.to_ace_text().is_err());
    }

    #[test]
    fn text_rejects() {
        for text in [
            "",
            ",\n,",
            "owner@:rwz::allow",
            "owner@:rwx:q:allow",
            "owner@:rwx:d:permit",
            "owner@:rwx::allow:5",
            "owner@:rwx:allow",
            "bob@:rwx::allow",
            "OWNER@:rwx::allow",
            "user:x:rwx::allow:notnum",
            "user:x:rwx::allow:-1",
            "user:x:rwx::allow:4294967295",
            "user:x:rwx::allow:99999999999",
            "user::rwx::allow",
            "user:x:rwx:allow",
            "user:x:rwx::allow:5:6",
            "group:x",
            "owner@:rwx::allow,bogus",
            "owner@:r\u{e9}::allow",
            "owner@:rwx::all\u{f6}w",
            "user: x:r::allow",
            "user:a\tb:r::allow",
            "user:a#b:r::allow",
        ] {
            assert!(Nfs4Acl::from_ace_text(text).is_err(), "{text:?}");
        }
    }

    /// Nothing an archive can hold makes the parsers panic: a sweep of short strings over the
    /// grammar's alphabet, and of byte strings for the XDR.
    #[test]
    fn hostile_input_never_panics() {
        let alphabet: Vec<char> = "owner@:user:group@,\n-rwxfdIS9 \u{e9}".chars().collect();
        let mut state = 0x2545_f491_u32;
        let mut next = || {
            state ^= state << 13;
            state ^= state >> 17;
            state ^= state << 5;
            state
        };
        for _ in 0..20_000 {
            let len = next() % 40;
            let text: String = (0..len)
                .map(|_| alphabet[next() as usize % alphabet.len()])
                .collect();
            let _ = Nfs4Acl::from_ace_text(&text);
            let bytes: Vec<u8> = (0..len * 2).map(|_| next() as u8 % 8).collect();
            let _ = Nfs4Acl::parse_xdr(&bytes);
            let _ = nfs4_is_trivial(&bytes);
        }
    }

    /// The aces `(type, flag, mask, who)` of an XDR fixture (`xdr`).
    type XdrAces<'a> = &'a [(u32, u32, u32, &'a str)];

    /// The `system.nfs4_acl` XDR of the aces `(type, flag, mask, who)`, laid out by hand as
    /// RFC 7530 has it.
    fn xdr(aces: &[(u32, u32, u32, &str)]) -> Vec<u8> {
        let mut out = (aces.len() as u32).to_be_bytes().to_vec();
        for &(kind, flag, mask, who) in aces {
            for word in [kind, flag, mask, who.len() as u32] {
                out.extend(word.to_be_bytes());
            }
            out.extend(who.as_bytes());
            out.resize(out.len().div_ceil(4) * 4, 0);
        }
        out
    }

    /// The XDR reads to the ACEs it holds and is written back byte for byte: a group by the
    /// IDENTIFIER_GROUP flag, an id as a number, a name with no local user kept.
    #[test]
    fn xdr_round_trip() {
        let fixture = xdr(&[
            (0, 0, 0x0016_01a7, "OWNER@"),
            (1, 0x40, 0x0000_0002, "GROUP@"),
            (0, 0x03, 0x0012_0081, &UID.to_string()),
            (1, 0x40 | 0x08, 0x0001_0040, &GID.to_string()),
            (2, 0x10, 0x0000_0001, "no-such-user-x@example.org"),
            (3, 0x20 | 0x80, 0x0000_0020, "EVERYONE@"),
        ]);
        let acl = Nfs4Acl::parse_xdr(&fixture).unwrap();
        assert_eq!(
            acl.aces,
            [
                ace(Who::Owner, 0x0016_01a7, 0, AceKind::Allow),
                ace(Who::OwningGroup, WRITE_DATA, 0, AceKind::Deny),
                ace(
                    Who::User(Named::Id(UID)),
                    READ_DATA | READ_ATTRIBUTES | READ_ACL | SYNCHRONIZE,
                    FILE_INHERIT | DIRECTORY_INHERIT,
                    AceKind::Allow
                ),
                ace(
                    Who::Group(Named::Id(GID)),
                    DELETE | DELETE_CHILD,
                    INHERIT_ONLY,
                    AceKind::Deny
                ),
                ace(
                    Who::User(Named::Name("no-such-user-x@example.org".to_string())),
                    READ_DATA,
                    SUCCESSFUL_ACCESS,
                    AceKind::Audit
                ),
                ace(
                    Who::Everyone,
                    EXECUTE,
                    FAILED_ACCESS | INHERITED,
                    AceKind::Alarm
                ),
            ]
        );
        assert_eq!(acl.to_xdr(), fixture);
        // Through the text and back.
        let text = acl.to_ace_text().unwrap();
        assert_eq!(Nfs4Acl::from_ace_text(&text).unwrap().to_xdr(), fixture);
        // No entries at all.
        assert_eq!(Nfs4Acl::parse_xdr(&xdr(&[])).unwrap(), Nfs4Acl::default());
    }

    /// A who in some NFSv4 domain is not taken for the local user of the same name: only a
    /// number is an id, and a name, with its domain or without, is kept as it came.
    #[test]
    fn xdr_names_are_kept_not_mapped() {
        for who in ["root@foreign.example", "root", "nobody@localdomain"] {
            let acl = Nfs4Acl::parse_xdr(&xdr(&[(0, 0, 1, who)])).unwrap();
            assert_eq!(
                acl.aces[0].who,
                Who::User(Named::Name(who.to_string())),
                "{who}"
            );
            assert_eq!(acl.to_xdr(), xdr(&[(0, 0, 1, who)]));
        }
        let acl = Nfs4Acl::parse_xdr(&xdr(&[(0, 0x40, 1, "0")])).unwrap();
        assert_eq!(acl.aces[0].who, Who::Group(Named::Id(0)));
    }

    /// More entries than any filesystem holds make a malformed ACL, refused before any name
    /// is looked up.
    #[test]
    fn text_entries_are_capped() {
        let at_cap = vec!["everyone@:r::allow"; MAX_ACES].join(",");
        assert_eq!(
            Nfs4Acl::from_ace_text(&at_cap).unwrap().aces.len(),
            MAX_ACES
        );
        let over = format!("{at_cap},owner@:r::allow");
        assert!(Nfs4Acl::from_ace_text(&over).is_err());
    }

    #[test]
    fn xdr_rejects() {
        let good = xdr(&[(0, 0, 1, "OWNER@")]);
        assert!(Nfs4Acl::parse_xdr(&good).is_ok());
        let mut torn = good.clone();
        torn.pop();
        let mut trailing = good.clone();
        trailing.extend([0, 0, 0, 0]);
        let mut too_many = good.clone();
        too_many[3] = 2;
        let mut huge = good.clone();
        huge[..4].copy_from_slice(&u32::MAX.to_be_bytes());
        let mut long_who = good.clone();
        long_who[19] = 0xff;
        for bytes in [
            Vec::new(),
            vec![0, 0],
            torn,
            trailing,
            too_many,
            huge,
            long_who,
            xdr(&[(4, 0, 1, "OWNER@")]),
            xdr(&[(0, 0x100, 1, "OWNER@")]),
            xdr(&[(0, 0, 0x200, "OWNER@")]),
            xdr(&[(0, 0, 1, "")]),
            xdr(&[(0, 0, 1, "a,b")]),
            xdr(&[(0, 0, 1, "a b")]),
            xdr(&[(0, 0, 1, "\u{0}")]),
            {
                let mut bad_utf8 = xdr(&[(0, 0, 1, "ab")]);
                bad_utf8[20] = 0xff;
                bad_utf8
            },
        ] {
            assert!(Nfs4Acl::parse_xdr(&bytes).is_err(), "{bytes:?}");
        }
    }

    /// Trivial as gnulib has it, read from the XDR or from the structure alike.
    #[test]
    fn trivial_agrees_with_the_xdr_check() {
        let cases: [(XdrAces, bool); 7] = [
            (&[(0, 0, 7, "OWNER@"), (0, 0x40, 1, "GROUP@")], true),
            (
                &[
                    (1, 0, 7, "OWNER@"),
                    (0, 0, 7, "OWNER@"),
                    (1, 0x40, 1, "GROUP@"),
                    (0, 0x40, 1, "GROUP@"),
                    (1, 0, 1, "EVERYONE@"),
                    (0, 0, 1, "EVERYONE@"),
                ],
                true,
            ),
            (&[(0, 0, 7, &UID.to_string())], false),
            (&[(2, 0, 7, "OWNER@")], false),
            (&[(0, 1, 7, "OWNER@")], false),
            (&[(0, 0, 7, "OWNER@"), (0, 0, 1, "OWNER@")], false),
            (&[(0, 0, 7, "OWNER@"); 7], false),
        ];
        for (aces, trivial) in cases {
            let bytes = xdr(aces);
            assert_eq!(nfs4_is_trivial(&bytes), trivial, "{aces:?}");
            let acl = Nfs4Acl::parse_xdr(&bytes).unwrap();
            assert_eq!(acl.is_trivial(), trivial, "{aces:?}");
        }
    }

    fn text(text: &str) -> Nfs4Acl {
        Nfs4Acl::from_ace_text(text).unwrap()
    }

    /// An ACL says only what the mode does where it grants each class just the mode's read,
    /// write and execute; what libarchive writes for any mode does.
    #[test]
    fn which_acls_say_only_the_mode() {
        for mode in 0..0o1000 {
            let implied = Nfs4Acl::default().with_mode_aces(mode);
            assert!(implied.says_only_mode(mode), "{mode:o}");
            assert!(implied.specials_said_by_mode(mode), "{mode:o}");
        }
        let plain = text("owner@:rw::allow,group@:r::allow,everyone@:r::allow");
        assert!(plain.says_only_mode(0o644));
        assert!(!plain.says_only_mode(0o640));
        assert!(!plain.says_only_mode(0o664));
        // gnulib's trivial, but others are denied what the mode grants them.
        let deny = text("everyone@:r::deny");
        assert!(deny.is_trivial());
        assert!(!deny.says_only_mode(0o644));
        assert!(deny.says_only_mode(0o000));
        // Order decides: an owner@ allow before the everyone@ deny.
        assert!(text("owner@:r::allow,everyone@:r::deny").says_only_mode(0o400));
        assert!(!text("everyone@:r::deny,owner@:r::allow").says_only_mode(0o400));
        // Someone named, a flag, an audit entry.
        assert!(!text(&format!("user:{UID}:r::allow:{UID}")).says_only_mode(0o444));
        assert!(!text("owner@:r:f:allow").says_only_mode(0o400));
        assert!(!text("owner@:r::audit").says_only_mode(0o000));
    }

    /// Anyone but the owner allowed to write, append, or change the ACL or the owner may change
    /// the file; a deny, the owner, or reading does not.
    #[test]
    fn who_may_change_the_file() {
        for may in [
            format!("user:{UID}:w::allow:{UID}"),
            format!("group:{GID}:p::allow:{GID}"),
            "group@:C::allow".to_string(),
            "everyone@:o::allow".to_string(),
        ] {
            assert!(text(&may).lets_others_change(), "{may}");
        }
        for may_not in [
            format!("user:{UID}:w::deny:{UID}"),
            format!("user:{UID}:raxRcAWs::allow:{UID}"),
            "owner@:rwpCo::allow".to_string(),
        ] {
            assert!(!text(&may_not).lets_others_change(), "{may_not}");
        }
    }

    /// macOS has no place for `owner@`, `group@` and `everyone@`: leaving one out loses nothing
    /// only where it is a plain allow, or a plain deny after every named entry that the mode
    /// repeats.
    #[test]
    fn which_special_entries_macos_may_leave_out() {
        let named = format!("user:{UID}:w::allow:{UID}");
        // The deny would have stopped a member of the owning group the named entry allows.
        let before = text(&format!("group@:w::deny,{named}"));
        assert!(!before.specials_said_by_mode(0o664));
        assert!(!before.specials_said_by_mode(0o644));
        // After every named entry, a deny the mode repeats, and one it does not.
        let after = text(&format!("{named},group@:w::deny"));
        assert!(after.specials_said_by_mode(0o644));
        assert!(!after.specials_said_by_mode(0o664));
        let everyone = text(&format!("{named},everyone@:x::deny"));
        assert!(everyone.specials_said_by_mode(0o644));
        assert!(!everyone.specials_said_by_mode(0o744));
        // A deny of more than read, write and execute; a flag; audit and alarm entries.
        assert!(!text(&format!("{named},group@:C::deny")).specials_said_by_mode(0o600));
        assert!(!text(&format!("{named},owner@:r:fd:allow")).specials_said_by_mode(0o600));
        assert!(!text(&format!("{named},everyone@:r::audit")).specials_said_by_mode(0o600));
        assert!(!text(&format!("{named},owner@:r::alarm")).specials_said_by_mode(0o600));
        // Allows are said by the mode, wherever they are.
        assert!(
            text(&format!("owner@:rwx::allow,{named},everyone@:r::allow"))
                .specials_said_by_mode(0o600)
        );
    }

    /// The entries libarchive adds to say what the mode does on an ACL read on macOS.
    #[test]
    fn mode_aces_follow_libarchive() {
        let acl = Nfs4Acl::default();
        assert_eq!(
            acl.with_mode_aces(0o754).to_ace_text().unwrap(),
            "owner@:rwxpaARWcCos::allow,group@:rxaRcs::allow,everyone@:raRcs::allow"
        );
        assert_eq!(
            acl.with_mode_aces(0o604).to_ace_text().unwrap(),
            "owner@:r::allow,group@:r::deny,owner@:rwpaARWcCos::allow,\
             group@:aRcs::allow,everyone@:raRcs::allow"
        );
        assert_eq!(
            acl.with_mode_aces(0o070).to_ace_text().unwrap(),
            "owner@:rwxp::deny,owner@:aARWcCos::allow,group@:rwxpaRcs::allow,everyone@:aRcs::allow"
        );
    }
}
