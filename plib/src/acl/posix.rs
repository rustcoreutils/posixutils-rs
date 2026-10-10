//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX.1e ACLs: the Linux xattr form and the text form star writes.

use super::invalid;
use std::io;

/// The most entries an ACL has: what the 64 KiB a Linux xattr holds can, at eight bytes each
/// after the four-byte header.
pub const MAX_ENTRIES: usize = (65536 - 4) / 8;

/// Who a POSIX ACL entry is about. The named forms carry the uid or gid.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Tag {
    UserObj,
    User(u32),
    GroupObj,
    Group(u32),
    Mask,
    Other,
}

/// One entry of a POSIX ACL: who, and the permissions (`r` 4, `w` 2, `x` 1, as in a mode).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Entry {
    pub tag: Tag,
    pub perm: u8,
}

/// A POSIX.1e ACL, an access or a default one: its entries in the order the kernel keeps
/// them -- owner, named users by uid, owning group, named groups by gid, mask, others.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PosixAcl {
    pub entries: Vec<Entry>,
}

/// `ACL_UNDEFINED_ID`: the id field of an entry that names nobody.
const UNDEFINED_ID: u32 = u32::MAX;
/// The only version of the Linux xattr format.
const XATTR_VERSION: u32 = 2;
/// The xattr tag values (`ACL_USER_OBJ` ...).
const USER_OBJ: u16 = 0x01;
const USER: u16 = 0x02;
const GROUP_OBJ: u16 = 0x04;
const GROUP: u16 = 0x08;
const MASK: u16 = 0x10;
const OTHER: u16 = 0x20;

impl PosixAcl {
    /// Whether it has only the entries the mode shows: owner, owning group, others.
    pub fn is_trivial(&self) -> bool {
        self.entries
            .iter()
            .all(|e| matches!(e.tag, Tag::UserObj | Tag::GroupObj | Tag::Other))
    }

    /// What a file made with the permission bits `mode` in a directory whose default ACL this
    /// is takes, by Linux's `posix_acl_create`: this ACL with the owner's, the mask's (the owning
    /// group's, where there is no mask) and others' entries masked by the matching bits of
    /// `mode`, and the permission bits that then show it. The umask plays no part.
    pub fn inherited(&self, mode: u32) -> (PosixAcl, u32) {
        let has_mask = self.entries.iter().any(|e| e.tag == Tag::Mask);
        let mut acl = self.clone();
        let mut bits = 0;
        for e in &mut acl.entries {
            let shift = match e.tag {
                Tag::UserObj => 6,
                Tag::Mask => 3,
                Tag::GroupObj if !has_mask => 3,
                Tag::Other => 0,
                _ => continue,
            };
            // At most `rwx`: three bits.
            e.perm &= ((mode >> shift) & 7) as u8;
            bits |= u32::from(e.perm) << shift;
        }
        (acl, bits)
    }

    /// This ACL with the entries a mode shows set to the bits of `mode`, as `chmod` sets them
    /// (Linux's `posix_acl_chmod`): the owner's, the mask's -- the owning group's where there is
    /// no mask -- and others'. Named entries, and the owning group's under a mask, are kept.
    pub fn with_mode(&self, mode: u32) -> PosixAcl {
        let has_mask = self.entries.iter().any(|e| e.tag == Tag::Mask);
        let mut acl = self.clone();
        for e in &mut acl.entries {
            let shift = match e.tag {
                Tag::UserObj => 6,
                Tag::Mask => 3,
                Tag::GroupObj if !has_mask => 3,
                Tag::Other => 0,
                _ => continue,
            };
            // At most `rwx`: three bits.
            e.perm = ((mode >> shift) & 7) as u8;
        }
        acl
    }

    /// Whether it names a user or group besides the owner and owning group.
    pub(super) fn names_others(&self) -> bool {
        self.entries
            .iter()
            .any(|e| matches!(e.tag, Tag::User(_) | Tag::Group(_)))
    }

    /// Parse the Linux xattr form: a little-endian 32-bit version (2), then 8-byte entries of
    /// a 16-bit tag, a 16-bit permission set and a 32-bit id. No entries at all is the empty
    /// ACL, which stands for none; any other ACL must be one the kernel accepts (`check`).
    pub fn parse_xattr(bytes: &[u8]) -> io::Result<PosixAcl> {
        let Some((version, rest)) = bytes.split_first_chunk::<4>() else {
            return Err(invalid("truncated ACL"));
        };
        if u32::from_le_bytes(*version) != XATTR_VERSION {
            return Err(invalid("unknown ACL version"));
        }
        let (chunks, torn) = rest.as_chunks::<8>();
        if !torn.is_empty() {
            return Err(invalid("truncated ACL"));
        }
        let mut entries = Vec::with_capacity(chunks.len());
        for chunk in chunks {
            let tag = u16::from_le_bytes([chunk[0], chunk[1]]);
            let perm = u16::from_le_bytes([chunk[2], chunk[3]]);
            let id = u32::from_le_bytes([chunk[4], chunk[5], chunk[6], chunk[7]]);
            let tag = match tag {
                USER_OBJ => Tag::UserObj,
                USER => Tag::User(id),
                GROUP_OBJ => Tag::GroupObj,
                GROUP => Tag::Group(id),
                MASK => Tag::Mask,
                OTHER => Tag::Other,
                _ => return Err(invalid("unknown ACL entry tag")),
            };
            let perm = u8::try_from(perm)
                .ok()
                .filter(|p| p & !7 == 0)
                .ok_or_else(|| invalid("invalid ACL permissions"))?;
            entries.push(Entry { tag, perm });
        }
        let acl = PosixAcl { entries };
        if !acl.entries.is_empty() {
            acl.check()?;
        }
        Ok(acl)
    }

    /// The Linux xattr form (`parse_xattr`).
    pub fn to_xattr(&self) -> Vec<u8> {
        let mut out = Vec::with_capacity(4 + 8 * self.entries.len());
        out.extend(XATTR_VERSION.to_le_bytes());
        for e in &self.entries {
            let (tag, id) = match e.tag {
                Tag::UserObj => (USER_OBJ, UNDEFINED_ID),
                Tag::User(uid) => (USER, uid),
                Tag::GroupObj => (GROUP_OBJ, UNDEFINED_ID),
                Tag::Group(gid) => (GROUP, gid),
                Tag::Mask => (MASK, UNDEFINED_ID),
                Tag::Other => (OTHER, UNDEFINED_ID),
            };
            out.extend(tag.to_le_bytes());
            out.extend(u16::from(e.perm).to_le_bytes());
            out.extend(id.to_le_bytes());
        }
        out
    }

    /// Fail unless the kernel would take it (`posix_acl_valid`): owner, named users in
    /// ascending uid order, owning group, named groups in ascending gid order, then a mask --
    /// required once anyone is named -- and others; each once, no permission beyond `rwx`.
    pub fn check(&self) -> io::Result<()> {
        // The entries in order are exactly what `Tag`'s ordering sorts, each once.
        let sorted = self.entries.windows(2).all(|w| w[0].tag < w[1].tag);
        let has = |tag| self.entries.iter().any(|e| e.tag == tag);
        let complete = has(Tag::UserObj) && has(Tag::GroupObj) && has(Tag::Other);
        if !sorted || !complete || self.entries.iter().any(|e| e.perm & !7 != 0) {
            return Err(invalid("invalid ACL"));
        }
        if self.names_others() && !has(Tag::Mask) {
            return Err(invalid("ACL names a user or group without a mask"));
        }
        Ok(())
    }

    /// The text form star and libarchive put in a pax archive (`SCHILY.acl.access`): entries
    /// separated by commas, a named one with its id as a fourth field --
    /// `user::rw-,user:alice:r--:1000,group::r--,mask::r--,other::r--`. A user or group with
    /// no name, or one the form cannot hold, is named by its number.
    pub fn to_text(&self) -> String {
        let fields: Vec<String> = self.entries.iter().map(entry_text).collect();
        fields.join(",")
    }

    /// Parse the text form (`to_text`), or the same without the id fields, entries separated
    /// by commas or newlines, the tags spelled out or by their first letter. A name is looked
    /// up first; one that is not known is taken from the id field, else read as a number.
    /// The entries are put in the kernel's order, and must make an ACL it takes (`check`); more
    /// than `MAX_ENTRIES` make none.
    pub fn from_text(text: &str) -> io::Result<PosixAcl> {
        let mut entries = Vec::new();
        for field in text.split([',', '\n']).map(str::trim) {
            if field.is_empty() {
                continue;
            }
            // Refused before the entry's name is looked up.
            if entries.len() == MAX_ENTRIES {
                return Err(invalid("ACL has too many entries"));
            }
            entries.push(parse_entry(field)?);
        }
        entries.sort_by_key(|e: &Entry| e.tag);
        let acl = PosixAcl { entries };
        acl.check()?;
        Ok(acl)
    }
}

fn perm_text(perm: u8) -> String {
    [(4, 'r'), (2, 'w'), (1, 'x')]
        .iter()
        .map(|&(bit, c)| if perm & bit != 0 { c } else { '-' })
        .collect()
}

fn entry_text(e: &Entry) -> String {
    let perm = perm_text(e.perm);
    // A name the text form can carry: UTF-8, not empty, holding no separator.
    let usable = |name: std::ffi::OsString| {
        name.into_string()
            .ok()
            .filter(|n| !n.is_empty() && !n.contains([',', ':', '\n']))
    };
    let named = |kind: &str, id: u32, name: Option<String>| {
        let name = name.unwrap_or_else(|| id.to_string());
        format!("{kind}:{name}:{perm}:{id}")
    };
    match e.tag {
        Tag::UserObj => format!("user::{perm}"),
        Tag::User(uid) => {
            let name = crate::user::lookup_by_uid(uid).ok().flatten();
            named("user", uid, name.and_then(|u| usable(u.name)))
        }
        Tag::GroupObj => format!("group::{perm}"),
        Tag::Group(gid) => {
            let name = crate::group::lookup_by_gid(gid).ok().flatten();
            named("group", gid, name.and_then(|g| usable(g.name)))
        }
        Tag::Mask => format!("mask::{perm}"),
        Tag::Other => format!("other::{perm}"),
    }
}

fn parse_entry(field: &str) -> io::Result<Entry> {
    let bad = || invalid("invalid ACL text");
    let parts: Vec<&str> = field.split(':').collect();
    let (kind, qualifier, perm, id) = match parts[..] {
        [kind, qualifier, perm] => (kind, qualifier, perm, None),
        [kind, qualifier, perm, id] => (kind, qualifier, perm, Some(id)),
        _ => return Err(bad()),
    };
    let mut bits = 0u8;
    for c in perm.chars() {
        bits |= match c {
            'r' => 4,
            'w' => 2,
            'x' => 1,
            '-' => 0,
            _ => return Err(bad()),
        };
    }
    let id = id
        .map(|id| id.parse::<u32>().map_err(|_| bad()))
        .transpose()?;
    let resolve = |lookup: &dyn Fn(&str) -> Option<u32>| {
        lookup(qualifier)
            .or(id)
            .or_else(|| qualifier.parse::<u32>().ok())
            .filter(|&id| id != UNDEFINED_ID)
            .ok_or_else(bad)
    };
    let tag = match (kind, qualifier.is_empty()) {
        ("user" | "u", true) => Tag::UserObj,
        ("user" | "u", false) => Tag::User(resolve(&|name| {
            crate::user::lookup_by_name(name)
                .ok()
                .flatten()
                .map(|u| u.uid)
        })?),
        ("group" | "g", true) => Tag::GroupObj,
        ("group" | "g", false) => Tag::Group(resolve(&|name| {
            crate::group::lookup_by_name(name)
                .ok()
                .flatten()
                .map(|g| g.gid)
        })?),
        ("mask" | "m", true) => Tag::Mask,
        ("other" | "o", true) => Tag::Other,
        _ => return Err(bad()),
    };
    Ok(Entry { tag, perm: bits })
}

#[cfg(test)]
pub(super) mod tests {
    use super::*;

    /// The xattr bytes of the entries `(tag, perm, id)`.
    fn xattr(entries: &[(u16, u16, u32)]) -> Vec<u8> {
        let mut out = 2u32.to_le_bytes().to_vec();
        for &(tag, perm, id) in entries {
            out.extend(tag.to_le_bytes());
            out.extend(perm.to_le_bytes());
            out.extend(id.to_le_bytes());
        }
        out
    }

    const NONE: u32 = u32::MAX;

    /// An ACL with a named user and group, and a mask.
    pub(in crate::acl) fn full() -> PosixAcl {
        let e = |tag, perm| Entry { tag, perm };
        PosixAcl {
            entries: vec![
                e(Tag::UserObj, 6),
                e(Tag::User(1000), 4),
                e(Tag::User(1001), 7),
                e(Tag::GroupObj, 4),
                e(Tag::Group(50), 5),
                e(Tag::Mask, 7),
                e(Tag::Other, 0),
            ],
        }
    }

    /// The xattr form is the kernel's, byte for byte, and reads back as written.
    #[test]
    fn xattr_round_trip() {
        let acl = full();
        let bytes = acl.to_xattr();
        assert_eq!(
            bytes,
            xattr(&[
                (0x01, 6, NONE),
                (0x02, 4, 1000),
                (0x02, 7, 1001),
                (0x04, 4, NONE),
                (0x08, 5, 50),
                (0x10, 7, NONE),
                (0x20, 0, NONE),
            ])
        );
        assert_eq!(PosixAcl::parse_xattr(&bytes).unwrap(), acl);
        assert!(!acl.is_trivial());
        // The id of an entry that names nobody is not kept.
        let minimal = xattr(&[(0x01, 7, 0), (0x04, 5, 0), (0x20, 5, 0)]);
        let parsed = PosixAcl::parse_xattr(&minimal).unwrap();
        assert!(parsed.is_trivial());
        assert_eq!(
            parsed.to_xattr(),
            xattr(&[(1, 7, NONE), (4, 5, NONE), (0x20, 5, NONE)])
        );
        // No entries: the empty ACL, which stands for none.
        assert!(PosixAcl::parse_xattr(&xattr(&[]))
            .unwrap()
            .entries
            .is_empty());
    }

    /// What the kernel would refuse is refused.
    #[test]
    fn xattr_validation_rejects() {
        let base = [(0x01, 6, NONE), (0x04, 4, NONE), (0x20, 4, NONE)];
        assert!(PosixAcl::parse_xattr(&xattr(&base)).is_ok());
        let mut other_version = xattr(&base);
        other_version[0] = 3;
        let mut torn = xattr(&base);
        torn.pop();
        let bad: [Vec<u8>; 12] = [
            other_version,
            torn,
            vec![2, 0],
            // Unsorted: group before user, named users out of order.
            xattr(&[(0x04, 4, NONE), (0x01, 6, NONE), (0x20, 4, NONE)]),
            xattr(&[
                (0x01, 6, NONE),
                (0x02, 4, 9),
                (0x02, 4, 8),
                (0x04, 4, NONE),
                (0x10, 4, NONE),
                (0x20, 4, NONE),
            ]),
            // A named user twice; the owner twice.
            xattr(&[
                (0x01, 6, NONE),
                (0x02, 4, 9),
                (0x02, 4, 9),
                (0x04, 4, NONE),
                (0x10, 4, NONE),
                (0x20, 4, NONE),
            ]),
            xattr(&[
                (0x01, 6, NONE),
                (0x01, 6, NONE),
                (0x04, 4, NONE),
                (0x20, 4, NONE),
            ]),
            // A named group with no mask.
            xattr(&[
                (0x01, 6, NONE),
                (0x04, 4, NONE),
                (0x08, 4, 5),
                (0x20, 4, NONE),
            ]),
            // No others; an unknown tag; a permission beyond rwx.
            xattr(&[(0x01, 6, NONE), (0x04, 4, NONE)]),
            xattr(&[
                (0x01, 6, NONE),
                (0x04, 4, NONE),
                (0x40, 4, NONE),
                (0x20, 4, NONE),
            ]),
            xattr(&[(0x01, 8, NONE), (0x04, 4, NONE), (0x20, 4, NONE)]),
            // A mask after others.
            xattr(&[
                (0x01, 6, NONE),
                (0x04, 4, NONE),
                (0x20, 4, NONE),
                (0x10, 4, NONE),
            ]),
        ];
        for bytes in bad {
            assert!(PosixAcl::parse_xattr(&bytes).is_err(), "{bytes:?}");
        }
        // A mask with nobody named is allowed.
        let masked = [
            (0x01, 6, NONE),
            (0x04, 4, NONE),
            (0x10, 4, NONE),
            (0x20, 4, NONE),
        ];
        assert!(PosixAcl::parse_xattr(&xattr(&masked)).is_ok());
    }

    /// The text form names the users and groups it can, gives the id as a fourth field, and
    /// reads back to the same ACL.
    #[test]
    fn text_round_trip() {
        // Ids nobody has: named by number.
        let e = |tag, perm| Entry { tag, perm };
        let unknown = PosixAcl {
            entries: vec![
                e(Tag::UserObj, 6),
                e(Tag::User(3_999_999_001), 4),
                e(Tag::GroupObj, 4),
                e(Tag::Group(3_999_999_002), 3),
                e(Tag::Mask, 7),
                e(Tag::Other, 1),
            ],
        };
        let text = unknown.to_text();
        assert_eq!(
            text,
            "user::rw-,user:3999999001:r--:3999999001,group::r--,\
             group:3999999002:-wx:3999999002,mask::rwx,other::--x"
        );
        assert_eq!(PosixAcl::from_text(&text).unwrap(), unknown);

        // A user with a name: the name, then the id.
        let root = crate::user::lookup_by_uid(0).unwrap().map(|u| u.name);
        if let Some(name) = root.and_then(|n| n.into_string().ok()) {
            let acl = PosixAcl {
                entries: vec![
                    e(Tag::UserObj, 7),
                    e(Tag::User(0), 5),
                    e(Tag::GroupObj, 5),
                    e(Tag::Mask, 5),
                    e(Tag::Other, 0),
                ],
            };
            let text = acl.to_text();
            assert!(text.contains(&format!("user:{name}:r-x:0,")), "{text}");
            assert_eq!(PosixAcl::from_text(&text).unwrap(), acl);
        }

        // Without id fields, by first letters, out of order, a line each.
        let acl = PosixAcl::from_text("o::r--\nm::rw-\nu:3999999001:rw-\ng::r--\nu::rwx").unwrap();
        assert_eq!(
            acl.to_text(),
            "user::rwx,user:3999999001:rw-:3999999001,group::r--,mask::rw-,other::r--"
        );
        // A name nobody has, with its id: the id.
        let acl = PosixAcl::from_text(
            "user::rw-,user:no-such-user-x:r--:4321,group::r--,mask::r--,other::---",
        )
        .unwrap();
        assert_eq!(acl.entries[1].tag, Tag::User(4321));
    }

    /// `posix_acl_create`: the owner, mask and others entries are masked by the mode, a named
    /// entry is not, and the owning group's is masked only where there is no mask.
    #[test]
    fn inherited_masks_by_the_creating_mode() {
        let default = PosixAcl::from_text("u::rwx,u:3999999001:rwx,g::r-x,m::rwx,o::---").unwrap();
        let (acl, bits) = default.inherited(0o754);
        assert_eq!(bits, 0o750);
        let want = PosixAcl::from_text("u::rwx,u:3999999001:rwx,g::r-x,m::r-x,o::---").unwrap();
        assert_eq!(acl, want);

        let (acl, bits) = default.inherited(0o600);
        assert_eq!(bits, 0o600);
        let want = PosixAcl::from_text("u::rw-,u:3999999001:rwx,g::r-x,m::---,o::---").unwrap();
        assert_eq!(acl, want);

        let plain = PosixAcl::from_text("u::rwx,g::rwx,o::r-x").unwrap();
        let (acl, bits) = plain.inherited(0o751);
        assert_eq!(bits, 0o751);
        assert_eq!(acl, PosixAcl::from_text("u::rwx,g::r-x,o::--x").unwrap());
    }

    /// More entries than the xattr form can hold make a malformed ACL, refused before any
    /// name is looked up.
    #[test]
    fn text_entries_are_capped() {
        let mut fields = vec!["u::rw-", "g::r--", "m::r--", "o::---"];
        let named: Vec<String> = (0..MAX_ENTRIES)
            .map(|i| format!("u:{}:r--", 3_000_000_000 + i))
            .collect();
        fields.extend(named.iter().map(String::as_str));
        let text = fields.join(",");
        assert!(PosixAcl::from_text(&text).is_err());
    }

    #[test]
    fn text_rejects() {
        for text in [
            "",
            "user::rw-,group::r--",
            "user::rw-,group::r--,other::r--,other::r--",
            "user::rwz,group::r--,other::r--",
            "user::rw-,group::r--,other:x:r--",
            "user::rw-,group::r--,other::r--:5:6",
            "user::rw-,user:no-such-user-x:r--,group::r--,mask::r--,other::---",
            "user::rw-,user:5:r--:x,group::r--,mask::r--,other::---",
            "user::rw-,user:5:r--,group::r--,other::---",
            "everyone::rw-,group::r--,other::r--",
        ] {
            assert!(PosixAcl::from_text(text).is_err(), "{text:?}");
        }
    }

    /// The entries a mode shows follow the mode, as `chmod` sets them: the owner's, the mask's
    /// (the owning group's where there is no mask) and others'; named entries are untouched.
    #[test]
    fn with_mode_sets_what_the_mode_shows() {
        let wide =
            PosixAcl::from_text("user::rwx,user:7:rwx,group::rwx,mask::rwx,other::rwx").unwrap();
        let narrowed = PosixAcl::from_text("user::rwx,user:7:rwx,group::rwx,mask::---,other::---");
        assert_eq!(wide.with_mode(0o4700), narrowed.unwrap());
        let plain = PosixAcl::from_text("user::rwx,group::rwx,other::rwx").unwrap();
        let narrowed = PosixAcl::from_text("user::r--,group::r-x,other::--x").unwrap();
        assert_eq!(plain.with_mode(0o451), narrowed);
    }
}
