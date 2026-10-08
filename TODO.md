# General TODO and future implementation notes

## Translations

* Standard OS error texts must be translated
* Clap error messages must be translated

## ACL support

Support access control lists throughout every utility that copies, moves,
archives, lists or changes file permissions. This comes after outstanding
fixes. Today no utility reads or writes ACLs.

* Kinds: POSIX.1e ACLs (Linux, FreeBSD UFS), NFSv4 ACLs (macOS, FreeBSD
  ZFS, NFSv4 mounts) and CIFS ACLs.
* cp `-p`/`-a`, and mv across devices: copy the source's ACL, as GNU and
  macOS cp do. Today an ACL on an existing destination directory survives
  `-p` unchanged.
* pax: store ACLs in the pax format's extended headers, and restore them
  under `-p`.
* ls: mark a file that has an ACL (POSIX's alternate access method `+`).
* chmod: keep the ACL mask consistent when changing group permission bits.
* One implementation in plib, beside the ACL check `plib::madefs` already
  makes for its trust rules.

## Other items

**make**: posixutils' standard is to _not_ use the src/ directory that
is standard for Rust binaries.  Update `make` to remove the src/
directory by moving files within the repo.

