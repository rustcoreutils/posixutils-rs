# General TODO and future implementation notes

## Upstream fixes wanted

* libc: missing confstr support on linux

## Translations

* Standard OS error texts must be translated
* Clap error messages must be translated

## OS errors

* OS error messaging: many errors fail to conform to the standard of
```
	filename: OS error message
```

## Other items

**find**: The `find` test `find_mtime_test` is hardcoded to use a specific
date offset.  This was increased to 7000 days to temporarily avoid
test failure.  The test should be improved.

**make**: posixutils' standard is to _not_ use the src/ directory that
is standard for Rust binaries.  Update `make` to remove the src/
directory by moving files within the repo.

