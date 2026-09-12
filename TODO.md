# General TODO and future implementation notes

## Translations

* Standard OS error texts must be translated
* Clap error messages must be translated

## OS errors

* OS error messaging: many errors fail to conform to the standard of
```
	filename: OS error message
```

## Other items

**make**: posixutils' standard is to _not_ use the src/ directory that
is standard for Rust binaries.  Update `make` to remove the src/
directory by moving files within the repo.

