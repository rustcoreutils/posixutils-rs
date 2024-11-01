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

The `find` test `find_mtime_test` is hardcoded to use a specific
date offset.  This was increased to 7000 days to temporarily avoid
test failure.  The test should be improved.

