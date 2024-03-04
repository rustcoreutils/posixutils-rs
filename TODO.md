# General TODO and future implementation notes

## Translations

* Standard OS error texts must be translated

## OS errors

* OS error messaging: many errors fail to conform to the standard of
```
	filename: OS error message
```

* `from_raw_os_error()` is probably pulling the wrong value and should
  query errno

