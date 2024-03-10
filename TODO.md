# General TODO and future implementation notes

## Translations

* Standard OS error texts must be translated

## OS errors

* OS error messaging: many errors fail to conform to the standard of
```
	filename: OS error message
```

## Testing

Tests hardcode `release/` directory for binaries.  Should instead detect
--release and run the debug/release tests accordingly.

