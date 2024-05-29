define(`reverse', `errprint(args($#:`$@'))ifelse(`$#', `0', , `$#', `1', ``$1'',`reverse(shift($@)), `$1'')')
reverse(`a', `b', `c')
