define(`reverse', `ifelse(`$#', `0', , `$#', `1', ``$1'',`reverse(shift($@)), `$1'')')
reverse(`a')
reverse(`a', `b', `c')
