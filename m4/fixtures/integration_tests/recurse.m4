define(`recurse', `errprint($1 )ifelse($1, 1, `finished', `recurse(incr($1))')')recurse(0)
