dnl for(iter, start, end, output)
define(`for', `pushdef(`$1', `$2')_for($@)popdef(`$1')')dnl
define(`_for', `$4`'ifelse($1, `$3', `', `define(`$1', incr($1))$0($@)')')dnl
for(`i', `1', `8', `i ')
