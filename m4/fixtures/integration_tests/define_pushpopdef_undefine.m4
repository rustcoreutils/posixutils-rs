hello(friend)
define(`hello',hi $1)dnl
hello(friend)
pushdef(`hello',goodbye $1)dnl
hello(friend)
popdef(`hello')dnl
hello(friend)
popdef(`hello')dnl
hello(friend)
pushdef(`hello',hi $1)dnl
pushdef(`hello',hello $1)dnl
define(`hello',wow $1)dnl
define(`hello',wow $1)dnl
hello(friend)
popdef(`hello')dnl
hello(friend)
pushdef(`hello',hi $1)dnl
pushdef(`hello',hello $1)dnl
undefine(`hello')dnl
hello(friend)
