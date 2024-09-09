1 hello(friend)
define(`hello',hi $1)dnl
2 hello(friend)
pushdef(`hello',goodbye $1)dnl
3 hello(friend)
popdef(`hello')dnl
4 hello(friend)
popdef(`hello')dnl
5 hello(friend)
pushdef(`hello',hi $1)dnl
pushdef(`hello',hello $1)dnl
define(`hello',wow $1)dnl
6 hello(friend)
popdef(`hello')dnl
7 hello(friend)
pushdef(`hello',hi $1)dnl
pushdef(`hello',hello $1)dnl
undefine(`hello')dnl
8 hello(friend)
