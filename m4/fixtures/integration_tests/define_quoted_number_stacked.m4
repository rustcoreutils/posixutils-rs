define(`x', `2,#')dnl
define(`y', `$@')dnl
1 x
2 y(`hello', `goodbye')
3 y(x)
