1 shift(a,b,c)
2 shift(a,b)
3 shift(a)
4 shift
5 define(`goodbye',`bye' `$@')define(`hello', `goodbye(shift($@))')hello(a,b,c)
6 define(`args', `a,b,c')shift(args)
