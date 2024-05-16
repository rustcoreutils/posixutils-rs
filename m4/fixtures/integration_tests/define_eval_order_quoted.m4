dnl The first definition of y is used in x (even when quoted!)
define(y, 5)define(x, `eval(1+y)')define(y, 100)x
