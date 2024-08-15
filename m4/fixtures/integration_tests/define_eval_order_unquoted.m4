dnl The first definition of y is used in x
define(y, 5)define(x, eval(1+y))define(y, 1)x
