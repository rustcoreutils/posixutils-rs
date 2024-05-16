dnl The first definition of y is used during the expansion of y in the definition of x.
define(y, 1)define(x, y)define(y, 5)x
