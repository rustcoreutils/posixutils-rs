dnl The first definition of y is used during the evaluation of the define for x.
define(`y', 1)define(`x', y)define(`y', 5)x
dnl The second definition of y is used during the expansion of x.
define(`y', 1)define(`x', `y')define(`y', 5)x
