dnl This demonstrates that the macros should be evaluated in
dnl a depth-first manner.
define(`x',`define(`i', `y') z undefine(`i')')dnl
define(`z', `i')dnl
x
