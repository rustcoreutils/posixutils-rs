dnl demonstrates that evaluation of macro arguments is deferred until it is called.
define(hello, error)dnl
define(m2, m1(hello) m1(\$1))dnl 
define(hello, goodbye)dnl
define(m1, ``m1'' \$1)dnl
m2(hi)
