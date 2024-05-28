define(hello, error)dnl
define(m2, m1(hello) m1($1))dnl 
dnl this actually defines a macro called `error' and so the original definition for hello remains the same.
define(hello, goodbye)dnl
define(m1, ``m1'' $1)dnl
m2(hi)
