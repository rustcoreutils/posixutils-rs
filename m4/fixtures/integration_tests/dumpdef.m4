dnl Disabled because (this won't match precisely with GNU's inbuilt macros)
dnl dumpdef 
define(`x', hello $0)dnl
define(`y', goodbye $0)dnl
errprint(1 )dumpdef(`x')dnl
errprint(2 )dumpdef(`x', `y')