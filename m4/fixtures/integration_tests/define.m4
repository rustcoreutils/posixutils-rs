define(`okay',just fine)dnl
define(`hello',hi ``$0'')dnl
hello
define(`hello',hi ``$1'')dnl
hello(computer)
define(`hello',``$0'' I hope $1 is having a great day ``$@'' ``$*'')dnl
hello(computer, okay)
