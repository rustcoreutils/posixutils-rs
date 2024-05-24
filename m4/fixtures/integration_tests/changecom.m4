define(hello, world)
| hello ^ # hello
changecom(|,^)dnl
| hello ^ # hello
dnl changecom(|)dnl
dnl          ^^^ GNU m4 errors out with this!
dnl
dnl With a single non-null argument, that argument shall become the
dnl begin-comment and the <newline> shall become the end-comment string.
dnl
dnl | hello ^ # hello
dnl TODO: enable again without using GNU m4
changecom dnl
| hello ^ # hello
