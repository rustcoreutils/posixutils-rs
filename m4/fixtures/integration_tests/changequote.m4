1 `hi'
2 changequote(<<,>>)<<`hi'>>
dnl changequote()<<`it is'>>
dnl           ^^^ this behaviour is unspecified
dnl
dnl changequote(<<,>>)<<`a'>>
dnl             ^^^^ looks like repeating this in GNU m4 is broken? It disables quoting!
dnl TODO: enable again without using GNU m4
3 changequote <<`very nice day'>>
dnl changequote(xx,yy)xx`hi'yy
dnl            ^^^^^^ This one isn't supported by GNU m4
dnl TODO: enable again without using GNU m4
