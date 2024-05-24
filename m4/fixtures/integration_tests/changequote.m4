changequote(<<,>>)<<`hi'>>
dnl changequote()<<`it is'>>
dnl           ^^^ this behaviour is unspecified
dnl changequote(<<,>>)<<`a'>>
dnl             ^^^^ looks like repeating this in GNU m4 is broken? It disables quoting!
changequote <<`very nice day'>>
dnl changequote(xx,yy)xx`hi'yy
dnl            ^^^^^^ This one isn't supported by GNU m4
