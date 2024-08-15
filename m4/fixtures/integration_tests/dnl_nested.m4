ifdef(`A', `', `dnl
ifdef(`B', `', `')')
define(`x',`b')
ifdef(`A',,dnl a
x c)