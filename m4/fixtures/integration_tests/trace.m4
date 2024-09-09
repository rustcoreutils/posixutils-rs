define(`x',`hello')dnl
define(`y',`x world $1')dnl
y(x)
traceon
y(x)
traceoff
y(x)
traceon(`x',`y')
y(x)
traceoff