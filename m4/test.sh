#!/bin/bash

# m4 << EndOfMessage
# define(m2, \`m1(\$1)')dnl
# define(m1, \`\$1')dnl
# m2(hello)
# EndOfMessage

# m4 << EndOfMessage
# dnl demonstrates that evaluation of macro arguments is deferred until it is called.
# define(hello, error)dnl
# define(m2, m1(hello) m1(\$1))dnl 
# define(hello, goodbye)dnl
# define(m1, \`\`m1'' \$1)dnl
# m2(hi)
# EndOfMessage

m4 << EndOfMessage
# demonstrates that the parsing of the macro arguments happens at definition time
define(hello, eval(<))dnl
EndOfMessage

m4 << EndOfMessage
# demonstrates that the parsing of the macro arguments within a quote is deferred until the time of calling
define(hello, \`eval(<)')dnl
hello
EndOfMessage
