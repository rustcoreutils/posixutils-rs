1 define(`hi',friend)hi
2 define(`hi',`friend')hi
3 define(`hi',``friend'')hi
4 define(`hello',hi ``$0'')hello
5 define(`hi', `_hi')hi
6 define(`hi', `hi_')hi
7 define(`_hi',`friend')_hi
8 define(`hi_',`friend')hi_
9 define(`newline',

)dnl
begin
newline
end
10 define(`newline', with

something)
begin
newline
end
11 define
12 define(`hi', `$1')hi(hello)
13 define(`hi', ``$1'')hi(hello)
15 define(`hi2', $1 friend)dnl
define(`hi', `hi2(`$1')')hi(hello)
