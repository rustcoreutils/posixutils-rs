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
15 define(`hi2', $1 friend)define(`hi', `hi2(`$1')')hi(hello)
16 define(`hi', `$@')hi(a,b)
17 define(`hi', ``$@'')hi(a,b)
18 define(`hi', ```$@''')hi(a,b)
19 define(`hi', `$@')hi(`a,b')
20 define(`hi2', ``hello'')define(`hi', `hi2')hi
21 define(`hi2', ``hello'')define(`hi', `# hi2')hi
22 define(`hi', ``hello'')$# hi
23 define(`hi2', ``hello'')define(`hi', `$# hi2')hi
24 define(`hello',`a,b')define(`hi', `$# 1:$1 2:$2 $@')hi(hello)
25 define(`hello',`a,b,`c,d'')define(`hi', `$# 1:$1 2:$2 3:$3 $@')hi(hello)
