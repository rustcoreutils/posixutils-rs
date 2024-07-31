divert(1)dnl
1 hello
divert(2)dnl
2 hi
divert(1)dnl
undivert(1, 2)dnl
divert dnl
world
divert(3)dnl
3 welcome
divert(4)
4 come
undivert(4, 3)
divert dnl
world
