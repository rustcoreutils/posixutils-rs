1 ifelse(a,b,3)
1.1 ifelse(a,b,`3')
2 ifelse(a,a,3)
3 ifelse(a,b,3,4)
4 ifelse(a,a,3,4)
5 ifelse(a,a,3,4)

define(`x',a)dnl
6 ifelse(a,`x',3,4)
7 ifelse(a,x,3,4)
8 ifelse(a,b,3,x,x,6,7)
9 ifelse(a,b,3,x,y,6,7)

