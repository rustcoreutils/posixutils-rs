define(`newline', `1
2')
changecom(`<<', `>>')
define(`comment', `<<1
2>>')
dnl
hi
newline
comment
1 comment `2
3'
bye