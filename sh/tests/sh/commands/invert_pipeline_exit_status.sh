! true
echo $?

! false
echo $?

! echo test | cat >/dev/null | false
echo $?

! echo test | cat >/dev/null | true
echo $?