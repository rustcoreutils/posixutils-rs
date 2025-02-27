! true
echo $?

! false
echo $?

! echo test | cat | false
echo $?

! echo test | cat | true
echo $?