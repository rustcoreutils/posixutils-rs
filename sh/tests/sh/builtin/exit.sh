(exit 0)
echo $?

(false; exit)
echo $?

(true; exit)
echo $?

(exit 100)
echo $?

exit 0
echo wrong;