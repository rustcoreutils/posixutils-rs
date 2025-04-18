umask
echo $?
umask -S
echo $?

umask 0777
echo $?
umask
umask -S

umask 022
echo test > $TEST_WRITE_DIR/umask.txt
for x in $(ls -l $TEST_WRITE_DIR/umask.txt); do
  echo $x
  break
done
rm $TEST_WRITE_DIR/umask.txt
