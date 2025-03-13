f () {
  echo correct
  return
  echo wrong
}

f
echo $?

g () {
  echo correct
  return 2
  echo wrong
}

g
echo $?

cat >$TEST_WRITE_DIR/return.txt <<end
echo correct
return
echo wrong
end

. $TEST_WRITE_DIR/return.txt
echo $?
rm $TEST_WRITE_DIR/return.txt