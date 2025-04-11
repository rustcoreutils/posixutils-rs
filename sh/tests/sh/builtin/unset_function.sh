test_function () {
  echo 'inside f'
}

test_function
unset -f test_function
test_function
echo $?