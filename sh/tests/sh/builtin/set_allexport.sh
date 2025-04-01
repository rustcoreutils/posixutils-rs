set -a
echo $?

test_var_a=a
echo $test_var_a
awk 'BEGIN { print ENVIRON["test_var_a"] }'

echo $((test_var_b=1234))
echo $test_var_b
awk 'BEGIN { print ENVIRON["test_var_b"] }'

echo ${test_var_c=c}
echo $test_var_c
awk 'BEGIN { print ENVIRON["test_var_c"] }'

set +a

test_var_d=d
echo $test_var_d
awk 'BEGIN { print ENVIRON["test_var_d"] }'
