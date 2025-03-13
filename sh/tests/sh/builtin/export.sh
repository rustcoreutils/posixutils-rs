export export_test_var=value
echo $?
awk 'BEGIN { print ENVIRON["export_test_var"] }'

export_test_var2=value2
export export_test_var2
echo $?
awk 'BEGIN { print ENVIRON["export_test_var2"] }'

export -p | grep export_test_var