export export_test_var=value
echo $?
awk 'BEGIN { print ENVIRON["export_test_var"] }'

export -- export_test_var2=value2
echo $?
awk 'BEGIN { print ENVIRON["export_test_var2"] }'

export_test_var3=value3
export export_test_var3
echo $?
awk 'BEGIN { print ENVIRON["export_test_var3"] }'

export -p | grep export_test_var
export -p -- | grep export_test_var