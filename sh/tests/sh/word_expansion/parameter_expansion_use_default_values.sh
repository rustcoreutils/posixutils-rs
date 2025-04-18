VAR=value NULL=''

echo ${VAR:-default}
echo ${NULL:-default}
echo ${unset:-default}
echo $VAR $NULL

echo ${VAR-default}
echo ${NULL-default}
echo ${unset-default}
echo $VAR $NULL