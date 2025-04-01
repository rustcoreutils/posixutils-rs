VAR=value NULL=''

echo ${VAR:=default}
echo $VAR
echo ${NULL:=default}
echo $NULL
echo ${UNSET:=default}
echo $UNSET

VAR=value NULL=''
unset UNSET

echo ${VAR=default}
echo $VAR
echo ${NULL=default}
echo $NULL
echo ${UNSET=default}
echo $UNSET
