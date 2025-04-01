VAR=value NULL=''

echo ${VAR:+alternative}
echo ${NULL:+alternative}
echo ${UNSET:+alternative}
echo $VAR $NULL $UNSET

echo ${VAR+alternative}
echo ${NULL+alternative}
echo ${UNSET+alternative}
echo $VAR $NULL $UNSET