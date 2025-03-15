var=value
echo $var
unset var
echo $var

var2=value
echo $var2
unset -v var2
echo $var2

var3=value
echo $var3
unset -- var3
echo $var3