readonly a=value
echo $?
echo $a
readonly -p

readonly b=val2 c=val2
echo $?
echo $b $c
readonly -p

var=value
echo $var
readonly var
echo $?
echo $var
readonly -p

echo $var2
readonly var2
echo $var2
readonly -p