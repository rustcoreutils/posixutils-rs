echo $(("1"))
echo $(("1+1"))
x=1
echo $((1 + $x))
echo $((1 + x))
echo $((1 + $(echo 1)))