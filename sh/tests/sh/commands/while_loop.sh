while true
do
    echo "inside loop"
    break
done
echo $?

while false
do
    echo "inside loop"
    break
done
echo $?