until true
do
    echo "inside loop"
    break
done
echo $?

until false
do
    echo "inside loop"
    break
done
echo $?