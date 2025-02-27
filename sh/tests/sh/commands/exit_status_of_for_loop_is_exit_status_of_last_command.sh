for x in
do
  false
done
echo $?

for x in a
do
  false
  true
done
echo $?

