for x in a b c
do
  echo $x
done

values='a b c'
for x in $values;
do
  echo $x
done

a='a b'
b=c

for x in $a $b
do
  echo $x
done

for x in
do
  echo 'in loop'
  echo $x
done