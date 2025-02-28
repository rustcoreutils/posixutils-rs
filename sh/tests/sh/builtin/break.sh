for x in a b c
do
  echo $x
  break
done

for x in a b c
do
  echo $x
  break 1
done

for x in a b c
do
  echo $x
  break 10
done

for x in a b c
do
  echo $x
  for y in d e f
  do
    echo $y
    break
  done
done

for x in a b c
do
  echo $x
  for y in d e f
  do
    echo $y
    break 1
  done
done

for x in a b c
do
  echo $x
  for y in d e f
  do
    echo $y
    break 2
  done
done

while true
do
  echo 'inside while'
  break
done

until false
do
  echo 'inside until'
  break
done