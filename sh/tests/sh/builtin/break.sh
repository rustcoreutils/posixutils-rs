for x in a b c
do
  echo $x
  break
  echo wrong
done

for x in a b c
do
  echo $x
  echo correct && break && echo wrong
done

for x in a b c
do
  echo $x
  break 1
  echo wrong
done

for x in a b c
do
  echo $x
  break 10
  echo wrong
done

for x in a b c
do
  echo $x
  for y in d e f
  do
    echo $y
    break
    echo wrong
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

while true
do
  break --
done

while true
do
  break -- 1
done