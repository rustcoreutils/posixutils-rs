for x in a b c
do
  continue
  echo $x
done

for x in a b c
do
  echo correct && continue && echo wrong
  echo $x
done

for x in a b c
do
  continue 1
  echo $x
done

for x in a b c
do
  continue 10
  echo $x
done

for x in a b c
do
  echo $x
  for y in d e f
  do
    echo $y
    continue
  done
done

for x in a b c
do
  echo $x
  for y in d e f
  do
    echo $y
    continue 1
  done
done

for x in a b c
do
  echo $x
  for y in d e f
  do
    echo $y
    continue 2
  done
done

var='continue'
while { echo 'in condition'; test $var = 'continue'; }
do
  var='stop'
  continue
  echo wrong
done

var='stop'
until { echo 'in condition'; test $var = 'continue'; }
do
  var='continue'
  continue
  echo wrong
done

for x in a b c; do
  continue --
  echo wrong
done

for x in a b c; do
  continue -- 1
  echo wrong
done