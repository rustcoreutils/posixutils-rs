g () {
  echo $a
  echo $b
  d=4
}

f () {
  echo $a
  b=2 g
  c=3
}

a=1 f
echo $a
echo $b
echo $c
echo $d
