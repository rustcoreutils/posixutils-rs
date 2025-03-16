g () {
  echo $a
  awk 'BEGIN {print ENVIRON["a"]}'
  echo $b
  d=5
}

f () {
  echo $a
  awk 'BEGIN {print ENVIRON["a"]}'
  a=2 b=3 g
  echo $a
  echo $b
  echo $d
  c=4
}

a=1 f
echo $a
echo $b
echo $c
echo $d
