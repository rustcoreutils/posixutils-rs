f () {
  echo $LINENO
  echo $LINENO
}

g () {
  echo $LINENO
  f
  echo $LINENO
}

echo $LINENO
f
g