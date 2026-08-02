IFS=:
VAR='a::b:::c::'
for x in $VAR; do
  echo $x
done

IFS=': '
VAR='   a:  b: :c:   '
for x in $VAR; do
  echo $x
done

IFS=' '
VAR=' '
for x in $VAR; do
  echo $x
done
