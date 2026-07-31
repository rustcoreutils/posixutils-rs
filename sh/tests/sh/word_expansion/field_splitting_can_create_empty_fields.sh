IFS=:
VAR='a::b:::c'
for x in $VAR; do
  echo $x
done
