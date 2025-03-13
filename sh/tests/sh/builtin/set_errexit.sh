set -e

fls () {
  false
}

while false; do
  :
done

until false; do
  break
done

if false; then
  :
elif fls; then
  :
elif { if true; then false; fi }; then
  :
fi

echo correct

false

echo wrong
