true && echo correct
echo $?
false && echo wrong
echo $?
true || echo wrong
echo $?
false || echo correct
echo $?
true && echo correct || echo wrong
echo $?
false && echo wrong || echo correct
echo $?

