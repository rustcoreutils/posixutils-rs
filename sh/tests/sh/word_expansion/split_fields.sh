IFS=,
VAR='a,b,c'
echo $VAR
VAR='   a,b,c

 '
echo $VAR

IFS=',:;'
VAR='  a,b;c:d,e
'
echo $VAR
VAR='a,  b;

c:d;e'
echo $VAR