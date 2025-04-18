alias a='echo hello'
f () {
  :
}

command -v a
command -v f
command -v set
command -v command

command -V a
command -V f
command -V set
command -V command

command echo test
var=value command :
echo $var
echo () {
  :
}

echo test
command echo test
