cd tests/read_dir

. ./file2.txt
echo $?

x=value y=other_value
. ./filea.txt
echo $?