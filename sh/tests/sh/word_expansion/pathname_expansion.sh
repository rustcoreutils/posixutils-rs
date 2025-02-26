cd tests/read_dir

echo dir/inner_dir/data1
echo dir/inner_dir/*
echo dir?inner_dir?data1
echo dir[x/y]inner_dir[x/y]data1
echo dir[[:graph:]]inner_dir[[:graph:]]data1
echo */*/*

echo *
echo ?hidden
echo *hidden
echo [!a]hidden
echo [[:punct:]]hidden
echo .*

echo file?.txt
echo f*.*
echo file[[:digit:]].txt
