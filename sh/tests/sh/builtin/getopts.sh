set -- -a -b -c
while getopts abc opt; do
    echo $opt
done
echo $OPTIND

OPTIND=1
while getopts abc opt -a -b -c; do
    echo $opt
done
echo $OPTIND

OPTIND=1
while getopts abc opt -a -; do
    echo $opt
done
echo $OPTIND

OPTIND=1
while getopts abc opt -a -- -b -c; do
    echo $opt
done
echo $OPTIND

OPTIND=1
while getopts abc opt -abc; do
    echo $opt
done
echo $OPTIND

OPTIND=1
while getopts a:b:c: opt -a arg1 -b arg2 -c arg3; do
    echo $opt $OPTARG $OPTIND
done
echo $OPTIND

OPTIND=1
while getopts a:b:c: opt -aarg1 -barg2 -carg3; do
    echo $opt $OPTARG $OPTIND
done
echo $OPTIND

OPTIND=1
while getopts :abc opt -d -a -b; do
    echo $opt $OPTARG
done
echo $OPTIND

OPTIND=1
while getopts :a: opt -a; do
    echo $opt $OPTARG
done
echo $OPTIND