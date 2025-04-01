if true
then
    echo correct
fi

if false
then
    echo wrong
fi

if true
then
    echo correct
else
    echo wrong
fi

if false
then
    echo wrong
elif true
then
    echo correct
fi

if false
then
    echo wrong
elif false
then
    echo wrong
else
    echo correct
fi