BEGIN {
	print FS;
}

{
	print $1, $2, $3
}