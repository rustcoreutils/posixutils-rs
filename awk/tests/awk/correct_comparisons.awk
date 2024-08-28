BEGIN {
	print(+"nan" > a)
	print("1" < +"nan")
}

{
	print($1 < +"nan")
	print($1 "" < +"nan")
}