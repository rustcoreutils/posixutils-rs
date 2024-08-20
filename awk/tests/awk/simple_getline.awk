{
	print $0;
	print NF, NR, FNR;
	getline;
	print NF, NR, FNR;
	print $0;
}

{
	print NF, NR, FNR;
	print $0;
}