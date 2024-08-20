{
	print $0;
	print NF, NR, FNR;
	getline next_record;
	print NF, NR, FNR;
	print $0;
	print next_record;
}

{
	print NF, NR, FNR;
	print $0;
}