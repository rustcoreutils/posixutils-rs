BEGIN {
	getline record < "tests/awk/test_data.txt";
	print record;
	getline < "tests/awk/test_data.txt";
	print $0;
	print NF;
	print $1;
	print $2;
	print $3;
	print $4;
	close("tests/awk/test_data.txt");
	getline < "tests/awk/test_data.txt";
	print $0;
	print NF;
	close("tests/awk/test_data.txt");
}

{
	getline record < "tests/awk/test_data.txt";
	print record;
	print $0;
	print NF, NR, FNR;
	getline < "tests/awk/test_data.txt";
}

{
	print $0;
	print NF, NR, FNR;
}
