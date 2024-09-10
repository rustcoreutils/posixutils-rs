BEGIN {
    print "first line" > "tests/awk/output_redirection_truncate.txt";
    print "second line" > "tests/awk/output_redirection_truncate.txt";
    close("tests/awk/output_redirection_truncate.txt");
    print "appended first line" >> "tests/awk/output_redirection_append.txt";
    print "appended second line" >> "tests/awk/output_redirection_append.txt";
    close("tests/awk/output_redirection_append.txt"); 
}