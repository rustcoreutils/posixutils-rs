BEGIN {
    a["test"] = "test";
    a["other"] = "other";
    print a["test"];
    print a["other"];
    delete a["test"];
    print a["test"];
    print a["other"];

    b[0] = 1;
    b[1] = 2;
    b[2] = 3;
    b[3] = 4;
    print b[0];
    print b[1];
    print b[2];
    print b[3];
    delete b;
    print b[0];
    print b[1];
    print b[2];
    print b[3];
}
