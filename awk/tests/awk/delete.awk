BEGIN {
    a["test"] = "test";
    a["other"] = "other";
    print a["test"];
    print a["other"];
    delete a["test"];
    print a["test"];
    print a["other"];
}