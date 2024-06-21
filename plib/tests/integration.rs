#[test]
fn test_walkdir_simple() {
    let test_dir = "target/tmp/walkdir_test";
    let a_1 = format!("{test_dir}/a/1");
    let a_2 = format!("{test_dir}/a/2");
    let b_1 = format!("{test_dir}/b/1");
    let b_2 = format!("{test_dir}/b/2");

    for dir in [&a_1, &a_2, &b_1, &b_2] {
        std::fs::create_dir_all(dir).unwrap();
    }

    let mut filenames = [
        format!("{test_dir}"),
        format!("{test_dir}/b"),
        format!("{test_dir}/b/1"),
        format!("{test_dir}/b/2"),
        format!("{test_dir}/a"),
        format!("{test_dir}/a/1"),
        format!("{test_dir}/a/2"),
    ];

    let mut resulting_filenames = Vec::new();

    plib::walkdir::traverse_directory(
        test_dir,
        |entry| {
            let s = format!("{}", entry.path().display());
            resulting_filenames.push(s);
            true
        },
        |_| {},
        |_, _| {},
        false,
        false,
    );

    filenames.sort();
    resulting_filenames.sort();
    assert_eq!(filenames.as_slice(), resulting_filenames.as_slice());

    std::fs::remove_dir_all(test_dir).unwrap();
}
