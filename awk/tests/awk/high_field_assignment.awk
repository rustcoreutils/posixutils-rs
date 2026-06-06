BEGIN {
    # Assigning a high field index grows the record; intervening fields are
    # created with the uninitialized value, and NF is updated. No silent cap.
    $2000 = "z"
    print NF
    print $2000
    print ($1500 == 0)
}
