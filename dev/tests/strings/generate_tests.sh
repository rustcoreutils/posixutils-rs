#!/bin/bash

rm *.correct.txt
rm *.bin

strings one.txt > one.correct.txt

printf '\x00first\x07\x08\x07Second?//\"\x00\x00\x80THIRD\x00s' > multiple.bin
strings multiple.bin > multiple.correct.txt

printf 'Здравствуйте\x01\x00🦀🦀🦀🦀' > utf8.bin
printf 'Здравствуйте\n🦀🦀🦀🦀\n' > utf8.correct.txt

printf 'double object_function() { return 2.0 * 5.4; }' > object.c
cc -c object.c
rm object.c
strings object.o > object.correct.txt

printf 'sh\x00or\x00t string' > short.bin
strings -n 2 short.bin > short.correct.txt

printf 'long string\x00\x00\x00\x01short' > long.bin
strings -n 7 long.bin > long.correct.txt

printf 'first\x00\x00\x00\x01second\x01\x02\x03\x04third\x01\x00\x8A\x04forth' > with_offset.bin
strings -t d with_offset.bin > with_decimal_offset.correct.txt
strings -t x with_offset.bin > with_hex_offset.correct.txt
strings -t o with_offset.bin > with_octal_offset.correct.txt
