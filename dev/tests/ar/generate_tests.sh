#!/bin/bash

rm *.o;
rm *.a;
rm *.correct;

gcc -c *.c*;

# delete
ar -q -c delete_base.a *.o;
## one
cp delete_base.a delete_one.a;
cp delete_base.a delete_one.correct.a;
ar -d delete_one.correct.a lib3.o;

## multiple
cp delete_base.a delete_multiple.a;
cp delete_base.a delete_multiple.correct.a;
ar -d delete_multiple.correct.a lib4.o lib2.o lib6.o;

## verbose
cp delete_base.a delete_verbose.a;
cp delete_base.a delete_verbose.correct.a;
ar -d delete_verbose.correct.a lib1.o;

rm delete_base.a

# move
ar -q -c move_base.a *.o;
## move to end
cp move_base.a move_to_end.a;
cp move_base.a move_to_end.correct.a;
ar -m move_to_end.correct.a lib4.o lib2.o;

## move after
cp move_base.a move_after.a;
cp move_base.a move_after.correct.a;
ar -m -a lib3.o move_after.correct.a lib2.o lib4.o;

## move before
cp move_base.a move_before.a;
cp move_base.a move_before.correct.a;
ar -m -b lib3.o move_before.correct.a lib2.o lib4.o;

rm move_base.a;

# print
ar -q -c print.a *.o;

ar -p print.a > print_all.correct;
ar -p print.a lib1.o lib5.o lib2.o > print_some.correct;
ar -p -v print.a > print_verbose.correct;

# quick append
ar -q -c quick_base.a lib1.o lib3.o lib4.o lib5.o;

## append
cp quick_base.a quick_append.a;
cp quick_base.a quick_append.correct.a;
ar -q quick_append.correct.a lib2.o lib6.o;

## create archive
ar -q -c quick_append_create.correct.a lib1.o lib4.o;

rm quick_base.a;

# replace

echo "void l7_old() {for (int i = 0; i < 10; ++i) {return;}}" > lib7.c;
echo "int l8_old_global = 1; float l8_old_function() {return 2.2;}" > lib8.c;
gcc -c lib7.c lib8.c;
ar -q -c replace_base.a lib1.o lib7.o lib3.o lib4.o lib5.o  lib8.o;
echo "void l7_replaced() {float x = 1 + 1; float y = x + 2;}" > lib7.c;
echo "int l8_new_global = 1; float l8_new_function() {return 1.0 * 2.4;}" > lib8.c;
gcc -c lib7.c lib8.c;


## replace
cp replace_base.a replace.a;
cp replace_base.a replace.correct.a;
ar -r replace.correct.a lib7.o lib8.o;

## replace verbose
cp replace_base.a replace_verbose.a;

## replace missing
cp replace_base.a replace_missing.a;
cp replace_base.a replace_missing.correct.a;
ar -r replace_missing.correct.a lib2.o lib6.o;

## replace missing insert before
cp replace_base.a replace_missing_insert_before.a;
cp replace_base.a replace_missing_insert_before.correct.a;
ar -r -b lib3.o replace_missing_insert_before.correct.a lib2.o lib6.o;

## replace missing insert after
cp replace_base.a replace_missing_insert_after.a;
cp replace_base.a replace_missing_insert_after.correct.a;
ar -r -a lib3.o replace_missing_insert_after.correct.a lib2.o lib6.o;

## replace missing verbose
cp replace_base.a replace_missing_verbose.a;

## create archive
ar -r -c replace_create.correct.a lib1.o lib4.o lib5.o;

rm replace_base.a;

# list
ar -q -c list.a lib1.o lib4.o lib5.o;

