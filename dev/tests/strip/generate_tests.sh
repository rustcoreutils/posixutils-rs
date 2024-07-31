#!/bin/bash

rm *.o;
rm *.a;
gcc -c -g *.c;
ar -q -c archive.a liba.o libb.o

cp liba.o stripped_elf_is_valid_elf.o

cp archive.a stripped_archive_contains_valid_elf_members.a

cp liba.o remove_all_non_section_symbols.o

cp liba.o remove_all_relocations.o

cp liba.o remove_all_debug_sections.o

