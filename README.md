
# posixutils-rs

This is a suite of Rust-native core command line utilties (cp, mv, rm,
ls, ...) using SuSv3 as the base POSIX specification.

## Goals

The goal is to create clean, race-free userland utilities using safe
Rust, while maximizing compatibility with existing shell scripts.

### Non-goals

It is _not_ a goal to be compatible with GNU utilities, which are
sometimes viewed as bloated and overloaded with rarely-used options.

Popular GNU options will be supported by virtue of the "don't break
scripts" rule.  Unpopular options will not be implemented, to prevent
bloat.

## Similar projects

A similar project with the aim of GNU compatibility is https://github.com/uutils/coreutils

A similar project from the author, written in C++, is https://github.com/jgarzik/posixutils

