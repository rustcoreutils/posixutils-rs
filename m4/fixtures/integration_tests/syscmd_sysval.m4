dnl Currently the ordering of this is different from GNU m4, 
dnl it might require a major re-architecture to achieve the same.
dnl We shall wait and see whether it affects real world usage.
1 syscmd(`echo "hello world"')
2 sysval
3 syscmd(`exit 1')
4 sysval
dnl GNU m4 seems to violate the POSIX definition here of:
dnl "The defining text shall be the string result of that command.""
5 define(`x', syscmd(`echo "hello"'))x x x
dnl Output is directly to stdout, it's not captured.
6 divert(1)hello syscmd(`echo "world"')undivert(1)
