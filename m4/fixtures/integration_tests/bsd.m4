
# Looks like Hanoi has an infinite loop at least for GNU m4
#
# include towers-of-hanoi
#
# include(fixtures/integration_tests/bsd/hanoi.m4)
#
# some reasonable set of disks
#
# hanoi(2)
#

# Looks like Hanoi has an infinite loop at least for GNU m4
#
# include ackermann's function
#
# include(fixtures/integration_tests/bsd/ack.m4)
#
# something like (3,3) will blow away un*x m4.
#
# ack(1,1)

#
# include a square_root function for fixed nums
#
include(fixtures/integration_tests/bsd/sqroot.m4)
#
# some square roots.
#
square_root(15)
square_root(100)
square_root(-4)
square_root(21372)