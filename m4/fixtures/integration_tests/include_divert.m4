divert(1)
# divert 1
divert(0)
# divert 0 root 1
include(`fixtures/integration_tests/include/divert.m4')
# after include 1
divert(3)
# divert 3 root
include(`fixtures/integration_tests/include/divert.m4')
# after include 2
divert(0)
# divert 0 root 2
