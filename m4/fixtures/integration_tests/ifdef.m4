# Undefined
1 ifdef(`x',`is defined', `not defined')
# Defined
2 define(`x', `hello')ifdef(`x',`is defined', `not defined')
# Nested undefine in third arg
3 define(`x', `hello')ifdef(`x',`is defined', undefine(`x')not defined)
4 define(`x', `hello')ifdef(`x',`is defined', `undefine(`x')not defined')