define(`x',`Hello from `$0'')dnl
1 x
define(`y', defn(`x'))dnl
2 y
define(`x', `this contains dnl not expanded!')dnl
3 x
4 defn(`x')
define(`x', k'k)define(`k', `X')
5 x
6 defn(`x')
7 defn(`doesnotexist')
8 defn(`define')