# substitution from macro
define(`x', 42)
eval(x * x)
define(`6' * 7)
# +
eval(1+2)
# - prefix
eval(-1+2)
eval(2-1)
# *, + and () ordering
eval(2*4+5)
eval(2*(4+5))
eval(2+4*5)
eval((2+4)*5)
# >
eval(2>1)
eval(2>2)
eval(1>2)
# <
eval(1<2)
eval(2<2)
eval(2<1)
# <=
eval(2<=1)
eval(2<=2)
eval(1<=2)
# <=
eval(2>=1)
eval(2>=2)
eval(1>=2)
# ==
eval(2==1)
eval(2==2)
eval(1==2)
# !=
eval(2!=1)
eval(2!=2)
eval(1!=2)
# !
eval(!1)
eval(!10)
eval(!0)
eval(!-1)
eval(!-10)
eval(!1-1)
eval(!1+1)
# ~
eval(~-3)
eval(~1-2)
eval(~3)
eval(~0)
# %
eval(4%2)
eval(4%3)
eval(-4%2)
eval(-4%3)
# &
eval(0&4)
eval(4&2)
eval(4&4)
eval(4&3)
# ^
eval(0^4)
eval(4^2)
eval(4^4)
eval(4^3)
# |
eval(0|4)
eval(4|2)
eval(4|4)
eval(4|3)
# &&
eval(0&&0)
eval(1&&1)
eval(1&&0)
eval(4&&1)
eval(3&4&&1&4)
# ||
eval(0||0)
eval(1||1)
eval(1||0)
eval(4||1)
eval(3|4||1|4)
