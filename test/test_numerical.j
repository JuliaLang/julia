@assert sin(+0) == 0
@assert signbit(sin(-0)) == -1
# Perpahs the following should be within eps(0.0235985099044395586343659)
@assert sin(2. ^ 64) - 0.0235985099044395586343659 < eps(Float64)
@assert sin(Inf) == NaN
# What should this do? NumericalMath Wiki talks of a clever hex representation of PI in fdlibm.
@assert sin(pi())

