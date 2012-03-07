load ("../jl/bigfloat.jl")

a=BigFloat("12.0000000000000000000000000000000000")
b=BigFloat("12.0000000000000000000000000000000001")

@assert typeof(a+0.0000000000000000000000000000000001) == BigFloat
@assert a + 0.0000000000000000000000000000000001 - b < 0.0000000000000000000000000000000001
@assert b - 0.0000000000000000000000000000000001 - a < 0.0000000000000000000000000000000001
@assert !(b == a)
@assert b > a
@assert b >= a
@assert !(b < a)
@assert !(b <= a)

c = BigFloat("24.0000000000000000000000000000000002")
@assert typeof(b * 2) == BigFloat
@assert_approx_eq(b*2, c)
@assert_approx_eq(c-b, b)
@assert_approx_eq(c, b + b)
@assert_approx_eq(c+0.0000000000000000000000000000000001, a+b)

d = BigFloat("-24.0000000000000000000000000000000002")
@assert typeof(d) == BigFloat
@assert_approx_eq(d,-c)

#Multiple calls for sanity check, since we're doing direct memory manipulation
@assert string(a) == "12.0000000000000000000000000000000000"
@assert string(b) == "12.0000000000000000000000000000000001"
@assert string(c) == "24.0000000000000000000000000000000002"
@assert string(d) == "-24.0000000000000000000000000000000002"

@assert_approx_eq(div(BigFloat(3), BigFloat(2)), BigFloat(1))
@assert_approx_eq(rem(BigFloat(3), BigFloat(2)), BigFloat(1))