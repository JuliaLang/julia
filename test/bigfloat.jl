
load ("../jl/bigfloat.jl")
a=BigFloat("12.34567890121")
b=BigFloat("12.34567890122")

@assert typeof(a+0.00000000001) == BigFloat
@assert a+0.00000000001 == b
@assert b == a+0.00000000001
@assert !(b == a)
@assert b > a
@assert b >= a
@assert !(b < a)
@assert !(b <= a)

c = BigFloat("24.69135780242")
@assert typeof(a * 2) == BigFloat
@assert a*2 == c
@assert c-a == a
@assert c == a + a
@assert c+1 == a+b

d = BigFloat("-24.69135780242")
@assert typeof(d) == BigFloat
@assert d == -c

#Multiple calls for sanity check, since we're doing direct memory manipulation
@assert string(a) == "12.34567890121"
@assert string(b) == "12.34567890122"
@assert string(c) == "24.69135780242"
@assert string(d) == "-24.69135780242"

@assert div(BigFloat(3), BigFloat(2)) == BigFloat(1)
@assert rem(BigFloat(3), BigFloat(2)) == BigFloat(1)

