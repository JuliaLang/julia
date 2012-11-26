cd("../extras") do
require("bigfloat")

a=BigFloat("12.34567890121")
b=BigFloat("12.34567890122")

@assert typeof(a+0.00000000001) == BigFloat
@assert abs(a+0.00000000001 - b) < 0.00000000001
@assert !(b == a)
@assert b > a
@assert b >= a
@assert !(b < a)
@assert !(b <= a)

c = BigFloat("24.69135780242")
@assert typeof(a * 2) == BigFloat
@assert abs(a*2 - c) < 0.00000000001
@assert abs(c-a - a) < 0.00000000001


d = BigFloat("-24.69135780242")
@assert typeof(d) == BigFloat
@assert abs(d + c) < 0.00000000001

#Multiple calls for sanity check, since we're doing direct memory manipulation
@assert string(a) == "12.34567890121"
@assert string(b) == "12.34567890122"
@assert string(c) == "24.69135780242"
@assert string(d) == "-24.69135780242"

@assert abs((BigFloat(3)/BigFloat(2)) - BigFloat(1.5)) < 0.00000000001

end # cd
