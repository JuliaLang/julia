a = BigFloat("12.34567890121")
b = BigFloat("12.34567890122")

@test typeof(a+0.00000000001) == BigFloat
@test abs(a+0.00000000001 - b) < 0.00000000001
@test !(b == a)
@test b > a
@test b >= a
@test !(b < a)
@test !(b <= a)

c = BigFloat("24.69135780242")
@test typeof(a * 2) == BigFloat
@test abs(a*2 - c) < 0.00000000001
@test abs(c-a - a) < 0.00000000001


d = BigFloat("-24.69135780242")
@test typeof(d) == BigFloat
@test abs(d + c) < 0.00000000001

#Multiple calls for sanity check, since we're doing direct memory manipulation
@test string(a) == "1.234567890121e+01"
@test string(b) == "1.234567890122e+01"
@test string(c) == "2.469135780242e+01"
@test string(d) == "-2.469135780242e+01"

@test abs((BigFloat(3)/BigFloat(2)) - BigFloat(1.5)) < 0.00000000001

@assert BigFloat(2) + BigInt(2) == BigFloat(4)
