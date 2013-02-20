tol = 1e-12

a = BigFloat("12.34567890121")
b = BigFloat("12.34567890122")

@test typeof(a+1e-11) == BigFloat
@test_approx_eq_eps a+1e-11 b tol
@test !(b == a)
@test b > a
@test b >= a
@test !(b < a)
@test !(b <= a)

c = BigFloat("24.69135780242")
@test typeof(a * 2) == BigFloat
@test_approx_eq_eps a*2 c tol
@test_approx_eq_eps (c-a) a tol


d = BigFloat("-24.69135780242")
@test typeof(d) == BigFloat
@test_approx_eq_eps d+c 0 tol

#Multiple calls for sanity check, since we're doing direct memory manipulation
@test string(a) == "1.234567890121e+01"
@test string(b) == "1.234567890122e+01"
@test string(c) == "2.469135780242e+01"
@test string(d) == "-2.469135780242e+01"

@test_approx_eq_eps (BigFloat(3)/BigFloat(2)) BigFloat(1.5) tol

@test typeof(BigFloat(typemax(Int8))) == BigFloat
@test typeof(BigFloat(typemax(Int16))) == BigFloat
@test typeof(BigFloat(typemax(Int32))) == BigFloat
@test typeof(BigFloat(typemax(Int64))) == BigFloat
#@test typeof(BigFloat(typemax(Int128))) == BigFloat

@test typeof(BigFloat(true)) == BigFloat
@test typeof(BigFloat(typemax(Uint8))) == BigFloat
@test typeof(BigFloat(typemax(Uint16))) == BigFloat
@test typeof(BigFloat(typemax(Uint32))) == BigFloat
@test typeof(BigFloat(typemax(Uint64))) == BigFloat
#@test typeof(BigFloat(typemax(Uint128))) == BigFloat

@test typeof(BigFloat(realmax(Float32))) == BigFloat
@test typeof(BigFloat(realmax(Float64))) == BigFloat

@test typeof(BigFloat(BigInt(1))) == BigFloat
@test typeof(BigFloat(BigFloat(1))) == BigFloat

@test typeof(BigFloat(1//1)) == BigFloat
@test typeof(BigFloat(one(Rational{BigInt}))) == BigFloat

f = BigFloat("1234567890.123")
g = BigFloat("1234567891.123")

tol = 1e-3

@test_approx_eq_eps f+int8(1) g tol
@test_approx_eq_eps f+int16(1) g tol
@test_approx_eq_eps f+int32(1) g tol
@test_approx_eq_eps f+int64(1) g tol
#@test_approx_eq_eps f+int128(1) g tol

@test_approx_eq_eps f+true g tol
@test_approx_eq_eps f+uint8(1) g tol
@test_approx_eq_eps f+uint16(1) g tol
@test_approx_eq_eps f+uint32(1) g tol
@test_approx_eq_eps f+uint64(1) g tol
#@test_approx_eq_eps f+uint128(1) g tol

@test_approx_eq_eps f+BigInt(1) g tol

@test_approx_eq_eps f+1f0 g tol
@test_approx_eq_eps f+1e0 g tol

@test_approx_eq_eps f+BigFloat(1) g tol

@test_approx_eq_eps f+(1//1) g tol

@test_approx_eq_eps f+one(Rational{BigInt}) g tol
