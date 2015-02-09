using Base.Grisu

function trimrep(buffer)
  len = length(bytestring(pointer(buffer)))
  ind = len
  for i = len:-1:1
    buffer[i] != 0x30 && break
    ind -= 1
  end
  buffer[ind+1] = 0
  return bytestring(pointer(buffer))
end

const bufsize = 500
buffer = Array(UInt8,bufsize);
fill!(buffer,0);

# Start by checking the byte-order.
ordered = 0x0123456789ABCDEF
@test 3512700564088504e-318 == reinterpret(Float64,ordered)

min_double64 = 0x0000000000000001
@test 5e-324 == reinterpret(Float64,min_double64)

max_double64 = 0x7fefffffffffffff
@test 1.7976931348623157e308 == reinterpret(Float64,max_double64)

# Start by checking the byte-order.
ordered = 0x01234567
@test float32(2.9988165487136453e-38) == reinterpret(Float32,ordered)

min_float32 = 0x00000001
@test float32(1.4e-45) == reinterpret(Float32,min_float32)

max_float32 = 0x7f7fffff
@test float32(3.4028234e38) == reinterpret(Float32,max_float32)

ordered = 0x0123456789ABCDEF
diy_fp  = Grisu.Float(reinterpret(Float64,ordered))
@test uint64(0x12) - uint64(0x3FF) - 52 == uint64(diy_fp.e)
# The 52 mantissa bits, plus the implicit 1 in bit 52 as a UINT64.
@test 0x0013456789ABCDEF== diy_fp.s

min_double64 = 0x0000000000000001
diy_fp  = Grisu.Float(reinterpret(Float64,min_double64))
@test -uint64(0x3FF) - int64(52) + int64(1) == uint64(diy_fp.e)
# This is a denormal so no hidden bit.
@test 1 == diy_fp.s

max_double64 = 0x7fefffffffffffff
diy_fp  = Grisu.Float(reinterpret(Float64,max_double64))
@test 0x7FE - 0x3FF - 52 == uint64(diy_fp.e)
@test 0x001fffffffffffff== diy_fp.s

ordered = 0x01234567
diy_fp  = Grisu.Float(reinterpret(Float32,ordered))
@test uint64(0x2) - uint64(0x7F) - 23 == uint64(diy_fp.e)
# The 23 mantissa bits, plus the implicit 1 in bit 24 as a uint32_t.
@test 0xA34567 == uint64(diy_fp.s)

min_float32 = 0x00000001
diy_fp  = Grisu.Float(reinterpret(Float32,min_float32))
@test -uint64(0x7F) - 23 + 1 == uint64(diy_fp.e)
# This is a denormal so no hidden bit.
@test 1 == uint64(diy_fp.s)

max_float32 = 0x7f7fffff
diy_fp  = Grisu.Float(reinterpret(Float32,max_float32))
@test 0xFE - 0x7F - 23 == uint64(diy_fp.e)
@test 0x00ffffff == uint64(diy_fp.s)

ordered = 0x0123456789ABCDEF
diy_fp  = Grisu.normalize(Grisu.Float(reinterpret(Float64,ordered)))
@test uint64(0x12) - uint64(0x3FF) - 52 - 11 == uint64(diy_fp.e)
@test 0x0013456789ABCDEF<< 11 == diy_fp.s

min_double64 = 0x0000000000000001
diy_fp  = Grisu.normalize(Grisu.Float(reinterpret(Float64,min_double64)))
@test -uint64(0x3FF) - 52 + 1 - 63 == uint64(diy_fp.e)
# This is a denormal so no hidden bit.
@test 0x8000000000000000== diy_fp.s

max_double64 = 0x7fefffffffffffff
diy_fp  = Grisu.normalize(Grisu.Float(reinterpret(Float64,max_double64)))
@test 0x7FE - 0x3FF - 52 - 11 == uint64(diy_fp.e)
@test (0x001fffffffffffff<< 11) == diy_fp.s

min_double64 = 0x0000000000000001
@test Grisu.isdenormal(reinterpret(Float64,min_double64))
float_bits = 0x000FFFFFFFFFFFFF
@test Grisu.isdenormal(reinterpret(Float64,float_bits))
float_bits = 0x0010000000000000
@test !Grisu.isdenormal(reinterpret(Float64,float_bits))

min_float32 = 0x00000001
@test Grisu.isdenormal(reinterpret(Float32,min_float32))
float_bits = 0x007FFFFF
@test Grisu.isdenormal(reinterpret(Float32,float_bits))
float_bits = 0x00800000
@test !Grisu.isdenormal(reinterpret(Float32,float_bits))

diy_fp = Grisu.normalize(Grisu.Float(1.5))
boundary_minus, boundary_plus = Grisu.normalizedbound(1.5)
@test diy_fp.e == boundary_minus.e
@test diy_fp.e == boundary_plus.e
# 1.5 does not have a significand of the form 2^p (for some p).
# Therefore its boundaries are at the same distance.
@test diy_fp.s - boundary_minus.s == boundary_plus.s - diy_fp.s
@test (1 << 10) == diy_fp.s - boundary_minus.s

diy_fp = Grisu.normalize(Grisu.Float(1.0))
boundary_minus, boundary_plus = Grisu.normalizedbound(1.0)
@test diy_fp.e == boundary_minus.e
@test diy_fp.e == boundary_plus.e
# 1.0 does have a significand of the form 2^p (for some p).
# Therefore its lower boundary is twice as close as the upper boundary.
@test boundary_plus.s - diy_fp.s > diy_fp.s - boundary_minus.s
@test (1 << 9) == diy_fp.s - boundary_minus.s
@test (1 << 10) == boundary_plus.s - diy_fp.s

min_double64 = 0x0000000000000001
diy_fp = Grisu.normalize(Grisu.Float(reinterpret(Float64,min_double64)))
boundary_minus, boundary_plus = Grisu.normalizedbound(reinterpret(Float64,min_double64))
@test diy_fp.e == boundary_minus.e
@test diy_fp.e == boundary_plus.e
# min-value does not have a significand of the form 2^p (for some p).
# Therefore its boundaries are at the same distance.
@test diy_fp.s - boundary_minus.s == boundary_plus.s - diy_fp.s
# Denormals have their boundaries much closer.
@test (uint64(1) << 62) == diy_fp.s - boundary_minus.s

smallest_normal64 = 0x0010000000000000
diy_fp = Grisu.normalize(reinterpret(Float64,smallest_normal64))
boundary_minus, boundary_plus = Grisu.normalizedbound(reinterpret(Float64,smallest_normal64))
@test diy_fp.e == boundary_minus.e
@test diy_fp.e == boundary_plus.e
# Even though the significand is of the form 2^p (for some p), its boundaries
# are at the same distance. (This is the only exception).
@test diy_fp.s - boundary_minus.s == boundary_plus.s - diy_fp.s
@test (1 << 10) == diy_fp.s - boundary_minus.s

largest_denormal64 = 0x000FFFFFFFFFFFFF
diy_fp = Grisu.normalize(reinterpret(Float64,largest_denormal64))
boundary_minus, boundary_plus = Grisu.normalizedbound(reinterpret(Float64,largest_denormal64))
@test diy_fp.e == boundary_minus.e
@test diy_fp.e == boundary_plus.e
@test diy_fp.s - boundary_minus.s == boundary_plus.s - diy_fp.s
@test (1 << 11) == diy_fp.s - boundary_minus.s

max_double64 = 0x7fefffffffffffff
diy_fp = Grisu.normalize(reinterpret(Float64,max_double64))
boundary_minus, boundary_plus = Grisu.normalizedbound(reinterpret(Float64,max_double64))
@test diy_fp.e == boundary_minus.e
@test diy_fp.e == boundary_plus.e
# max-value does not have a significand of the form 2^p (for some p).
# Therefore its boundaries are at the same distance.
@test diy_fp.s - boundary_minus.s == boundary_plus.s - diy_fp.s
@test (1 << 10) == diy_fp.s - boundary_minus.s

kOne64 = uint64(1)
diy_fp  = Grisu.normalize(Grisu.Float(float32(1.5)))
boundary_minus, boundary_plus = Grisu.normalizedbound(float32(1.5))
@test diy_fp.e == boundary_minus.e
@test diy_fp.e == boundary_plus.e
# 1.5 does not have a significand of the form 2^p (for some p).
# Therefore its boundaries are at the same distance.
@test diy_fp.s - boundary_minus.s == boundary_plus.s - diy_fp.s
# Normalization shifts the significand by 8 bits. Add 32 bits for the bigger
# data-type, and remove 1 because boundaries are at half a ULP.
@test (kOne64 << 39) == diy_fp.s - boundary_minus.s

diy_fp  = Grisu.normalize(Grisu.Float(float32(1.0)))
boundary_minus, boundary_plus = Grisu.normalizedbound(float32(1.0))
@test diy_fp.e == boundary_minus.e
@test diy_fp.e == boundary_plus.e
# 1.0 does have a significand of the form 2^p (for some p).
# Therefore its lower boundary is twice as close as the upper boundary.
@test boundary_plus.s - diy_fp.s > diy_fp.s - boundary_minus.s
@test (kOne64 << 38) == diy_fp.s - boundary_minus.s
@test (kOne64 << 39) == boundary_plus.s - diy_fp.s

min_float32 = 0x00000001
diy_fp  = Grisu.normalize(Grisu.Float(reinterpret(Float32,min_float32)))
boundary_minus, boundary_plus = Grisu.normalizedbound(reinterpret(Float32,min_float32))
@test diy_fp.e == boundary_minus.e
@test diy_fp.e == boundary_plus.e
# min-value does not have a significand of the form 2^p (for some p).
# Therefore its boundaries are at the same distance.
@test diy_fp.s - boundary_minus.s == boundary_plus.s - diy_fp.s
# Denormals have their boundaries much closer.
@test (kOne64 << 62) == diy_fp.s - boundary_minus.s

smallest_normal32 = 0x00800000
diy_fp  = Grisu.normalize(Grisu.Float(reinterpret(Float32,smallest_normal32)))
boundary_minus, boundary_plus = Grisu.normalizedbound(reinterpret(Float32,smallest_normal32))
@test diy_fp.e == boundary_minus.e
@test diy_fp.e == boundary_plus.e
# Even though the significand is of the form 2^p (for some p), its boundaries
# are at the same distance. (This is the only exception).
@test diy_fp.s - boundary_minus.s == boundary_plus.s - diy_fp.s
@test (kOne64 << 39) == diy_fp.s - boundary_minus.s

largest_denormal32 = 0x007FFFFF
diy_fp  = Grisu.normalize(Grisu.Float(reinterpret(Float32,largest_denormal32)))
boundary_minus, boundary_plus = Grisu.normalizedbound(reinterpret(Float32,largest_denormal32))
@test diy_fp.e == boundary_minus.e
@test diy_fp.e == boundary_plus.e
@test diy_fp.s - boundary_minus.s == boundary_plus.s - diy_fp.s
@test (kOne64 << 40) == diy_fp.s - boundary_minus.s

max_float32 = 0x7f7fffff
diy_fp  = Grisu.normalize(Grisu.Float(reinterpret(Float32,max_float32)))
boundary_minus, boundary_plus = Grisu.normalizedbound(reinterpret(Float32,max_float32))
@test diy_fp.e == boundary_minus.e
@test diy_fp.e == boundary_plus.e
# max-value does not have a significand of the form 2^p (for some p).
# Therefore its boundaries are at the same distance.
@test diy_fp.s - boundary_minus.s == boundary_plus.s - diy_fp.s
@test (kOne64 << 39) == diy_fp.s - boundary_minus.s

#fastshortest
min_double = 5e-324
status,len,point,buffer = Grisu.fastshortest(min_double, buffer)
@test status
@test "5" == trimrep(buffer)
@test -323 == point
fill!(buffer,0);

max_double = 1.7976931348623157e308
status,len,point,buffer = Grisu.fastshortest(max_double, buffer)
@test status
@test "17976931348623157" == trimrep(buffer)
@test 309 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastshortest(4294967272.0, buffer)
@test status
@test "4294967272" == trimrep(buffer)
@test 10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastshortest(4.1855804968213567e298, buffer)
@test status
@test "4185580496821357" == trimrep(buffer)
@test 299 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastshortest(5.5626846462680035e-309, buffer)
@test status
@test "5562684646268003" == trimrep(buffer)
@test -308 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastshortest(2147483648.0, buffer)
@test status
@test "2147483648" == trimrep(buffer)
@test 10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastshortest(3.5844466002796428e+298, buffer)
@test !status  # Not all Grisu.fastshortest variants manage to compute this number.
if status
  @test "35844466002796428" == trimrep(buffer)
  @test 299 == point
  fill!(buffer,0);
end

smallest_normal64 = 0x0010000000000000
v = reinterpret(Float64,smallest_normal64)
status,len,point,buffer = Grisu.fastshortest(v,  buffer)
if status
  @test "22250738585072014" == trimrep(buffer)
  @test -307 == point
  fill!(buffer,0);
end

largest_denormal64 = 0x000FFFFFFFFFFFFF
v = reinterpret(Float64,largest_denormal64)
status,len,point,buffer = Grisu.fastshortest(v,  buffer)
if status
  @test "2225073858507201" == trimrep(buffer)
  @test -307 == point
  fill!(buffer,0);
end


min_float = float32(1e-45)
status,len,point,buffer = Grisu.fastshortest(min_float, buffer)
@test status
@test "1" == trimrep(buffer)
@test -44 == point
fill!(buffer,0);

max_float = 3.4028234f38 #float32(3.4028234e38)
status,len,point,buffer = Grisu.fastshortest(max_float, buffer)
@test status
@test "34028235" == trimrep(buffer)
@test 39 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastshortest(float32(4294967272.0), buffer)
@test status
@test "42949673" == trimrep(buffer)
@test 10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastshortest(float32(3.32306998946228968226e+35), buffer)
@test status
@test "332307" == trimrep(buffer)
@test 36 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastshortest(float32(1.2341e-41), buffer)
@test status
@test "12341" == trimrep(buffer)
@test -40 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastshortest(float32(3.3554432e7), buffer)
@test status
@test "33554432" == trimrep(buffer)
@test 8 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastshortest(float32(3.26494756798464e14), buffer)
@test status
@test "32649476" == trimrep(buffer)
@test 15 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastshortest(float32(3.91132223637771935344e37), buffer)
if status  # Not all Grisu.fastshortest variants manage to compute this number.
  @test "39113222" == trimrep(buffer)
  @test 38 == point
  fill!(buffer,0);
end

smallest_normal32 = 0x00800000
v = reinterpret(Float32,smallest_normal32)
status,len,point,buffer = Grisu.fastshortest(v,  buffer)
if status
  @test "11754944" == trimrep(buffer)
  @test -37 == point
  fill!(buffer,0);
end

largest_denormal32 = 0x007FFFFF
v = reinterpret(Float32,largest_denormal32)
status,len,point,buffer = Grisu.fastshortest(v,  buffer)
@test status
@test "11754942" == trimrep(buffer)
@test -37 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastprecision(1.0, 3, buffer)
@test status
@test 3 >= len-1
@test "1" == trimrep(buffer)
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastprecision(1.5, 10, buffer)
if status
  @test 10 >= len-1
  @test "15" == trimrep(buffer)
  @test 1 == point
  fill!(buffer,0);
end

min_double = 5e-324
status,len,point,buffer = Grisu.fastprecision(min_double, 5,buffer)
@test status
@test "49407" == trimrep(buffer)
@test -323 == point
fill!(buffer,0);

max_double = 1.7976931348623157e308
status,len,point,buffer = Grisu.fastprecision(max_double, 7,buffer)
@test status
@test "1797693" == trimrep(buffer)
@test 309 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastprecision(4294967272.0, 14,buffer)
if status
  @test 14 >= len-1
  @test "4294967272" == trimrep(buffer)
  @test 10 == point
  fill!(buffer,0);
end

status,len,point,buffer = Grisu.fastprecision(4.1855804968213567e298, 17,buffer)
@test status
@test "41855804968213567" == trimrep(buffer)
@test 299 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastprecision(5.5626846462680035e-309, 1,buffer)
@test status
@test "6" == trimrep(buffer)
@test -308 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastprecision(2147483648.0, 5,buffer)
@test status
@test "21475" == trimrep(buffer)
@test 10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastprecision(3.5844466002796428e+298, 10,buffer)
@test status
@test 10 >= len-1
@test "35844466" == trimrep(buffer)
@test 299 == point
fill!(buffer,0);

smallest_normal64 = 0x0010000000000000
v = reinterpret(Float64,smallest_normal64)
status,len,point,buffer = Grisu.fastprecision(v, 17, buffer)
@test status
@test "22250738585072014" == trimrep(buffer)
@test -307 == point
fill!(buffer,0);

largest_denormal64 = 0x000FFFFFFFFFFFFF
v = reinterpret(Float64,largest_denormal64)
status,len,point,buffer = Grisu.fastprecision(v, 17, buffer)
@test status
@test 20 >= len-1
@test "22250738585072009" == trimrep(buffer)
@test -307 == point
fill!(buffer,0);

v = 3.3161339052167390562200598e-237
status,len,point,buffer = Grisu.fastprecision(v, 18, buffer)
@test status
@test "331613390521673906" == trimrep(buffer)
@test -236 == point
fill!(buffer,0);

v = 7.9885183916008099497815232e+191
status,len,point,buffer = Grisu.fastprecision(v, 4, buffer)
@test status
@test "7989" == trimrep(buffer)
@test 192 == point
fill!(buffer,0);

#fastfixedtoa
status,len,point,buffer = Grisu.fastfixedtoa(1.0, 0,1, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(1.0, 0,15, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(1.0, 0,0, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0xFFFFFFFF, 0,5, buffer)
@test "4294967295" == bytestring(pointer(buffer))
@test 10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(4294967296.0, 0,5, buffer)
@test "4294967296" == bytestring(pointer(buffer)) #todo
@test 10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(1e21, 0,5, buffer)
@test "1" == bytestring(pointer(buffer)) #todo extra '0's
@test 22 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(999999999999999868928.00, 0,2, buffer)
@test "999999999999999868928" == bytestring(pointer(buffer)) #todo extra '0'
@test 21 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(6.9999999999999989514240000e+21, 0,5, buffer)
@test "6999999999999998951424" == bytestring(pointer(buffer)) #todo short several '9's
@test 22 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(1.5, 0,5, buffer)
@test "15" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(1.55, 0,5, buffer)
@test "155" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(1.55, 0,1, buffer)
@test "16" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(1.00000001, 0,15, buffer)
@test "100000001" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.1, 0,10, buffer)
@test "1" == bytestring(pointer(buffer))
@test 0 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.01, 0,10, buffer)
@test "1" == bytestring(pointer(buffer))
@test -1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.001, 0,10, buffer)
@test "1" == bytestring(pointer(buffer))
@test -2 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0001, 0,10, buffer) #todo
@test "1" == bytestring(pointer(buffer))
@test -3 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00001, 0,10, buffer) #todo
@test "1" == bytestring(pointer(buffer))
@test -4 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000001, 0,10, buffer)
@test "1" == bytestring(pointer(buffer))
@test -5 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0000001, 0,10, buffer)
@test "1" == bytestring(pointer(buffer))
@test -6 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00000001, 0,10, buffer)
@test "1" == bytestring(pointer(buffer))
@test -7 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000001, 0,10, buffer)
@test "1" == bytestring(pointer(buffer))
@test -8 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0000000001, 0,15, buffer)
@test "1" == bytestring(pointer(buffer))
@test -9 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00000000001, 0,15, buffer)
@test "1" == bytestring(pointer(buffer))
@test -10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000000001, 0,15, buffer)
@test "1" == bytestring(pointer(buffer))
@test -11 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0000000000001, 0,15, buffer)
@test "1" == bytestring(pointer(buffer))
@test -12 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00000000000001, 0,15, buffer)
@test "1" == bytestring(pointer(buffer))
@test -13 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000000000001, 0,20, buffer)
@test "1" == bytestring(pointer(buffer))
@test -14 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0000000000000001, 0,20, buffer)
@test "1" == bytestring(pointer(buffer))
@test -15 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00000000000000001, 0,20, buffer)
@test "1" == bytestring(pointer(buffer))
@test -16 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000000000000001, 0,20, buffer)
@test "1" == bytestring(pointer(buffer))
@test -17 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0000000000000000001, 0,20, buffer)
@test "1" == bytestring(pointer(buffer))
@test -18 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00000000000000000001, 0,20, buffer)
@test "1" == bytestring(pointer(buffer))
@test -19 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.10000000004, 0,10, buffer)
@test "1" == bytestring(pointer(buffer))
@test 0 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.01000000004, 0,10, buffer)
@test "1" == bytestring(pointer(buffer))
@test -1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00100000004, 0,10, buffer)
@test "1" == bytestring(pointer(buffer))
@test -2 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00010000004, 0,10, buffer) #todo
@test "1" == bytestring(pointer(buffer))
@test -3 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00001000004, 0,10, buffer)
@test "1" == bytestring(pointer(buffer))
@test -4 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00000100004, 0,10, buffer)
@test "1" == bytestring(pointer(buffer))
@test -5 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00000010004, 0,10, buffer)
@test "1" == bytestring(pointer(buffer))
@test -6 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00000001004, 0,10, buffer)
@test "1" == bytestring(pointer(buffer))
@test -7 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00000000104, 0,10, buffer)
@test "1" == bytestring(pointer(buffer))
@test -8 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0000000001000004, 0,15, buffer)
@test "1" == bytestring(pointer(buffer))
@test -9 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0000000000100004, 0,15, buffer)
@test "1" == bytestring(pointer(buffer))
@test -10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0000000000010004, 0,15, buffer)
@test "1" == bytestring(pointer(buffer))
@test -11 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0000000000001004, 0,15, buffer)
@test "1" == bytestring(pointer(buffer))
@test -12 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0000000000000104, 0,15, buffer)
@test "1" == bytestring(pointer(buffer))
@test -13 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000000000001000004, 0,20, buffer)
@test "1" == bytestring(pointer(buffer))
@test -14 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000000000000100004, 0,20, buffer)
@test "1" == bytestring(pointer(buffer))
@test -15 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000000000000010004, 0,20, buffer)
@test "1" == bytestring(pointer(buffer))
@test -16 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000000000000001004, 0,20, buffer)
@test "1" == bytestring(pointer(buffer))
@test -17 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000000000000000104, 0,20, buffer)
@test "1" == bytestring(pointer(buffer))
@test -18 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000000000000000014, 0,20, buffer)
@test "1" == bytestring(pointer(buffer))
@test -19 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.10000000006, 0,10, buffer)
@test "1000000001" == bytestring(pointer(buffer))
@test 0 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.01000000006, 0,10, buffer)
@test "100000001" == bytestring(pointer(buffer))
@test -1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00100000006, 0,10, buffer)
@test "10000001" == bytestring(pointer(buffer))
@test -2 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00010000006, 0,10, buffer)
@test "1000001" == bytestring(pointer(buffer))
@test -3 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00001000006, 0,10, buffer)
@test "100001" == bytestring(pointer(buffer))
@test -4 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00000100006, 0,10, buffer)
@test "10001" == bytestring(pointer(buffer))
@test -5 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00000010006, 0,10, buffer)
@test "1001" == bytestring(pointer(buffer))
@test -6 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00000001006, 0,10, buffer)
@test "101" == bytestring(pointer(buffer))
@test -7 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00000000106, 0,10, buffer)
@test "11" == bytestring(pointer(buffer))
@test -8 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0000000001000006, 0,15, buffer)
@test "100001" == bytestring(pointer(buffer))
@test -9 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0000000000100006, 0,15, buffer)
@test "10001" == bytestring(pointer(buffer))
@test -10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0000000000010006, 0,15, buffer)
@test "1001" == bytestring(pointer(buffer))
@test -11 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0000000000001006, 0,15, buffer)
@test "101" == bytestring(pointer(buffer))
@test -12 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0000000000000106, 0,15, buffer)
@test "11" == bytestring(pointer(buffer))
@test -13 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000000000001000006, 0,20, buffer)
@test "100001" == bytestring(pointer(buffer))
@test -14 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000000000000100006, 0,20, buffer)
@test "10001" == bytestring(pointer(buffer))
@test -15 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000000000000010006, 0,20, buffer)
@test "1001" == bytestring(pointer(buffer))
@test -16 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000000000000001006, 0,20, buffer)
@test "101" == bytestring(pointer(buffer))
@test -17 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000000000000000106, 0,20, buffer)
@test "11" == bytestring(pointer(buffer))
@test -18 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000000000000000016, 0,20, buffer)
@test "2" == bytestring(pointer(buffer))
@test -19 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.6, 0,0, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.96, 0,1, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.996, 0,2, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.9996, 0,3, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.99996, 0,4, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.999996, 0,5, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.9999996, 0,6, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.99999996, 0,7, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.999999996, 0,8, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.9999999996, 0,9, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.99999999996, 0,10, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.999999999996, 0,11, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.9999999999996, 0,12, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.99999999999996, 0,13, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.999999999999996, 0,14, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.9999999999999996, 0,15, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00999999999999996, 0,16, buffer)
@test "1" == bytestring(pointer(buffer))
@test -1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000999999999999996, 0,17, buffer)
@test "1" == bytestring(pointer(buffer))
@test -2 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.0000999999999999996, 0,18, buffer)
@test "1" == bytestring(pointer(buffer))
@test -3 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.00000999999999999996, 0,19, buffer)
@test "1" == bytestring(pointer(buffer))
@test -4 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.000000999999999999996, 0,20, buffer)
@test "1" == bytestring(pointer(buffer))
@test -5 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(323423.234234, 0,10, buffer)
@test "323423234234" == bytestring(pointer(buffer))
@test 6 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(12345678.901234, 0,4, buffer)
@test "123456789012" == bytestring(pointer(buffer))
@test 8 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(98765.432109, 0,5, buffer)
@test "9876543211" == bytestring(pointer(buffer))
@test 5 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(42, 0,20, buffer)
@test "42" == bytestring(pointer(buffer))
@test 2 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(0.5, 0,0, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(1e-23, 0,10, buffer)
@test "" == bytestring(pointer(buffer))
@test -10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(1e-123, 0,2, buffer)
@test "" == bytestring(pointer(buffer))
@test -2 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(1e-123, 0,0, buffer)
@test "" == bytestring(pointer(buffer))
@test 0 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(1e-23, 0,20, buffer)
@test "" == bytestring(pointer(buffer))
@test -20 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(1e-21, 0,20, buffer)
@test "" == bytestring(pointer(buffer))
@test -20 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(1e-22, 0,20, buffer)
@test "" == bytestring(pointer(buffer))
@test -20 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(6e-21, 0,20, buffer)
@test "1" == bytestring(pointer(buffer))
@test -19 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(9.1193616301674545152000000e+19, 0,0,buffer)
@test "91193616301674545152" == bytestring(pointer(buffer))
@test 20 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(4.8184662102767651659096515e-04, 0,19,buffer)
@test "4818466210276765" == bytestring(pointer(buffer))
@test -3 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(1.9023164229540652612705182e-23, 0,8,buffer)
@test "" == bytestring(pointer(buffer))
@test -8 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(1000000000000000128.0, 0,0,buffer)
@test "1000000000000000128" == bytestring(pointer(buffer))
@test 19 == point
fill!(buffer,0);

#bignumdtoa
status,len,point,buffer = Grisu.bignumdtoa(1.0, Grisu.SHORTEST, 0, buffer)
@test "1" == trimrep(buffer)
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(1.0, Grisu.FIXED, 3, buffer)
@test 3 >= len - 1 - point
@test "1" == trimrep(buffer)
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(1.0, Grisu.PRECISION, 3, buffer)
@test 3 >= len - 1
@test "1" == trimrep(buffer)
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(1.5, Grisu.SHORTEST, 0, buffer)
@test "15" == trimrep(buffer)
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(1.5, Grisu.FIXED, 10, buffer)
@test 10 >= len - 1 - point
@test "15" == trimrep(buffer)
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(1.5, Grisu.PRECISION, 10, buffer)
@test 10 >= len - 1
@test "15" == trimrep(buffer)
@test 1 == point
fill!(buffer,0);

min_double = 5e-324
status,len,point,buffer = Grisu.bignumdtoa(min_double, Grisu.SHORTEST, 0, buffer)
@test "5" == trimrep(buffer)
@test -323 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(min_double, Grisu.FIXED, 5, buffer)
@test 5 >= len - 1 - point
@test "" == trimrep(buffer)

status,len,point,buffer = Grisu.bignumdtoa(min_double, Grisu.PRECISION, 5, buffer)
@test 5 >= len - 1
@test "49407" == trimrep(buffer)
@test -323 == point
fill!(buffer,0);

max_double = 1.7976931348623157e308
status,len,point,buffer = Grisu.bignumdtoa(max_double, Grisu.SHORTEST, 0, buffer)
@test "17976931348623157" == trimrep(buffer)
@test 309 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(max_double, Grisu.PRECISION, 7, buffer)
@test 7 >= len - 1
@test "1797693" == trimrep(buffer)
@test 309 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(4294967272.0, Grisu.SHORTEST, 0, buffer)
@test "4294967272" == bytestring(pointer(buffer))
@test 10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(4294967272.0, Grisu.FIXED, 5, buffer)
@test "429496727200000" == bytestring(pointer(buffer))
@test 10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(4294967272.0, Grisu.PRECISION, 14, buffer)
@test 14 >= len - 1
@test "4294967272" == trimrep(buffer)
@test 10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(4.1855804968213567e298, Grisu.SHORTEST, 0,buffer)
@test "4185580496821357" == trimrep(buffer)
@test 299 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(4.1855804968213567e298, Grisu.PRECISION, 20,buffer)
@test 20 >= len - 1
@test "41855804968213567225" == trimrep(buffer)
@test 299 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(5.5626846462680035e-309, Grisu.SHORTEST, 0, buffer)
@test "5562684646268003" == trimrep(buffer)
@test -308 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(5.5626846462680035e-309, Grisu.PRECISION, 1, buffer)
@test 1 >= len - 1
@test "6" == trimrep(buffer)
@test -308 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(2147483648.0, Grisu.SHORTEST, 0, buffer)
@test "2147483648" == trimrep(buffer)
@test 10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(2147483648.0, Grisu.FIXED, 2, buffer)
@test 2 >= len - 1 - point
@test "2147483648" == trimrep(buffer)
@test 10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(2147483648.0, Grisu.PRECISION, 5, buffer)
@test 5 >= len - 1
@test "21475" == trimrep(buffer)
@test 10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(3.5844466002796428e+298, Grisu.SHORTEST, 0, buffer)
@test "35844466002796428" == trimrep(buffer)
@test 299 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(3.5844466002796428e+298, Grisu.PRECISION, 10, buffer)
@test 10 >= len - 1
@test "35844466" == trimrep(buffer)
@test 299 == point
fill!(buffer,0);

v = reinterpret(Float64,0x0010000000000000)
status,len,point,buffer = Grisu.bignumdtoa(v, Grisu.SHORTEST, 0, buffer)
@test "22250738585072014" == trimrep(buffer)
@test -307 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(v, Grisu.PRECISION, 20, buffer)
@test 20 >= len - 1
@test "22250738585072013831" == trimrep(buffer)
@test -307 == point
fill!(buffer,0);

v = reinterpret(Float64,0x000FFFFFFFFFFFFF)
status,len,point,buffer = Grisu.bignumdtoa(v, Grisu.SHORTEST, 0, buffer)
@test "2225073858507201" == trimrep(buffer)
@test -307 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(v, Grisu.PRECISION, 20, buffer)
@test 20 >= len - 1
@test "2225073858507200889" == trimrep(buffer)
@test -307 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(4128420500802942e-24, Grisu.SHORTEST, 0, buffer)
@test "4128420500802942" == trimrep(buffer)
@test -8 == point
fill!(buffer,0);

v = 3.9292015898194142585311918e-10;
status,len,point,buffer = Grisu.bignumdtoa(v, Grisu.SHORTEST, 0, buffer)
@test "39292015898194143" == trimrep(buffer)

v = 4194304.0;
status,len,point,buffer = Grisu.bignumdtoa(v, Grisu.FIXED, 5, buffer)
@test 5 >= len - 1 - point
@test "4194304" == trimrep(buffer)

v = 3.3161339052167390562200598e-237;
status,len,point,buffer = Grisu.bignumdtoa(v, Grisu.PRECISION, 19, buffer)
@test 19 >= len - 1
@test "3316133905216739056" == trimrep(buffer)
@test -236 == point
fill!(buffer,0);

v = 7.9885183916008099497815232e+191;
status,len,point,buffer = Grisu.bignumdtoa(v, Grisu.PRECISION, 4, buffer)
@test 4 >= len - 1
@test "7989" == trimrep(buffer)
@test 192 == point
fill!(buffer,0);

v = 1.0000000000000012800000000e+17;
status,len,point,buffer = Grisu.bignumdtoa(v, Grisu.FIXED, 1, buffer)
@test 1 >= len - 1 - point
@test "100000000000000128" == trimrep(buffer)
@test 18 == point
fill!(buffer,0);


min_float = float32(1e-45)
status,len,point,buffer = Grisu.bignumdtoa(min_float, Grisu.SHORTEST, 0, buffer)
@test "1" == trimrep(buffer)
@test -44 == point
fill!(buffer,0);

max_float = float32(3.4028234e38)
status,len,point,buffer = Grisu.bignumdtoa(max_float, Grisu.SHORTEST, 0, buffer)
@test "34028235" == trimrep(buffer)
@test 39 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(float32(4294967272.0), Grisu.SHORTEST, 0, buffer)
@test "42949673" == trimrep(buffer)
@test 10 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(float32(3.32306998946228968226e+35), Grisu.SHORTEST, 0, buffer)
@test "332307" == trimrep(buffer)
@test 36 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(float32(1.2341e-41), Grisu.SHORTEST, 0, buffer)
@test "12341" == trimrep(buffer)
@test -40 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(float32(3.3554432e7), Grisu.SHORTEST, 0, buffer)
@test "33554432" == trimrep(buffer)
@test 8 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(float32(3.26494756798464e14), Grisu.SHORTEST, 0, buffer)
@test "32649476" == trimrep(buffer)
@test 15 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.bignumdtoa(float32(3.91132223637771935344e37), Grisu.SHORTEST, 0, buffer)
@test "39113222" == trimrep(buffer)
@test 38 == point
fill!(buffer,0);

v = reinterpret(Float32,0x00800000)
status,len,point,buffer = Grisu.bignumdtoa(v, Grisu.SHORTEST, 0, buffer)
@test "11754944" == trimrep(buffer)
@test -37 == point
fill!(buffer,0);

v = reinterpret(Float32,0x007FFFFF)
status,len,point,buffer = Grisu.bignumdtoa(v, Grisu.SHORTEST, 0, buffer)
@test "11754942" == trimrep(buffer)
@test -37 == point
fill!(buffer,0);

#float16
min_double = realmin(Float16)
status,len,point,buffer = Grisu.fastshortest(min_double,buffer)
@test status
@test "6104" == trimrep(buffer)
@test -4 == point
fill!(buffer,0);

max_double = realmax(Float16)
status,len,point,buffer = Grisu.fastshortest(max_double,buffer)
@test status
@test "655" == trimrep(buffer)
@test 5 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastprecision(float16(1.0), 3, buffer)
@test status
@test 3 >= len-1
@test "1" == trimrep(buffer)
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastprecision(float16(1.5), 10, buffer)
if status
  @test 10 >= len-1
  @test "15" == trimrep(buffer)
  @test 1 == point
  fill!(buffer,0);
end

status,len,point,buffer = Grisu.fastfixedtoa(float16(1.0), 0,1, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(1.0), 0,15, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(1.0), 0,0, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(1.5), 0,5, buffer)
@test "15" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(1.55), 0,5, buffer)
@test "15498" == bytestring(pointer(buffer)) #todo
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(1.55), 0,1, buffer)
@test "15" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(1.00000001), 0,15, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(0.1), 0,10, buffer)
@test "999755859" == bytestring(pointer(buffer))
@test -1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(0.01), 0,10, buffer)
@test "100021362" == bytestring(pointer(buffer))
@test -1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(0.001), 0,10, buffer)
@test "10004044" == bytestring(pointer(buffer))
@test -2 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(0.0001), 0,10, buffer) #todo
@test "1000166" == bytestring(pointer(buffer))
@test -3 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(0.00001), 0,10, buffer) #todo
@test "100136" == bytestring(pointer(buffer))
@test -4 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(0.000001), 0,10, buffer)
@test "10133" == bytestring(pointer(buffer))
@test -5 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(0.0000001), 0,10, buffer)
@test "1192" == bytestring(pointer(buffer))
@test -6 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(0.6), 0,0, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(0.96), 0,1, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(0.996), 0,2, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(0.9996), 0,3, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(0.99996), 0,4, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(0.999996), 0,5, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(0.9999996), 0,6, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(0.99999996), 0,7, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(42), 0,20, buffer)
@test "42" == bytestring(pointer(buffer))
@test 2 == point
fill!(buffer,0);

status,len,point,buffer = Grisu.fastfixedtoa(float16(0.5), 0,0, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

#dtoa
len,point,neg,buffer = Grisu.grisu(0.0, Grisu.SHORTEST, 0, buffer)
@test "0" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(float32(0.0), Grisu.SHORTEST, 0, buffer)
@test "0" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(0.0, Grisu.FIXED, 2, buffer)
@test 1 >= len-1
@test "0" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(0.0, Grisu.PRECISION, 3, buffer)
@test 1 >= len-1
@test "0" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(1.0, Grisu.SHORTEST, 0, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(float32(1.0), Grisu.SHORTEST, 0, buffer)
@test "1" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(1.0, Grisu.FIXED, 3, buffer)
@test 3 >= len-1-point
@test "1" == trimrep(buffer)
@test 1 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(1.0, Grisu.PRECISION, 3, buffer)
@test 3 >= len-1
@test "1" == trimrep(buffer)
@test 1 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(1.5, Grisu.SHORTEST, 0, buffer)
@test "15" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(float32(1.5), Grisu.SHORTEST, 0, buffer)
@test "15" == bytestring(pointer(buffer))
@test 1 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(1.5, Grisu.FIXED, 10, buffer)
@test 10 >= len-1-point
@test "15" == trimrep(buffer)
@test 1 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(1.5, Grisu.PRECISION, 10, buffer)
@test 10 >= len-1
@test "15" == trimrep(buffer)
@test 1 == point
fill!(buffer,0);

min_double = 5e-324
len,point,neg,buffer = Grisu.grisu(min_double, Grisu.SHORTEST, 0, buffer)
@test "5" == bytestring(pointer(buffer))
@test -323 == point
fill!(buffer,0);

min_float = 1e-45
len,point,neg,buffer = Grisu.grisu(float32(min_float), Grisu.SHORTEST, 0, buffer)
@test "1" == bytestring(pointer(buffer))
@test -44 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(min_double, Grisu.FIXED, 5, buffer)
@test 5 >= len-1-point
@test "" == trimrep(buffer)
@test -5 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(min_double, Grisu.PRECISION, 5, buffer)
@test 5 >= len-1
@test "49407" == trimrep(buffer)
@test -323 == point
fill!(buffer,0);

max_double = 1.7976931348623157e308
len,point,neg,buffer = Grisu.grisu(max_double, Grisu.SHORTEST, 0, buffer)
@test "17976931348623157" == bytestring(pointer(buffer))
@test 309 == point
fill!(buffer,0);

max_float = 3.4028234e38
len,point,neg,buffer = Grisu.grisu(float32(max_float), Grisu.SHORTEST, 0,buffer)
@test "34028235" == bytestring(pointer(buffer))
@test 39 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(max_double, Grisu.PRECISION, 7, buffer)
@test 7 >= len-1
@test "1797693" == trimrep(buffer)
@test 309 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(4294967272.0, Grisu.SHORTEST, 0, buffer)
@test "4294967272" == bytestring(pointer(buffer))
@test 10 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(float32(4294967272.0), Grisu.SHORTEST, 0, buffer)
@test "42949673" == bytestring(pointer(buffer))
@test 10 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(4294967272.0, Grisu.FIXED, 5, buffer)
@test 5 >= len-1-point
@test "4294967272" == trimrep(buffer)
@test 10 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(4294967272.0, Grisu.PRECISION, 14,buffer)
@test 14 >= len-1
@test "4294967272" == trimrep(buffer)
@test 10 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(4.1855804968213567e298, Grisu.SHORTEST, 0,buffer)
@test "4185580496821357" == bytestring(pointer(buffer))
@test 299 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(4.1855804968213567e298, Grisu.PRECISION, 20,buffer)
@test 20 >= len-1
@test "41855804968213567225" == trimrep(buffer)
@test 299 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(5.5626846462680035e-309, Grisu.SHORTEST, 0,buffer)
@test "5562684646268003" == bytestring(pointer(buffer))
@test -308 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(5.5626846462680035e-309, Grisu.PRECISION, 1,buffer)
@test 1 >= len-1
@test "6" == trimrep(buffer)
@test -308 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(-2147483648.0, Grisu.SHORTEST, 0,buffer)
@test 1 == neg
@test "2147483648" == bytestring(pointer(buffer))
@test 10 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(float32(-2147483648.), Grisu.SHORTEST, 0,buffer)
@test 1 == neg
@test "21474836" == bytestring(pointer(buffer))
@test 10 == point
fill!(buffer,0);


len,point,neg,buffer = Grisu.grisu(-2147483648.0, Grisu.FIXED, 2, buffer)
@test 2 >= len-1-point
@test "2147483648" == trimrep(buffer)
@test 10 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(-2147483648.0, Grisu.PRECISION, 5,buffer)
@test 5 >= len-1
@test "21475" == trimrep(buffer)
@test 10 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(-3.5844466002796428e+298, Grisu.SHORTEST, 0,buffer)
@test 1 == neg
@test "35844466002796428" == bytestring(pointer(buffer))
@test 299 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(-3.5844466002796428e+298, Grisu.PRECISION, 10,buffer)
@test 1 == neg
@test 10 >= len-1
@test "35844466" == trimrep(buffer)
@test 299 == point
fill!(buffer,0);

v = reinterpret(Float64,0x0010000000000000)
len,point,neg,buffer = Grisu.grisu(v, Grisu.SHORTEST, 0, buffer)
@test "22250738585072014" == bytestring(pointer(buffer))
@test -307 == point
fill!(buffer,0);

f = reinterpret(Float32,0x00800000)
len,point,neg,buffer = Grisu.grisu(f, Grisu.SHORTEST, 0, buffer)
@test "11754944" == bytestring(pointer(buffer))
@test -37 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(v, Grisu.PRECISION, 20, buffer)
@test 20 >= len-1
@test "22250738585072013831" == trimrep(buffer)
@test -307 == point
fill!(buffer,0);

v = reinterpret(Float64,0x000FFFFFFFFFFFFF)
len,point,neg,buffer = Grisu.grisu(v, Grisu.SHORTEST, 0, buffer)
@test "2225073858507201" == bytestring(pointer(buffer))
@test -307 == point
fill!(buffer,0);

f = reinterpret(Float32,0x007FFFFF)
len,point,neg,buffer = Grisu.grisu(f, Grisu.SHORTEST, 0, buffer)
@test "11754942" == bytestring(pointer(buffer))
@test -37 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(v, Grisu.PRECISION, 20, buffer)
@test 20 >= len-1
@test "2225073858507200889" == trimrep(buffer)
@test -307 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(4128420500802942e-24, Grisu.SHORTEST, 0,buffer)
@test 0 == neg
@test "4128420500802942" == bytestring(pointer(buffer))
@test -8 == point
fill!(buffer,0);

v = -3.9292015898194142585311918e-10
len,point,neg,buffer = Grisu.grisu(v, Grisu.SHORTEST, 0, buffer)
@test "39292015898194143" == bytestring(pointer(buffer))
fill!(buffer,0);

f = float32(-3.9292015898194142585311918e-10)
len,point,neg,buffer = Grisu.grisu(f, Grisu.SHORTEST, 0, buffer)
@test "39292017" == bytestring(pointer(buffer))
fill!(buffer,0);

v = 4194304.0
len,point,neg,buffer = Grisu.grisu(v, Grisu.FIXED, 5, buffer)
@test 5 >= len-1-point
@test "4194304" == trimrep(buffer)
fill!(buffer,0);

v = 3.3161339052167390562200598e-237
len,point,neg,buffer = Grisu.grisu(v, Grisu.PRECISION, 19, buffer)
@test 19 >= len-1
@test "3316133905216739056" == trimrep(buffer)
@test -236 == point
fill!(buffer,0);

len,point,neg,buffer = Grisu.grisu(0.0, Grisu.SHORTEST, 0, buffer)
@test !neg

len,point,neg,buffer = Grisu.grisu(-0.0, Grisu.SHORTEST, 0, buffer)
@test neg

len,point,neg,buffer = Grisu.grisu(1.0, Grisu.SHORTEST, 0, buffer)
@test !neg

len,point,neg,buffer = Grisu.grisu(-1.0, Grisu.SHORTEST, 0, buffer)
@test neg

len,point,neg,buffer = Grisu.grisu(float32(0.0), Grisu.SHORTEST, 0, buffer)
@test !neg

len,point,neg,buffer = Grisu.grisu(-float32(0.0), Grisu.SHORTEST, 0, buffer)
@test neg

len,point,neg,buffer = Grisu.grisu(float32(1.0), Grisu.SHORTEST, 0, buffer)
@test !neg

len,point,neg,buffer = Grisu.grisu(-float32(1.0), Grisu.SHORTEST, 0, buffer)
@test neg

len,point,neg,buffer = Grisu.grisu(0.0, Grisu.PRECISION, 1, buffer)
@test !neg

len,point,neg,buffer = Grisu.grisu(-0.0, Grisu.PRECISION, 1, buffer)
@test neg

len,point,neg,buffer = Grisu.grisu(1.0, Grisu.PRECISION, 1, buffer)
@test !neg

len,point,neg,buffer = Grisu.grisu(-1.0, Grisu.PRECISION, 1, buffer)
@test neg

len,point,neg,buffer = Grisu.grisu(0.0, Grisu.FIXED, 1, buffer)
@test !neg

len,point,neg,buffer = Grisu.grisu(-0.0, Grisu.FIXED, 1, buffer)
@test neg

len,point,neg,buffer = Grisu.grisu(1.0, Grisu.FIXED, 1, buffer)
@test !neg

len,point,neg,buffer = Grisu.grisu(-1.0, Grisu.FIXED, 1, buffer)
@test neg


len,point,neg,buffer = Grisu.grisu(0.0, Grisu.PRECISION, 0, buffer)
@test 0 >= len-1
@test "" == bytestring(pointer(buffer))
@test !neg

len,point,neg,buffer = Grisu.grisu(1.0, Grisu.PRECISION, 0, buffer)
@test 0 >= len-1
@test "" == bytestring(pointer(buffer))
@test !neg

len,point,neg,buffer = Grisu.grisu(0.0, Grisu.FIXED, 0, buffer)
@test 1 >= len-1
@test "0" == bytestring(pointer(buffer))
@test !neg

len,point,neg,buffer = Grisu.grisu(1.0, Grisu.FIXED, 0, buffer)
@test 1 >= len-1
@test "1" == bytestring(pointer(buffer))
@test !neg
