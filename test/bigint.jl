a = BigInt("123456789012345678901234567890")
b = BigInt("123456789012345678901234567891")

@test typeof(a+1) == BigInt
@test a+1 == b
@test b == a+1
@test !(b == a)
@test b > a
@test b >= a
@test !(b < a)
@test !(b <= a)

c = BigInt("246913578024691357802469135780")
@test typeof(a * 2) == BigInt
@test a*2 == c
@test c-a == a
@test c == a + a
@test c+1 == a+b

d = BigInt("-246913578024691357802469135780")
@test typeof(d) == BigInt
@test d == -c

ee = typemax(Int64)
@test typeof(BigInt(ee)) == BigInt
@test BigInt(ee)+1 == BigInt("9223372036854775808")

#Multiple calls for sanity check, since we're doing direct memory manipulation
@test string(a) == "123456789012345678901234567890"
@test string(b) == "123456789012345678901234567891"
@test string(c) == "246913578024691357802469135780"
@test string(d) == "-246913578024691357802469135780"
@test string(a) == "123456789012345678901234567890"

for i = -10:10, j = [-10:-1,1:10]
    @test div(BigInt(i), BigInt(j)) == div(i,j)
    @test fld(BigInt(i), BigInt(j)) == fld(i,j)
    @test mod(BigInt(i), BigInt(j)) == mod(i,j)
    @test rem(BigInt(i), BigInt(j)) == rem(i,j)
end

@test typeof(BigInt(typemax(Int8))) == BigInt
@test typeof(BigInt(typemax(Int16))) == BigInt
@test typeof(BigInt(typemax(Int32))) == BigInt
@test typeof(BigInt(typemax(Int64))) == BigInt
#@test typeof(BigInt(typemax(Int128))) == BigInt

@test typeof(BigInt(true)) == BigInt
@test typeof(BigInt(typemax(Uint8))) == BigInt
@test typeof(BigInt(typemax(Uint16))) == BigInt
@test typeof(BigInt(typemax(Uint32))) == BigInt
@test typeof(BigInt(typemax(Uint64))) == BigInt
#@test typeof(BigInt(typemax(Uint128))) == BigInt

@test typeof(BigInt(BigInt(1))) == BigInt

@test a+int8(1) == b
@test a+int16(1) == b
@test a+int32(1) == b
@test a+int64(1) == b
#@test a+int128(1) == b

@test a+true == b
@test a+uint8(1) == b
@test a+uint16(1) == b
@test a+uint32(1) == b
@test a+uint64(1) == b
#@test a+uint128(1) == b

@test a+BigInt(1) == b

@test BigInt(5) << 3 == 40
@test BigInt(5) >> 1 == 2
@test BigInt(-5) << 3 == -40
@test BigInt(-5) >> 1 == -3

@test ~BigInt(123) == -124
@test BigInt(123) & BigInt(234) == 106
@test BigInt(123) | BigInt(234) == 251
@test BigInt(123) $ BigInt(234) == 145

@test gcd(BigInt(48), BigInt(180)) == 12
@test lcm(BigInt(48), BigInt(180)) == 720

@test factorial(BigInt(40)) == BigInt("815915283247897734345611269596115894272000000000")
@test binomial(BigInt(-53), 42) == BigInt("959509335087854414441273718")
@test binomial(BigInt(113), BigInt(42)) == BigInt("18672199984318438125634054194360")

# Large Fibonacci to exercise BigInt
# from Bill Hart, https://groups.google.com/group/julia-dev/browse_frm/thread/798e2d1322daf633
function mul(a::Vector{BigInt}, b::Vector{BigInt})
   x = a[2]*b[2]
   c = Array(BigInt,3)
   c[1] = a[1]*b[1] + x
   c[2] = a[1]*b[2] + a[2]*b[3]
   c[3] = x + a[3]*b[3]
   return c
end

function bigfib(n)
   if n == 0
      BigInt(1)
   elseif n == 1
      BigInt(1)
   else
      r = [BigInt(1), BigInt(1), BigInt(0)]
      s = [BigInt(1), BigInt(0), BigInt(1)]
      while n != 0
         if (n & 1) == 1
            s = mul(s,r)
         end
         n >>= 1
         if n != 0
            r = mul(r,r)
         end
      end
      s[1]
   end
end
@test length(string(bigfib(1000000))) == 208988
