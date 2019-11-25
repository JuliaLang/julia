# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.Ryu

const maxMantissa = (UInt64(1) << 53) - 1
todouble(sign, exp, mant) = Core.bitcast(Float64, (UInt64(sign) << 63) | (UInt64(exp) << 52) | (UInt64(mant)))

@testset "Ryu" begin

@testset "Float64" begin

@testset "Basic" begin
    @test Ryu.writeshortest(0.0) == "0.0"
    @test Ryu.writeshortest(-0.0) == "-0.0"
    @test Ryu.writeshortest(1.0) == "1.0"
    @test Ryu.writeshortest(-1.0) == "-1.0"
    @test Ryu.writeshortest(NaN) == "NaN"
    @test Ryu.writeshortest(Inf) == "Inf"
    @test Ryu.writeshortest(-Inf) == "-Inf"
end

@testset "SwitchToSubnormal" begin
    @test "2.2250738585072014e-308" == Ryu.writeshortest(2.2250738585072014e-308)
end

@testset "MinAndMax" begin
    @test "1.7976931348623157e308" == Ryu.writeshortest(Core.bitcast(Float64, 0x7fefffffffffffff))
    @test "5.0e-324" == Ryu.writeshortest(Core.bitcast(Float64, Int64(1)))
end

@testset "LotsOfTrailingZeros" begin
    @test "2.9802322387695312e-8" == Ryu.writeshortest(2.98023223876953125e-8)
end

@testset "Regression" begin
    @test "-2.109808898695963e16" == Ryu.writeshortest(-2.109808898695963e16)
    @test "4.940656e-318" == Ryu.writeshortest(4.940656e-318)
    @test "1.18575755e-316" == Ryu.writeshortest(1.18575755e-316)
    @test "2.989102097996e-312" == Ryu.writeshortest(2.989102097996e-312)
    @test "9.0608011534336e15" == Ryu.writeshortest(9.0608011534336e15)
    @test "4.708356024711512e18" == Ryu.writeshortest(4.708356024711512e18)
    @test "9.409340012568248e18" == Ryu.writeshortest(9.409340012568248e18)
    @test "1.2345678" == Ryu.writeshortest(1.2345678)
end

@testset "LooksLikePow5" begin
    # These numbers have a mantissa that is a multiple of the largest power of 5 that fits,
    # and an exponent that causes the computation for q to result in 22, which is a corner
    # case for Ryu.
    @test "5.764607523034235e39" == Ryu.writeshortest(Core.bitcast(Float64, 0x4830F0CF064DD592))
    @test "1.152921504606847e40" == Ryu.writeshortest(Core.bitcast(Float64, 0x4840F0CF064DD592))
    @test "2.305843009213694e40" == Ryu.writeshortest(Core.bitcast(Float64, 0x4850F0CF064DD592))
end

@testset "OutputLength" begin
    @test "1.0" == Ryu.writeshortest(1.0) # already tested in Basic
    @test "1.2" == Ryu.writeshortest(1.2)
    @test "1.23" == Ryu.writeshortest(1.23)
    @test "1.234" == Ryu.writeshortest(1.234)
    @test "1.2345" == Ryu.writeshortest(1.2345)
    @test "1.23456" == Ryu.writeshortest(1.23456)
    @test "1.234567" == Ryu.writeshortest(1.234567)
    @test "1.2345678" == Ryu.writeshortest(1.2345678) # already tested in Regressi
    @test "1.23456789" == Ryu.writeshortest(1.23456789)
    @test "1.234567895" == Ryu.writeshortest(1.234567895) # 1.234567890 would be trimm
    @test "1.2345678901" == Ryu.writeshortest(1.2345678901)
    @test "1.23456789012" == Ryu.writeshortest(1.23456789012)
    @test "1.234567890123" == Ryu.writeshortest(1.234567890123)
    @test "1.2345678901234" == Ryu.writeshortest(1.2345678901234)
    @test "1.23456789012345" == Ryu.writeshortest(1.23456789012345)
    @test "1.234567890123456" == Ryu.writeshortest(1.234567890123456)
    @test "1.2345678901234567" == Ryu.writeshortest(1.2345678901234567)

  # Test 32-bit chunking
    @test "4.294967294" == Ryu.writeshortest(4.294967294) # 2^32 -
    @test "4.294967295" == Ryu.writeshortest(4.294967295) # 2^32 -
    @test "4.294967296" == Ryu.writeshortest(4.294967296) # 2^
    @test "4.294967297" == Ryu.writeshortest(4.294967297) # 2^32 +
    @test "4.294967298" == Ryu.writeshortest(4.294967298) # 2^32 +
end

# Test min, max shift values in shiftright128
@testset "MinMaxShift" begin
    # 32-bit opt-size=0:  49 <= dist <= 50
    # 32-bit opt-size=1:  30 <= dist <= 50
    # 64-bit opt-size=0:  50 <= dist <= 50
    # 64-bit opt-size=1:  30 <= dist <= 50
    @test "1.7800590868057611e-307" == Ryu.writeshortest(todouble(false, 4, 0))
    # 32-bit opt-size=0:  49 <= dist <= 49
    # 32-bit opt-size=1:  28 <= dist <= 49
    # 64-bit opt-size=0:  50 <= dist <= 50
    # 64-bit opt-size=1:  28 <= dist <= 50
    @test "2.8480945388892175e-306" == Ryu.writeshortest(todouble(false, 6, maxMantissa))
    # 32-bit opt-size=0:  52 <= dist <= 53
    # 32-bit opt-size=1:   2 <= dist <= 53
    # 64-bit opt-size=0:  53 <= dist <= 53
    # 64-bit opt-size=1:   2 <= dist <= 53
    @test "2.446494580089078e-296" == Ryu.writeshortest(todouble(false, 41, 0))
    # 32-bit opt-size=0:  52 <= dist <= 52
    # 32-bit opt-size=1:   2 <= dist <= 52
    # 64-bit opt-size=0:  53 <= dist <= 53
    # 64-bit opt-size=1:   2 <= dist <= 53
    @test "4.8929891601781557e-296" == Ryu.writeshortest(todouble(false, 40, maxMantissa))

    # 32-bit opt-size=0:  57 <= dist <= 58
    # 32-bit opt-size=1:  57 <= dist <= 58
    # 64-bit opt-size=0:  58 <= dist <= 58
    # 64-bit opt-size=1:  58 <= dist <= 58
    @test "1.8014398509481984e16" == Ryu.writeshortest(todouble(false, 1077, 0))
    # 32-bit opt-size=0:  57 <= dist <= 57
    # 32-bit opt-size=1:  57 <= dist <= 57
    # 64-bit opt-size=0:  58 <= dist <= 58
    # 64-bit opt-size=1:  58 <= dist <= 58
    @test "3.6028797018963964e16" == Ryu.writeshortest(todouble(false, 1076, maxMantissa))
    # 32-bit opt-size=0:  51 <= dist <= 52
    # 32-bit opt-size=1:  51 <= dist <= 59
    # 64-bit opt-size=0:  52 <= dist <= 52
    # 64-bit opt-size=1:  52 <= dist <= 59
    @test "2.900835519859558e-216" == Ryu.writeshortest(todouble(false, 307, 0))
    # 32-bit opt-size=0:  51 <= dist <= 51
    # 32-bit opt-size=1:  51 <= dist <= 59
    # 64-bit opt-size=0:  52 <= dist <= 52
    # 64-bit opt-size=1:  52 <= dist <= 59
    @test "5.801671039719115e-216" == Ryu.writeshortest(todouble(false, 306, maxMantissa))

    # https:#github.com/ulfjack/ryu/commit/19e44d16d80236f5de25800f56d82606d1be00b9#commitcomment-30146483
    # 32-bit opt-size=0:  49 <= dist <= 49
    # 32-bit opt-size=1:  44 <= dist <= 49
    # 64-bit opt-size=0:  50 <= dist <= 50
    # 64-bit opt-size=1:  44 <= dist <= 50
    @test "3.196104012172126e-27" == Ryu.writeshortest(todouble(false, 934, 0x000FA7161A4D6E0C))
end

@testset "SmallIntegers" begin
    @test "9.007199254740991e15" == Ryu.writeshortest(9007199254740991.0)
    @test "9.007199254740992e15" == Ryu.writeshortest(9007199254740992.0)

    @test "1.0" == Ryu.writeshortest(1.0e+0)
    @test "12.0" == Ryu.writeshortest(1.2e+1)
    @test "123.0" == Ryu.writeshortest(1.23e+2)
    @test "1234.0" == Ryu.writeshortest(1.234e+3)
    @test "12345.0" == Ryu.writeshortest(1.2345e+4)
    @test "123456.0" == Ryu.writeshortest(1.23456e+5)
    @test "1.234567e6" == Ryu.writeshortest(1.234567e+6)
    @test "1.2345678e7" == Ryu.writeshortest(1.2345678e+7)
    @test "1.23456789e8" == Ryu.writeshortest(1.23456789e+8)
    @test "1.23456789e9" == Ryu.writeshortest(1.23456789e+9)
    @test "1.234567895e9" == Ryu.writeshortest(1.234567895e+9)
    @test "1.2345678901e10" == Ryu.writeshortest(1.2345678901e+10)
    @test "1.23456789012e11" == Ryu.writeshortest(1.23456789012e+11)
    @test "1.234567890123e12" == Ryu.writeshortest(1.234567890123e+12)
    @test "1.2345678901234e13" == Ryu.writeshortest(1.2345678901234e+13)
    @test "1.23456789012345e14" == Ryu.writeshortest(1.23456789012345e+14)
    @test "1.234567890123456e15" == Ryu.writeshortest(1.234567890123456e+15)

  # 10^i
    @test "1.0" == Ryu.writeshortest(1.0e+0)
    @test "10.0" == Ryu.writeshortest(1.0e+1)
    @test "100.0" == Ryu.writeshortest(1.0e+2)
    @test "1000.0" == Ryu.writeshortest(1.0e+3)
    @test "10000.0" == Ryu.writeshortest(1.0e+4)
    @test "100000.0" == Ryu.writeshortest(1.0e+5)
    @test "1.0e6" == Ryu.writeshortest(1.0e+6)
    @test "1.0e7" == Ryu.writeshortest(1.0e+7)
    @test "1.0e8" == Ryu.writeshortest(1.0e+8)
    @test "1.0e9" == Ryu.writeshortest(1.0e+9)
    @test "1.0e10" == Ryu.writeshortest(1.0e+10)
    @test "1.0e11" == Ryu.writeshortest(1.0e+11)
    @test "1.0e12" == Ryu.writeshortest(1.0e+12)
    @test "1.0e13" == Ryu.writeshortest(1.0e+13)
    @test "1.0e14" == Ryu.writeshortest(1.0e+14)
    @test "1.0e15" == Ryu.writeshortest(1.0e+15)

  # 10^15 + 10^i
    @test "1.000000000000001e15" == Ryu.writeshortest(1.0e+15 + 1.0e+0)
    @test "1.00000000000001e15" == Ryu.writeshortest(1.0e+15 + 1.0e+1)
    @test "1.0000000000001e15" == Ryu.writeshortest(1.0e+15 + 1.0e+2)
    @test "1.000000000001e15" == Ryu.writeshortest(1.0e+15 + 1.0e+3)
    @test "1.00000000001e15" == Ryu.writeshortest(1.0e+15 + 1.0e+4)
    @test "1.0000000001e15" == Ryu.writeshortest(1.0e+15 + 1.0e+5)
    @test "1.000000001e15" == Ryu.writeshortest(1.0e+15 + 1.0e+6)
    @test "1.00000001e15" == Ryu.writeshortest(1.0e+15 + 1.0e+7)
    @test "1.0000001e15" == Ryu.writeshortest(1.0e+15 + 1.0e+8)
    @test "1.000001e15" == Ryu.writeshortest(1.0e+15 + 1.0e+9)
    @test "1.00001e15" == Ryu.writeshortest(1.0e+15 + 1.0e+10)
    @test "1.0001e15" == Ryu.writeshortest(1.0e+15 + 1.0e+11)
    @test "1.001e15" == Ryu.writeshortest(1.0e+15 + 1.0e+12)
    @test "1.01e15" == Ryu.writeshortest(1.0e+15 + 1.0e+13)
    @test "1.1e15" == Ryu.writeshortest(1.0e+15 + 1.0e+14)

  # Largest power of 2 <= 10^(i+1)
    @test "8.0" == Ryu.writeshortest(8.0)
    @test "64.0" == Ryu.writeshortest(64.0)
    @test "512.0" == Ryu.writeshortest(512.0)
    @test "8192.0" == Ryu.writeshortest(8192.0)
    @test "65536.0" == Ryu.writeshortest(65536.0)
    @test "524288.0" == Ryu.writeshortest(524288.0)
    @test "8.388608e6" == Ryu.writeshortest(8388608.0)
    @test "6.7108864e7" == Ryu.writeshortest(67108864.0)
    @test "5.36870912e8" == Ryu.writeshortest(536870912.0)
    @test "8.589934592e9" == Ryu.writeshortest(8589934592.0)
    @test "6.8719476736e10" == Ryu.writeshortest(68719476736.0)
    @test "5.49755813888e11" == Ryu.writeshortest(549755813888.0)
    @test "8.796093022208e12" == Ryu.writeshortest(8796093022208.0)
    @test "7.0368744177664e13" == Ryu.writeshortest(70368744177664.0)
    @test "5.62949953421312e14" == Ryu.writeshortest(562949953421312.0)
    @test "9.007199254740992e15" == Ryu.writeshortest(9007199254740992.0)

  # 1000 * (Largest power of 2 <= 10^(i+1))
    @test "8000.0" == Ryu.writeshortest(8.0e+3)
    @test "64000.0" == Ryu.writeshortest(64.0e+3)
    @test "512000.0" == Ryu.writeshortest(512.0e+3)
    @test "8.192e6" == Ryu.writeshortest(8192.0e+3)
    @test "6.5536e7" == Ryu.writeshortest(65536.0e+3)
    @test "5.24288e8" == Ryu.writeshortest(524288.0e+3)
    @test "8.388608e9" == Ryu.writeshortest(8388608.0e+3)
    @test "6.7108864e10" == Ryu.writeshortest(67108864.0e+3)
    @test "5.36870912e11" == Ryu.writeshortest(536870912.0e+3)
    @test "8.589934592e12" == Ryu.writeshortest(8589934592.0e+3)
    @test "6.8719476736e13" == Ryu.writeshortest(68719476736.0e+3)
    @test "5.49755813888e14" == Ryu.writeshortest(549755813888.0e+3)
    @test "8.796093022208e15" == Ryu.writeshortest(8796093022208.0e+3)
end

end # Float64

@testset "Float32" begin

@testset "Basic" begin
    @test "0.0" == Ryu.writeshortest(Float32(0.0))
    @test "-0.0" == Ryu.writeshortest(Float32(-0.0))
    @test "1.0" == Ryu.writeshortest(Float32(1.0))
    @test "-1.0" == Ryu.writeshortest(Float32(-1.0))
    @test "NaN" == Ryu.writeshortest(Float32(NaN))
    @test "Inf" == Ryu.writeshortest(Float32(Inf))
    @test "-Inf" == Ryu.writeshortest(Float32(-Inf))
end

@testset "SwitchToSubnormal" begin
    @test "1.1754944e-38" == Ryu.writeshortest(1.1754944f-38)
end

@testset "MinAndMax" begin
    @test "3.4028235e38" == Ryu.writeshortest(Core.bitcast(Float32, 0x7f7fffff))
    @test "1.0e-45" == Ryu.writeshortest(Core.bitcast(Float32, Int32(1)))
end

# Check that we return the exact boundary if it is the shortest
# representation, but only if the original floating point number is even.
@testset "BoundaryRoundeven" begin
    @test "3.355445e7" == Ryu.writeshortest(3.355445f7)
    @test "9.0e9" == Ryu.writeshortest(8.999999f9)
    @test "3.436672e10" == Ryu.writeshortest(3.4366717f10)
end

# If the exact value is exactly halfway between two shortest representations,
# then we round to even. It seems like this only makes a difference if the
# last two digits are ...2|5 or ...7|5, and we cut off the 5.
@testset "exactValueRoundeven" begin
    @test "305404.12" == Ryu.writeshortest(3.0540412f5)
    @test "8099.0312" == Ryu.writeshortest(8.0990312f3)
end

@testset "LotsOfTrailingZeros" begin
    # Pattern for the first test: 00111001100000000000000000000000
    @test "0.00024414062" == Ryu.writeshortest(2.4414062f-4)
    @test "0.0024414062" == Ryu.writeshortest(2.4414062f-3)
    @test "0.0043945312" == Ryu.writeshortest(4.3945312f-3)
    @test "0.0063476562" == Ryu.writeshortest(6.3476562f-3)
end

@testset "Regression" begin
    @test "4.7223665e21" == Ryu.writeshortest(4.7223665f21)
    @test "8.388608e6" == Ryu.writeshortest(8388608f0)
    @test "1.6777216e7" == Ryu.writeshortest(1.6777216f7)
    @test "3.3554436e7" == Ryu.writeshortest(3.3554436f7)
    @test "6.7131496e7" == Ryu.writeshortest(6.7131496f7)
    @test "1.9310392e-38" == Ryu.writeshortest(1.9310392f-38)
    @test "-2.47e-43" == Ryu.writeshortest(-2.47f-43)
    @test "1.993244e-38" == Ryu.writeshortest(1.993244f-38)
    @test "4103.9004" == Ryu.writeshortest(4103.9003f0)
    @test "5.3399997e9" == Ryu.writeshortest(5.3399997f9)
    @test "6.0898e-39" == Ryu.writeshortest(6.0898f-39)
    @test "0.0010310042" == Ryu.writeshortest(0.0010310042f0)
    @test "2.882326e17" == Ryu.writeshortest(2.8823261f17)
    @test "7.038531e-26" == Ryu.writeshortest(7.0385309f-26)
    @test "9.223404e17" == Ryu.writeshortest(9.2234038f17)
    @test "6.710887e7" == Ryu.writeshortest(6.7108872f7)
    @test "1.0e-44" == Ryu.writeshortest(1.0f-44)
    @test "2.816025e14" == Ryu.writeshortest(2.816025f14)
    @test "9.223372e18" == Ryu.writeshortest(9.223372f18)
    @test "1.5846086e29" == Ryu.writeshortest(1.5846085f29)
    @test "1.1811161e19" == Ryu.writeshortest(1.1811161f19)
    @test "5.368709e18" == Ryu.writeshortest(5.368709f18)
    @test "4.6143166e18" == Ryu.writeshortest(4.6143165f18)
    @test "0.007812537" == Ryu.writeshortest(0.007812537f0)
    @test "1.0e-45" == Ryu.writeshortest(1.4f-45)
    @test "1.18697725e20" == Ryu.writeshortest(1.18697724f20)
    @test "1.00014165e-36" == Ryu.writeshortest(1.00014165f-36)
    @test "200.0" == Ryu.writeshortest(200f0)
    @test "3.3554432e7" == Ryu.writeshortest(3.3554432f7)
end

@testset "LooksLikePow5" begin
    # These numbers have a mantissa that is the largest power of 5 that fits,
    # and an exponent that causes the computation for q to result in 10, which is a corner
    # case for Ryu.
    @test "6.7108864e17" == Ryu.writeshortest(Core.bitcast(Float32, 0x5D1502F9))
    @test "1.3421773e18" == Ryu.writeshortest(Core.bitcast(Float32, 0x5D9502F9))
    @test "2.6843546e18" == Ryu.writeshortest(Core.bitcast(Float32, 0x5E1502F9))
end

@testset "OutputLength" begin
    @test "1.0" == Ryu.writeshortest(Float32(1.0))
    @test "1.2" == Ryu.writeshortest(Float32(1.2))
    @test "1.23" == Ryu.writeshortest(Float32(1.23))
    @test "1.234" == Ryu.writeshortest(Float32(1.234))
    @test "1.2345" == Ryu.writeshortest(Float32(1.2345))
    @test "1.23456" == Ryu.writeshortest(Float32(1.23456))
    @test "1.234567" == Ryu.writeshortest(Float32(1.234567))
    @test "1.2345678" == Ryu.writeshortest(Float32(1.2345678))
    @test "1.23456735e-36" == Ryu.writeshortest(Float32(1.23456735e-36))
end

end # Float32

@testset "Float16" begin

@testset "Basic" begin
    @test "0.0" == Ryu.writeshortest(Float16(0.0))
    @test "-0.0" == Ryu.writeshortest(Float16(-0.0))
    @test "1.0" == Ryu.writeshortest(Float16(1.0))
    @test "-1.0" == Ryu.writeshortest(Float16(-1.0))
    @test "NaN" == Ryu.writeshortest(Float16(NaN))
    @test "Inf" == Ryu.writeshortest(Float16(Inf))
    @test "-Inf" == Ryu.writeshortest(Float16(-Inf))
end

let x=floatmin(Float16)
    while x <= floatmax(Float16)
        @test parse(Float16, Ryu.writeshortest(x)) == x
        x = nextfloat(x)
    end
end

# function testfloats(T)
#     x = floatmin(T)
#     i = 0
#     fails = 0
#     success = 0
#     while x < floatmax(T)
#         test = parse(T, Ryu.writeshortest(x)) == x
#         if !test

#             fails += 1
#         else
#             success += 1
#         end
#         x = nextfloat(x)
#         i += 1

#     end
#     return fails / (fails + success)
# end

end # Float16

@testset "Ryu.writefixed" begin
    @testset "Basic" begin
        @test Ryu.writefixed(todouble(false, 1234, 99999), 0) ==
            "3291009114715486435425664845573426149758869524108446525879746560"
    end
    @testset "Zero" begin
        @test Ryu.writefixed(0.0, 4) == "0.0000"
        @test Ryu.writefixed(0.0, 3) == "0.000"
        @test Ryu.writefixed(0.0, 2) == "0.00"
        @test Ryu.writefixed(0.0, 1) == "0.0"
        @test Ryu.writefixed(0.0, 0) == "0"
    end
    @testset "MinMax" begin
        @test Ryu.writefixed(todouble(false, 0, 1), 1074) ==
            "0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" *
            "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" *
            "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" *
            "000000000000000000000000000000000000000000000000000000049406564584124654417656879286822137" *
            "236505980261432476442558568250067550727020875186529983636163599237979656469544571773092665" *
            "671035593979639877479601078187812630071319031140452784581716784898210368871863605699873072" *
            "305000638740915356498438731247339727316961514003171538539807412623856559117102665855668676" *
            "818703956031062493194527159149245532930545654440112748012970999954193198940908041656332452" *
            "475714786901472678015935523861155013480352649347201937902681071074917033322268447533357208" *
            "324319360923828934583680601060115061698097530783422773183292479049825247307763759272478746" *
            "560847782037344696995336470179726777175851256605511991315048911014510378627381672509558373" *
            "89733598993664809941164205702637090279242767544565229087538682506419718265533447265625"
        @test Ryu.writefixed(todouble(false, 2046, 0xFFFFFFFFFFFFF), 0) ==
            "179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558" *
            "632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245" *
            "490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168" *
            "738177180919299881250404026184124858368"
    end
    @testset "RoundToEven" begin
        @test Ryu.writefixed(0.125, 3) == "0.125"
        @test Ryu.writefixed(0.125, 2) == "0.12"
        @test Ryu.writefixed(0.375, 3) == "0.375"
        @test Ryu.writefixed(0.375, 2) == "0.38"
    end
    @testset "RoundToEvenInteger" begin
        @test Ryu.writefixed(2.5, 1) == "2.5"
        @test Ryu.writefixed(2.5, 0) == "2"
        @test Ryu.writefixed(3.5, 1) == "3.5"
        @test Ryu.writefixed(3.5, 0) == "4"
    end
    @testset "NonRoundToEvenScenarios" begin
        @test Ryu.writefixed(0.748046875, 3) == "0.748"
        @test Ryu.writefixed(0.748046875, 2) == "0.75"
        @test Ryu.writefixed(0.748046875, 1) == "0.7"

        @test Ryu.writefixed(0.2509765625, 3) == "0.251"
        @test Ryu.writefixed(0.2509765625, 2) == "0.25"
        @test Ryu.writefixed(0.2509765625, 1) == "0.3"

        @test Ryu.writefixed(todouble(false, 1021, 1), 54) == "0.250000000000000055511151231257827021181583404541015625"
        @test Ryu.writefixed(todouble(false, 1021, 1),  3) == "0.250"
        @test Ryu.writefixed(todouble(false, 1021, 1),  2) == "0.25"
        @test Ryu.writefixed(todouble(false, 1021, 1),  1) == "0.3"
    end
    @testset "VaryingPrecision" begin
        @test Ryu.writefixed(1729.142857142857, 47) == "1729.14285714285711037518922239542007446289062500000"
        @test Ryu.writefixed(1729.142857142857, 46) == "1729.1428571428571103751892223954200744628906250000"
        @test Ryu.writefixed(1729.142857142857, 45) == "1729.142857142857110375189222395420074462890625000"
        @test Ryu.writefixed(1729.142857142857, 44) == "1729.14285714285711037518922239542007446289062500"
        @test Ryu.writefixed(1729.142857142857, 43) == "1729.1428571428571103751892223954200744628906250"
        @test Ryu.writefixed(1729.142857142857, 42) == "1729.142857142857110375189222395420074462890625"
        @test Ryu.writefixed(1729.142857142857, 41) == "1729.14285714285711037518922239542007446289062"
        @test Ryu.writefixed(1729.142857142857, 40) == "1729.1428571428571103751892223954200744628906"
        @test Ryu.writefixed(1729.142857142857, 39) == "1729.142857142857110375189222395420074462891"
        @test Ryu.writefixed(1729.142857142857, 38) == "1729.14285714285711037518922239542007446289"
        @test Ryu.writefixed(1729.142857142857, 37) == "1729.1428571428571103751892223954200744629"
        @test Ryu.writefixed(1729.142857142857, 36) == "1729.142857142857110375189222395420074463"
        @test Ryu.writefixed(1729.142857142857, 35) == "1729.14285714285711037518922239542007446"
        @test Ryu.writefixed(1729.142857142857, 34) == "1729.1428571428571103751892223954200745"
        @test Ryu.writefixed(1729.142857142857, 33) == "1729.142857142857110375189222395420074"
        @test Ryu.writefixed(1729.142857142857, 32) == "1729.14285714285711037518922239542007"
        @test Ryu.writefixed(1729.142857142857, 31) == "1729.1428571428571103751892223954201"
        @test Ryu.writefixed(1729.142857142857, 30) == "1729.142857142857110375189222395420"
        @test Ryu.writefixed(1729.142857142857, 29) == "1729.14285714285711037518922239542"
        @test Ryu.writefixed(1729.142857142857, 28) == "1729.1428571428571103751892223954"
        @test Ryu.writefixed(1729.142857142857, 27) == "1729.142857142857110375189222395"
        @test Ryu.writefixed(1729.142857142857, 26) == "1729.14285714285711037518922240"
        @test Ryu.writefixed(1729.142857142857, 25) == "1729.1428571428571103751892224"
        @test Ryu.writefixed(1729.142857142857, 24) == "1729.142857142857110375189222"
        @test Ryu.writefixed(1729.142857142857, 23) == "1729.14285714285711037518922"
        @test Ryu.writefixed(1729.142857142857, 22) == "1729.1428571428571103751892"
        @test Ryu.writefixed(1729.142857142857, 21) == "1729.142857142857110375189"
        @test Ryu.writefixed(1729.142857142857, 20) == "1729.14285714285711037519"
        @test Ryu.writefixed(1729.142857142857, 19) == "1729.1428571428571103752"
        @test Ryu.writefixed(1729.142857142857, 18) == "1729.142857142857110375"
        @test Ryu.writefixed(1729.142857142857, 17) == "1729.14285714285711038"
        @test Ryu.writefixed(1729.142857142857, 16) == "1729.1428571428571104"
        @test Ryu.writefixed(1729.142857142857, 15) == "1729.142857142857110"
        @test Ryu.writefixed(1729.142857142857, 14) == "1729.14285714285711"
        @test Ryu.writefixed(1729.142857142857, 13) == "1729.1428571428571"
        @test Ryu.writefixed(1729.142857142857, 12) == "1729.142857142857"
        @test Ryu.writefixed(1729.142857142857, 11) == "1729.14285714286"
        @test Ryu.writefixed(1729.142857142857, 10) == "1729.1428571429"
        @test Ryu.writefixed(1729.142857142857,  9) == "1729.142857143"
        @test Ryu.writefixed(1729.142857142857,  8) == "1729.14285714"
        @test Ryu.writefixed(1729.142857142857,  7) == "1729.1428571"
        @test Ryu.writefixed(1729.142857142857,  6) == "1729.142857"
        @test Ryu.writefixed(1729.142857142857,  5) == "1729.14286"
        @test Ryu.writefixed(1729.142857142857,  4) == "1729.1429"
        @test Ryu.writefixed(1729.142857142857,  3) == "1729.143"
        @test Ryu.writefixed(1729.142857142857,  2) == "1729.14"
        @test Ryu.writefixed(1729.142857142857,  1) == "1729.1"
        @test Ryu.writefixed(1729.142857142857,  0) == "1729"
    end

    @testset "Carrying" begin
        @test Ryu.writefixed(  0.0009, 4) == "0.0009"
        @test Ryu.writefixed(  0.0009, 3) == "0.001"
        @test Ryu.writefixed(  0.0029, 4) == "0.0029"
        @test Ryu.writefixed(  0.0029, 3) == "0.003"
        @test Ryu.writefixed(  0.0099, 4) == "0.0099"
        @test Ryu.writefixed(  0.0099, 3) == "0.010"
        @test Ryu.writefixed(  0.0299, 4) == "0.0299"
        @test Ryu.writefixed(  0.0299, 3) == "0.030"
        @test Ryu.writefixed(  0.0999, 4) == "0.0999"
        @test Ryu.writefixed(  0.0999, 3) == "0.100"
        @test Ryu.writefixed(  0.2999, 4) == "0.2999"
        @test Ryu.writefixed(  0.2999, 3) == "0.300"
        @test Ryu.writefixed(  0.9999, 4) == "0.9999"
        @test Ryu.writefixed(  0.9999, 3) == "1.000"
        @test Ryu.writefixed(  2.9999, 4) == "2.9999"
        @test Ryu.writefixed(  2.9999, 3) == "3.000"
        @test Ryu.writefixed(  9.9999, 4) == "9.9999"
        @test Ryu.writefixed(  9.9999, 3) == "10.000"
        @test Ryu.writefixed( 29.9999, 4) == "29.9999"
        @test Ryu.writefixed( 29.9999, 3) == "30.000"
        @test Ryu.writefixed( 99.9999, 4) == "99.9999"
        @test Ryu.writefixed( 99.9999, 3) == "100.000"
        @test Ryu.writefixed(299.9999, 4) == "299.9999"
        @test Ryu.writefixed(299.9999, 3) == "300.000"

        @test Ryu.writefixed(  0.09, 2) == "0.09"
        @test Ryu.writefixed(  0.09, 1) == "0.1"
        @test Ryu.writefixed(  0.29, 2) == "0.29"
        @test Ryu.writefixed(  0.29, 1) == "0.3"
        @test Ryu.writefixed(  0.99, 2) == "0.99"
        @test Ryu.writefixed(  0.99, 1) == "1.0"
        @test Ryu.writefixed(  2.99, 2) == "2.99"
        @test Ryu.writefixed(  2.99, 1) == "3.0"
        @test Ryu.writefixed(  9.99, 2) == "9.99"
        @test Ryu.writefixed(  9.99, 1) == "10.0"
        @test Ryu.writefixed( 29.99, 2) == "29.99"
        @test Ryu.writefixed( 29.99, 1) == "30.0"
        @test Ryu.writefixed( 99.99, 2) == "99.99"
        @test Ryu.writefixed( 99.99, 1) == "100.0"
        @test Ryu.writefixed(299.99, 2) == "299.99"
        @test Ryu.writefixed(299.99, 1) == "300.0"

        @test Ryu.writefixed(  0.9, 1) == "0.9"
        @test Ryu.writefixed(  0.9, 0) == "1"
        @test Ryu.writefixed(  2.9, 1) == "2.9"
        @test Ryu.writefixed(  2.9, 0) == "3"
        @test Ryu.writefixed(  9.9, 1) == "9.9"
        @test Ryu.writefixed(  9.9, 0) == "10"
        @test Ryu.writefixed( 29.9, 1) == "29.9"
        @test Ryu.writefixed( 29.9, 0) == "30"
        @test Ryu.writefixed( 99.9, 1) == "99.9"
        @test Ryu.writefixed( 99.9, 0) == "100"
        @test Ryu.writefixed(299.9, 1) == "299.9"
        @test Ryu.writefixed(299.9, 0) == "300"
    end

    @testset "RoundingResultZero" begin
        @test Ryu.writefixed(0.004, 3) == "0.004"
        @test Ryu.writefixed(0.004, 2) == "0.00"
        @test Ryu.writefixed(0.4, 1) == "0.4"
        @test Ryu.writefixed(0.4, 0) == "0"
        @test Ryu.writefixed(0.5, 1) == "0.5"
        @test Ryu.writefixed(0.5, 0) == "0"
    end

    @testset "Regression" begin
        @test Ryu.writefixed(7.018232e-82, 6) == "0.000000"
    end

end # fixed

@testset "Ryu.writeexp" begin

@testset "Basic" begin
    @test Ryu.writeexp(todouble(false, 1234, 99999), 62) ==
    "3.29100911471548643542566484557342614975886952410844652587974656e+63"
end

@testset "Zero" begin
    @test Ryu.writeexp(0.0, 4) == "0.0000e+00"
    @test Ryu.writeexp(0.0, 3) == "0.000e+00"
    @test Ryu.writeexp(0.0, 2) == "0.00e+00"
    @test Ryu.writeexp(0.0, 1) == "0.0e+00"
    @test Ryu.writeexp(0.0, 0) == "0e+00"
end

@testset "MinMax" begin
    @test Ryu.writeexp(todouble(false, 0, 1), 750) ==
    "4.9406564584124654417656879286822137236505980261432476442558568250067550727020875186529983" *
    "636163599237979656469544571773092665671035593979639877479601078187812630071319031140452784" *
    "581716784898210368871863605699873072305000638740915356498438731247339727316961514003171538" *
    "539807412623856559117102665855668676818703956031062493194527159149245532930545654440112748" *
    "012970999954193198940908041656332452475714786901472678015935523861155013480352649347201937" *
    "902681071074917033322268447533357208324319360923828934583680601060115061698097530783422773" *
    "183292479049825247307763759272478746560847782037344696995336470179726777175851256605511991" *
    "315048911014510378627381672509558373897335989936648099411642057026370902792427675445652290" *
    "87538682506419718265533447265625e-324"

    @test Ryu.writeexp(todouble(false, 2046, 0xFFFFFFFFFFFFF), 308) ==
    "1.7976931348623157081452742373170435679807056752584499659891747680315726078002853876058955" *
    "863276687817154045895351438246423432132688946418276846754670353751698604991057655128207624" *
    "549009038932894407586850845513394230458323690322294816580855933212334827479782620414472316" *
    "8738177180919299881250404026184124858368e+308"
end

@testset "RoundToEven" begin
    @test Ryu.writeexp(0.125, 2) == "1.25e-01"
    @test Ryu.writeexp(0.125, 1) == "1.2e-01"
    @test Ryu.writeexp(0.375, 2) == "3.75e-01"
    @test Ryu.writeexp(0.375, 1) == "3.8e-01"
end

@testset "RoundToEvenInteger" begin
    @test Ryu.writeexp(2.5, 1) == "2.5e+00"
    @test Ryu.writeexp(2.5, 0) == "2e+00"
    @test Ryu.writeexp(3.5, 1) == "3.5e+00"
    @test Ryu.writeexp(3.5, 0) == "4e+00"
end

@testset "NonRoundToEvenScenarios" begin
    @test Ryu.writeexp(0.748046875, 2) == "7.48e-01"
    @test Ryu.writeexp(0.748046875, 1) == "7.5e-01"
    @test Ryu.writeexp(0.748046875, 0) == "7e-01"    # 0.75 would round to "8e-01", but this is smaller

    @test Ryu.writeexp(0.2509765625, 2) == "2.51e-01"
    @test Ryu.writeexp(0.2509765625, 1) == "2.5e-01"
    @test Ryu.writeexp(0.2509765625, 0) == "3e-01"    # 0.25 would round to "2e-01", but this is larger

    @test Ryu.writeexp(todouble(false, 1021, 1), 53) ==
    "2.50000000000000055511151231257827021181583404541015625e-01"
    @test Ryu.writeexp(todouble(false, 1021, 1),  2) ==
    "2.50e-01"
    @test Ryu.writeexp(todouble(false, 1021, 1),  1) ==
    "2.5e-01"
    @test Ryu.writeexp(todouble(false, 1021, 1),  0) ==
    "3e-01"    # 0.25 would round to "2e-01", but this is larger (again)
end

@testset "VaryingPrecision" begin
    @test Ryu.writeexp(1729.142857142857, 50) == "1.72914285714285711037518922239542007446289062500000e+03"
    @test Ryu.writeexp(1729.142857142857, 49) == "1.7291428571428571103751892223954200744628906250000e+03"
    @test Ryu.writeexp(1729.142857142857, 48) == "1.729142857142857110375189222395420074462890625000e+03"
    @test Ryu.writeexp(1729.142857142857, 47) == "1.72914285714285711037518922239542007446289062500e+03"
    @test Ryu.writeexp(1729.142857142857, 46) == "1.7291428571428571103751892223954200744628906250e+03"
    @test Ryu.writeexp(1729.142857142857, 45) == "1.729142857142857110375189222395420074462890625e+03"
    @test Ryu.writeexp(1729.142857142857, 44) == "1.72914285714285711037518922239542007446289062e+03"
    @test Ryu.writeexp(1729.142857142857, 43) == "1.7291428571428571103751892223954200744628906e+03"
    @test Ryu.writeexp(1729.142857142857, 42) == "1.729142857142857110375189222395420074462891e+03"
    @test Ryu.writeexp(1729.142857142857, 41) == "1.72914285714285711037518922239542007446289e+03"
    @test Ryu.writeexp(1729.142857142857, 40) == "1.7291428571428571103751892223954200744629e+03"
    @test Ryu.writeexp(1729.142857142857, 39) == "1.729142857142857110375189222395420074463e+03"
    @test Ryu.writeexp(1729.142857142857, 38) == "1.72914285714285711037518922239542007446e+03"
    @test Ryu.writeexp(1729.142857142857, 37) == "1.7291428571428571103751892223954200745e+03"
    @test Ryu.writeexp(1729.142857142857, 36) == "1.729142857142857110375189222395420074e+03"
    @test Ryu.writeexp(1729.142857142857, 35) == "1.72914285714285711037518922239542007e+03"
    @test Ryu.writeexp(1729.142857142857, 34) == "1.7291428571428571103751892223954201e+03"
    @test Ryu.writeexp(1729.142857142857, 33) == "1.729142857142857110375189222395420e+03"
    @test Ryu.writeexp(1729.142857142857, 32) == "1.72914285714285711037518922239542e+03"
    @test Ryu.writeexp(1729.142857142857, 31) == "1.7291428571428571103751892223954e+03"
    @test Ryu.writeexp(1729.142857142857, 30) == "1.729142857142857110375189222395e+03"
    @test Ryu.writeexp(1729.142857142857, 29) == "1.72914285714285711037518922240e+03"
    @test Ryu.writeexp(1729.142857142857, 28) == "1.7291428571428571103751892224e+03"
    @test Ryu.writeexp(1729.142857142857, 27) == "1.729142857142857110375189222e+03"
    @test Ryu.writeexp(1729.142857142857, 26) == "1.72914285714285711037518922e+03"
    @test Ryu.writeexp(1729.142857142857, 25) == "1.7291428571428571103751892e+03"
    @test Ryu.writeexp(1729.142857142857, 24) == "1.729142857142857110375189e+03"
    @test Ryu.writeexp(1729.142857142857, 23) == "1.72914285714285711037519e+03"
    @test Ryu.writeexp(1729.142857142857, 22) == "1.7291428571428571103752e+03"
    @test Ryu.writeexp(1729.142857142857, 21) == "1.729142857142857110375e+03"
    @test Ryu.writeexp(1729.142857142857, 20) == "1.72914285714285711038e+03"
    @test Ryu.writeexp(1729.142857142857, 19) == "1.7291428571428571104e+03"
    @test Ryu.writeexp(1729.142857142857, 18) == "1.729142857142857110e+03"
    @test Ryu.writeexp(1729.142857142857, 17) == "1.72914285714285711e+03"
    @test Ryu.writeexp(1729.142857142857, 16) == "1.7291428571428571e+03"
    @test Ryu.writeexp(1729.142857142857, 15) == "1.729142857142857e+03"
    @test Ryu.writeexp(1729.142857142857, 14) == "1.72914285714286e+03"
    @test Ryu.writeexp(1729.142857142857, 13) == "1.7291428571429e+03"
    @test Ryu.writeexp(1729.142857142857, 12) == "1.729142857143e+03"
    @test Ryu.writeexp(1729.142857142857, 11) == "1.72914285714e+03"
    @test Ryu.writeexp(1729.142857142857, 10) == "1.7291428571e+03"
    @test Ryu.writeexp(1729.142857142857,  9) == "1.729142857e+03"
    @test Ryu.writeexp(1729.142857142857,  8) == "1.72914286e+03"
    @test Ryu.writeexp(1729.142857142857,  7) == "1.7291429e+03"
    @test Ryu.writeexp(1729.142857142857,  6) == "1.729143e+03"
    @test Ryu.writeexp(1729.142857142857,  5) == "1.72914e+03"
    @test Ryu.writeexp(1729.142857142857,  4) == "1.7291e+03"
    @test Ryu.writeexp(1729.142857142857,  3) == "1.729e+03"
    @test Ryu.writeexp(1729.142857142857,  2) == "1.73e+03"
    @test Ryu.writeexp(1729.142857142857,  1) == "1.7e+03"
    @test Ryu.writeexp(1729.142857142857,  0) == "2e+03"
end

@testset "Carrying" begin
    @test Ryu.writeexp(2.0009, 4) == "2.0009e+00"
    @test Ryu.writeexp(2.0009, 3) == "2.001e+00"
    @test Ryu.writeexp(2.0029, 4) == "2.0029e+00"
    @test Ryu.writeexp(2.0029, 3) == "2.003e+00"
    @test Ryu.writeexp(2.0099, 4) == "2.0099e+00"
    @test Ryu.writeexp(2.0099, 3) == "2.010e+00"
    @test Ryu.writeexp(2.0299, 4) == "2.0299e+00"
    @test Ryu.writeexp(2.0299, 3) == "2.030e+00"
    @test Ryu.writeexp(2.0999, 4) == "2.0999e+00"
    @test Ryu.writeexp(2.0999, 3) == "2.100e+00"
    @test Ryu.writeexp(2.2999, 4) == "2.2999e+00"
    @test Ryu.writeexp(2.2999, 3) == "2.300e+00"
    @test Ryu.writeexp(2.9999, 4) == "2.9999e+00"
    @test Ryu.writeexp(2.9999, 3) == "3.000e+00"
    @test Ryu.writeexp(9.9999, 4) == "9.9999e+00"
    @test Ryu.writeexp(9.9999, 3) == "1.000e+01"

    @test Ryu.writeexp(2.09, 2) == "2.09e+00"
    @test Ryu.writeexp(2.09, 1) == "2.1e+00"
    @test Ryu.writeexp(2.29, 2) == "2.29e+00"
    @test Ryu.writeexp(2.29, 1) == "2.3e+00"
    @test Ryu.writeexp(2.99, 2) == "2.99e+00"
    @test Ryu.writeexp(2.99, 1) == "3.0e+00"
    @test Ryu.writeexp(9.99, 2) == "9.99e+00"
    @test Ryu.writeexp(9.99, 1) == "1.0e+01"

    @test Ryu.writeexp(2.9, 1) == "2.9e+00"
    @test Ryu.writeexp(2.9, 0) == "3e+00"
    @test Ryu.writeexp(9.9, 1) == "9.9e+00"
    @test Ryu.writeexp(9.9, 0) == "1e+01"
end

@testset "Exponents" begin
    @test Ryu.writeexp(9.99e-100, 2) == "9.99e-100"
    @test Ryu.writeexp(9.99e-99 , 2) == "9.99e-99"
    @test Ryu.writeexp(9.99e-10 , 2) == "9.99e-10"
    @test Ryu.writeexp(9.99e-09 , 2) == "9.99e-09"
    @test Ryu.writeexp(9.99e-01 , 2) == "9.99e-01"
    @test Ryu.writeexp(9.99e+00 , 2) == "9.99e+00"
    @test Ryu.writeexp(9.99e+01 , 2) == "9.99e+01"
    @test Ryu.writeexp(9.99e+09 , 2) == "9.99e+09"
    @test Ryu.writeexp(9.99e+10 , 2) == "9.99e+10"
    @test Ryu.writeexp(9.99e+99 , 2) == "9.99e+99"
    @test Ryu.writeexp(9.99e+100, 2) == "9.99e+100"

    @test Ryu.writeexp(9.99e-100, 1) == "1.0e-99"
    @test Ryu.writeexp(9.99e-99 , 1) == "1.0e-98"
    @test Ryu.writeexp(9.99e-10 , 1) == "1.0e-09"
    @test Ryu.writeexp(9.99e-09 , 1) == "1.0e-08"
    @test Ryu.writeexp(9.99e-01 , 1) == "1.0e+00"
    @test Ryu.writeexp(9.99e+00 , 1) == "1.0e+01"
    @test Ryu.writeexp(9.99e+01 , 1) == "1.0e+02"
    @test Ryu.writeexp(9.99e+09 , 1) == "1.0e+10"
    @test Ryu.writeexp(9.99e+10 , 1) == "1.0e+11"
    @test Ryu.writeexp(9.99e+99 , 1) == "1.0e+100"
    @test Ryu.writeexp(9.99e+100, 1) == "1.0e+101"
end

@testset "PrintDecimalPoint" begin
  # These values exercise each codepath.
    @test Ryu.writeexp(1e+54, 0) == "1e+54"
    @test Ryu.writeexp(1e+54, 1) == "1.0e+54"
    @test Ryu.writeexp(1e-63, 0) == "1e-63"
    @test Ryu.writeexp(1e-63, 1) == "1.0e-63"
    @test Ryu.writeexp(1e+83, 0) == "1e+83"
    @test Ryu.writeexp(1e+83, 1) == "1.0e+83"
end

end # exp

@testset "compact" begin

    stringcompact(x) = sprint(show, x; context=:compact => true)

    @test stringcompact(0.49999999) == "0.5"
    @test stringcompact(0.459999999) == "0.46"
    @test stringcompact(0.20058603493384108) == "0.200586"
    @test stringcompact(0.9999999) == "1.0"
    @test stringcompact(0.1999999) == "0.2"
    @test stringcompact(123.4567) == "123.457"
    @test stringcompact(0.001234567) == "0.00123457"
    @test stringcompact(0.1234567) == "0.123457"
    @test stringcompact(1234567.0) == "1.23457e6"
    @test stringcompact(12345678910.0) == "1.23457e10"
    @test stringcompact(12345678.0) == "1.23457e7"
    @test stringcompact(0.10000049) == "0.1"
    @test stringcompact(22.89825) == "22.8983"
    @test stringcompact(0.646690981531646) == "0.646691"
    @test stringcompact(6.938893903907228e-17) == "6.93889e-17"
    @test stringcompact(1.015625) == "1.01562"
    @test stringcompact(1.046875) == "1.04688"
    @test stringcompact(0.025621074) == "0.0256211"

    # subnormals
    @test stringcompact(eps(0.0)) == "5.0e-324"
    @test stringcompact(eps(0f0)) == "1.0f-45"
    @test stringcompact(eps(Float16(0.0))) == "6.0e-8"
end

end # Ryu
