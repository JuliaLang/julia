# This file is a part of Julia. License is MIT: http://julialang.org/license

@test big(2.0)^big(3) == 8

for T in [Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64, Int128, UInt128, BigInt]
    @test T(2)^big(3.0) == 8
    @test big(2.0)^T(3) == 8
end

# issue 15659
@test (setprecision(53) do; big(1/3); end) < 1//3
