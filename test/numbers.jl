# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.MathConstants
using Random
using LinearAlgebra

const ≣ = isequal # convenient for comparing NaNs

# remove these tests and re-enable the same ones in the
# testset "issue #4156" later in this file when #23866 is resolved
@test fld(0.3,0.01) == 29.0
@test div(0.3,0.01) == 29.0

@testset "basic booleans" begin
    @test true
    @test !false
    @test !!true
    @test !!!false

    @test true  == true
    @test false == false
    @test true  != false
    @test false != true

    @test ~true == false
    @test ~false == true

    @test false & false == false
    @test true  & false == false
    @test false & true  == false
    @test true  & true  == true

    @test false | false == false
    @test true  | false == true
    @test false | true  == true
    @test true  | true  == true

    @test false ⊻ false == false
    @test true  ⊻ false == true
    @test false ⊻ true  == true
    @test true  ⊻ true  == false
    @test xor(false, false) == false
    @test xor(true,  false) == true
    @test xor(false, true)  == true
    @test xor(true,  true)  == false
end
@testset "bool operator" begin
    @test Bool(false) == false
    @test Bool(true) == true
    @test Bool(0) == false
    @test Bool(1) == true
    @test_throws InexactError Bool(-1)
    @test Bool(0.0) == false
    @test Bool(1.0) == true
    @test_throws InexactError Bool(0.1)
    @test_throws InexactError Bool(-1.0)
    @test Bool(Complex(0,0)) == false
    @test Bool(Complex(1,0)) == true
    @test_throws InexactError Bool(Complex(0,1))
    @test Bool(0//1) == false
    @test Bool(1//1) == true
    @test_throws InexactError Bool(1//2)

    @test iszero(false) && !iszero(true)
    @test isone(true) && !isone(false)

    @test typemin(Bool) == false
    @test typemax(Bool) == true
    @test abs(false) == false
    @test abs(true) == true
end
@testset "basic arithmetic" begin
    @test 2 + 3 == 5
    @test 2.0 + 3.0 == 5.
    @test 2 * 3 == 6
    @test 2.0 * 3 == 6
    @test 2.0 * 3.0 == 6.
    @test min(1.0,1) == 1
end
@testset "min, max and minmax" begin
    @test min(1) === 1
    @test max(1) === 1
    @test minmax(1) === (1, 1)
    @test minmax(5, 3) == (3, 5)
    @test minmax(3., 5.) == (3., 5.)
    @test minmax(5., 3.) == (3., 5.)
    @test minmax(3., NaN) ≣ (NaN, NaN)
    @test minmax(NaN, 3) ≣ (NaN, NaN)
    @test minmax(Inf, NaN) ≣ (NaN, NaN)
    @test minmax(NaN, Inf) ≣ (NaN, NaN)
    @test minmax(-Inf, NaN) ≣ (NaN, NaN)
    @test minmax(NaN, -Inf) ≣ (NaN, NaN)
    @test minmax(NaN, NaN) ≣ (NaN, NaN)
    @test min(-0.0,0.0) === min(0.0,-0.0)
    @test max(-0.0,0.0) === max(0.0,-0.0)
    @test minmax(-0.0,0.0) === minmax(0.0,-0.0)
    @test max(-3.2, 5.1) == max(5.1, -3.2) == 5.1
    @test min(-3.2, 5.1) == min(5.1, -3.2) == -3.2
    @test max(-3.2, Inf) == max(Inf, -3.2) == Inf
    @test max(-3.2, NaN) ≣ max(NaN, -3.2) ≣ NaN
    @test min(5.1, Inf) == min(Inf, 5.1) == 5.1
    @test min(5.1, -Inf) == min(-Inf, 5.1) == -Inf
    @test min(5.1, NaN) ≣ min(NaN, 5.1) ≣ NaN
    @test min(5.1, -NaN) ≣ min(-NaN, 5.1) ≣ NaN
    @test minmax(-3.2, 5.1) == (min(-3.2, 5.1), max(-3.2, 5.1))
    @test minmax(-3.2, Inf) == (min(-3.2, Inf), max(-3.2, Inf))
    @test minmax(-3.2, NaN) ≣ (min(-3.2, NaN), max(-3.2, NaN))
    @test (max(Inf,NaN), max(-Inf,NaN), max(Inf,-NaN), max(-Inf,-NaN)) ≣ (NaN,NaN,NaN,NaN)
    @test (max(NaN,Inf), max(NaN,-Inf), max(-NaN,Inf), max(-NaN,-Inf)) ≣ (NaN,NaN,NaN,NaN)
    @test (min(Inf,NaN), min(-Inf,NaN), min(Inf,-NaN), min(-Inf,-NaN)) ≣ (NaN,NaN,NaN,NaN)
    @test (min(NaN,Inf), min(NaN,-Inf), min(-NaN,Inf), min(-NaN,-Inf)) ≣ (NaN,NaN,NaN,NaN)
    @test minmax(-Inf,NaN) ≣ (min(-Inf,NaN), max(-Inf,NaN))
end
@testset "fma" begin
    let x = Int64(7)^7
        @test fma(x-1, x-2, x-3) == (x-1) * (x-2) + (x-3)
        @test (fma((x-1)//(x-2), (x-3)//(x-4), (x-5)//(x-6)) ==
               (x-1)//(x-2) * (x-3)//(x-4) + (x-5)//(x-6))
    end

    let x = BigInt(7)^77
        @test fma(x-1, x-2, x-3) == (x-1) * (x-2) + (x-3)
        @test (fma((x-1)//(x-2), (x-3)//(x-4), (x-5)//(x-6)) ==
               (x-1)//(x-2) * (x-3)//(x-4) + (x-5)//(x-6))
    end

    let eps = 1//BigInt(2)^30, one_eps = 1+eps,
        eps64 = Float64(eps), one_eps64 = Float64(one_eps)
        @test eps64 == Float64(eps)
        @test rationalize(BigInt, eps64, tol=0) == eps
        @test one_eps64 == Float64(one_eps)
        @test rationalize(BigInt, one_eps64, tol=0) == one_eps
        @test one_eps64 * one_eps64 - 1 != Float64(one_eps * one_eps - 1)
        @test fma(one_eps64, one_eps64, -1) == Float64(one_eps * one_eps - 1)
    end

    let eps = 1//BigInt(2)^15, one_eps = 1+eps,
        eps32 = Float32(eps), one_eps32 = Float32(one_eps)
        @test eps32 == Float32(eps)
        @test rationalize(BigInt, eps32, tol=0) == eps
        @test one_eps32 == Float32(one_eps)
        @test rationalize(BigInt, one_eps32, tol=0) == one_eps
        @test one_eps32 * one_eps32 - 1 != Float32(one_eps * one_eps - 1)
        @test fma(one_eps32, one_eps32, -1) == Float32(one_eps * one_eps - 1)
    end

    let eps = 1//BigInt(2)^7, one_eps = 1+eps,
        eps16 = Float16(Float32(eps)), one_eps16 = Float16(Float32(one_eps))
        @test eps16 == Float16(Float32(eps))
        @test rationalize(BigInt, eps16, tol=0) == eps
        @test one_eps16 == Float16(Float32(one_eps))
        @test rationalize(BigInt, one_eps16, tol=0) == one_eps
        @test one_eps16 * one_eps16 - 1 != Float16(Float32(one_eps * one_eps - 1))
        @test (fma(one_eps16, one_eps16, -1) ==
               Float16(Float32(one_eps * one_eps - 1)))
    end

    let eps = 1//BigInt(2)^200, one_eps = 1+eps,
        eps256 = BigFloat(eps), one_eps256 = BigFloat(one_eps)
        @test eps256 == BigFloat(eps)
        @test rationalize(BigInt, eps256, tol=0) == eps
        @test one_eps256 == BigFloat(one_eps)
        @test rationalize(BigInt, one_eps256, tol=0) == one_eps
        @test one_eps256 * one_eps256 - 1 != BigFloat(one_eps * one_eps - 1)
        @test fma(one_eps256, one_eps256, -1) == BigFloat(one_eps * one_eps - 1)
    end
end
@testset "muladd" begin
    let eps = 1//BigInt(2)^30, one_eps = 1+eps,
        eps64 = Float64(eps), one_eps64 = Float64(one_eps)
        @test eps64 == Float64(eps)
        @test one_eps64 == Float64(one_eps)
        @test one_eps64 * one_eps64 - 1 != Float64(one_eps * one_eps - 1)
        @test muladd(one_eps64, one_eps64, -1) ≈ Float64(one_eps * one_eps - 1)
    end

    let eps = 1//BigInt(2)^15, one_eps = 1+eps,
        eps32 = Float32(eps), one_eps32 = Float32(one_eps)
        @test eps32 == Float32(eps)
        @test one_eps32 == Float32(one_eps)
        @test one_eps32 * one_eps32 - 1 != Float32(one_eps * one_eps - 1)
        @test muladd(one_eps32, one_eps32, -1) ≈ Float32(one_eps * one_eps - 1)
    end

    let eps = 1//BigInt(2)^7, one_eps = 1+eps,
        eps16 = Float16(Float32(eps)), one_eps16 = Float16(Float32(one_eps))
        @test eps16 == Float16(Float32(eps))
        @test one_eps16 == Float16(Float32(one_eps))
        @test one_eps16 * one_eps16 - 1 != Float16(Float32(one_eps * one_eps - 1))
        @test muladd(one_eps16, one_eps16, -1) ≈ Float16(Float32(one_eps * one_eps - 1))
    end
    @test muladd(1,2,3) == 1*2+3
    @test muladd(big(1),2,3) == big(1)*2+3
    @test muladd(UInt(1),2,3) == UInt(1)*2+3
    @test muladd(1//1,2,3) == (1//1)*2+3
    @test muladd(big(1//1),2,3) == big(1//1)*2+3
    @test muladd(1.0,2,3) == 1.0*2+3
    @test muladd(big(1.0),2,3) == big(1.0)*2+3
end
# lexing typemin(Int64)
@test (-9223372036854775808)^1 == -9223372036854775808
@test [1 -1 -9223372036854775808] == [1 -1 typemin(Int64)]

# large integer literals
@test isa(-170141183460469231731687303715884105729,BigInt)
@test isa(-170141183460469231731687303715884105728,Int128)
@test isa(-9223372036854775809,Int128)
@test isa(-9223372036854775808,Int64)
@test isa(9223372036854775807,Int64)
@test isa(9223372036854775808,Int128)
@test isa(170141183460469231731687303715884105727,Int128)
@test isa(170141183460469231731687303715884105728,BigInt)

@test isa(0170141183460469231731687303715884105728,BigInt)

# GMP allocation overflow should not cause crash
if Base.GMP.ALLOC_OVERFLOW_FUNCTION[] && sizeof(Int) > 4
  @test_throws OutOfMemoryError BigInt(2)^(typemax(Culong))
end

# exponentiating with a negative base
@test -3^2 == -9
@test -9223372036854775808^2 == -(9223372036854775808^2)
@test -10000000000000000000^2 == -(10000000000000000000^2)
@test -170141183460469231731687303715884105728^2 ==
    -(170141183460469231731687303715884105728^2)

# numeric literal coefficients
let x = 10
    @test 2x == 20
    @test 9223372036854775808x == 92233720368547758080
    @test 170141183460469231731687303715884105728x ==
        1701411834604692317316873037158841057280
end

@test 2(10) == 20
@test 9223372036854775808(10) == 92233720368547758080
@test 170141183460469231731687303715884105728(10) ==
    1701411834604692317316873037158841057280

@testset "bin/oct/dec/hex/base for extreme integers" begin
    # definition and printing of extreme integers
    @test string(typemin(UInt8), base = 2) == "0"
    @test string(typemax(UInt8), base = 2) == "1"^8
    @test string(typemin(UInt8), base = 8) == "0"
    @test string(typemax(UInt8), base = 8) == "377"
    @test string(typemin(UInt8), base = 10) == "0"
    @test string(typemax(UInt8), base = 10) == "255"
    @test string(typemin(UInt8), base = 16) == "0"
    @test string(typemax(UInt8), base = 16) == "ff"
    @test repr(typemin(UInt8)) == "0x00"
    @test string(typemin(UInt8)) == "0"
    @test repr(typemax(UInt8)) == "0xff"
    @test string(typemax(UInt8)) == "255"
    @test string(typemin(UInt8), base = 3) == "0"
    @test string(typemax(UInt8), base = 3) == "100110"
    @test string(typemin(UInt8), base = 12) == "0"
    @test string(typemax(UInt8), base = 12) == "193"

    @test string(typemin(UInt16), base = 2) == "0"
    @test string(typemax(UInt16), base = 2) == "1"^16
    @test string(typemin(UInt16), base = 8) == "0"
    @test string(typemax(UInt16), base = 8) == "177777"
    @test string(typemin(UInt16), base = 10) == "0"
    @test string(typemax(UInt16), base = 10) == "65535"
    @test string(typemin(UInt16), base = 16) == "0"
    @test string(typemax(UInt16), base = 16) == "ffff"
    @test repr(typemin(UInt16)) == "0x0000"
    @test string(typemin(UInt16)) == "0"
    @test repr(typemax(UInt16)) == "0xffff"
    @test string(typemax(UInt16)) == "65535"
    @test string(typemin(UInt16), base = 3) == "0"
    @test string(typemax(UInt16), base = 3) == "10022220020"
    @test string(typemin(UInt16), base = 12) == "0"
    @test string(typemax(UInt16), base = 12) == "31b13"

    @test string(typemin(UInt32), base = 2) == "0"
    @test string(typemax(UInt32), base = 2) == "1"^32
    @test string(typemin(UInt32), base = 8) == "0"
    @test string(typemax(UInt32), base = 8) == "37777777777"
    @test string(typemin(UInt32), base = 10) == "0"
    @test string(typemax(UInt32), base = 10) == "4294967295"
    @test string(typemin(UInt32), base = 16) == "0"
    @test string(typemax(UInt32), base = 16) == "ffffffff"
    @test repr(typemin(UInt32)) == "0x00000000"
    @test string(typemin(UInt32)) == "0"
    @test repr(typemax(UInt32)) == "0xffffffff"
    @test string(typemax(UInt32)) == "4294967295"
    @test string(typemin(UInt32), base = 3) == "0"
    @test string(typemax(UInt32), base = 3) == "102002022201221111210"
    @test string(typemin(UInt32), base = 12) == "0"
    @test string(typemax(UInt32), base = 12) == "9ba461593"

    @test string(typemin(UInt64), base = 2) == "0"
    @test string(typemax(UInt64), base = 2) == "1"^64
    @test string(typemin(UInt64), base = 8) == "0"
    @test string(typemax(UInt64), base = 8) == "1777777777777777777777"
    @test string(typemin(UInt64), base = 10) == "0"
    @test string(typemax(UInt64), base = 10) == "18446744073709551615"
    @test string(typemin(UInt64), base = 16) == "0"
    @test string(typemax(UInt64), base = 16) == "ffffffffffffffff"
    @test repr(typemin(UInt64)) == "0x0000000000000000"
    @test string(typemin(UInt64)) == "0"
    @test repr(typemax(UInt64)) == "0xffffffffffffffff"
    @test string(typemax(UInt64)) == "18446744073709551615"
    @test string(typemin(UInt64), base = 3) == "0"
    @test string(typemax(UInt64), base = 3) == "11112220022122120101211020120210210211220"
    @test string(typemin(UInt64), base = 12) == "0"
    @test string(typemax(UInt64), base = 12) == "839365134a2a240713"

    @test string(typemin(UInt128), base = 2) == "0"
    @test string(typemax(UInt128), base = 2) == "1"^128
    @test string(typemin(UInt128), base = 8) == "0"
    @test string(typemax(UInt128), base = 8) == "3777777777777777777777777777777777777777777"
    @test string(typemin(UInt128), base = 16) == "0"
    @test string(typemax(UInt128), base = 16) == "ffffffffffffffffffffffffffffffff"
    @test repr(typemin(UInt128)) == "0x00000000000000000000000000000000"
    @test string(typemin(UInt128)) == "0"
    @test repr(typemax(UInt128)) == "0xffffffffffffffffffffffffffffffff"
    @test string(typemax(UInt128)) == "340282366920938463463374607431768211455"

    @test string(typemin(UInt128), base = 10) == "0"
    @test string(typemax(UInt128), base = 10) == "340282366920938463463374607431768211455"
    @test string(typemin(UInt128), base = 3) == "0"
    @test string(typemax(UInt128), base = 3) ==
        "202201102121002021012000211012011021221022212021111001022110211020010021100121010"
    @test string(typemin(UInt128), base = 12) == "0"
    @test string(typemax(UInt128), base = 12) == "5916b64b41143526a777873841863a6a6993"

    @test string(typemin(Int8), base = 2) == "-1"*"0"^7
    @test string(typemax(Int8), base = 2) == "1"^7
    @test string(typemin(Int8), base = 8) == "-200"
    @test string(typemax(Int8), base = 8) == "177"
    @test string(typemin(Int8), base = 10) == "-128"
    @test string(typemax(Int8), base = 10) == "127"
    @test string(typemin(Int8), base = 16) == "-80"
    @test string(typemax(Int8), base = 16) == "7f"
    @test string(typemin(Int8)) == "-128"
    @test string(typemax(Int8)) == "127"
    @test string(typemin(Int8), base = 3) == "-11202"
    @test string(typemax(Int8), base = 3) == "11201"
    @test string(typemin(Int8), base = 12) == "-a8"
    @test string(typemax(Int8), base = 12) == "a7"

    @test string(typemin(Int16), base = 2) == "-1"*"0"^15
    @test string(typemax(Int16), base = 2) == "1"^15
    @test string(typemin(Int16), base = 8) == "-100000"
    @test string(typemax(Int16), base = 8) == "77777"
    @test string(typemin(Int16), base = 10) == "-32768"
    @test string(typemax(Int16), base = 10) == "32767"
    @test string(typemin(Int16), base = 16) == "-8000"
    @test string(typemax(Int16), base = 16) == "7fff"
    @test string(typemin(Int16)) == "-32768"
    @test string(typemax(Int16)) == "32767"
    @test string(typemin(Int16), base = 3) == "-1122221122"
    @test string(typemax(Int16), base = 3) == "1122221121"
    @test string(typemin(Int16), base = 12) == "-16b68"
    @test string(typemax(Int16), base = 12) == "16b67"

    @test string(typemin(Int32), base = 2) == "-1"*"0"^31
    @test string(typemax(Int32), base = 2) == "1"^31
    @test string(typemin(Int32), base = 8) == "-20000000000"
    @test string(typemax(Int32), base = 8) == "17777777777"
    @test string(typemin(Int32), base = 10) == "-2147483648"
    @test string(typemax(Int32), base = 10) == "2147483647"
    @test string(typemin(Int32), base = 16) == "-80000000"
    @test string(typemax(Int32), base = 16) == "7fffffff"
    @test string(typemin(Int32)) == "-2147483648"
    @test string(typemax(Int32)) == "2147483647"
    @test string(typemin(Int32), base = 3) == "-12112122212110202102"
    @test string(typemax(Int32), base = 3) == "12112122212110202101"
    @test string(typemin(Int32), base = 12) == "-4bb2308a8"
    @test string(typemax(Int32), base = 12) == "4bb2308a7"

    @test string(typemin(Int64), base = 2) == "-1"*"0"^63
    @test string(typemax(Int64), base = 2) == "1"^63
    @test string(typemin(Int64), base = 8) == "-1000000000000000000000"
    @test string(typemax(Int64), base = 8) == "777777777777777777777"
    @test string(typemin(Int64), base = 10) == "-9223372036854775808"
    @test string(typemax(Int64), base = 10) == "9223372036854775807"
    @test string(typemin(Int64), base = 16) == "-8000000000000000"
    @test string(typemax(Int64), base = 16) == "7fffffffffffffff"
    @test string(typemin(Int64)) == "-9223372036854775808"
    @test string(typemax(Int64)) == "9223372036854775807"
    @test string(typemin(Int64), base = 3) == "-2021110011022210012102010021220101220222"
    @test string(typemax(Int64), base = 3) == "2021110011022210012102010021220101220221"
    @test string(typemin(Int64), base = 12) == "-41a792678515120368"
    @test string(typemax(Int64), base = 12) == "41a792678515120367"

    @test string(typemin(Int128), base = 2) == "-1"*"0"^127
    @test string(typemax(Int128), base = 2) == "1"^127
    @test string(typemin(Int128), base = 8) == "-2000000000000000000000000000000000000000000"
    @test string(typemax(Int128), base = 8) == "1777777777777777777777777777777777777777777"
    @test string(typemin(Int128), base = 16) == "-80000000000000000000000000000000"
    @test string(typemax(Int128), base = 16) == "7fffffffffffffffffffffffffffffff"

    @test string(typemin(Int128), base = 10) == "-170141183460469231731687303715884105728"
    @test string(typemax(Int128), base = 10) == "170141183460469231731687303715884105727"
    @test string(typemin(Int128)) == "-170141183460469231731687303715884105728"
    @test string(typemax(Int128)) == "170141183460469231731687303715884105727"
    @test string(typemin(Int128), base = 3) ==
        "-101100201022001010121000102002120122110122221010202000122201220121120010200022002"
    @test string(typemax(Int128), base = 3) ==
        "101100201022001010121000102002120122110122221010202000122201220121120010200022001"
    @test string(typemin(Int128), base = 12) == "-2a695925806818735399a37a20a31b3534a8"
    @test string(typemax(Int128), base = 12) == "2a695925806818735399a37a20a31b3534a7"
end
@testset "floating-point printing" begin
    @test repr(1.0) == "1.0"
    @test repr(-1.0) == "-1.0"
    @test repr(0.0) == "0.0"
    @test repr(-0.0) == "-0.0"
    @test repr(0.1) == "0.1"
    @test repr(0.2) == "0.2"
    @test repr(0.3) == "0.3"
    @test repr(0.1+0.2) != "0.3"
    @test repr(Inf) == "Inf"
    @test repr(-Inf) == "-Inf"
    @test repr(NaN) == "NaN"
    @test repr(-NaN) == "NaN"
    @test repr(Float64(pi)) == "3.141592653589793"
    # issue 6608
    @test sprint(show, 666666.6, context=:compact => true) == "666667.0"
    @test sprint(show, 666666.049, context=:compact => true) == "666666.0"
    @test sprint(show, 666665.951, context=:compact => true) == "666666.0"
    @test sprint(show, 66.66666, context=:compact => true) == "66.6667"
    @test sprint(show, -666666.6, context=:compact => true) == "-666667.0"
    @test sprint(show, -666666.049, context=:compact => true) == "-666666.0"
    @test sprint(show, -666665.951, context=:compact => true) == "-666666.0"
    @test sprint(show, -66.66666, context=:compact => true) == "-66.6667"

    @test repr(1.0f0) == "1.0f0"
    @test repr(-1.0f0) == "-1.0f0"
    @test repr(0.0f0) == "0.0f0"
    @test repr(-0.0f0) == "-0.0f0"
    @test repr(0.1f0) == "0.1f0"
    @test repr(0.2f0) == "0.2f0"
    @test repr(0.3f0) == "0.3f0"
    @test repr(0.1f0+0.2f0) == "0.3f0"
    @test repr(Inf32) == "Inf32"
    @test repr(-Inf32) == "-Inf32"
    @test repr(NaN32) == "NaN32"
    @test repr(-NaN32) == "NaN32"
    @test repr(Float32(pi)) == "3.1415927f0"
end
@testset "signs" begin
    @test sign(1) == 1
    @test sign(-1) == -1
    @test sign(0) == 0
    @test sign(1.0) == 1
    @test sign(-1.0) == -1
    @test sign(0.0) == 0
    @test sign(-0.0) == 0
    @test sign( 1.0/0.0) == 1
    @test sign(-1.0/0.0) == -1
    @test sign(Inf) == 1
    @test sign(-Inf) == -1
    @test isequal(sign(NaN), NaN)
    @test isequal(sign(-NaN), NaN)
    @test sign(2//3) == 1
    @test sign(-2//3) == -1
    @test sign(0//1) == 0
    @test sign(-0//1) == 0
    @test sign(1//0) == 1
    @test sign(-1//0) == -1
    @test isa(sign(2//3), Rational{Int})
    @test isa(2//3 + 2//3im, Complex{Rational{Int}})
    @test isa(sign(2//3 + 2//3im), Complex{Float64})
    @test sign(one(UInt)) == 1
    @test sign(zero(UInt)) == 0

    @test signbit(1) == 0
    @test signbit(0) == 0
    @test signbit(-1) == 1
    @test signbit(1.0) == 0
    @test signbit(0.0) == 0
    @test signbit(-0.0) == 1
    @test signbit(-1.0) == 1
    @test signbit(1.0/0.0) == 0
    @test signbit(-1.0/0.0) == 1
    @test signbit(Inf) == 0
    @test signbit(-Inf) == 1
    @test signbit(NaN) == 0
    @test signbit(-NaN) == 1
    @test signbit(2//3) == 0
    @test signbit(-2//3) == 1
    @test signbit(0//1) == 0
    @test signbit(-0//1) == 0
    @test signbit(1//0) == 0
    @test signbit(-1//0) == 1
end
@testset "copysign" begin
    @test copysign(big(1.0),big(-2.0)) == big(-1.0)

    @test copysign(-1,1) == 1
    @test copysign(1,-1) == -1

    @test copysign(-1,1.0) == 1
    @test copysign(1,-1.0) == -1

    @test copysign(-1,1//2) == 1
    @test copysign(1,-1//2) == -1

    @test copysign(1.0,-1) == -1.0
    @test copysign(-1.0,1) == 1.0

    @test copysign(1.0,-1.0) == -1.0
    @test copysign(-1.0,1.0) == 1.0

    @test copysign(1.0,-1//2) == -1.0
    @test copysign(-1.0,1//2) == 1.0

    @test copysign(1//2,-1) == -1//2
    @test copysign(-1//2,1) == 1//2

    @test copysign(1//2,-1//2) == -1//2
    @test copysign(-1//2,1//2) == 1//2

    @test copysign(1//2,-1.0) == -1//2
    @test copysign(-1//2,1.0) == 1//2

    # verify type stability with integer (x is negative)
    @test eltype(copysign(-1,1)) <: Integer
    @test eltype(copysign(-1,BigInt(1))) <: Integer
    @test eltype(copysign(-1,1.0)) <: Integer
    @test eltype(copysign(-1,1//2)) <: Integer
    @test eltype(copysign(-BigInt(1),1)) <: Integer
    @test eltype(copysign(-BigInt(1),1.0)) <: Integer
    @test eltype(copysign(-BigInt(1),1//2)) <: Integer
    @test eltype(copysign(-BigInt(1),BigInt(1))) <: Integer
    @test eltype(copysign(-1,-1)) <: Integer
    @test eltype(copysign(-1,-BigInt(1))) <: Integer
    @test eltype(copysign(-1,-1.0)) <: Integer
    @test eltype(copysign(-1,-1//2)) <: Integer
    @test eltype(copysign(-BigInt(1),-1)) <: Integer
    @test eltype(copysign(-BigInt(1),-1.0)) <: Integer
    @test eltype(copysign(-BigInt(1),-1//2)) <: Integer
    @test eltype(copysign(-BigInt(1),-BigInt(1))) <: Integer

    # verify type stability with integer (x is positive)
    @test eltype(copysign(1,1)) <: Integer
    @test eltype(copysign(1,BigInt(1))) <: Integer
    @test eltype(copysign(1,1.0)) <: Integer
    @test eltype(copysign(1,1//2)) <: Integer
    @test eltype(copysign(BigInt(1),1)) <: Integer
    @test eltype(copysign(BigInt(1),1.0)) <: Integer
    @test eltype(copysign(BigInt(1),1//2)) <: Integer
    @test eltype(copysign(BigInt(1),BigInt(1))) <: Integer
    @test eltype(copysign(1,-1)) <: Integer
    @test eltype(copysign(1,-BigInt(1))) <: Integer
    @test eltype(copysign(1,-1.0)) <: Integer
    @test eltype(copysign(1,-1//2)) <: Integer
    @test eltype(copysign(BigInt(1),-1)) <: Integer
    @test eltype(copysign(BigInt(1),-1.0)) <: Integer
    @test eltype(copysign(BigInt(1),-1//2)) <: Integer
    @test eltype(copysign(BigInt(1),-BigInt(1))) <: Integer

    # verify type stability with real (x is negative)
    @test eltype(copysign(-1.0,1)) <: Real
    @test eltype(copysign(-1.0,BigInt(1))) <: Real
    @test eltype(copysign(-1.0,1.0)) <: Real
    @test eltype(copysign(-1.0,1//2)) <: Real
    @test eltype(copysign(-1.0,-1)) <: Real
    @test eltype(copysign(-1.0,-BigInt(1))) <: Real
    @test eltype(copysign(-1.0,-1.0)) <: Real
    @test eltype(copysign(-1.0,-1//2)) <: Real

    # Verify type stability with real (x is positive)
    @test eltype(copysign(1.0,1)) <: Real
    @test eltype(copysign(1.0,BigInt(1))) <: Real
    @test eltype(copysign(1.0,1.0)) <: Real
    @test eltype(copysign(1.0,1//2)) <: Real
    @test eltype(copysign(1.0,-1)) <: Real
    @test eltype(copysign(1.0,-BigInt(1))) <: Real
    @test eltype(copysign(1.0,-1.0)) <: Real
    @test eltype(copysign(1.0,-1//2)) <: Real

    # Verify type stability with rational (x is negative)
    @test eltype(copysign(-1//2,1)) <: Rational
    @test eltype(copysign(-1//2,BigInt(1))) <: Rational
    @test eltype(copysign(-1//2,1.0)) <: Rational
    @test eltype(copysign(-1//2,1//2)) <: Rational
    @test eltype(copysign(-1//2,-1)) <: Rational
    @test eltype(copysign(-1//2,-BigInt(1))) <: Rational
    @test eltype(copysign(-1//2,-1.0)) <: Rational
    @test eltype(copysign(-1//2,-1//2)) <: Rational

    # Verify type stability with rational (x is positive)
    @test eltype(copysign(-1//2,1)) <: Rational
    @test eltype(copysign(-1//2,BigInt(1))) <: Rational
    @test eltype(copysign(-1//2,1.0)) <: Rational
    @test eltype(copysign(-1//2,1//2)) <: Rational
    @test eltype(copysign(-1//2,-1)) <: Rational
    @test eltype(copysign(-1//2,-BigInt(1))) <: Rational
    @test eltype(copysign(-1//2,-1.0)) <: Rational
    @test eltype(copysign(-1//2,-1//2)) <: Rational

    # test x = NaN
    @test isnan(copysign(0/0,1))
    @test isnan(copysign(0/0,-1))

    # test x = Inf
    @test isinf(copysign(1/0,1))
    @test isinf(copysign(1/0,-1))

    # with Unsigned argument
    @test copysign(Float16(-1), 0x01) === Float16(1)
    @test copysign(Float16(1), 0x01) === Float16(1)
    @test copysign(-1.0f0, 0x01) === 1.0f0
    @test copysign(1.0f0, 0x01) === 1.0f0
    @test copysign(-1.0, 0x01) === 1.0
    @test copysign(-1, 0x02) === 1
    @test copysign(big(-1), 0x02) == 1
    @test copysign(big(-1.0), 0x02) == 1.0
    @test copysign(-1//2, 0x01) == 1//2
end

@testset "isnan/isinf/isfinite" begin
    @test isnan(1)     == false
    @test isnan(1.0)   == false
    @test isnan(-1.0)  == false
    @test isnan(Inf)   == false
    @test isnan(-Inf)  == false
    @test isnan(NaN)   == true
    @test isnan(1//2)  == false
    @test isnan(-2//3) == false
    @test isnan(5//0)  == false
    @test isnan(-3//0) == false

    @test isinf(1)     == false
    @test isinf(1.0)   == false
    @test isinf(-1.0)  == false
    @test isinf(Inf)   == true
    @test isinf(-Inf)  == true
    @test isinf(NaN)   == false
    @test isinf(1//2)  == false
    @test isinf(-2//3) == false
    @test isinf(5//0)  == true
    @test isinf(-3//0) == true

    @test isfinite(1)     == true
    @test isfinite(1.0)   == true
    @test isfinite(-1.0)  == true
    @test isfinite(Inf)   == false
    @test isfinite(-Inf)  == false
    @test isfinite(NaN)   == false
    @test isfinite(1//2)  == true
    @test isfinite(-2//3) == true
    @test isfinite(5//0)  == false
    @test isfinite(-3//0) == false
    @test isfinite(pi)    == true

    @test isequal(-Inf,-Inf)
    @test isequal(-1.0,-1.0)
    @test isequal(-0.0,-0.0)
    @test isequal(+0.0,+0.0)
    @test isequal(+1.0,+1.0)
    @test isequal(+Inf,+Inf)
    @test isequal(-NaN,-NaN)
    @test isequal(-NaN,+NaN)
    @test isequal(+NaN,-NaN)
    @test isequal(+NaN,+NaN)

    @test !isequal(-Inf,+Inf)
    @test !isequal(-1.0,+1.0)
    @test !isequal(-0.0,+0.0)
    @test !isequal(+0.0,-0.0)
    @test !isequal(+1.0,-1.0)
    @test !isequal(+Inf,-Inf)

    @test  isequal(-0.0f0,-0.0)
    @test  isequal( 0.0f0, 0.0)
    @test !isequal(-0.0f0, 0.0)
    @test !isequal(0.0f0 ,-0.0)

    @test !isless(-Inf,-Inf)
    @test  isless(-Inf,-1.0)
    @test  isless(-Inf,-0.0)
    @test  isless(-Inf,+0.0)
    @test  isless(-Inf,+1.0)
    @test  isless(-Inf,+Inf)
    @test  isless(-Inf,-NaN)
    @test  isless(-Inf,+NaN)

    @test !isless(-1.0,-Inf)
    @test !isless(-1.0,-1.0)
    @test  isless(-1.0,-0.0)
    @test  isless(-1.0,+0.0)
    @test  isless(-1.0,+1.0)
    @test  isless(-1.0,+Inf)
    @test  isless(-1.0,-NaN)
    @test  isless(-1.0,+NaN)

    @test !isless(-0.0,-Inf)
    @test !isless(-0.0,-1.0)
    @test !isless(-0.0,-0.0)
    @test  isless(-0.0,+0.0)
    @test  isless(-0.0,+1.0)
    @test  isless(-0.0,+Inf)
    @test  isless(-0.0,-NaN)
    @test  isless(-0.0,+NaN)

    @test !isless(+0.0,-Inf)
    @test !isless(+0.0,-1.0)
    @test !isless(+0.0,-0.0)
    @test !isless(+0.0,+0.0)
    @test  isless(+0.0,+1.0)
    @test  isless(+0.0,+Inf)
    @test  isless(+0.0,-NaN)
    @test  isless(+0.0,+NaN)

    @test !isless(+1.0,-Inf)
    @test !isless(+1.0,-1.0)
    @test !isless(+1.0,-0.0)
    @test !isless(+1.0,+0.0)
    @test !isless(+1.0,+1.0)
    @test  isless(+1.0,+Inf)
    @test  isless(+1.0,-NaN)
    @test  isless(+1.0,+NaN)

    @test !isless(+Inf,-Inf)
    @test !isless(+Inf,-1.0)
    @test !isless(+Inf,-0.0)
    @test !isless(+Inf,+0.0)
    @test !isless(+Inf,+1.0)
    @test !isless(+Inf,+Inf)
    @test  isless(+Inf,-NaN)
    @test  isless(+Inf,+NaN)

    @test !isless(-NaN,-Inf)
    @test !isless(-NaN,-1.0)
    @test !isless(-NaN,-0.0)
    @test !isless(-NaN,+0.0)
    @test !isless(-NaN,+1.0)
    @test !isless(-NaN,+Inf)
    @test !isless(-NaN,-NaN)
    @test !isless(-NaN,+NaN)

    @test !isless(+NaN,-Inf)
    @test !isless(+NaN,-1.0)
    @test !isless(+NaN,-0.0)
    @test !isless(+NaN,+0.0)
    @test !isless(+NaN,+1.0)
    @test !isless(+NaN,+Inf)
    @test !isless(+NaN,-NaN)
    @test !isless(+NaN,+NaN)
    @test !isless(+NaN,1)
    @test !isless(-NaN,1)

    @test  isequal(   0, 0.0)
    @test  isequal( 0.0,   0)
    @test !isequal(   0,-0.0)
    @test !isequal(-0.0,   0)
    @test   isless(-0.0,   0)
    @test  !isless(   0,-0.0)

    @test isless(-0.0, 0.0f0)
    @test cmp(isless, -0.0, 0.0f0) == -1
    @test cmp(isless, 0.0, -0.0f0) == 1
    @test cmp(isless, NaN, 1) == 1
    @test cmp(isless, 1, NaN) == -1
    @test cmp(isless, NaN, NaN) == 0
end
@testset "Float vs Integer comparison" begin
    for x=-5:5, y=-5:5
        @test (x==y)==(Float64(x)==Int64(y))
        @test (x!=y)==(Float64(x)!=Int64(y))
        @test (x< y)==(Float64(x)< Int64(y))
        @test (x> y)==(Float64(x)> Int64(y))
        @test (x<=y)==(Float64(x)<=Int64(y))
        @test (x>=y)==(Float64(x)>=Int64(y))

        @test (x==y)==(Int64(x)==Float64(y))
        @test (x!=y)==(Int64(x)!=Float64(y))
        @test (x< y)==(Int64(x)< Float64(y))
        @test (x> y)==(Int64(x)> Float64(y))
        @test (x<=y)==(Int64(x)<=Float64(y))
        @test (x>=y)==(Int64(x)>=Float64(y))

        if x >= 0
            @test (x==y)==(UInt64(x)==Float64(y))
            @test (x!=y)==(UInt64(x)!=Float64(y))
            @test (x< y)==(UInt64(x)< Float64(y))
            @test (x> y)==(UInt64(x)> Float64(y))
            @test (x<=y)==(UInt64(x)<=Float64(y))
            @test (x>=y)==(UInt64(x)>=Float64(y))
        end
        if y >= 0
            @test (x==y)==(Float64(x)==UInt64(y))
            @test (x!=y)==(Float64(x)!=UInt64(y))
            @test (x< y)==(Float64(x)< UInt64(y))
            @test (x> y)==(Float64(x)> UInt64(y))
            @test (x<=y)==(Float64(x)<=UInt64(y))
            @test (x>=y)==(Float64(x)>=UInt64(y))
        end
    end

    function _cmp_(x::Union{Int64,UInt64}, y::Float64)
        if x==Int64(2)^53-2 && y==2.0^53-2; return  0; end
        if x==Int64(2)^53-2 && y==2.0^53-1; return -1; end
        if x==Int64(2)^53-2 && y==2.0^53  ; return -1; end
        if x==Int64(2)^53-2 && y==2.0^53+2; return -1; end
        if x==Int64(2)^53-2 && y==2.0^53+3; return -1; end
        if x==Int64(2)^53-2 && y==2.0^53+4; return -1; end

        if x==Int64(2)^53-1 && y==2.0^53-2; return +1; end
        if x==Int64(2)^53-1 && y==2.0^53-1; return  0; end
        if x==Int64(2)^53-1 && y==2.0^53  ; return -1; end
        if x==Int64(2)^53-1 && y==2.0^53+2; return -1; end
        if x==Int64(2)^53-1 && y==2.0^53+3; return -1; end
        if x==Int64(2)^53-1 && y==2.0^53+4; return -1; end

        if x==Int64(2)^53   && y==2.0^53-2; return +1; end
        if x==Int64(2)^53   && y==2.0^53-1; return +1; end
        if x==Int64(2)^53   && y==2.0^53  ; return  0; end
        if x==Int64(2)^53   && y==2.0^53+2; return -1; end
        if x==Int64(2)^53   && y==2.0^53+4; return -1; end

        if x==Int64(2)^53+1 && y==2.0^53-2; return +1; end
        if x==Int64(2)^53+1 && y==2.0^53-1; return +1; end
        if x==Int64(2)^53+1 && y==2.0^53  ; return +1; end
        if x==Int64(2)^53+1 && y==2.0^53+2; return -1; end
        if x==Int64(2)^53+1 && y==2.0^53+4; return -1; end

        if x==Int64(2)^53+2 && y==2.0^53-2; return +1; end
        if x==Int64(2)^53+2 && y==2.0^53-1; return +1; end
        if x==Int64(2)^53+2 && y==2.0^53  ; return +1; end
        if x==Int64(2)^53+2 && y==2.0^53+2; return  0; end
        if x==Int64(2)^53+2 && y==2.0^53+4; return -1; end

        if x==Int64(2)^53+3 && y==2.0^53-2; return +1; end
        if x==Int64(2)^53+3 && y==2.0^53-1; return +1; end
        if x==Int64(2)^53+3 && y==2.0^53  ; return +1; end
        if x==Int64(2)^53+3 && y==2.0^53+2; return +1; end
        if x==Int64(2)^53+3 && y==2.0^53+4; return -1; end

        if x==Int64(2)^53+4 && y==2.0^53-2; return +1; end
        if x==Int64(2)^53+4 && y==2.0^53-1; return +1; end
        if x==Int64(2)^53+4 && y==2.0^53  ; return +1; end
        if x==Int64(2)^53+4 && y==2.0^53+2; return +1; end
        if x==Int64(2)^53+4 && y==2.0^53+4; return  0; end

        if x==Int64(2)^53+5 && y==2.0^53-2; return +1; end
        if x==Int64(2)^53+5 && y==2.0^53-1; return +1; end
        if x==Int64(2)^53+5 && y==2.0^53  ; return +1; end
        if x==Int64(2)^53+5 && y==2.0^53+2; return +1; end
        if x==Int64(2)^53+5 && y==2.0^53+4; return +1; end

        error("invalid: _cmp_($x,$y)")
    end

    for x = Int64(2)^53-2:Int64(2)^53+5,
        y = [2.0^53-2 2.0^53-1 2.0^53 2.0^53+2 2.0^53+4]
        u = UInt64(x)
        @test y == Float64(trunc(Int64,y))

        @test (x==y)==(y==x)
        @test (x!=y)==!(x==y)
        @test (-x==-y)==(-y==-x)
        @test (-x!=-y)==!(-x==-y)

        @test (x<y)==(x<=y)&(x!=y)
        @test (x<=y)==(x<y)|(x==y)
        @test (x==y)==(x<=y)&!(x<y)

        @test -x != x
        @test -y != y
        @test -x != y
        @test -y != x
        @test -x <  x
        @test -y <  y
        @test -x <  y
        @test -y <  x
        @test -x <= x
        @test -y <= y
        @test -x <= y
        @test -y <= x

        @test -y != u
        @test -y <  u
        @test -y <= u

        c = _cmp_(x,y)
        if c < 0
            @test !(x == y)
            @test  (x <  y)
            @test !(y <  x)
            @test  (x <= y)
            @test !(y <= x)

            @test !(u == y)
            @test  (u <  y)
            @test !(y <  u)
            @test  (u <= y)
            @test !(y <= u)

            @test !(-x == -y)
            @test !(-x <  -y)
            @test  (-y <  -x)
            @test !(-x <= -y)
            @test  (-y <= -x)
        elseif c > 0
            @test !(x == y)
            @test !(x <  y)
            @test  (y <  x)
            @test !(x <= y)
            @test  (y <= x)

            @test !(u == y)
            @test !(u <  y)
            @test  (y <  u)
            @test !(u <= y)
            @test  (y <= u)

            @test !(-x == -y)
            @test  (-x <  -y)
            @test !(-y <  -x)
            @test  (-x <= -y)
            @test !(-y <= -x)
        else
            @test  (x == y)
            @test !(x <  y)
            @test !(y <  x)
            @test  (x <= y)
            @test  (y <= x)

            @test  (u == y)
            @test !(u <  y)
            @test !(y <  u)
            @test  (u <= y)
            @test  (y <= u)

            @test  (-x == -y)
            @test !(-x <  -y)
            @test !(-y <  -x)
            @test  (-x <= -y)
            @test  (-y <= -x)
        end
    end

    @test Int64(2)^62-1 != 2.0^62
    @test Int64(2)^62   == 2.0^62
    @test Int64(2)^62+1 != 2.0^62
    @test 2.0^62 != Int64(2)^62-1
    @test 2.0^62 == Int64(2)^62
    @test 2.0^62 != Int64(2)^62+1

    @test typemax(Int64)   != +2.0^63
    @test typemin(Int64)   == -2.0^63
    @test typemin(Int64)+1 != -2.0^63

    @test UInt64(2)^60-1 != 2.0^60
    @test UInt64(2)^60   == 2.0^60
    @test UInt64(2)^60+1 != 2.0^60
    @test 2.0^60 != UInt64(2)^60-1
    @test 2.0^60 == UInt64(2)^60
    @test 2.0^60 != UInt64(2)^60+1

    @test UInt64(2)^63-1 != 2.0^63
    @test UInt64(2)^63   == 2.0^63
    @test UInt64(2)^63+1 != 2.0^63
    @test 2.0^63 != UInt64(2)^63-1
    @test 2.0^63 == UInt64(2)^63
    @test 2.0^63 != UInt64(2)^63+1

    @test typemax(UInt64) != 2.0^64
    # issue #9085
    f9085() = typemax(UInt64) != 2.0^64
    @test f9085()

    @test typemax(UInt64) < Float64(typemax(UInt64))
    @test typemax(Int64) < Float64(typemax(Int64))
    @test typemax(UInt64) <= Float64(typemax(UInt64))
    @test typemax(Int64) <= Float64(typemax(Int64))

    @test Float64(typemax(UInt64)) > typemax(UInt64)
    @test Float64(typemax(Int64)) > typemax(Int64)
    @test Float64(typemax(UInt64)) >= typemax(UInt64)
    @test Float64(typemax(Int64)) >= typemax(Int64)

    @test Float64(Int128(0)) == 0.0
    @test Float32(Int128(0)) == 0.0f0
    @test Float64(Int128(-1)) == -1.0
    @test Float32(Int128(-1)) == -1.0f0
    @test Float64(Int128(3)) == 3.0
    @test Float32(Int128(3)) == 3.0f0
    @test Float64(UInt128(10121)) == 10121.0
    @test Float32(UInt128(10121)) == 10121.0f0
    @test Float64(typemin(Int128)) == -2.0^127
    @test Float32(typemin(Int128)) == -2.0f0^127
    @test Float64(typemax(Int128)) == 2.0^127
    @test Float32(typemax(Int128)) == 2.0f0^127
    @test Float64(typemin(UInt128)) == 0.0
    @test Float32(typemin(UInt128)) == 0.0f0
    @test Float64(typemax(UInt128)) == 2.0^128
    @test Float32(typemax(UInt128)) == 2.0f0^128
    # leading zeros > 53
    @test Float64(UInt128(Int64(2)^54)) == 1.8014398509481984e16
    @test Float64(Int128(Int64(2)^54)) == 1.8014398509481984e16
    @test Float32(UInt128(Int64(2)^54)) == 1.8014399f16
    @test Float32(Int128(Int64(2)^54)) == 1.8014399f16

    # check for double rounding in conversion
    @test Float64(10633823966279328163822077199654060032) == 1.0633823966279327e37 #0x1p123
    @test Float64(10633823966279328163822077199654060033) == 1.063382396627933e37 #nextfloat(0x1p123)
    @test Float64(-10633823966279328163822077199654060032) == -1.0633823966279327e37
    @test Float64(-10633823966279328163822077199654060033) == -1.063382396627933e37
end
@testset "Float vs Int128 comparisons" begin
    @test Int128(1e30) == 1e30
    @test Int128(1e30)+1 > 1e30

    @test Int128(-2.0^127) == typemin(Int128)
    @test Float64(UInt128(3.7e19)) == 3.7e19
    @test Float64(UInt128(3.7e30)) == 3.7e30
end
@testset "Float16 vs Int comparisons" begin
    @test Inf16 != typemax(Int16)
    @test Inf16 != typemax(Int32)
    @test Inf16 != typemax(Int64)
    @test Inf16 != typemax(Int128)
    @test Inf16 != typemax(UInt16)
    @test Inf16 != typemax(UInt32)
    @test Inf16 != typemax(UInt64)
    @test Inf16 != typemax(UInt128)
end
@testset "NaN comparisons" begin
    @test !(NaN <= 1)
    @test !(NaN >= 1)
    @test !(NaN < 1)
    @test !(NaN > 1)
    @test !(1 <= NaN)
    @test !(1 >= NaN)
    @test !(1 < NaN)
    @test !(1 > NaN)
end

@testset "Irrational zero and one" begin
    @test one(pi) === true
    @test zero(pi) === false
    @test one(typeof(pi)) === true
    @test zero(typeof(pi)) === false
end

@testset "Irrationals compared with Irrationals" begin
    for i in (π, ℯ, γ, catalan)
        for j in (π, ℯ, γ, catalan)
            @test isequal(i==j, Float64(i)==Float64(j))
            @test isequal(i!=j, Float64(i)!=Float64(j))
            @test isequal(i<=j, Float64(i)<=Float64(j))
            @test isequal(i>=j, Float64(i)>=Float64(j))
            @test isequal(i<j, Float64(i)<Float64(j))
            @test isequal(i>j, Float64(i)>Float64(j))
        end
    end
end

@testset "Irrational Inverses, Issue #30882" begin
    @test @inferred(inv(π)) ≈ 0.3183098861837907
end

@testset "Irrationals compared with Rationals and Floats" begin
    @test Float64(pi,RoundDown) < pi
    @test Float64(pi,RoundUp) > pi
    @test !(Float64(pi,RoundDown) > pi)
    @test !(Float64(pi,RoundUp) < pi)
    @test Float64(pi,RoundDown) <= pi
    @test Float64(pi,RoundUp) >= pi
    @test Float64(pi,RoundDown) != pi
    @test Float64(pi,RoundUp) != pi

    @test Float32(pi,RoundDown) < pi
    @test Float32(pi,RoundUp) > pi
    @test !(Float32(pi,RoundDown) > pi)
    @test !(Float32(pi,RoundUp) < pi)

    @test prevfloat(big(pi)) < pi
    @test nextfloat(big(pi)) > pi
    @test !(prevfloat(big(pi)) > pi)
    @test !(nextfloat(big(pi)) < pi)

    @test 2646693125139304345//842468587426513207 < pi
    @test !(2646693125139304345//842468587426513207 > pi)
    @test 2646693125139304345//842468587426513207 != pi

    @test sqrt(2) == 1.4142135623730951
end
@testset "Irrational printing" begin
    @test sprint(show, "text/plain", π) == "π = 3.1415926535897..."
    @test sprint(show, "text/plain", π, context=:compact => true) == "π"
    @test sprint(show, π) == "π"

end
@testset "issue #6365" begin
    for T in (Float32, Float64)
        for i = 9007199254740992:9007199254740996
            @test T(i) == T(BigFloat(i))
            @test T(-i) == T(BigFloat(-i))
            for r in (RoundNearest,RoundUp,RoundDown,RoundToZero)
                @test T(i,r) == T(BigFloat(i),r)
                @test T(-i,r) == T(BigFloat(-i),r)
            end
        end
    end
end

@testset "arithmetic with Ints and Floats" begin
    @test 1+1.5 == 2.5
    @test 1.5+1 == 2.5
    @test 1+1.5+2 == 4.5
    @test isa(convert(Complex{Int16},1), Complex{Int16})
    @test Complex(1,2)+1 == Complex(2,2)
    @test Complex(1,2)+1.5 == Complex(2.5,2.0)
    @test 1/Complex(2,2) == Complex(.25,-.25)
    @test Complex(1.5,1.0) + 1//2 == Complex(2.0,1.0)
    @test real(Complex(1//2,2//3)) == 1//2
    @test imag(Complex(1//2,2//3)) == 2//3
    @test Complex(1,2) + 1//2 == Complex(3//2,2//1)
    @test Complex(1,2) + 1//2 * 0.5 == Complex(1.25,2.0)
    @test (Complex(1,2) + 1//2) * 0.5 == Complex(0.75,1.0)
    @test (Complex(1,2)/Complex(2.5,3.0))*Complex(2.5,3.0) ≈ Complex(1,2)
    @test 0.7 < real(sqrt(Complex(0,1))) < 0.707107
end
for T in Base.BitSigned_types
    @test abs(typemin(T)) == -typemin(T)
    #for x in (typemin(T),convert(T,-1),zero(T),one(T),typemax(T))
    #    @test signed(unsigned(x)) == x
    #end
end

#for T in (UInt8,UInt16,UInt32,UInt64,UInt128)
#    x in (typemin(T),one(T),typemax(T))
#    @test unsigned(signed(x)) == x
#end

for S = Base.BitSigned64_types,
    U = Base.BitUnsigned64_types
    @test !(-one(S) == typemax(U))
    @test -one(S) != typemax(U)
    @test -one(S) < typemax(U)
    @test !(typemax(U) <= -one(S))
end

# check type of constructed complexes
real_types = [Base.BitInteger64_types...,
              [Rational{T} for T in Base.BitInteger64_types]...,
              Float32, Float64]
for A = real_types, B = real_types
    T = promote_type(A,B)
    @test typeof(Complex(convert(A,2),convert(B,3))) <: Complex{T}
end

# comparison should fail on complex
@test_throws MethodError complex(1,2) > 0
@test_throws MethodError complex(1,2) > complex(0,0)

@testset "div, fld, cld, rem, mod" begin
    for yr = Any[
        1:6,
        0.25:0.25:6.0,
        1//4:1//4:6//1
    ], xr = Any[
        0:6,
        0.0:0.25:6.0,
        0//1:1//4:6//1
    ]
        for y = yr, x = xr
            # check basic div functionality
            if 0 <= x < 1y
                @test div(+x,+y) == 0
                @test div(+x,-y) == 0
                @test div(-x,+y) == 0
                @test div(-x,-y) == 0
            end
            if 1y <= x < 2y
                @test div(+x,+y) == +1
                @test div(+x,-y) == -1
                @test div(-x,+y) == -1
                @test div(-x,-y) == +1
            end
            if 2y <= x < 3y
                @test div(+x,+y) == +2
                @test div(+x,-y) == -2
                @test div(-x,+y) == -2
                @test div(-x,-y) == +2
            end

            # check basic fld functionality
            if 0 == x
                @test fld(+x,+y) == 0
                @test fld(+x,-y) == 0
                @test fld(-x,+y) == 0
                @test fld(-x,-y) == 0
            end
            if 0 < x < 1y
                @test fld(+x,+y) == +0
                @test fld(+x,-y) == -1
                @test fld(-x,+y) == -1
                @test fld(-x,-y) == +0
            end
            if 1y == x
                @test fld(+x,+y) == +1
                @test fld(+x,-y) == -1
                @test fld(-x,+y) == -1
                @test fld(-x,-y) == +1
            end
            if 1y < x < 2y
                @test fld(+x,+y) == +1
                @test fld(+x,-y) == -2
                @test fld(-x,+y) == -2
                @test fld(-x,-y) == +1
            end
            if 2y == x
                @test fld(+x,+y) == +2
                @test fld(+x,-y) == -2
                @test fld(-x,+y) == -2
                @test fld(-x,-y) == +2
            end
            if 2y < x < 3y
                @test fld(+x,+y) == +2
                @test fld(+x,-y) == -3
                @test fld(-x,+y) == -3
                @test fld(-x,-y) == +2
            end

            # check basic cld functionality
            if 0 == x
                @test cld(+x,+y) == 0
                @test cld(+x,-y) == 0
                @test cld(-x,+y) == 0
                @test cld(-x,-y) == 0
            end
            if 0 < x < 1y
                @test cld(+x,+y) == +1
                @test cld(+x,-y) == +0
                @test cld(-x,+y) == +0
                @test cld(-x,-y) == +1
            end
            if 1y == x
                @test cld(+x,+y) == +1
                @test cld(+x,-y) == -1
                @test cld(-x,+y) == -1
                @test cld(-x,-y) == +1
            end
            if 1y < x < 2y
                @test cld(+x,+y) == +2
                @test cld(+x,-y) == -1
                @test cld(-x,+y) == -1
                @test cld(-x,-y) == +2
            end
            if 2y == x
                @test cld(+x,+y) == +2
                @test cld(+x,-y) == -2
                @test cld(-x,+y) == -2
                @test cld(-x,-y) == +2
            end
            if 2y < x < 3y
                @test cld(+x,+y) == +3
                @test cld(+x,-y) == -2
                @test cld(-x,+y) == -2
                @test cld(-x,-y) == +3
            end

            # check everything else in terms of div, fld, cld
            d = div(x,y)
            f = fld(x,y)
            c = cld(x,y)
            r = rem(x,y)
            m = mod(x,y)
            d2, r2 = divrem(x,y)
            f2, m2 = fldmod(x,y)

            t1 = isa(x,Rational) && isa(y,Rational) ?
                                   promote_type(typeof(numerator(x)),typeof(numerator(y))) :
                 isa(x,Rational) ? promote_type(typeof(numerator(x)),typeof(y)) :
                 isa(y,Rational) ? promote_type(typeof(x),typeof(numerator(y))) :
                                   promote_type(typeof(x),typeof(y))

            t2 = promote_type(typeof(x),typeof(y))

            @test typeof(d) <: t1
            @test typeof(f) <: t1
            @test typeof(c) <: t1
            @test typeof(r) <: t2
            @test typeof(m) <: t2

            @test d == f
            @test c == f + (m == 0 ? 0 : 1)
            @test r == m
            @test 0 <= r < y
            @test x == y*d + r

            @test typeof(d2) == typeof(d)
            @test typeof(r2) == typeof(r)
            @test typeof(f2) == typeof(f)
            @test typeof(m2) == typeof(m)

            @test d2 == d
            @test r2 == r
            @test f2 == f
            @test m2 == m

            for X=[-1,1], Y=[-1,1]
                sx = X*x
                sy = Y*y

                sd = div(sx,sy)
                sf = fld(sx,sy)
                sc = cld(sx,sy)
                sr = rem(sx,sy)
                sm = mod(sx,sy)
                sd2, sr2 = divrem(sx,sy)
                sf2, sm2 = fldmod(sx,sy)

                @test typeof(sd) <: t1
                @test typeof(sf) <: t1
                @test typeof(sc) <: t1
                @test typeof(sr) <: t2
                @test typeof(sm) <: t2

                @test sx < 0 ? -y < sr <= 0 : 0 <= sr < +y
                @test sy < 0 ? -y < sm <= 0 : 0 <= sm < +y
                @test sx == sy*sd + sr
                @test sx == sy*sf + sm

                @test typeof(sd2) == typeof(sd)
                @test typeof(sr2) == typeof(sr)
                @test typeof(sf2) == typeof(sf)
                @test typeof(sm2) == typeof(sm)

                @test sd2 == sd
                @test sr2 == sr
                @test sf2 == sf
                @test sm2 == sm
            end
        end
    end

    @test div(typemax(Int64)  , 1) ==  9223372036854775807
    @test div(typemax(Int64)  , 2) ==  4611686018427387903
    @test div(typemax(Int64)  , 7) ==  1317624576693539401
    @test div(typemax(Int64)  ,-1) == -9223372036854775807
    @test div(typemax(Int64)  ,-2) == -4611686018427387903
    @test div(typemax(Int64)  ,-7) == -1317624576693539401
    @test div(typemax(Int64)-1, 1) ==  9223372036854775806
    @test div(typemax(Int64)-1, 2) ==  4611686018427387903
    @test div(typemax(Int64)-1, 7) ==  1317624576693539400
    @test div(typemax(Int64)-1,-1) == -9223372036854775806
    @test div(typemax(Int64)-1,-2) == -4611686018427387903
    @test div(typemax(Int64)-1,-7) == -1317624576693539400
    @test div(typemax(Int64)-2, 1) ==  9223372036854775805
    @test div(typemax(Int64)-2, 2) ==  4611686018427387902
    @test div(typemax(Int64)-2, 7) ==  1317624576693539400
    @test div(typemax(Int64)-2,-1) == -9223372036854775805
    @test div(typemax(Int64)-2,-2) == -4611686018427387902
    @test div(typemax(Int64)-2,-7) == -1317624576693539400

    @test div(typemin(Int64)  , 1) == -9223372036854775807-1
    @test div(typemin(Int64)  , 2) == -4611686018427387904
    @test div(typemin(Int64)  , 7) == -1317624576693539401
    @test div(typemin(Int64)  ,-2) ==  4611686018427387904
    @test div(typemin(Int64)  ,-7) ==  1317624576693539401
    @test div(typemin(Int64)+1, 1) == -9223372036854775807
    @test div(typemin(Int64)+1, 2) == -4611686018427387903
    @test div(typemin(Int64)+1, 7) == -1317624576693539401
    @test div(typemin(Int64)+1,-1) ==  9223372036854775807
    @test div(typemin(Int64)+1,-2) ==  4611686018427387903
    @test div(typemin(Int64)+1,-7) ==  1317624576693539401
    @test div(typemin(Int64)+2, 1) == -9223372036854775806
    @test div(typemin(Int64)+2, 2) == -4611686018427387903
    @test div(typemin(Int64)+2, 7) == -1317624576693539400
    @test div(typemin(Int64)+2,-1) ==  9223372036854775806
    @test div(typemin(Int64)+2,-2) ==  4611686018427387903
    @test div(typemin(Int64)+2,-7) ==  1317624576693539400
    @test div(typemin(Int64)+3, 1) == -9223372036854775805
    @test div(typemin(Int64)+3, 2) == -4611686018427387902
    @test div(typemin(Int64)+3, 7) == -1317624576693539400
    @test div(typemin(Int64)+3,-1) ==  9223372036854775805
    @test div(typemin(Int64)+3,-2) ==  4611686018427387902
    @test div(typemin(Int64)+3,-7) ==  1317624576693539400

    @test fld(typemax(Int64)  , 1) ==  9223372036854775807
    @test fld(typemax(Int64)  , 2) ==  4611686018427387903
    @test fld(typemax(Int64)  , 7) ==  1317624576693539401
    @test fld(typemax(Int64)  ,-1) == -9223372036854775807
    @test fld(typemax(Int64)  ,-2) == -4611686018427387904
    @test fld(typemax(Int64)  ,-7) == -1317624576693539401
    @test fld(typemax(Int64)-1, 1) ==  9223372036854775806
    @test fld(typemax(Int64)-1, 2) ==  4611686018427387903
    @test fld(typemax(Int64)-1, 7) ==  1317624576693539400
    @test fld(typemax(Int64)-1,-1) == -9223372036854775806
    @test fld(typemax(Int64)-1,-2) == -4611686018427387903
    @test fld(typemax(Int64)-1,-7) == -1317624576693539401
    @test fld(typemax(Int64)-2, 1) ==  9223372036854775805
    @test fld(typemax(Int64)-2, 2) ==  4611686018427387902
    @test fld(typemax(Int64)-2, 7) ==  1317624576693539400
    @test fld(typemax(Int64)-2,-1) == -9223372036854775805
    @test fld(typemax(Int64)-2,-2) == -4611686018427387903
    @test fld(typemax(Int64)-2,-7) == -1317624576693539401

    @test fld(typemin(Int64)  , 1) == -9223372036854775807-1
    @test fld(typemin(Int64)  , 2) == -4611686018427387904
    @test fld(typemin(Int64)  , 7) == -1317624576693539402
    @test fld(typemin(Int64)  ,-2) ==  4611686018427387904
    @test fld(typemin(Int64)  ,-7) ==  1317624576693539401
    @test fld(typemin(Int64)+1, 1) == -9223372036854775807
    @test fld(typemin(Int64)+1, 2) == -4611686018427387904
    @test fld(typemin(Int64)+1, 7) == -1317624576693539401
    @test fld(typemin(Int64)+1,-1) ==  9223372036854775807
    @test fld(typemin(Int64)+1,-2) ==  4611686018427387903
    @test fld(typemin(Int64)+1,-7) ==  1317624576693539401
    @test fld(typemin(Int64)+2, 1) == -9223372036854775806
    @test fld(typemin(Int64)+2, 2) == -4611686018427387903
    @test fld(typemin(Int64)+2, 7) == -1317624576693539401
    @test fld(typemin(Int64)+2,-1) ==  9223372036854775806
    @test fld(typemin(Int64)+2,-2) ==  4611686018427387903
    @test fld(typemin(Int64)+2,-7) ==  1317624576693539400
    @test fld(typemin(Int64)+3, 1) == -9223372036854775805
    @test fld(typemin(Int64)+3, 2) == -4611686018427387903
    @test fld(typemin(Int64)+3, 7) == -1317624576693539401
    @test fld(typemin(Int64)+3,-1) ==  9223372036854775805
    @test fld(typemin(Int64)+3,-2) ==  4611686018427387902
    @test fld(typemin(Int64)+3,-7) ==  1317624576693539400

    @test cld(typemax(Int64)  , 1) ==  9223372036854775807
    @test cld(typemax(Int64)  , 2) ==  4611686018427387904
    @test cld(typemax(Int64)  , 7) ==  1317624576693539401
    @test cld(typemax(Int64)  ,-1) == -9223372036854775807
    @test cld(typemax(Int64)  ,-2) == -4611686018427387903
    @test cld(typemax(Int64)  ,-7) == -1317624576693539401
    @test cld(typemax(Int64)-1, 1) ==  9223372036854775806
    @test cld(typemax(Int64)-1, 2) ==  4611686018427387903
    @test cld(typemax(Int64)-1, 7) ==  1317624576693539401
    @test cld(typemax(Int64)-1,-1) == -9223372036854775806
    @test cld(typemax(Int64)-1,-2) == -4611686018427387903
    @test cld(typemax(Int64)-1,-7) == -1317624576693539400
    @test cld(typemax(Int64)-2, 1) ==  9223372036854775805
    @test cld(typemax(Int64)-2, 2) ==  4611686018427387903
    @test cld(typemax(Int64)-2, 7) ==  1317624576693539401
    @test cld(typemax(Int64)-2,-1) == -9223372036854775805
    @test cld(typemax(Int64)-2,-2) == -4611686018427387902
    @test cld(typemax(Int64)-2,-7) == -1317624576693539400

    @test cld(typemin(Int64)  , 1) == -9223372036854775807-1
    @test cld(typemin(Int64)  , 2) == -4611686018427387904
    @test cld(typemin(Int64)  , 7) == -1317624576693539401
    @test cld(typemin(Int64)  ,-2) ==  4611686018427387904
    @test cld(typemin(Int64)  ,-7) ==  1317624576693539402
    @test cld(typemin(Int64)+1, 1) == -9223372036854775807
    @test cld(typemin(Int64)+1, 2) == -4611686018427387903
    @test cld(typemin(Int64)+1, 7) == -1317624576693539401
    @test cld(typemin(Int64)+1,-1) ==  9223372036854775807
    @test cld(typemin(Int64)+1,-2) ==  4611686018427387904
    @test cld(typemin(Int64)+1,-7) ==  1317624576693539401
    @test cld(typemin(Int64)+2, 1) == -9223372036854775806
    @test cld(typemin(Int64)+2, 2) == -4611686018427387903
    @test cld(typemin(Int64)+2, 7) == -1317624576693539400
    @test cld(typemin(Int64)+2,-1) ==  9223372036854775806
    @test cld(typemin(Int64)+2,-2) ==  4611686018427387903
    @test cld(typemin(Int64)+2,-7) ==  1317624576693539401
    @test cld(typemin(Int64)+3, 1) == -9223372036854775805
    @test cld(typemin(Int64)+3, 2) == -4611686018427387902
    @test cld(typemin(Int64)+3, 7) == -1317624576693539400
    @test cld(typemin(Int64)+3,-1) ==  9223372036854775805
    @test cld(typemin(Int64)+3,-2) ==  4611686018427387903
    @test cld(typemin(Int64)+3,-7) ==  1317624576693539401

    for x=Any[typemin(Int64), -typemax(Int64), -typemax(Int64)+1, -typemax(Int64)+2,
              typemax(Int64)-2, typemax(Int64)-1, typemax(Int64),
              typemax(UInt64)-1, typemax(UInt64)-2, typemax(UInt64)],
        y=[-7,-2,-1,1,2,7]
        if x >= 0
            @test div(unsigned(x),y) == unsigned(div(x,y))
            @test fld(unsigned(x),y) == unsigned(fld(x,y))
            @test cld(unsigned(x),y) == unsigned(cld(x,y))
        end
        if isa(x,Signed) && y >= 0
            @test div(x,unsigned(y)) == div(x,y)
            @test fld(x,unsigned(y)) == fld(x,y)
            @test cld(x,unsigned(y)) == cld(x,y)
        end
    end

    for x=0:5, y=1:5
        @test div(UInt(x),UInt(y)) == div(x,y)
        @test div(UInt(x),y) == div(x,y)
        @test div(x,UInt(y)) == div(x,y)
        @test div(UInt(x),-y) == reinterpret(UInt,div(x,-y))
        @test div(-x,UInt(y)) == div(-x,y)

        @test fld(UInt(x),UInt(y)) == fld(x,y)
        @test fld(UInt(x),y) == fld(x,y)
        @test fld(x,UInt(y)) == fld(x,y)
        @test fld(UInt(x),-y) == reinterpret(UInt,fld(x,-y))
        @test fld(-x,UInt(y)) == fld(-x,y)

        @test cld(UInt(x),UInt(y)) == cld(x,y)
        @test cld(UInt(x),y) == cld(x,y)
        @test cld(x,UInt(y)) == cld(x,y)
        @test cld(UInt(x),-y) == reinterpret(UInt,cld(x,-y))
        @test cld(-x,UInt(y)) == cld(-x,y)

        @test rem(UInt(x),UInt(y)) == rem(x,y)
        @test rem(UInt(x),y) == rem(x,y)
        @test rem(x,UInt(y)) == rem(x,y)
        @test rem(UInt(x),-y) == rem(x,-y)
        @test rem(-x,UInt(y)) == rem(-x,y)

        @test mod(UInt(x),UInt(y)) == mod(x,y)
        @test mod(UInt(x),y) == mod(x,y)
        @test mod(x,UInt(y)) == mod(x,y)
        @test mod(UInt(x),-y) == mod(x,-y)
        @test mod(-x,UInt(y)) == mod(-x,y)
    end

    @test div(typemax(UInt64)  , 1) ==  typemax(UInt64)
    @test div(typemax(UInt64)  ,-1) == -typemax(UInt64)
    @test div(typemax(UInt64)-1, 1) ==  typemax(UInt64)-1
    @test div(typemax(UInt64)-1,-1) == -typemax(UInt64)+1
    @test div(typemax(UInt64)-2, 1) ==  typemax(UInt64)-2
    @test div(typemax(UInt64)-2,-1) == -typemax(UInt64)+2

    @test signed(div(unsigned(typemax(Int64))+2, 1)) ==  typemax(Int64)+2
    @test signed(div(unsigned(typemax(Int64))+2,-1)) == -typemax(Int64)-2
    @test signed(div(unsigned(typemax(Int64))+1, 1)) ==  typemax(Int64)+1
    @test signed(div(unsigned(typemax(Int64))+1,-1)) == -typemax(Int64)-1
    @test signed(div(unsigned(typemax(Int64))  , 1)) ==  typemax(Int64)
    @test signed(div(unsigned(typemax(Int64))  ,-1)) == -typemax(Int64)

    @test signed(div(typemax(UInt),typemax(Int)))        ==  2
    @test signed(div(typemax(UInt),(typemax(Int)>>1)+1)) ==  3
    @test signed(div(typemax(UInt),typemax(Int)>>1))     ==  4
    @test signed(div(typemax(UInt),typemin(Int)))        == -1
    @test signed(div(typemax(UInt),typemin(Int)+1))      == -2
    @test signed(div(typemax(UInt),typemin(Int)>>1))     == -3
    @test signed(div(typemax(UInt),(typemin(Int)>>1)+1)) == -4

    @test fld(typemax(UInt64)  , 1) ==  typemax(UInt64)
    @test fld(typemax(UInt64)  ,-1) == -typemax(UInt64)
    @test fld(typemax(UInt64)-1, 1) ==  typemax(UInt64)-1
    @test fld(typemax(UInt64)-1,-1) == -typemax(UInt64)+1
    @test fld(typemax(UInt64)-2, 1) ==  typemax(UInt64)-2
    @test fld(typemax(UInt64)-2,-1) == -typemax(UInt64)+2

    @test signed(fld(unsigned(typemax(Int64))+2, 1)) ==  typemax(Int64)+2
    @test signed(fld(unsigned(typemax(Int64))+2,-1)) == -typemax(Int64)-2
    @test signed(fld(unsigned(typemax(Int64))+1, 1)) ==  typemax(Int64)+1
    @test signed(fld(unsigned(typemax(Int64))+1,-1)) == -typemax(Int64)-1
    @test signed(fld(unsigned(typemax(Int64))  , 1)) ==  typemax(Int64)
    @test signed(fld(unsigned(typemax(Int64))  ,-1)) == -typemax(Int64)

    @test signed(fld(typemax(UInt),typemax(Int)))        ==  2
    @test signed(fld(typemax(UInt),(typemax(Int)>>1)+1)) ==  3
    @test signed(fld(typemax(UInt),typemax(Int)>>1))     ==  4
    @test signed(fld(typemax(UInt),typemin(Int)))        == -2
    @test signed(fld(typemax(UInt),typemin(Int)+1))      == -3
    @test signed(fld(typemax(UInt),typemin(Int)>>1))     == -4
    @test signed(fld(typemax(UInt),(typemin(Int)>>1)+1)) == -5

    @test cld(typemax(UInt64)  , 1) ==  typemax(UInt64)
    @test cld(typemax(UInt64)  ,-1) == -typemax(UInt64)
    @test cld(typemax(UInt64)-1, 1) ==  typemax(UInt64)-1
    @test cld(typemax(UInt64)-1,-1) == -typemax(UInt64)+1
    @test cld(typemax(UInt64)-2, 1) ==  typemax(UInt64)-2
    @test cld(typemax(UInt64)-2,-1) == -typemax(UInt64)+2

    @test signed(cld(unsigned(typemax(Int64))+2, 1)) ==  typemax(Int64)+2
    @test signed(cld(unsigned(typemax(Int64))+2,-1)) == -typemax(Int64)-2
    @test signed(cld(unsigned(typemax(Int64))+1, 1)) ==  typemax(Int64)+1
    @test signed(cld(unsigned(typemax(Int64))+1,-1)) == -typemax(Int64)-1
    @test signed(cld(unsigned(typemax(Int64))  , 1)) ==  typemax(Int64)
    @test signed(cld(unsigned(typemax(Int64))  ,-1)) == -typemax(Int64)

    @test signed(cld(typemax(UInt),typemax(Int)))        ==  3
    @test signed(cld(typemax(UInt),(typemax(Int)>>1)+1)) ==  4
    @test signed(cld(typemax(UInt),typemax(Int)>>1))     ==  5
    @test signed(cld(typemax(UInt),typemin(Int)))        == -1
    @test signed(cld(typemax(UInt),typemin(Int)+1))      == -2
    @test signed(cld(typemax(UInt),typemin(Int)>>1))     == -3
    @test signed(cld(typemax(UInt),(typemin(Int)>>1)+1)) == -4

    @testset "UInt128 div/rem" begin
        uints = [0xadc0db298a7401251f4fa96ba429cb24, 0xd11ef418102afb1f7959b6df48d08044]
        for x in uints, y in uints, sx in 0:128, sy in 0:127
            xs = x >> sx
            ys = y >> sy
            q, r = @inferred(divrem(xs, ys))::Tuple{UInt128,UInt128}
            @test xs == q*ys + r && r < ys
            @test q == @inferred(div(xs, ys))::UInt128
            @test r == @inferred(rem(xs, ys))::UInt128
        end
    end

    @testset "exceptions and special cases" begin
        for T in (Int8,Int16,Int32,Int64,Int128, UInt8,UInt16,UInt32,UInt64,UInt128)
            @test_throws DivideError div(T(1), T(0))
            @test_throws DivideError fld(T(1), T(0))
            @test_throws DivideError cld(T(1), T(0))
            @test_throws DivideError rem(T(1), T(0))
            @test_throws DivideError mod(T(1), T(0))
        end
        for T in (Int8,Int16,Int32,Int64,Int128)
            @test_throws DivideError div(typemin(T), T(-1))
            @test_throws DivideError fld(typemin(T), T(-1))
            @test_throws DivideError cld(typemin(T), T(-1))
            @test rem(typemin(T), T(-1)) === T(0)
            @test mod(typemin(T), T(-1)) === T(0)
        end
    end
    @testset "issue #4156" begin
        @test fld(1.4,0.35667494393873234) == 3.0
        @test div(1.4,0.35667494393873234) == 3.0
        # @test fld(0.3,0.01) == 29.0 # uncomment when #23866 is resolved
        # @test div(0.3,0.01) == 29.0 # uncomment when #23866 is resolved
        # see https://github.com/JuliaLang/julia/issues/3127
    end
    @testset "issue #8831" begin
        @test rem(prevfloat(1.0),1.0) == prevfloat(1.0)
        @test mod(prevfloat(1.0),1.0) == prevfloat(1.0)
    end
    # issue #3046
    @test mod(Int64(2),typemax(Int64)) == 2
end
@testset "return types" begin
    for T in (Int8,Int16,Int32,Int64,Int128, UInt8,UInt16,UInt32,UInt64,UInt128)
        z, o = T(0), T(1)
        @test typeof(+z) === T
        @test typeof(-z) === T
        @test typeof(abs(z)) === T
        @test typeof(sign(z)) === T
        @test typeof(copysign(z,z)) === T
        @test typeof(flipsign(z,z)) === T
        @test typeof(z+z) === T
        @test typeof(z-z) === T
        @test typeof(z*z) === T
        @test typeof(z÷o) === T
        @test typeof(z%o) === T
        @test typeof(fld(z,o)) === T
        @test typeof(mod(z,o)) === T
        @test typeof(cld(z,o)) === T
    end
end

# things related to floating-point epsilon
@test eps() == eps(Float64)
@test eps(Float64) == eps(1.0)
@test eps(Float64) == eps(1.5)
@test eps(Float32) == eps(1f0)
@test eps(float(0)) == 5e-324
@test eps(-float(0)) == 5e-324
@test eps(nextfloat(float(0))) == 5e-324
@test eps(-nextfloat(float(0))) == 5e-324
@test eps(floatmin()) == 5e-324
@test eps(-floatmin()) == 5e-324
@test eps(floatmax()) ==  2.0^(1023-52)
@test eps(-floatmax()) ==  2.0^(1023-52)
@test isnan(eps(NaN))
@test isnan(eps(Inf))
@test isnan(eps(-Inf))

@test .1+.1+.1 != .3
@test .1+.1+.1 ≈ .3
@test .1+.1+.1-.3 ≉ 0
@test .1+.1+.1-.3 ≈ 0 atol=eps(.3)
@test 1.1 ≈ 1.1f0

@test div(1e50,1) == 1e50
@test fld(1e50,1) == 1e50
@test cld(1e50,1) == 1e50

@testset "binary literals" begin
    @test 0b1010101 == 0x55
    @test isa(0b00000000,UInt8)
    @test isa(0b000000000,UInt16)
    @test isa(0b0000000000000000,UInt16)
    @test isa(0b00000000000000000,UInt32)
    @test isa(0b00000000000000000000000000000000,UInt32)
    @test isa(0b000000000000000000000000000000000,UInt64)
    @test isa(0b0000000000000000000000000000000000000000000000000000000000000000,UInt64)
    @test isa(0b00000000000000000000000000000000000000000000000000000000000000000,UInt128)
    @test isa(0b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,UInt128)
    # remove BigInt unsigned integer literals #11105
    @test_throws Meta.ParseError Meta.parse("0b000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
    @test isa(0b11111111,UInt8)
    @test isa(0b111111111,UInt16)
    @test isa(0b1111111111111111,UInt16)
    @test isa(0b11111111111111111,UInt32)
    @test isa(0b11111111111111111111111111111111,UInt32)
    @test isa(0b111111111111111111111111111111111,UInt64)
    @test isa(0b1111111111111111111111111111111111111111111111111111111111111111,UInt64)
    @test isa(0b11111111111111111111111111111111111111111111111111111111111111111,UInt128)
    @test isa(0b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111,UInt128)
    # remove BigInt unsigned integer literals #11105
    @test_throws Meta.ParseError Meta.parse("0b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111")
end
@testset "octal literals" begin
    @test 0o10 == 0x8
    @test 0o100 == 0x40
    @test 0o1000 == 0x200
    @test 0o724 == 0x1d4
    @test isa(0o00,UInt8)
    @test isa(0o000,UInt8)
    @test isa(0o00000,UInt16)
    @test isa(0o000000,UInt16)
    @test isa(0o0000000000,UInt32)
    @test isa(0o00000000000,UInt32)
    @test isa(0o000000000000000000000,UInt64)
    @test isa(0o0000000000000000000000,UInt64)
    @test isa(0o000000000000000000000000000000000000000000,UInt128)
    # remove BigInt unsigned integer literals #11105
    @test_throws Meta.ParseError Meta.parse("0o00000000000000000000000000000000000000000000")
    @test isa(0o11,UInt8)
    @test isa(0o111,UInt8)
    @test isa(0o11111,UInt16)
    @test isa(0o111111,UInt16)
    @test isa(0o1111111111,UInt32)
    @test isa(0o11111111111,UInt32)
    @test isa(0o111111111111111111111,UInt64)
    @test isa(0o1111111111111111111111,UInt64)
    @test isa(0o111111111111111111111111111111111111111111,UInt128)
    @test isa(0o1111111111111111111111111111111111111111111,UInt128)
    @test isa(0o3777777777777777777777777777777777777777777,UInt128)
    @test_throws Meta.ParseError Meta.parse("0o4000000000000000000000000000000000000000000")
    # remove BigInt unsigned integer literals #11105
    @test_throws Meta.ParseError Meta.parse("0o11111111111111111111111111111111111111111111")
    @test isa(0o077, UInt8)
    @test isa(0o377, UInt8)
    @test isa(0o400, UInt16)
    @test isa(0o077777, UInt16)
    @test isa(0o177777, UInt16)
    @test isa(0o200000, UInt32)
    @test isa(0o00000000000, UInt32)
    @test isa(0o17777777777, UInt32)
    @test isa(0o40000000000, UInt64)
    @test isa(0o0000000000000000000000, UInt64)
    @test isa(0o1000000000000000000000, UInt64)
    @test isa(0o2000000000000000000000, UInt128)
    @test isa(0o0000000000000000000000000000000000000000000, UInt128)
    @test isa(0o1000000000000000000000000000000000000000000, UInt128)
    @test isa(0o2000000000000000000000000000000000000000000, UInt128)
    @test_throws Meta.ParseError Meta.parse("0o4000000000000000000000000000000000000000000")

    @test String([0o110, 0o145, 0o154, 0o154, 0o157, 0o054, 0o040, 0o127, 0o157, 0o162, 0o154, 0o144, 0o041]) == "Hello, World!"

end
@testset "hexadecimal literals" begin
    @test isa(0x00,UInt8)
    @test isa(0x000,UInt16)
    @test isa(0x0000,UInt16)
    @test isa(0x00000,UInt32)
    @test isa(0x00000000,UInt32)
    @test isa(0x000000000,UInt64)
    @test isa(0x0000000000000000,UInt64)
    @test isa(0x00000000000000000,UInt128)
    @test isa(0x00000000000000000000000000000000,UInt128)
    # remove BigInt unsigned integer literals #11105
    @test_throws Meta.ParseError Meta.parse("0x000000000000000000000000000000000")

    @test isa(0x11,UInt8)
    @test isa(0x111,UInt16)
    @test isa(0x1111,UInt16)
    @test isa(0x11111,UInt32)
    @test isa(0x11111111,UInt32)
    @test isa(0x111111111,UInt64)
    @test isa(0x1111111111111111,UInt64)
    @test isa(0x11111111111111111,UInt128)
    @test isa(0x11111111111111111111111111111111,UInt128)
    # remove BigInt unsigned integer literals #11105
    @test_throws Meta.ParseError Meta.parse("0x111111111111111111111111111111111")
end
@testset "minus sign and unsigned literals" begin
    # "-" is not part of unsigned literals
    @test -0x10 == -(0x10)
    @test -0b10 == -(0b10)
    @test -0o10 == -(0o10)
    @test -0x0010 == -(0x0010)
    @test -0b0010 == -(0b0010)
    @test -0o0010 == -(0o0010)
    @test -0x00000000000000001 == -(0x00000000000000001)
    @test -0o0000000000000000000001 == -(0o0000000000000000000001)
    @test -0b00000000000000000000000000000000000000000000000000000000000000001 ==
        -(0b00000000000000000000000000000000000000000000000000000000000000001)

    @test isa(-0x00,UInt8)
    @test isa(-0x0000000000000000,UInt64)
    @test isa(-0x00000000000000000,UInt128)
    @test isa(-0x00000000000000000000000000000000,UInt128)
    # remove BigInt unsigned integer literals #11105
    @test_throws Meta.ParseError Meta.parse("-0x000000000000000000000000000000000")
end
@testset "Float32 literals" begin
    @test isa(1f0,Float32)
    @test isa(1.f0,Float32)
    @test isa(1.0f0,Float32)
    @test 1f0 == 1.
    @test isa(1f1,Float32)
    @test 1f1 == 10.
end
@testset "hexadecimal float literals" begin
    @test 0x1p0   === 1.
    @test 0x1p1   === 2.
    @test 0x.1p0  === 0.0625
    @test 0x.1p1  === 0.125
    @test 0xfp0   === 15.
    @test 0xfp1   === 30.
    @test 0x.fp0  === 0.9375
    @test 0x.fp1  === 1.875
    @test 0x1.p0  === 1.
    @test 0x1.p1  === 2.
    @test 0xf.p0  === 15.
    @test 0xf.p1  === 30.
    @test 0x1.0p0 === 1.
    @test 0x1.0p1 === 2.
    @test 0x1.1p0 === 1.0625
    @test 0x1.1p1 === 2.125
    @test 0x1.fp0 === 1.9375
    @test 0x1.fp1 === 3.875
    @test 0xf.0p0 === 15.
    @test 0xf.0p1 === 30.
    @test 0xf.1p0 === 15.0625
    @test 0xf.1p1 === 30.125
    @test 0xf.fp0 === 15.9375
    @test 0xf.fp1 === 31.875
    @test 0x1P0   === 1.
    @test 0x1P1   === 2.
    @test 0x.1P0  === 0.0625
    @test 0x.1P1  === 0.125
    @test 0xfP0   === 15.
    @test 0xfP1   === 30.
    @test 0x.fP0  === 0.9375
    @test 0x.fP1  === 1.875
    @test 0x1.P0  === 1.
    @test 0x1.P1  === 2.
    @test 0xf.P0  === 15.
    @test 0xf.P1  === 30.
    @test 0x1.0P0 === 1.
    @test 0x1.0P1 === 2.
    @test 0x1.1P0 === 1.0625
    @test 0x1.1P1 === 2.125
    @test 0x1.fP0 === 1.9375
    @test 0x1.fP1 === 3.875
    @test 0xf.0P0 === 15.
    @test 0xf.0P1 === 30.
    @test 0xf.1P0 === 15.0625
    @test 0xf.1P1 === 30.125
    @test 0xf.fP0 === 15.9375
    @test 0xf.fP1 === 31.875
    @test -0x1.0p2 === -4.0
end
@testset "eps / floatmin / floatmax" begin
    @test 0x1p-52 == eps()
    @test 0x1p-52 + 1 != 1
    @test 0x1p-53 + 1 == 1
    @test 0x1p-1022 == floatmin()
    @test 0x1.fffffffffffffp1023 == floatmax()
    @test isinf(nextfloat(0x1.fffffffffffffp1023))
end
@testset "issue #1308" begin
    @test string(~UInt128(0), base = 16) == "f"^32
    @test (~0)%UInt128 == ~UInt128(0)
    @test Int128(~0) == ~Int128(0)
end

# no loss of precision for rational powers (issue #18114)
@test BigFloat(2)^(BigFloat(1)/BigFloat(3)) == BigFloat(2)^(1//3)

@testset "large shift amounts" begin
    @test Int32(-1)>>31 == -1
    @test Int32(-1)>>32 == -1
    @test Int32(-1)>>33 == -1
    @test 10>>64 == 0
    @test 10>>>64 == 0
    @test 10<<64 == 0
end
# issue #3520 - certain int literals on 32-bit systems
@test -536870913 === -536870912-1

@testset "check gcd and related functions against GMP" begin
    for T in (Int32,Int64), ii = -20:20, jj = -20:20
        i::T, j::T = ii, jj
        local d = gcd(i,j)
        @test d >= 0
        @test lcm(i,j) >= 0
        local ib = big(i)
        local jb = big(j)
        @test d == gcd(ib,jb)
        @test lcm(i,j) == lcm(ib,jb)
        @test gcdx(i,j) == gcdx(ib,jb)
        if j == 0 || d != 1
            @test_throws DomainError invmod(i,j)
            @test_throws DomainError invmod(ib,jb)
        else
            n = invmod(i,j)
            @test div(n, j) == 0
            @test n == invmod(ib,jb)
            @test mod(n*i,j) == mod(1,j)
        end
    end
end
@testset "powermod" begin
    # check powermod function against few types (in particular [U]Int128 and BigInt)
    for i = -10:10, p = 0:5, m = -10:10
        m == 0 && continue
        x = powermod(i, p, m)
        for T in [Int32, Int64, Int128, UInt128, BigInt]
            T <: Unsigned && m < 0 && continue
            let xT = powermod(i, p, T(m))
                @test x == xT
                @test isa(xT, T)
            end
            T <: Unsigned && i < 0 && continue
            @test x == mod(T(i)^p, T(m))
        end
    end

    # with m==1 should give 0
    @test powermod(1,0,1) == 0
    @test powermod(1,0,big(1)) == 0
    @test powermod(1,0,-1) == 0
    @test powermod(1,0,big(-1)) == 0
    # divide by zero error
    @test_throws DivideError powermod(1,0,0)
    @test_throws DivideError powermod(1,0,big(0))
    # negative powers perform modular inversion before exponentiation
    @test powermod(1, -1, 1) == 0
    @test powermod(1, -1, big(1)) == 0
end
@testset "additional BigInt powermod tests" begin
    @test powermod(0, 1, big(6)) == 0
    @test powermod(1, 0, big(6)) == 1
    @test powermod(big(6), big(6), big(6)) == 0
    @test powermod(10, 50, big(10)^50 - 1) == 1

    @test powermod(-1, 1, big(6)) == 5
    @test powermod(-1, 0, big(6)) == 1
    @test powermod(-1, -1, big(6)) == 5
    @test powermod(-1, 1, big(-6)) == -1
    @test powermod(-1, 0, big(-6)) == -5
    @test powermod(-1, -1, big(-6)) == -1

    @test_throws DivideError powermod(2, -1, big(6))
    @test_throws DivideError powermod(-2, -1, big(6))
end
@testset "other divide-by-zero errors" begin
    @test_throws DivideError div(1,0)
    @test_throws DivideError rem(1,0)
    @test_throws DivideError divrem(1,0)
    @test_throws DivideError fld(1,0)
    @test_throws DivideError mod(1,0)
    @test_throws DivideError fldmod(1,0)
    @test_throws DivideError cld(1,0)

    @test_throws DivideError div(-1,0)
    @test_throws DivideError rem(-1,0)
    @test_throws DivideError divrem(-1,0)
    @test_throws DivideError fld(-1,0)
    @test_throws DivideError mod(-1,0)
    @test_throws DivideError fldmod(-1,0)
    @test_throws DivideError cld(-1,0)

    @test_throws DivideError div(UInt(1),UInt(0))
    @test_throws DivideError rem(UInt(1),UInt(0))
    @test_throws DivideError divrem(UInt(1),UInt(0))
    @test_throws DivideError fld(UInt(1),UInt(0))
    @test_throws DivideError mod(UInt(1),UInt(0))
    @test_throws DivideError fldmod(UInt(1),UInt(0))
    @test_throws DivideError cld(UInt(1),UInt(0))

    @test_throws DivideError div(typemin(Int),-1)
    @test_throws DivideError fld(typemin(Int),-1)
    @test_throws DivideError cld(typemin(Int),-1)
    @test_throws DivideError divrem(typemin(Int),-1)
    @test_throws DivideError fldmod(typemin(Int),-1)
    @test rem(typemin(Int),-1) == 0
    @test mod(typemin(Int),-1) == 0
end
@testset "prevpow(2, _)/nextpow(2, _)" begin
    for i = 1:2
        @test nextpow(2, i) == prevpow(2, i) == i
    end
    @test nextpow(2, 56789) == 65536
    @test_throws DomainError nextpow(2, -56789)
    @test prevpow(2, 56789) == 32768
    @test_throws DomainError prevpow(2, -56789)
    for i = 1:100
        @test nextpow(2, i) == nextpow(2, big(i))
        @test prevpow(2, i) == prevpow(2, big(i))
    end
    for T in (Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64)
        @test nextpow(2, T(42)) === T(64)
        @test prevpow(2, T(42)) === T(32)
    end
end
@testset "ispow2" begin
    @test  ispow2(64)
    @test !ispow2(42)
    @test !ispow2(~typemax(Int))
end
@testset "nextpow/prevpow" begin
    @test nextpow(2,1) == 1
    @test prevpow(2,1) == 1
    @test nextpow(3,243) == 243
    @test prevpow(3,243) == 243
    @test nextpow(3,241) == 243
    @test prevpow(3,244) == 243
    for a = -1:1
        @test_throws DomainError nextpow(a, 2)
        @test_throws DomainError prevpow(a, 2)
    end
    @test_throws DomainError nextpow(2,0)
    @test_throws DomainError prevpow(2,0)
end
@testset "nextprod" begin
    @test_throws ArgumentError nextprod([2,3,5],Int128(typemax(Int))+1)
    @test nextprod([2,3,5],30) == 30
    @test nextprod([2,3,5],33) == 36
end
@testset "nextfloat/prevfloat" begin
    @test nextfloat(0.0) == 5.0e-324
    @test prevfloat(0.0) == -5.0e-324
    @test nextfloat(-0.0) == 5.0e-324
    @test prevfloat(-0.0) == -5.0e-324
    @test nextfloat(-5.0e-324) === -0.0
    @test prevfloat(5.0e-324) == 0.0
    @test nextfloat(-1.0) > -1.0
    @test prevfloat(-1.0) < -1.0
    @test nextfloat(nextfloat(0.0),-2) == -5.0e-324
    @test nextfloat(prevfloat(0.0), 2) ==  5.0e-324
    @test nextfloat(Inf) === Inf
    @test prevfloat(-Inf) === -Inf
    @test isequal(nextfloat(NaN), NaN)
    @test nextfloat(Inf32) === Inf32
    @test prevfloat(-Inf32) === -Inf32
    @test isequal(nextfloat(NaN32), NaN32)
end
@testset "issue #16206" begin
    @test prevfloat(Inf) == 1.7976931348623157e308
    @test prevfloat(Inf32) == 3.4028235f38
    @test nextfloat(prevfloat(Inf)) == Inf
    @test nextfloat(prevfloat(Inf),2) == Inf
    @test nextfloat(1.0,typemax(Int64)) == Inf
    @test nextfloat(0.0,typemin(Int64)) == -Inf
    @test nextfloat(1f0,typemin(Int64)) == -Inf32
    @test nextfloat(1.0,typemax(UInt64)) == Inf
    @test nextfloat(1.0,typemax(UInt128)) == Inf
    @test nextfloat(1.0,big(2)^67) == Inf
    @test nextfloat(1.0,-big(2)^67) == -Inf
end
for F in (Float16,Float32,Float64)
    @test reinterpret(Unsigned,one(F)) === Base.exponent_one(F)
    @test reinterpret(Signed,one(F)) === signed(Base.exponent_one(F))
end

@test eps(floatmax(Float64)) == 1.99584030953472e292
@test eps(-floatmax(Float64)) == 1.99584030953472e292

# modular multiplicative inverses of odd numbers via exponentiation

for T = (UInt8,Int8,UInt16,Int16,UInt32,Int32,UInt64,Int64,UInt128,Int128)
    for n = 1:2:1000
        @test n*(n^typemax(T)) & typemax(T) == 1
        n = rand(T) | one(T)
        @test n*(n^typemax(T)) == 1
    end
end

@testset "Irrational/Bool multiplication" begin
    @test false*pi === 0.0
    @test pi*false === 0.0
    @test true*pi === Float64(pi)
    @test pi*true === Float64(pi)
end
# issue #5492
@test -0.0 + false === -0.0

@testset "issue #5881" begin
    @test bitstring(true) == "00000001"
    @test bitstring(false) == "00000000"
end
@testset "edge cases of intrinsics" begin
    let g() = sqrt(-1.0)
        @test_throws DomainError sqrt(-1.0)
    end
    @test sqrt(NaN) === NaN
    let g() = sqrt(NaN)
        @test g() === NaN
    end
    let g(x) = sqrt(x)
        @test g(NaN) === NaN
    end
end
@testset "widen and widemul" begin
    @test widen(1.5f0) === 1.5
    @test widen(Int32(42)) === Int64(42)
    @test widen(Int8) === Int16
    @test widen(Int64) === Int128
    @test widen(Float32) === Float64
    @test widen(Float16) === Float32
    ## Note: this should change to e.g. Float128 at some point
    @test widen(Float64) === BigFloat
    @test widen(BigInt) === BigInt

    @test widemul(typemax(Int64),typemax(Int64)) == 85070591730234615847396907784232501249
    @test typeof(widemul(Int64(1),UInt64(1))) == Int128
    @test typeof(widemul(UInt64(1),Int64(1))) == Int128
    @test typeof(widemul(Int128(1),UInt128(1))) == BigInt
    @test typeof(widemul(UInt128(1),Int128(1))) == BigInt

    # Check that the widen() fallback doesn't trigger a StackOverflowError
    @test_throws MethodError widen(String)
end
@testset ".//" begin
    @test [1,2,3] // 4 == [1//4, 2//4, 3//4]
    @test [1,2,3] .// [4,5,6] == [1//4, 2//5, 3//6]
    @test [1+2im,3+4im] .// [5,6] == [(1+2im)//5,(3+4im)//6]
    @test [1//3+2im,3+4im] .// [5,6] == [(1//3+2im)//5,(3+4im)//6]
end
@testset "issue #7441" begin
    @test_throws InexactError Int32(2.0^50)

    @test_throws InexactError round(UInt8, 255.5)
    @test round(UInt8, 255.4) === 0xff

    @test_throws InexactError round(Int16, -32768.7)
    @test round(Int16, -32768.1) === Int16(-32768)
end
# issue #7508
@test_throws ErrorException reinterpret(Int, 0x01)

@testset "issue #12832" begin
    @test_throws ErrorException reinterpret(Float64, Complex{Int64}(1))
    @test_throws ErrorException reinterpret(Float64, ComplexF32(1))
    @test_throws ErrorException reinterpret(ComplexF32, Float64(1))
    @test_throws ErrorException reinterpret(Int32, false)
end
# issue #41
ndigf(n) = Float64(log(Float32(n)))
@test Float64(log(Float32(256))) == ndigf(256) == 5.545177459716797

# cmp on unsigned integers (see commit 24b236321e03c6d9b8cb91a450f567256a793196)
@test cmp(0x77777777,0x88888888) == -1
@test cmp(0x3959dcc5d7fd177b67df4e10bc350850, 0xd63d5b1183221b0a9e38c6809b33cdec) == -1

# issue #7911
@test sum([Int128(1) Int128(2)]) == Int128(3)

@testset "digits and digits!" begin
    @test digits(24, base = 2) == [0, 0, 0, 1, 1]
    @test digits(24, base = 2, pad = 3) == [0, 0, 0, 1, 1]
    @test digits(24, base = 2, pad = 7) == [0, 0, 0, 1, 1, 0, 0]
    @test digits(100) == [0, 0, 1]
    @test digits(BigInt(2)^128, base = 2) == [zeros(128); 1]
    let a = zeros(Int, 3)
        digits!(a, 50)
        @test a == [0, 5, 0]
        digits!(a, 9, base = 2)
        @test a == [1, 0, 0]
        digits!(a, 7, base = 2)
        @test a == [1, 1, 1]
    end
end
# Fill a pre allocated 2x4 matrix
let a = zeros(Int,(2,4))
    for i in 0:3
        digits!(view(a,:,i+1),i, base = 2)
    end
    @test a == [0 1 0 1;
                0 0 1 1]
end
@test_throws InexactError convert(UInt8, 256)
@test_throws InexactError convert(UInt, -1)
@test_throws InexactError convert(Int, big(2)^100)
@test_throws InexactError convert(Int16, big(2)^100)
@test_throws InexactError convert(Int, typemax(UInt))

@testset "issue #9789" begin
    @test_throws InexactError convert(Int8, typemax(UInt64))
    @test_throws InexactError convert(Int16, typemax(UInt64))
    @test_throws InexactError convert(Int, typemax(UInt64))
end
@testset "issue #14549" begin
    for T in (Int8, Int16, UInt8, UInt16)
        for F in (Float32,Float64)
            @test_throws InexactError convert(T, F(200000.0))
        end
    end
end
let x = big(-0.0)
    @test signbit(x) && !signbit(abs(x))
end

@testset "mod1 and fld1" begin
    @test all(x -> (m=mod1(x,3); 0<m<=3), -5:+5)
    @test all(x -> x == (fld1(x,3)-1)*3 + mod1(x,3), -5:+5)
    @test all(x -> fldmod1(x,3) == (fld1(x,3), mod1(x,3)), -5:+5)
end
#Issue #5570
@test map(x -> Int(mod1(UInt(x),UInt(5))), 0:15) == [5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5]

@testset "Issue #9618: errors thrown by large exponentiations" begin
    @test_throws DomainError big(2)^-(big(typemax(UInt))+1)
    @test_throws OverflowError big(2)^(big(typemax(UInt))+1)
    @test 0==big(0)^(big(typemax(UInt))+1)
end
@testset "bswap (issue #9726)" begin
    @test bswap(0x0002) === 0x0200
    @test bswap(0x01020304) === 0x04030201
    @test reinterpret(Float64,bswap(0x000000000000f03f)) === 1.0
    @test reinterpret(Float32,bswap(0x0000c03f)) === 1.5f0
    @test reinterpret(Float16,bswap(0x003c)) === Float16(1.0)
    @test bswap(reinterpret(Float64,0x000000000000f03f)) === 1.0
    @test bswap(reinterpret(Float32,0x0000c03f)) === 1.5f0
    @test bswap(reinterpret(Float16,0x003e)) === Float16(1.5)
    zbuf = IOBuffer([0xbf, 0xc0, 0x00, 0x00, 0x40, 0x20, 0x00, 0x00,
                     0x40, 0x0c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                     0xc0, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
    z1 = read(zbuf, ComplexF32)
    z2 = read(zbuf, ComplexF64)
    @test bswap(z1) === -1.5f0 + 2.5f0im
    @test bswap(z2) ===  3.5 - 4.5im
end

#isreal(x::Real) = true
for x in [1.23, 7, ℯ, 4//5] #[FP, Int, Irrational, Rat]
    @test isreal(x) == true
end

function allsubtypes!(m::Module, x::DataType, sts::Set)
    for s in names(m, all = true)
        if isdefined(m, s) && !Base.isdeprecated(m, s)
            t = getfield(m, s)
            if isa(t, Type) && t <: x && t != Union{}
                push!(sts, t)
            elseif isa(t, Module) && t !== m && nameof(t) === s && parentmodule(t) === m
                allsubtypes!(t, x, sts)
            end
        end
    end
end

@testset "eltype and ndims" begin
    let number_types = Set()
        allsubtypes!(Base, Number, number_types)
        allsubtypes!(Core, Number, number_types)

        @test !isempty(number_types)

        #eltype{T<:Number}(::Type{T}) = T
        for T in number_types
            @test eltype(T) == T
        end

        #ndims{T<:Number}(::Type{T}) = 0
        for x in number_types
            @test ndims(x) == 0
        end
    end
end
@testset "getindex(x::Number) = x" begin
    for x in [1.23, 7, ℯ, 4//5] #[FP, Int, Irrational, Rat]
        @test getindex(x) == x
        @test getindex(x, 1, 1) == x
    end
end
@testset "getindex error throwing" begin
    #getindex(x::Number,-1) throws BoundsError
    #getindex(x::Number,0) throws BoundsError
    #getindex(x::Number,2) throws BoundsError
    #getindex(x::Array,-1) throws BoundsError
    #getindex(x::Array,0 throws BoundsError
    #getindex(x::Array,length(x::Array)+1) throws BoundsError
    for x in [1.23, 7, ℯ, 4//5] #[FP, Int, Irrational, Rat]
        @test_throws BoundsError getindex(x,-1)
        @test_throws BoundsError getindex(x,0)
        @test_throws BoundsError getindex(x,2)
        @test_throws BoundsError getindex([x x],-1)
        @test_throws BoundsError getindex([x x],0)
        @test_throws BoundsError getindex([x x],length([x,x])+1)
        @test_throws BoundsError getindex(x, 1, 0)
    end
end
@testset "copysign and flipsign" begin
    # copysign(x::Real, y::Real) = ifelse(signbit(x)!=signbit(y), -x, x)
    # flipsign(x::Real, y::Real) = ifelse(signbit(y), -x, x)
    for x in [1.23, 7, ℯ, 4//5]
        for y in [1.23, 7, ℯ, 4//5]
            @test copysign(x, y) == x
            @test copysign(x, -y) == -x
            @test copysign(-x, y) == x
            @test copysign(-x, -y) == -x
            @test flipsign(x, y) == x
            @test flipsign(x, -y) == -x
            @test flipsign(-x, y) == -x
            @test flipsign(-x, -y) == x
        end
    end
end
@testset "angle(z::Real) = atan(zero(z), z)" begin
    #function only returns two values, depending on sign
    @test angle(10) == 0.0
    @test angle(-10) == 3.141592653589793
end
@testset "in(x::Number, y::Number) = x == y" begin
    @test in(3,3) == true #Int
    @test in(2.0,2.0) == true #FP
    @test in(ℯ,ℯ) == true #Const
    @test in(4//5,4//5) == true #Rat
    @test in(1+2im, 1+2im) == true #Imag
    @test in(3, 3.0) == true #mixed
end
@testset "map(f::Callable, x::Number, ys::Number...) = f(x)" begin
    @test map(sin, 3) == sin(3)
    @test map(cos, 3) == cos(3)
    @test map(tan, 3) == tan(3)
    @test map(log, 3) == log(3)
    @test map(copysign, 1.0, -2.0) == -1.0
    @test map(muladd, 2, 3, 4) == 10
end

# issue #10311
let n = 1
    @test n//n + n//big(n)*im == 1//1 + 1//1*im
end

# BigInt - (small negative) is tricky because gmp only has gmpz_sub_ui
@test big(-200) - Int8(-128) == -72

@testset "n % Type" begin
    for T in Any[Int16, Int32, UInt32, Int64, UInt64, BigInt]
        if !(T <: Unsigned)
            @test convert(T, -200) %  Int8 === Int8(56)
            @test convert(T, -200) % UInt8 === 0x38
            @test convert(T, -300) %  Int8 === Int8(-44)
            @test convert(T, -300) % UInt8 === 0xd4
            @test convert(T, -128) %  Int8 === Int8(-128)
            @test convert(T, -128) % UInt8 === 0x80
        end
        @test convert(T,  127) %  Int8 === Int8(127)
        @test convert(T,  127) % UInt8 === 0x7f
        @test convert(T,  128) %  Int8 === Int8(-128)
        @test convert(T,  128) % UInt8 === 0x80
        @test convert(T,  200) %  Int8 === Int8(-56)
        @test convert(T,  300) % UInt8 === 0x2c
    end
end

@testset "InexactErrors for Ints" begin
    @test_throws InexactError convert(UInt8, big(300))
    @test_throws InexactError UInt128(-1)
    for T in (Int8,Int16,Int32,Int64,Int128,UInt8,UInt16,UInt32,UInt64,UInt128)
        @test_throws InexactError T(big(typemax(T))+1)
        @test_throws InexactError T(big(typemin(T))-1)
    end
end

for (d,B) in ((4//2+1im,Rational{BigInt}),(3.0+1im,BigFloat),(2+1im,BigInt))
    @test typeof(big(d)) == Complex{B}
    @test big(d) == d
    @test typeof(big.([d])) == Vector{Complex{B}}
    @test big.([d]) == [d]
end

# big fallback
import Base: zero, big
struct TestNumber{Inner} <: Number
    inner::Inner
end
zero(::Type{TestNumber{Inner}}) where {Inner} = TestNumber(zero(Inner))
big(test_number::TestNumber) = TestNumber(big(test_number.inner))
@test big(TestNumber{Int}) == TestNumber{BigInt}

@testset "multiplicative inverses" begin
    function testmi(numrange, denrange)
        for d in denrange
            d == 0 && continue
            fastd = Base.multiplicativeinverse(d)
            for n in numrange
                @test div(n,d) == div(n,fastd)
            end
        end
    end
    testmi(-1000:1000, -100:100)
    testmi(typemax(Int)-1000:typemax(Int), -100:100)
    testmi(typemin(Int)+1:typemin(Int)+1000, -100:100)
    @test_throws ArgumentError Base.multiplicativeinverse(0)
    testmi(map(UInt32, 0:1000), map(UInt32, 1:100))
    testmi(typemax(UInt32)-UInt32(1000):typemax(UInt32), map(UInt32, 1:100))
end
@testset "ndims/indices/size/length" begin
    @test ndims(1) == 0
    @test ndims(Integer) == 0
    @test size(1,1) == 1
    @test_throws BoundsError size(1,-1)
    @test axes(1) == ()
    @test axes(1,1) == 1:1
    @test_throws BoundsError axes(1,-1)
    @test isinteger(Integer(2)) == true
    @test !isinteger(π)
    @test size(1) == ()
    @test length(1) == 1
    @test firstindex(1) == 1
    @test lastindex(1) == 1
    @test eltype(Integer) == Integer
end

@testset "PR #16995" begin
    let types = (Base.BitInteger_types..., BigInt, Bool,
                 Rational{Int}, Rational{BigInt},
                 Float16, Float32, Float64, BigFloat,
                 Complex{Int}, Complex{UInt}, ComplexF16, ComplexF32, ComplexF64)
        for S in types
            for op in (+, -)
                T = @inferred Base.promote_op(op, S)
                t = @inferred op(one(S))
                @test T === typeof(t)
            end

            for R in types
                for op in (+, -, *, /, ^)
                    T = @inferred Base.promote_op(op, S, R)
                    t = @inferred op(one(S), one(R))
                    @test T === typeof(t)
                end
            end
        end

        @test @inferred(Base.promote_op(!, Bool)) === Bool
    end

    let types = (Base.BitInteger_types..., BigInt, Bool,
                 Rational{Int}, Rational{BigInt},
                 Float16, Float32, Float64, BigFloat)
        for S in types, T in types
            for op in (<, >, <=, >=, (==))
                @test @inferred(Base.promote_op(op, S, T)) === Bool
            end
        end
    end

    let types = (Base.BitInteger_types..., BigInt, Bool)
        for S in types
            T = @inferred Base.promote_op(~, S)
            t = @inferred ~one(S)
            @test T === typeof(t)

            for R in types
                for op in (&, |, <<, >>, (>>>), %, ÷)
                    T = @inferred Base.promote_op(op, S, R)
                    t = @inferred op(one(S), one(R))
                    @test T === typeof(t)
                end
            end
        end
    end
end

@test !isempty(complex(1,2))

@testset "rem $T rounded" for T in (Float16, Float32, Float64, BigFloat)
    @test rem(T(1), T(2), RoundToZero)  == 1
    @test rem(T(1), T(2), RoundNearest) == 1
    @test rem(T(1), T(2), RoundDown)    == 1
    @test rem(T(1), T(2), RoundUp)      == -1
    @test rem(T(1.5), T(2), RoundToZero)  == 1.5
    @test rem(T(1.5), T(2), RoundNearest) == -0.5
    @test rem(T(1.5), T(2), RoundDown)    == 1.5
    @test rem(T(1.5), T(2), RoundUp)      == -0.5
    @test rem(T(-1), T(2), RoundToZero)  == -1
    @test rem(T(-1), T(2), RoundNearest) == -1
    @test rem(T(-1), T(2), RoundDown)    == 1
    @test rem(T(-1), T(2), RoundUp)      == -1
    @test rem(T(-1.5), T(2), RoundToZero)  == -1.5
    @test rem(T(-1.5), T(2), RoundNearest) == 0.5
    @test rem(T(-1.5), T(2), RoundDown)    == 0.5
    @test rem(T(-1.5), T(2), RoundUp)      == -1.5
end

@testset "rem for $T RoundNearest" for T in (Int8, UInt8, Int16, UInt16, Int32,
                                             UInt32, Int64, UInt64, Int128, UInt128)
       @test rem(T(7), T(5), RoundNearest) == 2.0
       @test rem(T(4), T(2), RoundNearest) == 0.0
end

@testset "rem2pi $T" for T in (Float16, Float32, Float64, BigFloat)
    @test rem2pi(T(1), RoundToZero)  == 1
    @test rem2pi(T(1), RoundNearest) == 1
    @test rem2pi(T(1), RoundDown)    == 1
    @test rem2pi(T(1), RoundUp)      ≈ 1-2pi
    @test rem2pi(T(2), RoundToZero)  == 2
    @test rem2pi(T(2), RoundNearest) == 2
    @test rem2pi(T(2), RoundDown)    == 2
    @test rem2pi(T(2), RoundUp)      ≈ 2-2pi
    @test rem2pi(T(4), RoundToZero)  == 4
    @test rem2pi(T(4), RoundNearest) ≈ 4-2pi
    @test rem2pi(T(4), RoundDown)    == 4
    @test rem2pi(T(4), RoundUp)      ≈ 4-2pi
    @test rem2pi(T(-4), RoundToZero)  == -4
    @test rem2pi(T(-4), RoundNearest) ≈ 2pi-4
    @test rem2pi(T(-4), RoundDown)    ≈ 2pi-4
    @test rem2pi(T(-4), RoundUp)      == -4
end

import Base.^
struct PR20530; end
struct PR20889; x; end
^(::PR20530, p::Int) = 1
^(t::PR20889, b) = t.x + b
^(t::PR20889, b::Integer) = t.x + b
Base.literal_pow(::typeof(^), ::PR20530, ::Val{p}) where {p} = 2
@testset "literal powers" begin
    x = PR20530()
    p = 2
    @test x^p == 1
    @test x^2 == 2
    @test [x, x, x].^2 == [2, 2, 2]
    for T in (Float16, Float32, Float64, BigFloat, Int8, Int, BigInt, Complex{Int}, Complex{Float64})
        for p in -4:4
            v = eval(:($T(2)^$p))
            @test 2.0^p == v
            if p >= 0 || T == float(T)
                @test v == T(2)^p
                @test v isa T
            else
                @test v isa float(T)
            end
        end
    end
    @test PR20889(2)^3 == 5
    @test [2,4,8].^-2 == [0.25, 0.0625, 0.015625]
    @test [2, 4, 8].^-2 .* 4 == [1.0, 0.25, 0.0625] # nested literal_pow
    @test ℯ^-2 == exp(-2) ≈ inv(ℯ^2) ≈ (ℯ^-1)^2 ≈ sqrt(ℯ^-4)
end
module M20889 # do we get the expected behavior without importing Base.^?
    using Test
    struct PR20889; x; end
    ^(t::PR20889, b) = t.x + b
    Test.@test PR20889(2)^3 == 5
end

@testset "literal negative power accuracy" begin
    @test 0.7130409001548401^-2 == 0.7130409001548401^-2.0
    @test 0.09496527f0^-2 == 0.09496527f0^-2.0f0
    @test 0.20675883960662367^-100 == 0.20675883960662367^-100.0
    @test 0.6123676f0^-100 == 0.6123676f0^-100.0f0
    @test 0.004155780785470562^-1 == 0.004155780785470562^-1.0
end

@testset "iszero & isone" begin
    # Numeric scalars
    for T in [Float16, Float32, Float64, BigFloat,
              Int8, Int16, Int32, Int64, Int128, BigInt,
              UInt8, UInt16, UInt32, UInt64, UInt128]
        @test iszero(T(0))
        @test isone(T(1))
        @test iszero(Complex{T}(0))
        @test isone(Complex{T}(1))
        if T <: Integer
            @test iszero(Rational{T}(0))
            @test isone(Rational{T}(1))
        elseif T <: AbstractFloat
            @test iszero(T(-0.0))
            @test iszero(Complex{T}(-0.0))
        end
    end
    @test !iszero(nextfloat(BigFloat(0)))
    @test !isone(nextfloat(BigFloat(1)))
    for x in (π, ℯ, γ, catalan, φ)
        @test !iszero(x)
        @test !isone(x)
    end

    # Array reduction
    @test !iszero([0, 1, 2, 3])
    @test iszero(zeros(Int, 5))
    @test !isone(tril(fill(1, 5, 5)))
    @test !isone(triu(fill(1, 5, 5)))
    @test !isone(zeros(Int, 5, 5))
    @test isone(Matrix(1I, 5, 5))
    @test isone(Matrix(1I, 1000, 1000)) # sizeof(X) > 2M == ISONE_CUTOFF
end

f20065(B, i) = UInt8(B[i])
@testset "issue 20065" begin
    # f20065 must be called from global scope to exhibit the buggy behavior
    for B in (Vector{Bool}(undef, 10),
                Matrix{Bool}(undef, 10,10),
                reinterpret(Bool, rand(UInt8, 10)))
        @test all(x-> x <= 1, (f20065(B, i) for i in eachindex(B)))
        for i in 1:length(B)
            @test (@eval f20065($B, $i) <= 1)
        end
    end
end

@test inv(3//4) === 4//3 === 1 / (3//4) === 1 // (3//4)

# issues #23244 & #23250
@testset "convert preserves NaN payloads" begin
    @testset "smallest NaNs" begin
        @test convert(Float32,  NaN16) ===  NaN32
        @test convert(Float32, -NaN16) === -NaN32
        @test convert(Float64,  NaN16) ===  NaN64
        @test convert(Float64, -NaN16) === -NaN64
        @test convert(Float16,  NaN32) ===  NaN16
        @test convert(Float16, -NaN32) === -NaN16
        @test convert(Float64,  NaN32) ===  NaN64
        @test convert(Float64, -NaN32) === -NaN64
        @test convert(Float32,  NaN64) ===  NaN32
        @test convert(Float32, -NaN64) === -NaN32
        @test convert(Float16,  NaN64) ===  NaN16
        @test convert(Float16, -NaN64) === -NaN16
    end

    @testset "largest NaNs" begin
        @test convert(Float32, reinterpret(Float16, typemax(UInt16))) ===
              reinterpret(Float32, typemax(UInt32) >> 13 << 13)
        @test convert(Float64, reinterpret(Float16, typemax(UInt16))) ===
              reinterpret(Float64, typemax(UInt64) >> 42 << 42)
        @test convert(Float16, reinterpret(Float32, typemax(UInt32))) ===
              reinterpret(Float16, typemax(UInt16) >> 00 << 00)
        @test convert(Float64, reinterpret(Float32, typemax(UInt32))) ===
              reinterpret(Float64, typemax(UInt64) >> 29 << 29)
        @test convert(Float32, reinterpret(Float64, typemax(UInt64))) ===
              reinterpret(Float32, typemax(UInt32) >> 00 << 00)
        @test convert(Float16, reinterpret(Float64, typemax(UInt64))) ===
              reinterpret(Float16, typemax(UInt16) >> 00 << 00)
    end

    @testset "random NaNs" begin
        nans = AbstractFloat[NaN16, NaN32, NaN64]
        F = [Float16, Float32, Float64]
        U = [UInt16, UInt32, UInt64]
        sig = [11, 24, 53]
        for i = 1:length(F), j = 1:length(F)
            for _ = 1:100
                nan = reinterpret(F[i], rand(U[i]) | reinterpret(U[i], nans[i]))
                z = sig[i] - sig[j]
                nan′ = i <= j ? nan : reinterpret(F[i], reinterpret(U[i], nan) >> z << z)
                @test convert(F[i], convert(F[j], nan)) === nan′
            end
        end
    end
end

# issue #26324
@testset "irrational promotion" begin
    @test π*ComplexF32(2) isa ComplexF32
    @test π/ComplexF32(2) isa ComplexF32
    @test log(π,ComplexF32(2)) isa ComplexF32
end

@testset "printing non finite floats" begin
    let float_types = Set()
        allsubtypes!(Base, AbstractFloat, float_types)
        allsubtypes!(Core, AbstractFloat, float_types)
        @test !isempty(float_types)

        for T in float_types
            for (x, sx) in [(T(NaN), "NaN"),
                            (-T(NaN), "NaN"),
                            (T(Inf), "Inf"),
                            (-T(Inf), "-Inf")]
                @assert x isa T
                @test string(x) == sx
                @test sprint(show, x, context=:compact => true) == sx
                @test sprint(print, x) == sx
            end
        end
    end
end
