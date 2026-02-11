# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using TOML
using TOML: Internals

# Construct an explicit Parser to test the "cached" version of parsing
const test_parser = TOML.Parser()

function testval(s, v)
    f = "foo = $s"
    # First, test with the standard entrypoint
    parsed = TOML.parse(f)["foo"]
    return isequal(v, parsed) && typeof(v) == typeof(parsed)
    (!isequal(v, parsed) || typeof(v) != typeof(parsed)) && return false
    # Next, test with the "cached" (explicit Parser) entrypoint
    parsed = TOML.parse(test_parser, f)["foo"]
    (!isequal(v, parsed) || typeof(v) != typeof(parsed)) && return false
    return true
end

function failval(s, v)
    f = "foo = $s"
    # First, test with the standard entrypoint
    err = TOML.tryparse(f);
    return err isa TOML.Internals.ParserError && err.type == v
    (!isa(err, TOML.Internals.ParserError) || err.type != v) && return false
    # Next, test with the "cached" (explicit Parser) entrypoint
    err = TOML.tryparse(test_parser, f);
    (!isa(err, TOML.Internals.ParserError) || err.type != v) && return false
    return true
end

@testset "Numbers" begin
    @test failval("00"                   , Internals.ErrParsingDateTime)
    @test failval("-00"                  , Internals.ErrParsingDateTime)
    @test failval("+00"                  , Internals.ErrParsingDateTime)
    @test failval("00.0"                 , Internals.ErrParsingDateTime)
    @test failval("-00.0"                , Internals.ErrParsingDateTime)
    @test failval("+00.0"                , Internals.ErrParsingDateTime)

    @test failval("0."        , Internals.ErrNoTrailingDigitAfterDot)
    @test failval("0.e"       , Internals.ErrNoTrailingDigitAfterDot)
    @test failval("0.E"       , Internals.ErrNoTrailingDigitAfterDot)
    @test failval("0.0E"      , Internals.ErrGenericValueError)
    @test failval("0.0e"      , Internals.ErrGenericValueError)
    @test failval("0.0e-"     , Internals.ErrGenericValueError)
    @test failval("0.0e+"     , Internals.ErrGenericValueError)
    @test_broken failval("0.0e+00" , Internals.ErrGenericValueError)

    @test testval("1.0"         , 1.0)
    @test testval("1.0e0"       , 1.0)
    @test testval("1.0e+0"      , 1.0)
    @test testval("1.0e-0"      , 1.0)
    @test testval("0e-3"        , 0.0)
    @test testval("1.001e-0"    , 1.001)
    @test testval("2e10"        , 2e10)
    @test testval("2e+10"       , 2e10)
    @test testval("2e-10"       , 2e-10)
    @test testval("2_0.0"       , 20.0)
    @test testval("2_0.0_0e0_0" , 20.0)
    @test testval("2_0.1_0e1_0" , 20.1e10)

    @test testval("1_0"    , 10    |> Int64)
    @test testval("1_0_0"  , 100   |> Int64)
    @test testval("1_000"  , 1000  |> Int64)
    @test testval("+1_000" , 1000  |> Int64)
    @test testval("-1_000" , -1000 |> Int64)

    @test testval("0x6E", 0x6E|> UInt64)
    @test testval("0x8f1e", 0x8f1e|> UInt64)
    @test testval("0x765f3173", 0x765f3173|> UInt64)
    @test testval("0xc13b830a807cc7f4", 0xc13b830a807cc7f4|> UInt64)
    @test testval("0x937efe_0a4241_edb24a04b97bd90ef363", 0x937efe0a4241edb24a04b97bd90ef363 |> UInt128)

    @test testval("0o140", 0o140 |> UInt64) # UInt8
    @test testval("0o46244", 0o46244 |> UInt64) # UInt16
    @test testval("0o32542120656", 0o32542120656 |> UInt64) # UInt32
    @test testval("0o1526535761042630654411", 0o1526535761042630654411 |> UInt64) # UInt64
    @test testval("0o3467204325743773607311464533371572447656531", 0o3467204325743773607311464533371572447656531 |> UInt128) # UInt128
    @test testval("0o34672043257437736073114645333715724476565312", 0o34672043257437736073114645333715724476565312 |> BigInt) # BigInt

    @test testval("0b10001010",0b10001010 |> UInt64) # UInt8
    @test testval("0b11111010001100",0b11111010001100 |> UInt64) # UInt16
    @test testval("0b11100011110000010101000010101",0b11100011110000010101000010101 |> UInt64) # UInt32
    @test testval("0b10000110100111011010001000000111110110000011111101101110011011",0b10000110100111011010001000000111110110000011111101101110011011 |> UInt64) # UInt64
    @test testval(
        "0b1101101101101100110001010110111011101000111010101110011000011100110100101111110001010001011001000001000001010010011101100100111",
        0b1101101101101100110001010110111011101000111010101110011000011100110100101111110001010001011001000001000001010010011101100100111 |> UInt128) # UInt128
    @test testval(
        "0b110110110110110011000101011011101110100011101010111001100001110011010010111111000101000101100100000100000101001001110110010011111",
        0b110110110110110011000101011011101110100011101010111001100001110011010010111111000101000101100100000100000101001001110110010011111 |> BigInt) # BigInt

    @test failval("0_"     , Internals.ErrUnderscoreNotSurroundedByDigits)
    @test failval("0__0"   , Internals.ErrUnderscoreNotSurroundedByDigits)
    @test failval("__0"    , Internals.ErrUnexpectedStartOfValue)
    @test failval("1_0_"   , Internals.ErrTrailingUnderscoreNumber)
    @test failval("1_0__0" , Internals.ErrUnderscoreNotSurroundedByDigits)
end


@testset "Booleans" begin
    @test testval("true", true)
    @test testval("false", false)

    @test failval("true2"  , Internals.ErrExpectedNewLineKeyValue)
    @test failval("false2" , Internals.ErrExpectedNewLineKeyValue)
    @test failval("talse"  , Internals.ErrGenericValueError)
    @test failval("frue"   , Internals.ErrGenericValueError)
    @test failval("t1"     , Internals.ErrGenericValueError)
    @test failval("f1"     , Internals.ErrGenericValueError)
end

@testset "Datetime" begin
    @test testval("2016-09-09T09:09:09"     , DateTime(2016 , 9 , 9 , 9 , 9 , 9))
    @test testval("2016-09-09T09:09:09Z"    , DateTime(2016 , 9 , 9 , 9 , 9 , 9))
    @test testval("2016-09-09T09:09:09.0Z"  , DateTime(2016 , 9 , 9 , 9 , 9 , 9))
    @test testval("2016-09-09T09:09:09.012" , DateTime(2016 , 9 , 9 , 9 , 9 , 9  , 12))
    @test testval("2016-09-09T09:09:09.2"   , DateTime(2016 , 9 , 9 , 9 , 9 , 9  , 200))
    @test testval("2016-09-09T09:09:09.20"  , DateTime(2016 , 9 , 9 , 9 , 9 , 9  , 200))
    @test testval("2016-09-09T09:09:09.02"  , DateTime(2016 , 9 , 9 , 9 , 9 , 9  , 20))

    @test failval("2016-09-09T09:09:09.0+10:00"   , Internals.ErrOffsetDateNotSupported)
    @test failval("2016-09-09T09:09:09.012-02:00" , Internals.ErrOffsetDateNotSupported)
    @test failval("2016-09-09T09:09:09.0+10:00"   , Internals.ErrOffsetDateNotSupported)
    @test failval("2016-09-09T09:09:09.012-02:00" , Internals.ErrOffsetDateNotSupported)

    @test failval("2016-09-09T09:09:09.Z" , Internals.ErrParsingDateTime)
    @test failval("2016-9-09T09:09:09Z"   , Internals.ErrParsingDateTime)
    @test failval("2016-13-09T09:09:09Z"  , Internals.ErrParsingDateTime)
    @test failval("2016-02-31T09:09:09Z"  , Internals.ErrParsingDateTime)
    @test failval("2016-09-09T09:09:09x"  , Internals.ErrParsingDateTime)
    @test failval("2016-09-09s09:09:09Z"  , Internals.ErrParsingDateTime)
    @test failval("2016-09-09T09:09:09x"  , Internals.ErrParsingDateTime)
end

@testset "Time" begin
    @test testval("09:09:09.99"    , Time(9 , 9 , 9 , 990))
    @test testval("09:09:09.99999" , Time(9 , 9 , 9 , 999))
    @test testval("00:00:00.2"     , Time(0 , 0 , 0 , 200))
    @test testval("00:00:00.20"    , Time(0 , 0 , 0 , 200))
    @test testval("00:00:00.23"    , Time(0 , 0 , 0 , 230))
    @test testval("00:00:00.234"   , Time(0 , 0 , 0 , 234))

    @test failval("09:09x09", Internals.ErrParsingDateTime)
end

# TODO: Add more dedicated value tests

@testset "String" begin
    @test failval("\"foooo", Internals.ErrUnexpectedEndString)

    #=
    Found these examples of string tests somewhere
    quot0=""" """    # valid
    quot1=""" """"   # valid
    quot2=""" """""  # valid
    quot3=""" """""" # invalid
    apos0=''' '''    # valid
    apos1=''' ''''   # valid
    apos2=''' '''''  # valid
    apos3=''' '''''' # invalid

    quot4=""""""     # valid (6 in a row, empty string)
    quot5="""" """   # valid
    quot6=""""" """  # valid
    quot7="""""" """ # invalid
    apos4=''''''     # valid (6 in a row, empty string)
    apos5='''' '''   # valid
    apos6=''''' '''  # valid
    apos7='''''' ''' # invalid

    quot8="""""\""""""     # valid (5 at start, 6 at end)
    quot9="""""\"""\"""""" # valid (3 in the middle 5 at start, 6 at end)

    =#
end

@testset "Array" begin
    @test testval("[1,2,3]", Int64[1,2,3])
    @test testval("[1.0, 2.0, 3.0]", Float64[1.0, 2.0, 3.0])
    @test testval("[1.0, 2.0, 3]", Any[1.0, 2.0, Int64(3)])
    @test testval("[1.0, 2, \"foo\"]", Any[1.0, Int64(2), "foo"])
end
