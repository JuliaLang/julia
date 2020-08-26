using Test
using TOML
using TOML: Internals

function testval(s, v)
    f = "foo = $s"
    parsed = TOML.parse(f)["foo"]
    return isequal(v, parsed) && typeof(v) == typeof(parsed)
end

function failval(s, v)
    f = "foo = $s"
    err = TOML.tryparse(f);
    return err isa TOML.Internals.ParserError && err.type == v
end

@testset "Numbers" begin
    @test failval("00"                   , Internals.ErrParsingDateTime)
    @test failval("-00"                  , Internals.ErrParsingDateTime)
    @test failval("+00"                  , Internals.ErrParsingDateTime)
    @test failval("00.0"                 , Internals.ErrParsingDateTime)
    @test failval("-00.0"                , Internals.ErrParsingDateTime)
    @test failval("+00.0"                , Internals.ErrParsingDateTime)
    @test failval("9223372036854775808"  , Internals.ErrOverflowError)
    @test failval("-9223372036854775809" , Internals.ErrOverflowError)

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

    @test failval("0_"     , Internals.ErrLeadingZeroNotAllowedInteger)
    @test failval("0__0"   , Internals.ErrLeadingZeroNotAllowedInteger)
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
    @test testval("09:09:09.99"    , Time(9 , 9 , 9 , 99))
    @test testval("09:09:09.99999" , Time(9 , 9 , 9 , 999))

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
    @test testval("[1.0, 2.0, 3]", Union{Int64, Float64}[1.0, 2.0, Int64(3)])
    @test testval("[1.0, 2, \"foo\"]", Any[1.0, Int64(2), "foo"])
end
