# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "integer parsing" begin
    @test parse(Int32,"0", base = 36) === Int32(0)
    @test parse(Int32,"1", base = 36) === Int32(1)
    @test parse(Int32,"9", base = 36) === Int32(9)
    @test parse(Int32,"A", base = 36) === Int32(10)
    @test parse(Int32,"a", base = 36) === Int32(10)
    @test parse(Int32,"B", base = 36) === Int32(11)
    @test parse(Int32,"b", base = 36) === Int32(11)
    @test parse(Int32,"F", base = 36) === Int32(15)
    @test parse(Int32,"f", base = 36) === Int32(15)
    @test parse(Int32,"Z", base = 36) === Int32(35)
    @test parse(Int32,"z", base = 36) === Int32(35)

    @test parse(Int,"0") == 0
    @test parse(Int,"-0") == 0
    @test parse(Int,"1") == 1
    @test parse(Int,"-1") == -1
    @test parse(Int,"9") == 9
    @test parse(Int,"-9") == -9
    @test parse(Int,"10") == 10
    @test parse(Int,"-10") == -10
    @test parse(Int64,"3830974272") == 3830974272
    @test parse(Int64,"-3830974272") == -3830974272

    @test parse(Int,'3') == 3
    @test parse(Int,'3', base = 8) == 3
    @test parse(Int, 'a', base=16) == 10
    @test_throws ArgumentError parse(Int, 'a')
    @test_throws ArgumentError parse(Int,typemax(Char))
end

# Issue 29451
struct Issue29451String <: AbstractString end
Base.ncodeunits(::Issue29451String) = 12345
Base.lastindex(::Issue29451String) = 1
Base.isvalid(::Issue29451String, i::Integer) = i == 1
Base.iterate(::Issue29451String, i::Integer=1) = i == 1 ? ('0', 2) : nothing

@test Issue29451String() == "0"
@test parse(Int, Issue29451String()) == 0

@testset "Issue 20587, T=$T" for T in Any[BigInt, Int128, Int16, Int32, Int64, Int8, UInt128, UInt16, UInt32, UInt64, UInt8]
    T === BigInt && continue # TODO: make BigInt pass this test
    for s in ["", " ", "  "]
        # Without a base (handles things like "0x00001111", etc)
        result = @test_throws ArgumentError parse(T, s)
        exception_without_base = result.value
        if T == Bool
            if s == ""
                @test exception_without_base.msg == "input string is empty"
            else
                @test exception_without_base.msg == "input string only contains whitespace"
            end
        else
            @test exception_without_base.msg == "input string is empty or only contains whitespace"
        end

        # With a base
        result = @test_throws ArgumentError parse(T, s, base = 16)
        exception_with_base = result.value
        if T == Bool
            if s == ""
                @test exception_with_base.msg == "input string is empty"
            else
                @test exception_with_base.msg == "input string only contains whitespace"
            end
        else
            @test exception_with_base.msg == "input string is empty or only contains whitespace"
        end
    end

    # Test `tryparse_internal` with part of a string
    let b = "                   "
        result = @test_throws ArgumentError Base.tryparse_internal(Bool, b, 7, 11, 0, true)
        exception_bool = result.value
        @test exception_bool.msg == "input string only contains whitespace"

        result = @test_throws ArgumentError Base.tryparse_internal(Int, b, 7, 11, 0, true)
        exception_int = result.value
        @test exception_int.msg == "input string is empty or only contains whitespace"

        result = @test_throws ArgumentError Base.tryparse_internal(UInt128, b, 7, 11, 0, true)
        exception_uint = result.value
        @test exception_uint.msg == "input string is empty or only contains whitespace"
    end

    # Test that the entire input string appears in error messages
    let s = "     false    true     "
        result = @test_throws(ArgumentError,
            Base.tryparse_internal(Bool, s, firstindex(s), lastindex(s), 0, true))
        @test result.value.msg == "invalid Bool representation: $(repr(s))"
    end

    # Test that leading and trailing whitespace is ignored.
    for v in (1, 2, 3)
        @test parse(Int, "    $v"    ) == v
        @test parse(Int, "    $v\n"  ) == v
        @test parse(Int, "$v    "    ) == v
        @test parse(Int, "    $v    ") == v
    end
    for v in (true, false)
        @test parse(Bool, "    $v"    ) == v
        @test parse(Bool, "    $v\n"  ) == v
        @test parse(Bool, "$v    "    ) == v
        @test parse(Bool, "    $v    ") == v
    end
    for v in (0.05, -0.05, 2.5, -2.5)
        @test parse(Float64, "    $v"    ) == v
        @test parse(Float64, "    $v\n"  ) == v
        @test parse(Float64, "$v    "    ) == v
        @test parse(Float64, "    $v    ") == v
    end
    @test parse(Float64, "    .5"    ) == 0.5
    @test parse(Float64, "    .5\n"  ) == 0.5
    @test parse(Float64, "    .5    ") == 0.5
    @test parse(Float64, ".5    "    ) == 0.5
end

@testset "parse as Bool, bin, hex, oct" begin
    @test parse(Bool, "\u202f true") === true
    @test parse(Bool, "\u202f false") === false

    parsebin(s) = parse(Int,s, base = 2)
    parseoct(s) = parse(Int,s, base = 8)
    parsehex(s) = parse(Int,s, base = 16)

    @test parsebin("0") == 0
    @test parsebin("-0") == 0
    @test parsebin("1") == 1
    @test parsebin("-1") == -1
    @test parsebin("10") == 2
    @test parsebin("-10") == -2
    @test parsebin("11") == 3
    @test parsebin("-11") == -3
    @test parsebin("1111000011110000111100001111") == 252645135
    @test parsebin("-1111000011110000111100001111") == -252645135

    @test parseoct("0") == 0
    @test parseoct("-0") == 0
    @test parseoct("1") == 1
    @test parseoct("-1") == -1
    @test parseoct("7") == 7
    @test parseoct("-7") == -7
    @test parseoct("10") == 8
    @test parseoct("-10") == -8
    @test parseoct("11") == 9
    @test parseoct("-11") == -9
    @test parseoct("72") == 58
    @test parseoct("-72") == -58
    @test parseoct("3172207320") == 434704080
    @test parseoct("-3172207320") == -434704080

    @test parsehex("0") == 0
    @test parsehex("-0") == 0
    @test parsehex("1") == 1
    @test parsehex("-1") == -1
    @test parsehex("9") == 9
    @test parsehex("-9") == -9
    @test parsehex("a") == 10
    @test parsehex("-a") == -10
    @test parsehex("f") == 15
    @test parsehex("-f") == -15
    @test parsehex("10") == 16
    @test parsehex("-10") == -16
    @test parsehex("0BADF00D") == 195948557
    @test parsehex("-0BADF00D") == -195948557
    @test parse(Int64,"BADCAB1E", base = 16) == 3135023902
    @test parse(Int64,"-BADCAB1E", base = 16) == -3135023902
    @test parse(Int64,"CafeBabe", base = 16) == 3405691582
    @test parse(Int64,"-CafeBabe", base = 16) == -3405691582
    @test parse(Int64,"DeadBeef", base = 16) == 3735928559
    @test parse(Int64,"-DeadBeef", base = 16) == -3735928559
end

@testset "parse with delimiters" begin
    @test parse(Int,"2\n") == 2
    @test parse(Int,"   2 \n ") == 2
    @test parse(Int," 2 ") == 2
    @test parse(Int,"2 ") == 2
    @test parse(Int," 2") == 2
    @test parse(Int,"+2\n") == 2
    @test parse(Int,"-2") == -2
    @test_throws ArgumentError parse(Int,"   2 \n 0")
    @test_throws ArgumentError parse(Int,"2x")
    @test_throws ArgumentError parse(Int,"-")

    # multibyte spaces
    @test parse(Int, "3\u2003\u202F") == 3
    @test_throws ArgumentError parse(Int, "3\u2003\u202F,")
end

@testset "parse from bin/hex/oct" begin
    @test parse(Int,"1234") == 1234
    @test parse(Int,"0x1234") == 0x1234
    @test parse(Int,"0o1234") == 0o1234
    @test parse(Int,"0b1011") == 0b1011
    @test parse(Int,"-1234") == -1234
    @test parse(Int,"-0x1234") == -Int(0x1234)
    @test parse(Int,"-0o1234") == -Int(0o1234)
    @test parse(Int,"-0b1011") == -Int(0b1011)
end

@testset "parsing extrema of Integer types" begin
    for T in (Int8, Int16, Int32, Int64, Int128)
        @test parse(T,string(typemin(T))) == typemin(T)
        @test parse(T,string(typemax(T))) == typemax(T)
        @test_throws OverflowError parse(T,string(big(typemin(T))-1))
        @test_throws OverflowError parse(T,string(big(typemax(T))+1))
    end

    for T in (UInt8,UInt16,UInt32,UInt64,UInt128)
        @test parse(T,string(typemin(T))) == typemin(T)
        @test parse(T,string(typemax(T))) == typemax(T)
        @test_throws ArgumentError parse(T,string(big(typemin(T))-1))
        @test_throws OverflowError parse(T,string(big(typemax(T))+1))
    end
end

# make sure base can be any Integer
@testset "issue #15597, T=$T" for T in (Int, BigInt)
    let n = parse(T, "123", base = Int8(10))
        @test n == 123
        @test isa(n, T)
    end
end

@testset "issue #17065" begin
    @test parse(Int, "2") === 2
    @test parse(Bool, "true") === true
    @test parse(Bool, "false") === false
    @test tryparse(Bool, "true") === true
    @test tryparse(Bool, "false") === false
    @test_throws ArgumentError parse(Int, "2", base = 1)
    @test_throws ArgumentError parse(Int, "2", base = 63)
end

# issue #17333: tryparse should still throw on invalid base
for T in (Int32, BigInt), base in (0,1,100)
    @test_throws ArgumentError tryparse(T, "0", base = base)
end

# error throwing branch from #10560
@test_throws ArgumentError Base.tryparse_internal(Bool, "foo", 1, 2, 10, true)

@test tryparse(Float64, "1.23") === 1.23
@test tryparse(Float32, "1.23") === 1.23f0
@test tryparse(Float16, "1.23") === Float16(1.23)

# parsing complex numbers (#22250)
@testset "complex parsing" begin
    for sign in ('-','+'), Im in ("i","j","im"), s1 in (""," "), s2 in (""," "), s3 in (""," "), s4 in (""," ")
        for r in (1,0,-1), i in (1,0,-1),
            n = Complex(r, sign == '+' ? i : -i)
            s = string(s1, r, s2, sign, s3, i, Im, s4)
            @test n === parse(Complex{Int}, s)
            @test Complex(r) === parse(Complex{Int}, string(s1, r, s2))
            @test Complex(0,i) === parse(Complex{Int}, string(s3, i, Im, s4))
            for T in (Float64, BigFloat)
                nT = parse(Complex{T}, s)
                @test nT isa Complex{T}
                @test nT == n
                @test n == parse(Complex{T}, string(s1, r, ".0", s2, sign, s3, i, ".0", Im, s4))
                @test n*parse(T,"1e-3") == parse(Complex{T}, string(s1, r, "e-3", s2, sign, s3, i, "e-3", Im, s4))
            end
        end
        for r in (-1.0,-1e-9,Inf,-Inf,NaN), i in (-1.0,-1e-9,Inf,NaN)
            n = Complex(r, sign == '+' ? i : -i)
            s = lowercase(string(s1, r, s2, sign, s3, i, Im, s4))
            @test n === parse(ComplexF64, s)
            @test Complex(r) === parse(ComplexF64, string(s1, r, s2))
            @test Complex(0,i) === parse(ComplexF64, string(s3, i, Im, s4))
        end
    end
    @test parse(Complex{Float16}, "3.3+4i") === Complex{Float16}(3.3+4im)
    @test parse(Complex{Int}, SubString("xxxxxx1+2imxxxx", 7, 10)) === 1+2im
    for T in (Int, Float64), bad in ("3 + 4*im", "3 + 4", "1+2ij", "1im-3im", "++4im")
        @test_throws ArgumentError parse(Complex{T}, bad)
    end
    @test_throws ArgumentError parse(Complex{Int}, "3 + 4.2im")
end

@testset "parse and tryparse type inference" begin
    @inferred parse(Int, "12")
    @inferred parse(Float64, "12")
    @inferred parse(Complex{Int}, "12")
    @test eltype([parse(Int, s, base=16) for s in String[]]) == Int
    @test eltype([parse(Float64, s) for s in String[]]) == Float64
    @test eltype([parse(Complex{Int}, s) for s in String[]]) == Complex{Int}
    @test eltype([tryparse(Int, s, base=16) for s in String[]]) == Union{Nothing, Int}
    @test eltype([tryparse(Float64, s) for s in String[]]) == Union{Nothing, Float64}
    @test eltype([tryparse(Complex{Int}, s) for s in String[]]) == Union{Nothing, Complex{Int}}
end

@testset "isssue #29980" begin
    @test parse(Bool, "1") === true
    @test parse(Bool, "01") === true
    @test parse(Bool, "0") === false
    @test parse(Bool, "000000000000000000000000000000000000000000000000001") === true
    @test parse(Bool, "000000000000000000000000000000000000000000000000000") === false
    @test_throws ArgumentError parse(Bool, "1000000000000000000000000000000000000000000000000000")
    @test_throws ArgumentError parse(Bool, "2")
    @test_throws ArgumentError parse(Bool, "02")
end

@testset "inf and nan parsing" begin
    for (v,vs) in ((NaN,"nan"), (Inf,"inf"), (Inf,"infinity")), sbefore in ("", "  "), safter in ("", "  "), sign in (+, -), case in (lowercase, uppercase)
        s = case(string(sbefore, sign, vs, safter))
        @test isequal(parse(Float64, s), sign(v))
    end
end

@testset "unary ± and ∓" begin
    @test Meta.parse("±x") == Expr(:call, :±, :x)
    @test Meta.parse("∓x") == Expr(:call, :∓, :x)
end

@test [Int, Float64, String, Bool] .<: Union{Int, String} == Bool[1, 0, 1, 0]
