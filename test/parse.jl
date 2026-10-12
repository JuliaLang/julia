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

    @test parse(Int32,'1',base=2)==1
    @test parse(Int32,'c',base=58) == 38
    @test parse(Int32,'d',base=62)==39
    @test parse(Int32,'8') == 8
    @test parse(Int,'3') == 3
    @test parse(Int,'3', base = 8) == 3
    @test parse(Int, 'a', base=16) == 10
    @test_throws ArgumentError parse(Int, 'a')
    @test_throws ArgumentError parse(Int,typemax(Char))
    @test_throws ArgumentError parse(Int8,'A',base=64)
    @test_throws ArgumentError parse(Int8,'B',base=1)
    @test_throws ArgumentError parse(Int8,'φ',base=20)
    @test_throws ArgumentError parse(Int32,'A',base=10)
end

# Issue 29451
struct Issue29451String <: AbstractString end
Base.ncodeunits(::Issue29451String) = 12345
Base.lastindex(::Issue29451String) = 1
Base.isvalid(::Issue29451String, i::Integer) = i == 1
Base.iterate(::Issue29451String, i::Integer=1) = i == 1 ? ('0', 2) : nothing

@test Issue29451String() == "0"
@test parse(Int, Issue29451String()) == 0

# https://github.com/JuliaStrings/InlineStrings.jl/issues/57
struct InlineStringIssue57 <: AbstractString end
Base.ncodeunits(::InlineStringIssue57) = 4
Base.lastindex(::InlineStringIssue57) = 4
Base.isvalid(::InlineStringIssue57, i::Integer) = 0 < i < 5
Base.iterate(::InlineStringIssue57, i::Integer=1) = i == 1 ? ('t', 2) : i == 2 ? ('r', 3) : i == 3 ? ('u', 4) : i == 4 ? ('e', 5) : nothing
Base.:(==)(::SubString{InlineStringIssue57}, x::String) = x == "true"

@test parse(Bool, InlineStringIssue57())

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

# `Base.Parsers.parsevalue` parses all of a byte vector, returning a `ParseFailure` instead
# of throwing. GMP's `BigInt` parser is the reference for the values.
@testset "Base.Parsers.parsevalue" begin
    P = Base.Parsers
    for T in (Int8, Int16, Int32, Int64, Int128, UInt8, UInt16, UInt32, UInt64, UInt128),
            base in (2, 8, 10, 16, 36, 62),
            x in (big(typemin(T)) - 1, typemin(T), typemin(T) ÷ 7, 0, 1, typemax(T) ÷ 3,
                  typemax(T), big(typemax(T)) + 1)
        sign = x < 0 ? "-" : ""
        expected = T <: Unsigned && x < 0 ? P.INVALID :
                   typemin(T) <= x <= typemax(T) ? T(x) : P.OVERFLOW
        for s in (sign * string(abs(big(x)); base), sign * "0"^45 * string(abs(big(x)); base))
            @test parse(BigInt, s; base) == x
            bytes = Vector{UInt8}(s)
            padded = [0x2c; bytes; 0x2c]
            for buf in (bytes, codeunits(s), Memory{UInt8}(bytes), view(bytes, 1:1:length(bytes)),
                        view(padded, 2:length(padded) - 1))
                @test P.parsevalue(T, buf; base) === expected
            end
        end
    end
    # a bad byte at each position of one and two eight-digit blocks
    for n in (8, 16, 17), k in 1:n, c in ('/', ':', ' ', 'a', '\xff'), T in (Int64, UInt64, Int128)
        s = "9"^(k - 1) * c * "7"^(n - k)
        @test P.parsevalue(T, codeunits(s)) === P.INVALID
    end
    @test P.parsevalue(Int, view(codeunits("x=-42;"), 3:5)) === -42
    for s in ("", "-", "+", " 1", "1 ", "--1", "0x1")
        @test P.parsevalue(Int, codeunits(s)) === P.INVALID
    end
    @test P.parsevalue(UInt8, codeunits("+1")) === P.INVALID
    @test_throws ArgumentError P.parsevalue(Int, codeunits("12"); base = 63)
    @test repr(P.OVERFLOW) == "Base.Parsers.OVERFLOW"
    # integer types other than the fixed-width ones take a generic method
    generic(T, s, base, neg) = invoke(P.parseint,
        Tuple{Type{<:Integer}, AbstractVector{UInt8}, Int, Int, Int, Bool},
        T, codeunits(s), 1, ncodeunits(s), base, neg)
    for T in (Int8, Int64, Int128, UInt8, UInt64, UInt128), base in (2, 10, 16, 62),
            s in (string(typemax(T); base), string(big(typemax(T)) + 1; base), "0"^40 * "1", "12x"),
            neg in (false, T <: Signed)
        @test generic(T, s, base, neg) === P.parseint(T, codeunits(s), 1, ncodeunits(s), base, neg)
    end
end

# Base's grammar around the digits, for every kind of string, and its error messages,
# which name the first problem from the left.
@testset "integer parsing grammar and errors" begin
    @test parse(Int, "\u202f-\u00a042\u202f") === -42
    @test parse(Int, "\u85 7\u3000") === 7
    @test parse(Int8, "- 0x80") === typemin(Int8)
    @test parse(UInt8, " 0xff ") === 0xff
    @test Base.tryparse_internal(Int, "x-123y", 2, 5, 10, true) === -123
    for s in ("12", " 12 ", "-0x7f", "1 2")
        x = tryparse(Int, s)
        @test tryparse(Int, GenericString(s)) === x
        @test tryparse(Int, SubString("<$s>", 2, ncodeunits(s) + 1)) === x
        @test tryparse(Int, StringView(view(Vector{UInt8}(s), 1:1:ncodeunits(s)))) === x
    end
    msg(T, s) = try parse(T, s); "" catch err sprint(showerror, err) end
    @test msg(Int, " - ") == "ArgumentError: input string is empty or only contains whitespace"
    @test msg(Int, "0x") == "ArgumentError: premature end of integer: \"0x\""
    @test msg(Int, "0x ") == "ArgumentError: invalid base 16 digit ' ' in \"0x \""
    @test msg(Int, "0x-1") == "ArgumentError: invalid base 16 digit '-' in \"0x-1\""
    @test msg(UInt8, "+1") == "ArgumentError: invalid base 10 digit '+' in \"+1\""
    @test msg(Int, "12β") == "ArgumentError: invalid base 10 digit 'β' in \"12β\""
    s = "1\xff"
    @test msg(Int, s) == "ArgumentError: invalid base 10 digit $(repr(s[2])) in $(repr(s))"
    s = "1\u202f2"
    @test msg(Int, s) == "ArgumentError: extra characters after whitespace in $(repr(s))"
    @test msg(Int8, "1000x") == "OverflowError: overflow parsing \"1000x\""
    @test msg(Int8, "1000 2") == "OverflowError: overflow parsing \"1000 2\""
    @test msg(Int8, "10x00") == "ArgumentError: invalid base 10 digit 'x' in \"10x00\""
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

@testset "issue #42616" begin
    @test tryparse(Bool, "") === nothing
    @test tryparse(Bool, " ") === nothing
    @test_throws ArgumentError parse(Bool, "")
    @test_throws ArgumentError parse(Bool, " ")
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
    @test_throws ArgumentError parse(ComplexF64, "3 β+ 4im")
    @test_throws ArgumentError parse(ComplexF64, "3 + 4αm")
end

@testset "parse and tryparse type inference" begin
    for T in (Int8, Int16, Int32, Int64, Int128, UInt8, UInt16, UInt32, UInt64, UInt128)
        @inferred parse(T, "12")
        @inferred Nothing tryparse(T, "12")
        @inferred Base.Parsers.ParseFailure Base.Parsers.parsevalue(T, codeunits("12"))
        @test eltype([parse(T, s) for s in AbstractString[]]) == T
    end
    @inferred parse(Float64, "12")
    @inferred parse(Complex{Int}, "12")
    @test eltype([parse(Int, s, base=16) for s in String[]]) == Int
    @test eltype([parse(Float64, s) for s in String[]]) == Float64
    @test eltype([parse(Complex{Int}, s) for s in String[]]) == Complex{Int}
    @test eltype([tryparse(Int, s, base=16) for s in String[]]) == Union{Nothing, Int}
    @test eltype([tryparse(Float64, s) for s in String[]]) == Union{Nothing, Float64}
    @test eltype([tryparse(Complex{Int}, s) for s in String[]]) == Union{Nothing, Complex{Int}}
end

@testset "issue #29980" begin
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
