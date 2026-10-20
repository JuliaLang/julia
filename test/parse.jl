# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random

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
    # the same for hex digits, with bytes just outside each range of digits
    for n in (8, 16, 17), k in 1:n, c in ('/', ':', '@', 'G', '`', 'g', ' ', '\x7f', '\xff'),
            T in (Int64, UInt64, Int128)
        s = "f"^(k - 1) * c * "A"^(n - k)
        @test P.parsevalue(T, codeunits(s); base = 16) === P.INVALID
    end
    @test P.parsevalue(UInt64, codeunits("0123456789aBcDeF"); base = 16) === 0x0123456789abcdef
    @test P.parsevalue(UInt128, codeunits("FEDCBA9876543210fedcba9876543210"); base = 16) ===
        0xfedcba9876543210fedcba9876543210
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
    # plain decimal digits take a fast path; anything else falls back to the full grammar
    for (s, x) in (("7", 7), ("-7", -7), ("+7", 7), ("-", nothing), ("+", nothing), ("-0", 0),
                   ("07", 7), ("-07", -7), ("0x7", 7), ("1 ", 1), ("1x", nothing), ("1x1", nothing),
                   ("--1", nothing), ("-+1", nothing), (string(typemax(Int)), typemax(Int)),
                   (string(typemin(Int)), typemin(Int)), (string(big(typemax(Int)) + 1), nothing))
        @test tryparse(Int, s) === x
        @test tryparse(Int, SubString(" $s ", 2, ncodeunits(s) + 1)) === x
    end
    @test tryparse(UInt8, "+1") === nothing
    @test tryparse(UInt8, "-1") === nothing
    @test tryparse(UInt8, "256") === nothing
    @test msg(Int8, "128") == "OverflowError: overflow parsing \"128\""
    @test msg(Int8, "1x") == "ArgumentError: invalid base 10 digit 'x' in \"1x\""
    @test Base.tryparse_internal(Int, "x-7y", 2, 3, 10, true) === -7
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

# Floats are correctly rounded. MPFR is the reference, at a precision where rounding its
# result again to the type gives the same as rounding the exact value once.
@testset "float parsing" begin
    P = Base.Parsers
    function reference(T, s; precision = 4000)
        r = BigFloat(s; precision)
        x = T(r)
        return isinf(x) ? P.OVERFLOW : iszero(x) && !iszero(r) ? P.UNDERFLOW : x
    end
    mismatches(T, inputs; kw...) =
        [s for s in inputs if P.parsevalue(T, codeunits(s)) !== reference(T, s; kw...)]
    @test isempty(filter(P.POW5_MIN:P.POW5_MAX) do q
        # fast_float's table of 128-bit significands of powers of five
        p = big(5)^abs(q)
        z = ndigits(p; base = 2)
        t = q >= 0 ? (z <= 128 ? p << (128 - z) : p >> (z - 128)) :
            q >= -27 ? div(big(2)^(z + 127), p) + 1 :
            (c = div(big(2)^(2z + 128), p) + 1; c >> (ndigits(c; base = 2) - 128))
        P.POW5[q - P.POW5_MIN + 1] != t
    end)
    inputs = ["0", "-0", "+0.0", "1", "-1", "1.", ".5", "-.5e1", "1E+5", "0001.5",
              "0.0000000000000000000000000000015", "9007199254740993", "9007199254740995",
              "4503599627370496.5", "4503599627370497.5", "16777217", "16777219",
              "1024.00006103515625", "1024.00018310546875", "2.4703282292062327e-324",
              "2.4703282292062328e-324", "2.2250738585072011e-308",
              "1.7976931348623157e308", "1.7976931348623159e308", "3.4028235e38",
              "3.4028236e38", "7.0e-46", "7.1e-46", "65504", "65519.999", "65520", "2048e4",
              "2.98023223876953125e-8", "2.98023223876953126e-8", "1.000488281250001",
              "99999999999999999999", "9999999999999999999.5e300", "1e400", "-1e-400",
              "1e39", "-1e-46", "1" * "0"^400, "0." * "0"^400 * "1"]
    rng = Xoshiro(1)
    # decimals of up to 30 digits, with the point anywhere and exponents up to 360
    for _ in 1:20_000
        d = String(rand(rng, '0':'9', rand(rng, 1:30)))
        k = rand(rng, 0:length(d))
        s = rand(rng, ("", "-", "+")) * d[1:k] * rand(rng, ("", ".")) * d[k+1:end]
        if rand(rng, Bool)
            s *= rand(rng, ("e", "E", "e-", "e+")) * string(rand(rng, 0:360))
        end
        push!(inputs, s)
    end
    # the shortest representations of random floats
    for _ in 1:5_000, T in (Float64, Float32, Float16)
        x = reinterpret(T, rand(rng, Base.uinttype(T)))
        isfinite(x) && push!(inputs, replace(string(x), 'f' => 'e'))
    end
    # hexadecimal floats of up to 24 digits, with the point anywhere
    hexes = ["0x1p-1074", "0x1p-1075", "0x1.0000000000001p-1075", "0x1.fffffffffffff8p1023",
             "0x1.fffffffffffff7ffffp1023", "0x1p-150", "0x1.000001p-150", "0x1.ffffffp127",
             "0x1p-25", "0x1.002p-25", "0x1.ffep15", "-0X1.8P1", "0x1.8", "0x.8", "0x0p99"]
    for _ in 1:5_000
        d = String(rand(rng, ['0':'9'; 'a':'f'; 'A':'F'], rand(rng, 1:24)))
        k = rand(rng, 0:length(d))
        s = rand(rng, ("", "-")) * "0x" * d[1:k] * rand(rng, ("", ".")) * d[k+1:end]
        rand(rng, Bool) && (s *= rand(rng, ("p", "p-", "P+")) * string(rand(rng, 0:1100)))
        push!(hexes, s)
    end
    for T in (Float64, Float32, Float16)
        @test mismatches(T, inputs) == String[]
        @test mismatches(T, hexes; precision = 200) == String[]
    end
    # midpoints between adjacent floats, exactly, one unit away in their last digit, and
    # with a nonzero digit after more than 800 significant digits
    for T in (Float64, Float32, Float16)
        midpoints = String[]
        xs = abs.(reinterpret.(T, rand(rng, Base.uinttype(T), 300)))
        for x in [zero(T); floatmax(T); filter(isfinite, xs)]
            r = Rational{BigInt}(x) + Rational{BigInt}(eps(x)) / 2
            k = trailing_zeros(denominator(r))
            n = numerator(r) * big(5)^k
            push!(midpoints, "$(n)e-$k", "$(n - 1)e-$k", "$(n + 1)e-$k",
                  "$(n)$("0"^800)1e-$(k + 801)")
        end
        @test mismatches(T, midpoints; precision = 8000) == String[]
    end

    @test P.parsevalue(Float64, codeunits("1e99999999999")) === P.OVERFLOW
    @test P.parsevalue(Float32, codeunits("-1e-99999999999")) === P.UNDERFLOW
    @test P.parsevalue(Float64, codeunits("-0e99999999999")) === -0.0
    @test P.parsevalue(Float16, codeunits("0x1p99999999999")) === P.OVERFLOW
    @test repr(P.UNDERFLOW) == "Base.Parsers.UNDERFLOW"
    @test tryparse(Float16, "70000") === nothing
    @test tryparse(Float16, "1e-10") === nothing
    for (s, x) in (("inf", Inf), ("-Inf", -Inf), ("+INFINITY", Inf), ("nan", NaN),
                   ("-NaN", -NaN)), T in (Float64, Float32, Float16)
        @test P.parsevalue(T, codeunits(s)) === T(x)
    end
    # As in glibc, the parentheses in `nan(...)` hold the payload as C reads an unsigned
    # integer, saturated, or anything else made of letters, digits and '_' for none.
    for (s, p) in (("nan()", 0), ("nan(123)", 123), ("NaN(0x7B)", 123), ("-nan(017)", 15),
                   ("nan(abc)", 0), ("nan(1_2)", 0), ("nan(0x)", 0), ("nan(08)", 0),
                   ("nan(99999999999999999999)", typemax(UInt64))),
            T in (Float64, Float32, Float16)
        U = Base.uinttype(T)
        x = reinterpret(T, reinterpret(U, T(NaN)) |
                           (p % U & (one(U) << (Base.significand_bits(T) - 1) - one(U))))
        @test P.parsevalue(T, codeunits(s)) === (startswith(s, '-') ? -x : x)
    end
    invalid = ["", "+", "-", ".", "-.", "e5", ".e5", "1e", "1e+", "1e5.5", "1.5x", "1,5",
               "--1", "+-1", "- 1", " 1", "1 ", "1.5\0", "in", "infinity"[1:7],
               "infinityy", "nanx", "nan(", "nan(1", "nan(1 2)", "nan(-1)", "nan(1))", "0x",
               "0x.", "0xp1", "0x1p", "0x1p+", "0xg", "0x1.2.3", "0x1p1.5"]
    for s in invalid, T in (Float64, Float32, Float16)
        @test P.parsevalue(T, codeunits(s)) === P.INVALID
    end
    # a byte that is not a hexadecimal digit at each position of one and two blocks of eight
    for n in (8, 16, 17), k in 1:n, c in ('/', ':', '@', 'G', '`', 'g', '\x10', '\x90'),
            prefix in ("0x", "0x1.")
        s = prefix * "f"^(k - 1) * c * "a"^(n - k)
        @test P.parsevalue(Float64, codeunits(s)) === P.INVALID
    end
    for s in (" 1.5", "1.5 ", "\t\n1.5\r\n", "\v\f1.5")
        @test tryparse(Float64, s) === 1.5
    end
    @test tryparse(Float64, "1 .5") === nothing
    for s in ("1.5", "-0.000123456789e-3", "123456789012345678901234567890", "1e400",
              "0x1.8p3", "nan(7)", "x")
        x = P.parsevalue(Float64, codeunits(s))
        bytes = Vector{UInt8}(s)
        padded = [0x2c; bytes; 0x2c]
        sub = SubString("<$s>", 2, length(s) + 1)
        for buf in (bytes, Memory{UInt8}(bytes), view(bytes, 1:1:length(bytes)),
                    view(padded, 2:length(padded) - 1), codeunits(sub))
            @test P.parsevalue(Float64, buf) === x
        end
        y = x isa P.ParseFailure ? nothing : x
        for str in (" $s ", GenericString(s), sub, StringView(bytes),
                    StringView(view(padded, 2:length(padded) - 1)))
            @test tryparse(Float64, str) === y
        end
    end
end

@testset "UUID parsing" begin
    P = Base.Parsers
    rng = Xoshiro(2)
    for _ in 1:1000
        u = Base.UUID(rand(rng, UInt128))
        s = string(u)
        for str in (s, uppercase(s), SubString("<$s>", 2, 37), GenericString(s),
                    StringView(codeunits(s)))
            @test tryparse(Base.UUID, str) === u
        end
        @test P.parsevalue(Base.UUID, view(codeunits("<$s>"), 2:37)) === u
    end
    s = "0123abcd-4567-89ef-ABCD-0123456789Ef"
    @test parse(Base.UUID, s) === Base.UUID(0x0123abcd456789efabcd0123456789ef)
    # each position with a byte just outside the hex digits, a space, a dash or a digit,
    # except those that leave a valid UUID
    for k in 1:36, c in ('/', ':', '@', 'G', '`', 'g', '-', ' ', '\x7f', '\xff', '0')
        (s[k] == '-' ? c == '-' : c == '0') && continue
        t = s[1:k-1] * c * s[k+1:end]
        @test P.parsevalue(Base.UUID, codeunits(t)) === P.INVALID
        @test tryparse(Base.UUID, t) === nothing
    end
    for t in ("", s[1:35], s * "0", "{$s}", s[1:8] * s[10:end] * "0")
        @test tryparse(Base.UUID, t) === nothing
    end
    @test_throws "Malformed UUID string: \"x\"" parse(Base.UUID, "x")
end

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
    for T in (Float16, Float32, Float64)
        @inferred parse(T, "12")
        @inferred Nothing tryparse(T, "12")
        @inferred Base.Parsers.ParseFailure Base.Parsers.parsevalue(T, codeunits("12"))
    end
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
