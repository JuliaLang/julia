using JuliaSyntax:
    julia_string_to_number,
    triplequoted_string_indentation,
    unescape_julia_string,
    process_triple_strings!

hexint(s) = julia_string_to_number(s, K"HexInt")
binint(s) = julia_string_to_number(s, K"BinInt")
octint(s) = julia_string_to_number(s, K"OctInt")

@testset "Number parsing" begin
    # Integers
    @testset "Integers" begin
        @test julia_string_to_number("-1", K"Integer") isa Int
        @test julia_string_to_number("1", K"Integer") isa Int
        @test julia_string_to_number("2147483647", K"Integer") isa Int
        @test julia_string_to_number("9223372036854775807", K"Integer") isa Int64
        @test julia_string_to_number("9223372036854775808", K"Integer") isa Int128
        @test julia_string_to_number("170141183460469231731687303715884105727", K"Integer") isa Int128
        @test julia_string_to_number("170141183460469231731687303715884105728", K"Integer") isa BigInt
    end

    # Floats
    @testset "Floats" begin
        @test julia_string_to_number("10e-0", K"Float") === Float64(10)
        @test julia_string_to_number("10f-0", K"Float") === Float32(10)
        @test julia_string_to_number("0x0ap-0", K"Float") === Float64(10)
    end

    # HexInt
    @testset "HexInt numeric limits for different types" begin
        @test hexint("0xff")  === UInt8(0xff)
        @test hexint("0x100") === UInt16(0x100)
        @test hexint("0xffff") === UInt16(0xffff)
        @test hexint("0x10000") === UInt32(0x10000)
        @test hexint("0xffffffff") === UInt32(0xffffffff)
        @test hexint("0x100000000") === UInt64(0x100000000)
        @test hexint("0xffffffffffffffff") === UInt64(0xffffffffffffffff)
        @test hexint("0x10000000000000000") === UInt128(0x10000000000000000)
        @test hexint("0xffffffffffffffffffffffffffffffff") === UInt128(0xffffffffffffffffffffffffffffffff)
        @test (n = hexint("0x100000000000000000000000000000000");
               n isa BigInt && n == 0x100000000000000000000000000000000)
    end
    @testset "HexInt string length limits for different types" begin
        @test hexint("0x00")  === UInt8(0)
        @test hexint("0x000")  === UInt16(0)
        @test hexint("0x0000")  === UInt16(0)
        @test hexint("0x00000")  === UInt32(0)
        @test hexint("0x00000000") === UInt32(0)
        @test hexint("0x000000000") === UInt64(0)
        @test hexint("0x0000000000000000") === UInt64(0)
        @test hexint("0x00000000000000000") === UInt128(0)
        @test hexint("0x00000000000000000000000000000000") === UInt128(0)
        @test (n = hexint("0x000000000000000000000000000000000");
               n isa BigInt && n == 0)
    end

    # BinInt
    @testset "BinInt numeric limits for different types" begin
        @test binint("0b11111111")  === UInt8(0xff)
        @test binint("0b100000000") === UInt16(0x100)
        @test binint("0b1111111111111111") === UInt16(0xffff)
        @test binint("0b10000000000000000") === UInt32(0x10000)
        @test binint("0b11111111111111111111111111111111") === UInt32(0xffffffff)
        @test binint("0b100000000000000000000000000000000") === UInt64(0x100000000)
        @test binint("0b1111111111111111111111111111111111111111111111111111111111111111") === UInt64(0xffffffffffffffff)
        @test binint("0b10000000000000000000000000000000000000000000000000000000000000000") === UInt128(0x10000000000000000)
        @test binint("0b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111") === UInt128(0xffffffffffffffffffffffffffffffff)
        @test (n = binint("0b100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
               n isa BigInt && n == 0x100000000000000000000000000000000)
    end
    @testset "BinInt string length limits for different types" begin
        @test binint("0b00000000")  === UInt8(0)
        @test binint("0b000000000")  === UInt16(0)
        @test binint("0b0000000000000000")  === UInt16(0)
        @test binint("0b00000000000000000")  === UInt32(0)
        @test binint("0b00000000000000000000000000000000") === UInt32(0)
        @test binint("0b000000000000000000000000000000000") === UInt64(0)
        @test binint("0b0000000000000000000000000000000000000000000000000000000000000000") === UInt64(0)
        @test binint("0b00000000000000000000000000000000000000000000000000000000000000000") === UInt128(0)
        @test binint("0b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000") === UInt128(0)
        @test (n = binint("0b000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
               n isa BigInt && n == 0)
    end

    # OctInt
    @testset "OctInt numeric limits for different types" begin
        @test octint("0o377")  === UInt8(0xff)
        @test octint("0o400") === UInt16(0x100)
        @test octint("0o177777") === UInt16(0xffff)
        @test octint("0o200000") === UInt32(0x10000)
        @test octint("0o37777777777") === UInt32(0xffffffff)
        @test octint("0o40000000000") === UInt64(0x100000000)
        @test octint("0o1777777777777777777777") === UInt64(0xffffffffffffffff)
        @test octint("0o2000000000000000000000") === UInt128(0x10000000000000000)
        @test octint("0o3777777777777777777777777777777777777777777") === UInt128(0xffffffffffffffffffffffffffffffff)
        @test (n = octint("0o4000000000000000000000000000000000000000000");
               n isa BigInt && n == 0x100000000000000000000000000000000)
    end
    @testset "OctInt string length limits for different types" begin
        @test octint("0o000")  === UInt8(0)
        @test octint("0o0000")  === UInt16(0)
        @test octint("0o000000")  === UInt16(0)
        @test octint("0o0000000")  === UInt32(0)
        @test octint("0o00000000000") === UInt32(0)
        @test octint("0o000000000000") === UInt64(0)
        @test octint("0o0000000000000000000000") === UInt64(0)
        @test octint("0o00000000000000000000000") === UInt128(0)
        @test octint("0o0000000000000000000000000000000000000000000") === UInt128(0)
        @test (n = octint("0o00000000000000000000000000000000000000000000");
               n isa BigInt && n == 0)
    end

    @testset "Underscore separators" begin
        @test julia_string_to_number("10_000",      K"Integer") === 10000
        @test julia_string_to_number("10_000.0",    K"Float")   === Float64(10000)
        @test julia_string_to_number("0xff_ff",     K"HexInt")  === 0xffff
        @test julia_string_to_number("0b1111_1111", K"BinInt")  === 0xff
        @test julia_string_to_number("0o177_777",   K"OctInt")  === 0xffff
    end

    @testset "\\minus ('\\u2212' / '−') allowed in numbers" begin
        @test julia_string_to_number("−10",        K"Integer") === -10
        @test julia_string_to_number("−10.0",      K"Float")   === Float64(-10)
        @test julia_string_to_number("10e\u22121", K"Float")   === Float64(1)
    end
end

@testset "String unescaping" begin
    unesc(str) = unescape_julia_string(str, false, false)
    # Allowed escapes of delimiters and dollar sign
    @test only(unesc("\\\\")) == '\\'
    @test only(unesc("\\\"")) == '"'
    @test only(unesc("\\\$")) == '$'
    @test only(unesc("\\'"))  == '\''
    @test only(unesc("\\`"))  == '`'

    # Newline normalization
    @test unesc("a\nb\rc\r\nd") == "a\nb\nc\nd"

    # Removal of backslash-escaped newlines & indentation
    @test unesc("a\\\nb") == "ab"
    @test unesc("a\\\rb") == "ab"
    @test unesc("a\\\r\nb") == "ab"
    @test unesc("a\\\n  b") == "ab"
    @test unesc("a\\\r\n \tb") == "ab"
    @test unesc("a\\\n") == "a"
    @test unesc("a\\\r") == "a"
    @test unesc("a\\\r\n") == "a"

    # Invalid escapes
    @test_throws ArgumentError unesc("\\.")
    @test_throws ArgumentError unesc("\\z")

    # Standard C escape sequences
    @test codeunits(unesc("\\n\\t\\r\\e\\b\\f\\v\\a")) ==
        UInt8[0x0a, 0x09, 0x0d, 0x1b, 0x08, 0x0c, 0x0b, 0x07]

    # Hex and unicode escapes; \x \u and \U
    @test unesc("x\\x61x") == "xax"
    @test unesc("x\\u03b1x") == "xαx"
    @test unesc("x\\U001F604x") == "x😄x"
    # Maximum unicode code point
    @test unesc("x\\U10ffffx") == "x\U10ffffx"
    @test_throws ArgumentError unesc("x\\U110000x")

    # variable-length octal
    @test unesc("x\\7x") == "x\ax"
    @test unesc("x\\77x") == "x?x"
    @test unesc("x\\141x") == "xax"
    @test unesc("x\\377x") == "x\xffx"
    @test_throws ArgumentError unesc("x\\400x")
end

@testset "Raw string unescaping" begin
    # " delimited
    # x\"x ==> x"x
    @test unescape_julia_string("x\\\"x",     false, true) == "x\"x"
    # x\`x ==> x\`x
    @test unescape_julia_string("x\\`x",      false, true) == "x\\`x"
    # x\\\"x ==> x\"x
    @test unescape_julia_string("x\\\\\\\"x", false, true) == "x\\\"x"
    # x\\\`x ==> x\\\`x
    @test unescape_julia_string("x\\\\\\`x",  false, true) == "x\\\\\\`x"
    # '\\ ' ==> '\\ '
    @test unescape_julia_string("\\\\ ",      false, true) == "\\\\ "

    # ` delimited
    # x\"x ==> x\"x
    @test unescape_julia_string("x\\\"x",     true, true) == "x\\\"x"
    # x\`x ==> x`x
    @test unescape_julia_string("x\\`x",      true, true)  == "x`x"
    # x\\\"x ==> x\"x
    @test unescape_julia_string("x\\\\\\\"x", true, true) == "x\\\\\\\"x"
    # x\\\`x ==> x\`x
    @test unescape_julia_string("x\\\\\\`x",  true, true) == "x\\`x"
    # '\\ ' ==> '\\ '
    @test unescape_julia_string("\\\\ ",      true, true) == "\\\\ "
end

@testset "Triple quoted string indentation" begin
    # Alias for non-raw triple str indentation
    triplestr_indent(str) = triplequoted_string_indentation(str, false)

    @test triplestr_indent([]) == 0

    # Spaces or tabs acceptable
    @test triplestr_indent(["\n  "]) == 2
    @test triplestr_indent(["\n\t "]) == 2
    @test triplestr_indent(["\n \t"]) == 2
    @test triplestr_indent(["\n\t\t"]) == 2

    # Start of the string is not indentation, as it's always preceded by a
    # delimiter in the source
    @test triplestr_indent(["  "]) == 0
    @test triplestr_indent(["  ", "  "]) == 0

    # Various newlines are allowed. empty lines are ignored
    @test triplestr_indent(["\n\n  x"]) == 2
    @test triplestr_indent(["\n\r  x"]) == 2
    @test triplestr_indent(["\r\n  x"]) == 2
    @test triplestr_indent(["\r\r  x"]) == 2
    @test triplestr_indent(["\n\r\r\n"]) == 0

    # Empty line at the end of any chunk implies the next source line started
    # with a delimiter, yielding zero indentation
    @test triplestr_indent(["  \n"]) == 0
    @test triplestr_indent(["  \r"]) == 0
    @test triplestr_indent(["  \n\n"]) == 0
    @test triplestr_indent(["  ", "  \n"]) == 0
    @test triplestr_indent(["  \n", "  "]) == 0

    # Find the minimum common prefix in one or several chunks
    @test triplestr_indent(["\n  ", "\n  "]) == 2
    @test triplestr_indent(["\n ", "\n  "]) == 1
    @test triplestr_indent(["\n  ", "\n "]) == 1
    @test triplestr_indent(["\n  ", "\n "]) == 1
    @test triplestr_indent(["\n \t", "\n  "]) == 1
    @test triplestr_indent(["\n  ", "\n \t"]) == 1
    @test triplestr_indent(["\n \t", "\n \t"]) == 2
    @test triplestr_indent(["\n\t ", "\n\t "]) == 2
    @test triplestr_indent(["\n  \n  "]) == 2
    @test triplestr_indent(["\n  \n "]) == 1
    @test triplestr_indent(["\n \n  "]) == 1
    # Increasing widths
    @test triplestr_indent(["\n\n \n  \n   "]) == 1
    # Decreasing widths
    @test triplestr_indent(["\n   \n  \n "]) == 1

    # Some cases of no indentation
    @test triplestr_indent(["hi"]) == 0
    @test triplestr_indent(["x\ny", "z"]) == 0

    # Escaped newlines
    @test triplestr_indent(["\\\n  "]) == 0
    @test triplestr_indent(["\\\r  "]) == 0
    @test triplestr_indent(["\\\r\n  "]) == 0
    @test triplestr_indent(["\\\r\n  "]) == 0
    @test triplestr_indent(["\n  \\\n "]) == 2
    @test triplestr_indent(["\n \\\n  "]) == 1

    # Raw strings don't have escaped newline processing
    @test triplequoted_string_indentation(["\n  \\\n "], true) == 1
    @test triplequoted_string_indentation(["\n \\\n  "], true) == 1
end

@testset "Triple quoted string deindentation" begin
    # Weird thing I noticed: In Julia 1.7 this @testset for loop adds an
    # absurd amount of testing latency given how trivial it is. Why? Is it
    # because of compiler heuristics which try to compile all for loops?
    @testset "Raw=$raw" for raw in (false, true)
        # Various combinations of dedent + leading newline stripping
        @test process_triple_strings!(["\n x", "\n y"], raw)        == ["x", "\ny"]
        @test process_triple_strings!(["\n\tx", "\n\ty"], raw)      == ["x", "\ny"]
        @test process_triple_strings!(["\r x", "\r y"], raw)        == ["x", "\ny"]
        @test process_triple_strings!(["\r x\r y"], raw)            == ["x\ny"]
        @test process_triple_strings!(["\r x\r\r y"], raw)          == ["x\n\ny"]
        @test process_triple_strings!(["\n \t x", "\n \t y"], raw)  == ["x", "\ny"]
        @test process_triple_strings!(["x\n\n y", "\n z"], raw)     == ["x\n\ny", "\nz"]
        # Cases of no dedent + newline normalization
        @test process_triple_strings!(["\n x", "\ny"], raw) == [" x", "\ny"]
        @test process_triple_strings!(["\nx", "\n y"], raw) == ["x", "\n y"]
        @test process_triple_strings!(["\n y\n"], raw) == [" y\n"]
        @test process_triple_strings!(["\n y\r"], raw) == [" y\n"]
    end
end

@testset "Normalization of identifiers" begin
    # NFC normalization
    # https://github.com/JuliaLang/julia/issues/5434
    # https://github.com/JuliaLang/julia/pull/19464
    @test JuliaSyntax.normalize_identifier("\u0069\u0302") == "\u00ee"

    # Special Julia normalization
    # https://github.com/JuliaLang/julia/pull/42561
    @test JuliaSyntax.normalize_identifier("julia\u025B\u00B5\u00B7\u0387\u2212") ==
        "julia\u03B5\u03BC\u22C5\u22C5\u002D"
end
