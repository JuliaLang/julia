using JuliaSyntax:
    parse_int_literal,
    parse_uint_literal,
    parse_float_literal,
    unescape_julia_string

@testset "Float parsing" begin
    # Float64
    @test parse_float_literal(Float64, "123", 1, 4)   === (123.0, :ok)
    @test parse_float_literal(Float64, "123", 2, 4)   === (23.0,  :ok)
    @test parse_float_literal(Float64, "123", 2, 3)   === (2.0,   :ok)
    @test parse_float_literal(Float64, "1.3", 1, 4)   === (1.3,   :ok)
    @test parse_float_literal(Float64, "1.3e2", 1, 6) === (1.3e2, :ok)
    @test parse_float_literal(Float64, "1.3E2", 1, 6) === (1.3e2, :ok)
    @test parse_float_literal(Float64, "1.0e-1000", 1, 10) === (0.0, :underflow)
    @test parse_float_literal(Float64, "1.0e+1000", 1, 10) === (Inf, :overflow)
    # Slow path (exceeds static buffer size)
    @test parse_float_literal(Float64, "0.000000000000000000000000000000000000000000000000000000000001", 1, 63) === (1e-60, :ok)
    # hexfloat
    @test parse_float_literal(Float64, "0x0ap-0", 1, 8) === (Float64(10), :ok)
    @test parse_float_literal(Float64, "0xffp-0", 1, 8) === (Float64(255), :ok)

    # Float32
    @test parse_float_literal(Float32, "123", 1, 4) === (123.0f0, :ok)
    @test parse_float_literal(Float32, "1.3f2", 1, 6) === (1.3f2, :ok)
    if !Sys.iswindows()
        @test parse_float_literal(Float32, "1.0f-50", 1, 8) === (0.0f0, :underflow)
    end
    @test parse_float_literal(Float32, "1.0f+50", 1, 8) === (Inf32, :overflow)

    # Assertions
    @test_throws ErrorException parse_float_literal(Float64, "x", 1, 2)
    @test_throws ErrorException parse_float_literal(Float64, "1x", 1, 3)

    # Underscore and \minus allowed
    @test parse_float_literal(Float64, "10_000.0_0", 1, 9) === (Float64(10000), :ok)
    @test parse_float_literal(Float64, "−10.0", 1, 8)      === (Float64(-10), :ok)
    @test parse_float_literal(Float64, "10e\u22121", 1, 8) === (Float64(1), :ok)
end

hexint(s) = parse_uint_literal(s, K"HexInt")
binint(s) = parse_uint_literal(s, K"BinInt")
octint(s) = parse_uint_literal(s, K"OctInt")

@testset "Integer parsing" begin
    # Integers
    @testset "Signed Integers" begin
        @test parse_int_literal("-1") isa Int
        @test parse_int_literal("1") isa Int
        @test parse_int_literal("2147483647") isa Int
        @test parse_int_literal("9223372036854775807") isa Int64
        @test parse_int_literal("9223372036854775808") isa Int128
        @test parse_int_literal("170141183460469231731687303715884105727") isa Int128
        @test parse_int_literal("170141183460469231731687303715884105728") isa BigInt
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
               n isa BigInt && n == big"0x100000000000000000000000000000000")
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
               n isa BigInt && n == big"0x100000000000000000000000000000000")
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
               n isa BigInt && n == big"0x100000000000000000000000000000000")
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
        @test parse_int_literal("10_000") === 10000
        @test parse_uint_literal("0xff_ff",     K"HexInt")  === 0xffff
        @test parse_uint_literal("0b1111_1111", K"BinInt")  === 0xff
        @test parse_uint_literal("0o177_777",   K"OctInt")  === 0xffff
    end

    @testset "\\minus ('\\u2212' / '−') allowed in numbers" begin
        @test parse_int_literal("−10")  === -10
    end
end

function unesc(str, firstind=firstindex(str), endind=lastindex(str)+1; diagnostics=false)
    io = IOBuffer()
    ds = JuliaSyntax.Diagnostic[]
    unescape_julia_string(io, Vector{UInt8}(str), firstind, endind, ds)
    if diagnostics
        ds
    else
        @test isempty(ds)
        String(take!(io))
    end
end

@testset "String unescaping" begin
    # offsets
    @test unesc("abcd", 1, 3) == "ab"
    @test unesc("abcd", 2, 4) == "bc"
    @test unesc("abcd", 3, 5) == "cd"

    # Allowed escapes of delimiters and dollar sign
    @test unesc("\\\\") == "\\"
    @test unesc("\\\"") == "\""
    @test unesc("\\\$") == "\$"
    @test unesc("\\'")  == "\'"
    @test unesc("\\`")  == "`"

    # Newline normalization
    @test unesc("a\nb\rc\r\nd") == "a\nb\nc\nd"

    # Invalid escapes
    @test !isempty(unesc("\\.", diagnostics=true))
    @test !isempty(unesc("\\z", diagnostics=true))

    # Standard C escape sequences
    @test codeunits(unesc("\\n\\t\\r\\e\\b\\f\\v\\a")) ==
        UInt8[0x0a, 0x09, 0x0d, 0x1b, 0x08, 0x0c, 0x0b, 0x07]

    # Hex and unicode escapes; \x \u and \U
    @test unesc("x\\x61x") == "xax"
    @test unesc("x\\u03b1x") == "xαx"
    @test unesc("x\\U001F604x") == "x😄x"
    # Maximum unicode code point
    @test unesc("x\\U10ffffx") == "x\U10ffffx"
    @test !isempty(unesc("x\\U110000x", diagnostics=true))

    # variable-length octal
    @test unesc("x\\7x") == "x\ax"
    @test unesc("x\\77x") == "x?x"
    @test unesc("x\\141x") == "xax"
    @test unesc("x\\377x") == "x\xffx"
    @test !isempty(unesc("x\\400x", diagnostics=true))
end

function unesc_raw(str, is_cmd)
    io = IOBuffer()
    JuliaSyntax.unescape_raw_string(io, Vector{UInt8}(str),
                                    firstindex(str), lastindex(str)+1, is_cmd)
    return String(take!(io))
end

@testset "Raw string unescaping" begin
    # " delimited
    # x\"x ==> x"x
    @test unesc_raw("x\\\"x",     false) == "x\"x"
    # x\`x ==> x\`x
    @test unesc_raw("x\\`x",      false) == "x\\`x"
    # x\\\"x ==> x\"x
    @test unesc_raw("x\\\\\\\"x", false) == "x\\\"x"
    # x\\\`x ==> x\\\`x
    @test unesc_raw("x\\\\\\`x",  false) == "x\\\\\\`x"
    # '\\ ' ==> '\\ '
    @test unesc_raw("\\\\ ",      false) == "\\\\ "
    # '\\' ==> '\'
    @test unesc_raw("\\\\",       false) == "\\"
    # '\\\\' ==> '\\'
    @test unesc_raw("\\\\\\\\",   false) == "\\\\"

    # ` delimited
    # x\"x ==> x\"x
    @test unesc_raw("x\\\"x",     true) == "x\\\"x"
    # x\`x ==> x`x
    @test unesc_raw("x\\`x",      true)  == "x`x"
    # x\\\"x ==> x\"x
    @test unesc_raw("x\\\\\\\"x", true) == "x\\\\\\\"x"
    # x\\\`x ==> x\`x
    @test unesc_raw("x\\\\\\`x",  true) == "x\\`x"
    # '\\ ' ==> '\\ '
    @test unesc_raw("\\\\ ",      true) == "\\\\ "
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

    # https://github.com/JuliaLang/julia/issues/48870
    # ℏ -> ħ
    @test JuliaSyntax.normalize_identifier("\u210f") == "\u0127"
end
