using JuliaSyntax: triplequoted_string_indentation,
                   unescape_julia_string,
                   process_triple_strings!

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
