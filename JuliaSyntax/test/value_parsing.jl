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

    # Removal of backslash-escaped newlines
    @test unesc("a\\\nb") == "ab"
    @test unesc("a\\\rb") == "ab"
    @test unesc("a\\\r\nb") == "ab"
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
    @test triplequoted_string_indentation([]) == 0

    # Spaces or tabs
    @test triplequoted_string_indentation(["  "]) == 2
    @test triplequoted_string_indentation(["\t "]) == 2
    @test triplequoted_string_indentation([" \t"]) == 2
    @test triplequoted_string_indentation(["\t\t"]) == 2

    # Various newlines; empty lines ignored
    @test triplequoted_string_indentation(["  \n\n  x"]) == 2
    @test triplequoted_string_indentation(["  \n\r  x"]) == 2
    @test triplequoted_string_indentation(["  \r\n  x"]) == 2
    @test triplequoted_string_indentation(["  \r\r  x"]) == 2
    @test triplequoted_string_indentation(["\n\r\r\n"]) == 0
    # Empty newline at the end not ignored
    @test triplequoted_string_indentation(["  \n"]) == 0
    @test triplequoted_string_indentation(["  \r"]) == 0
    @test triplequoted_string_indentation(["  \n\n"]) == 0
    @test triplequoted_string_indentation(["  ", "  \n"]) == 0

    # Finds the minimum common prefix
    @test triplequoted_string_indentation(["  ", "  "]) == 2
    @test triplequoted_string_indentation([" ", "  "]) == 1
    @test triplequoted_string_indentation(["  ", " "]) == 1
    @test triplequoted_string_indentation(["  ", " "]) == 1
    @test triplequoted_string_indentation([" \t", "  "]) == 1
    @test triplequoted_string_indentation(["  ", " \t"]) == 1
    @test triplequoted_string_indentation([" \t", " \t"]) == 2
    @test triplequoted_string_indentation(["\t ", "\t "]) == 2
    @test triplequoted_string_indentation(["  \n  "]) == 2
    @test triplequoted_string_indentation(["  \n "]) == 1
    @test triplequoted_string_indentation([" \n  "]) == 1
    @test triplequoted_string_indentation(["\n \n  \n   "]) == 1
    @test triplequoted_string_indentation(["   \n  \n "]) == 1

    # Cases of no indentation
    @test triplequoted_string_indentation(["hi"]) == 0
    @test triplequoted_string_indentation(["x\ny", "z"]) == 0
end

@testset "Triple quoted string deindentation" begin
    @test process_triple_strings!([" x", " y"], false)   == ["x", "y"]
    @test process_triple_strings!([" x", "y"], false)    == [" x", "y"]
    @test process_triple_strings!(["\n x", " y"], false) == ["x", "y"]
    @test process_triple_strings!([" x", " y\n"], false) == [" x", " y\n"]
    @test process_triple_strings!([" \tx", " \ty"], false) == ["x", "y"]
    @test process_triple_strings!([" \tx", "  y"], false)  == ["\tx", " y"]
end

