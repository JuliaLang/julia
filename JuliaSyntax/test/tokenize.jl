# Hack: Introduce a module here to isolate some Tokenize internals from JuliaSyntax
module TokenizeTests

using Test

using JuliaSyntax:
    JuliaSyntax,
    @K_str,
    Kind,
    kind,
    is_error,
    is_operator

using JuliaSyntax.Tokenize:
    Tokenize,
    tokenize,
    untokenize,
    RawToken

using ..Main: toks

tok(str, i = 1) = collect(tokenize(str))[i]

strtok(str) = untokenize.(collect(tokenize(str)), str)

function onlytok(str)
    ts = collect(tokenize(str))
    (length(ts) == 2 && ts[2].kind == K"EndMarker") ||
        error("Expected one token got $(length(ts)-1)")
    return ts[1].kind
end

@testset "tokens" begin
    for s in ["a", IOBuffer("a")]
        l = tokenize(s)
        @test Tokenize.readchar(l) == 'a'

        l_old = l
        @test l == l_old
        @test Tokenize.eof(l)
        @test Tokenize.readchar(l) == Tokenize.EOF_CHAR

    end
end # testset

@testset "tokenize unicode" begin
    # FIXME: rm VERSION check once we implement our own is_identifier_char
    emoji = VERSION < v"1.5" ? "😄" : "\U1F3F3\UFE0F\U200D\U1F308" # 🏳️‍🌈 requires newer Unicode
    str = "𝘋 =2"*emoji
    for s in [str, IOBuffer(str)]
        l = tokenize(s)
        kinds = [K"Identifier", K"Whitespace", K"=",
                 K"Integer", K"Identifier", K"EndMarker"]
        token_strs = ["𝘋", " ", "=", "2", emoji, ""]
        for (i, n) in enumerate(l)
            @test kind(n) == kinds[i]
            @test untokenize(n, str)  == token_strs[i]
        end
    end
end # testset

@testset "tokenize complex piece of code" begin

    str = """
    function foo!{T<:Bar}(x::{T}=12)
        @time (x+x, x+x);
    end
    try
        foo
    catch
        bar
    end
    @time x+x
    y[[1 2 3]]
    [1*2,2;3,4]
    "string"; 'c'
    (a&&b)||(a||b)
    # comment
    #= comment
    is done here =#
    2%5
    a'/b'
    a.'\\b.'
    `command`
    12_sin(12)
    {}
    '
    """

    # Generate the following with
    # ```
    # for t in kind.(collect(tokenize(str)))
    #    print(kind(t), ",")
    # end
    # ```
    # and *check* it afterwards.

    kinds = [K"function",K"Whitespace",K"Identifier",K"{",K"Identifier",
            K"<:",K"Identifier",K"}",K"(",K"Identifier",K"::",
            K"{",K"Identifier",K"}",K"=",K"Integer",K")",

            K"NewlineWs",K"@",K"Identifier",K"Whitespace",K"(",
            K"Identifier",K"+",K"Identifier",K",",K"Whitespace",
            K"Identifier",K"+",K"Identifier",K")",K";",

            K"NewlineWs",K"end",

            K"NewlineWs",K"try",
            K"NewlineWs",K"Identifier",
            K"NewlineWs",K"catch",
            K"NewlineWs",K"Identifier",
            K"NewlineWs",K"end",

            K"NewlineWs",K"@",K"Identifier",K"Whitespace",K"Identifier",
            K"+",K"Identifier",

            K"NewlineWs",K"Identifier",K"[",K"[",K"Integer",K"Whitespace",
            K"Integer",K"Whitespace",K"Integer",K"]",K"]",

            K"NewlineWs",K"[",K"Integer",K"*",K"Integer",K",",K"Integer",
            K";",K"Integer",K",",K"Integer",K"]",

            K"NewlineWs",K"\"",K"String",K"\"",K";",K"Whitespace",K"'",K"Char",K"'",

            K"NewlineWs",K"(",K"Identifier",K"&&",K"Identifier",K")",K"||",
            K"(",K"Identifier",K"||",K"Identifier",K")",

            K"NewlineWs",K"Comment",

            K"NewlineWs",K"Comment",

            K"NewlineWs",K"Integer",K"%",K"Integer",

            K"NewlineWs",K"Identifier",K"'",K"/",K"Identifier",K"'",

            K"NewlineWs",K"Identifier",K".",K"'",K"\\",K"Identifier",K".",K"'",

            K"NewlineWs",K"`",K"CmdString",K"`",

            K"NewlineWs",K"Integer",K"Identifier",K"(",K"Integer",K")",

            K"NewlineWs",K"{",K"}",

            K"NewlineWs",K"'",K"Char",K"EndMarker"]

    for (i, n) in enumerate(tokenize(str))
        @test kind(n) == kinds[i]
    end

    @testset "roundtrippability" begin
        @test join(untokenize.(collect(tokenize(str)), str)) == str
    end

    @test all((t.endbyte - t.startbyte + 1)==sizeof(untokenize(t, str)) for t in tokenize(str))
end # testset

@testset "issue 5, '..'" begin
    @test kind.(collect(tokenize("1.23..3.21"))) == [K"Float",K"..",K"Float",K"EndMarker"]
end

@testset "issue 17, >>" begin
    str = ">> "
    @test untokenize(tok(str), str)==">>"
end


@testset "test added operators" begin
    @test tok("1+=2",  2).kind == K"+="
    @test tok("1-=2",  2).kind == K"-="
    @test tok("1:=2",  2).kind == K":="
    @test tok("1*=2",  2).kind == K"*="
    @test tok("1^=2",  2).kind == K"^="
    @test tok("1÷=2",  2).kind == K"÷="
    @test tok("1\\=2", 2).kind == K"\="
    @test tok("1\$=2", 2).kind == K"$="
    @test tok("1-->2", 2).kind == K"-->"
    @test tok("1<--2", 2).kind == K"<--"
    @test tok("1<-->2", 2).kind == K"<-->"
    @test tok("1>:2",  2).kind == K">:"
end

@testset "infix" begin
    @test tok("1 in 2",  3).kind == K"in"
    @test tok("1 in[1]", 3).kind == K"in"

    @test tok("1 isa 2",  3).kind == K"isa"
    @test tok("1 isa[2]", 3).kind == K"isa"
end

@testset "tokenizing true/false literals" begin
    @test tok("somtext true", 3).kind == K"true"
    @test tok("somtext false", 3).kind == K"false"
    @test tok("somtext tr", 3).kind == K"Identifier"
    @test tok("somtext falsething", 3).kind == K"Identifier"
end


roundtrip(str) = join(untokenize.(collect(tokenize(str)), str))

@testset "lexing anon functions '->' " begin
    @test tok("a->b", 2).kind==K"->"
end

@testset "comments" begin
    ts = collect(tokenize("""
       #
       \"\"\"
       f
       \"\"\"
       1
       """))

    kinds = [K"Comment", K"NewlineWs",
             K"\"\"\"", K"String", K"String", K"\"\"\"", K"NewlineWs",
             K"Integer", K"NewlineWs",
             K"EndMarker"]
    @test kind.(ts) == kinds

    @test toks("#=# text=#") == ["#=# text=#"=>K"Comment"]

    @test toks("#=   #=   =#") == ["#=   #=   =#"=>K"ErrorEofMultiComment"]
    @test toks("#=#==#=#") == ["#=#==#=#"=>K"Comment"]
    @test toks("#=#==#=")  == ["#=#==#="=>K"ErrorEofMultiComment"]
end


@testset "invalid UTF-8" begin
    @test toks("#=\xf5b\n=#") == [
        "#=\xf5b\n=#" => K"ErrorInvalidUTF8",
    ]
    @test toks("#\xf5b\n") == [
        "#\xf5b" => K"ErrorInvalidUTF8",
        "\n" => K"NewlineWs"
    ]
    @test toks("\"\xf5\"") == [
        "\""   => K"\""
        "\xf5" => K"ErrorInvalidUTF8"
        "\""   => K"\""
    ]
    @test toks("'\xf5'") == [
        "'"    => K"'"
        "\xf5" => K"ErrorInvalidUTF8"
        "'"    => K"'"
    ]
    @test toks("`\xf5`") == [
        "`"    => K"`"
        "\xf5" => K"ErrorInvalidUTF8"
        "`"    => K"`"
    ]
end

@testset "primes" begin
    str = """
    ImageMagick.save(fn, reinterpret(ARGB32, [0xf0884422]''))
    D = ImageMagick.load(fn)
    """
    tokens = collect(tokenize(str))
    @test string(untokenize(tokens[16], str)) == string(untokenize(tokens[17], str))=="'"

    @test roundtrip("'a'") == "'a'"
    @test kind.(collect(tokenize("'a'"))) == [K"'", K"Char", K"'", K"EndMarker"]

    # ' is not an operator here, so doesn't consume the suffix ᵀ
    @test roundtrip("'ᵀ'") == "'ᵀ'"
    @test kind.(collect(tokenize("'₁'"))) == [K"'", K"Char", K"'", K"EndMarker"]

    @test roundtrip("''") == "''"
    @test kind.(collect(tokenize("''"))) == [K"'", K"'", K"EndMarker"]

    @test roundtrip("'''") == "'''"
    @test kind.(collect(tokenize("'''"))) == [K"'", K"Char", K"'", K"EndMarker"]

    @test roundtrip("''''") == "''''"
    @test kind.(collect(tokenize("''''"))) == [K"'", K"Char", K"'", K"'", K"EndMarker"]

    @test tok("()'", 3).kind == K"'"
    @test tok("{}'", 3).kind == K"'"
    @test tok("[]'", 3).kind == K"'"
    @test tok("outer'", 2).kind == K"'"
    @test tok("mutable'", 2).kind == K"'"
    @test tok("as'", 2).kind == K"'"
    @test tok("isa'", 2).kind == K"'"
    @test untokenize.(collect(tokenize("a'ᵀ")), "a'ᵀ") == ["a", "'ᵀ", ""]
end

@testset "keywords" begin
      for kw in    ["baremodule",
                    "begin",
                    "break",
                    "catch",
                    "const",
                    "continue",
                    "do",
                    "else",
                    "elseif",
                    "end",
                    "export",
                    "finally",
                    "for",
                    "function",
                    "global",
                    "if",
                    "import",
                    "let",
                    "local",
                    "macro",
                    "module",
                    "quote",
                    "return",
                    "struct",
                    "try",
                    "using",
                    "while",

                    "abstract",
                    "as",
                    "doc",
                    "mutable",
                    "outer",
                    "primitive",
                    "type",
                    "var"]

        @test kind(tok(kw)) == convert(Kind, kw)
    end
end

@testset "issue in PR #45" begin
    @test length(collect(tokenize("x)"))) == 3
end

@testset "xor_eq" begin
    @test tok("1 ⊻= 2", 3).kind==K"⊻="
end

@testset "lex binary" begin
    @test tok("0b0101").kind==K"BinInt"
end

@testset "show" begin
    io = IOBuffer()
    show(io, collect(tokenize("\"abc\nd\"ef"))[2])
    @test String(take!(io)) == "1-5        String         "
end

~(tok::RawToken, t::Tuple) = tok.kind == t[1] && untokenize(tok, t[3]) == t[2]

@testset "raw strings" begin
    str = raw""" str"x $ \ y" """
    ts = collect(tokenize(str))
    @test ts[1] ~ (K"Whitespace" , " "        , str)
    @test ts[2] ~ (K"Identifier" , "str"      , str)
    @test ts[3] ~ (K"\""         , "\""       , str)
    @test ts[4] ~ (K"String"     , "x \$ \\ y", str)
    @test ts[5] ~ (K"\""         , "\""       , str)
    @test ts[6] ~ (K"Whitespace" , " "        , str)
    @test ts[7] ~ (K"EndMarker"  , ""         , str)

    str = raw"""`x $ \ y`"""
    ts = collect(tokenize(str))
    @test ts[1] ~ (K"`"         , "`"         , str)
    @test ts[2] ~ (K"CmdString" , "x \$ \\ y" , str)
    @test ts[3] ~ (K"`"         , "`"         , str)
    @test ts[4] ~ (K"EndMarker" , ""          , str)

    # str"\\"
    str = "str\"\\\\\""
    ts = collect(tokenize(str))
    @test ts[1] ~ (K"Identifier" , "str"  , str)
    @test ts[2] ~ (K"\""         , "\""   , str)
    @test ts[3] ~ (K"String"     , "\\\\" , str)
    @test ts[4] ~ (K"\""         , "\""   , str)
    @test ts[5] ~ (K"EndMarker"  , ""     , str)

    # str"\\\""
    str = "str\"\\\\\\\"\""
    ts = collect(tokenize(str))
    @test ts[1] ~ (K"Identifier" , "str"      , str)
    @test ts[2] ~ (K"\""         , "\""       , str)
    @test ts[3] ~ (K"String"     , "\\\\\\\"" , str)
    @test ts[4] ~ (K"\""         , "\""       , str)
    @test ts[5] ~ (K"EndMarker"  , ""         , str)

    # Contextual keywords and operators allowed as raw string prefixes
    str = raw""" var"x $ \ y" """
    ts = collect(tokenize(str))
    @test ts[2] ~ (K"var"        , "var", str)
    @test ts[4] ~ (K"String"     , "x \$ \\ y", str)

    str = raw""" outer"x $ \ y" """
    ts = collect(tokenize(str))
    @test ts[2] ~ (K"outer"      , "outer", str)
    @test ts[4] ~ (K"String"     , "x \$ \\ y", str)

    str = raw""" isa"x $ \ y" """
    ts = collect(tokenize(str))
    @test ts[2] ~ (K"isa"        , "isa", str)
    @test ts[4] ~ (K"String"     , "x \$ \\ y", str)
end

@testset "string escaped newline whitespace" begin
    str = "\"x\\\n \ty\""
    ts = collect(tokenize(str))
    @test ts[1] ~ (K"\"", "\"", str)
    @test ts[2] ~ (K"String", "x", str)
    @test ts[3] ~ (K"Whitespace", "\\\n \t", str)
    @test ts[4] ~ (K"String", "y", str)
    @test ts[5] ~ (K"\"", "\"", str)

    # No newline escape for raw strings
    str = "r\"x\\\ny\""
    ts = collect(tokenize(str))
    @test ts[1] ~ (K"Identifier", "r", str)
    @test ts[2] ~ (K"\"", "\"", str)
    @test ts[3] ~ (K"String", "x\\\ny", str)
    @test ts[4] ~ (K"\"", "\"", str)
end

@testset "triple quoted string line splitting" begin
    str = "\"\"\"\nx\r\ny\rz\n\r\"\"\""
    ts = collect(tokenize(str))
    @test ts[1] ~ (K"\"\"\"" , "\"\"\"", str)
    @test ts[2] ~ (K"String" , "\n", str)
    @test ts[3] ~ (K"String" , "x\r\n", str)
    @test ts[4] ~ (K"String" , "y\r", str)
    @test ts[5] ~ (K"String" , "z\n", str)
    @test ts[6] ~ (K"String" , "\r", str)
    @test ts[7] ~ (K"\"\"\"" , "\"\"\"", str)

    # Also for raw strings
    str = "r\"\"\"\nx\ny\"\"\""
    ts = collect(tokenize(str))
    @test ts[1] ~ (K"Identifier" , "r", str)
    @test ts[2] ~ (K"\"\"\""     , "\"\"\"", str)
    @test ts[3] ~ (K"String"     , "\n", str)
    @test ts[4] ~ (K"String"     , "x\n", str)
    @test ts[5] ~ (K"String"     , "y", str)
    @test ts[6] ~ (K"\"\"\""     , "\"\"\"", str)
end

@testset "interpolation" begin
    @testset "basic" begin
        str = "\"\$x \$y\""
        ts = collect(tokenize(str))
        @test ts[1]  ~ (K"\""         , "\"", str)
        @test ts[2]  ~ (K"$"          , "\$", str)
        @test ts[3]  ~ (K"Identifier" , "x" , str)
        @test ts[4]  ~ (K"String"     , " " , str)
        @test ts[5]  ~ (K"$"          , "\$", str)
        @test ts[6]  ~ (K"Identifier" , "y" , str)
        @test ts[7]  ~ (K"\""         , "\"", str)
        @test ts[8]  ~ (K"EndMarker"  , ""  , str)
    end

    @testset "nested" begin
        str = """"str: \$(g("str: \$(h("str"))"))" """
        ts = collect(tokenize(str))
        @test length(ts) == 23
        @test ts[1]  ~ (K"\""        , "\""   , str)
        @test ts[2]  ~ (K"String"    , "str: ", str)
        @test ts[3]  ~ (K"$"         , "\$"   , str)
        @test ts[4]  ~ (K"("         , "("    , str)
        @test ts[5]  ~ (K"Identifier", "g"    , str)
        @test ts[6]  ~ (K"("         , "("    , str)
        @test ts[7]  ~ (K"\""        , "\""   , str)
        @test ts[8]  ~ (K"String"    , "str: ", str)
        @test ts[9]  ~ (K"$"         , "\$"   , str)
        @test ts[10] ~ (K"("         , "("    , str)
        @test ts[11] ~ (K"Identifier", "h"    , str)
        @test ts[12] ~ (K"("         , "("    , str)
        @test ts[13] ~ (K"\""        , "\""   , str)
        @test ts[14] ~ (K"String"    , "str"  , str)
        @test ts[15] ~ (K"\""        , "\""   , str)
        @test ts[16] ~ (K")"         , ")"    , str)
        @test ts[17] ~ (K")"         , ")"    , str)
        @test ts[18] ~ (K"\""        , "\""   , str)
        @test ts[19] ~ (K")"         , ")"    , str)
        @test ts[20] ~ (K")"         , ")"    , str)
        @test ts[21] ~ (K"\""        , "\""   , str)
        @test ts[22] ~ (K"Whitespace", " "    , str)
        @test ts[23] ~ (K"EndMarker" , ""     , str)
    end

    @testset "duplicate \$" begin
        str = "\"\$\$\""
        ts = collect(tokenize(str))
        @test ts[1]  ~ (K"\""        , "\"", str)
        @test ts[2]  ~ (K"$"         , "\$", str)
        @test ts[3]  ~ (K"$"         , "\$", str)
        @test ts[4]  ~ (K"\""        , "\"", str)
        @test ts[5]  ~ (K"EndMarker" , ""  , str)
    end

    @testset "Unmatched parens" begin
        # issue 73: https://github.com/JuliaLang/Tokenize.jl/issues/73
        str = "\"\$(fdsf\""
        ts = collect(tokenize(str))
        @test ts[1] ~ (K"\""         , "\""   , str)
        @test ts[2] ~ (K"$"          , "\$"   , str)
        @test ts[3] ~ (K"("          , "("    , str)
        @test ts[4] ~ (K"Identifier" , "fdsf" , str)
        @test ts[5] ~ (K"\""         , "\""   , str)
        @test ts[6] ~ (K"EndMarker"  , ""     , str)
    end

    @testset "Unicode" begin
        # issue 178: https://github.com/JuliaLang/Tokenize.jl/issues/178
        str = """ "\$uₕx \$(uₕx - ux)" """
        ts = collect(tokenize(str))
        @test ts[ 1] ~ (K"Whitespace" , " "   , str)
        @test ts[ 2] ~ (K"\""         , "\""  , str)
        @test ts[ 3] ~ (K"$"          , "\$"  , str)
        @test ts[ 4] ~ (K"Identifier" , "uₕx" , str)
        @test ts[ 5] ~ (K"String"     , " "   , str)
        @test ts[ 6] ~ (K"$"          , "\$"  , str)
        @test ts[ 7] ~ (K"("          , "("   , str)
        @test ts[ 8] ~ (K"Identifier" , "uₕx" , str)
        @test ts[ 9] ~ (K"Whitespace" , " "   , str)
        @test ts[10] ~ (K"-"          , "-"   , str)
        @test ts[11] ~ (K"Whitespace" , " "   , str)
        @test ts[12] ~ (K"Identifier" , "ux"  , str)
        @test ts[13] ~ (K")"          , ")"   , str)
        @test ts[14] ~ (K"\""         , "\""  , str)
        @test ts[15] ~ (K"Whitespace" , " "   , str)
        @test ts[16] ~ (K"EndMarker"  , ""    , str)
    end

    @testset "var\"...\" disabled in interpolations" begin
        str = """ "\$var"x" " """
        ts = collect(tokenize(str))
        @test ts[ 1] ~ (K"Whitespace" , " "   , str)
        @test ts[ 2] ~ (K"\""         , "\""  , str)
        @test ts[ 3] ~ (K"$"          , "\$"  , str)
        @test ts[ 4] ~ (K"var"        , "var" , str)
        @test ts[ 5] ~ (K"\""         , "\""  , str)
        @test ts[ 6] ~ (K"Identifier" , "x"   , str)
        @test ts[ 7] ~ (K"\""         , "\""  , str)
        @test ts[ 8] ~ (K"String"     , " "   , str)
        @test ts[ 9] ~ (K"\""         , "\""  , str)
        @test ts[10] ~ (K"Whitespace" , " "   , str)
        @test ts[11] ~ (K"EndMarker"  , ""    , str)
    end

    @testset "chars after interpolation identifier" begin
        # Operators allowed
        @test toks("\"\$x?\"") == [
            "\""=>K"\""
            "\$"=>K"$"
            "x"=>K"Identifier"
            "?"=>K"String"
            "\""=>K"\""
        ]
        @test toks("\"\$x⫪\"") == [
            "\""=>K"\""
            "\$"=>K"$"
            "x"=>K"Identifier"
            "⫪"=>K"String"
            "\""=>K"\""
        ]
        # Some chars disallowed (eg, U+0DF4)
        @test toks("\"\$x෴\"") == [
            "\""=>K"\""
            "\$"=>K"$"
            "x"=>K"Identifier"
            "෴"=>K"ErrorInvalidInterpolationTerminator"
            "\""=>K"\""
        ]
    end
end

@testset "inferred" begin
    l = tokenize("abc")
    @inferred Tokenize.next_token(l)
end

@testset "modifying function names (!) followed by operator" begin
    @test toks("a!=b") == ["a"=>K"Identifier", "!="=>K"!=", "b"=>K"Identifier"]
    @test toks("a!!=b") == ["a!"=>K"Identifier", "!="=>K"!=", "b"=>K"Identifier"]
    @test toks("!=b") == ["!="=>K"!=", "b"=>K"Identifier"]
end

@testset "integer literals" begin
    @test onlytok("1234")  == K"Integer"
    @test onlytok("12_34") == K"Integer"

    @test toks("1234_") == ["1234"=>K"Integer", "_"=>K"Identifier"]
    @test toks("1234x") == ["1234"=>K"Integer", "x"=>K"Identifier"]

    @test onlytok("_1234") == K"Identifier"

    @test toks("1__2") == ["1"=>K"Integer", "__2"=>K"Identifier"]
end

@testset "hex integer literals" begin
    @test onlytok("0x0167_032") == K"HexInt"
    @test onlytok("0x2_0_2")    == K"HexInt"
    # trailing junk
    # https://github.com/JuliaLang/julia/issues/16356
    @test onlytok("0xenomorph") == K"ErrorInvalidNumericConstant"
    @test onlytok("0xaα")    == K"ErrorInvalidNumericConstant"
    @test toks("0x ") == ["0x"=>K"ErrorInvalidNumericConstant", " "=>K"Whitespace"]
    @test onlytok("0x") == K"ErrorInvalidNumericConstant"
    @test onlytok("0xg") == K"ErrorInvalidNumericConstant"
    @test onlytok("0x_") == K"ErrorInvalidNumericConstant"
    @test toks("0x-") == ["0x"=>K"ErrorInvalidNumericConstant", "-"=>K"-"]
end

@testset "hexfloat literals" begin
    @test onlytok("0x.1p1")    == K"Float"
    @test onlytok("0x00p2")    == K"Float"
    @test onlytok("0x00P2")    == K"Float"
    @test onlytok("0x0.00p23") == K"Float"
    @test onlytok("0x0.0ap23") == K"Float"
    @test onlytok("0x0.0_0p2") == K"Float"
    @test onlytok("0x0_0_0.0_0p2") == K"Float"
    @test onlytok("0x0p+2")    == K"Float"
    @test onlytok("0x0p-2")    == K"Float"
    # errors
    @test onlytok("0x") == K"ErrorInvalidNumericConstant"
    @test onlytok("0x2__2") == K"ErrorInvalidNumericConstant"
    @test onlytok("0x1p") == K"ErrorInvalidNumericConstant"
    @test onlytok("0x.p0") == K"ErrorInvalidNumericConstant"
    @test onlytok("0x.")   == K"ErrorHexFloatMustContainP"
    @test onlytok("0x1.0") == K"ErrorHexFloatMustContainP"
end

@testset "binary literals" begin
    @test onlytok("0b0101001_0100_0101")  == K"BinInt"

    @test onlytok("0b") == K"ErrorInvalidNumericConstant"
    @test toks("0b ") == ["0b"=>K"ErrorInvalidNumericConstant", " "=>K"Whitespace"]
    @test onlytok("0b101__101") == K"ErrorInvalidNumericConstant"
    @test onlytok("0b123") == K"ErrorInvalidNumericConstant"
end

@testset "octal literals" begin
    @test onlytok("0o0167") == K"OctInt"
    @test onlytok("0o01054001_0100_0101") == K"OctInt"

    @test onlytok("0o") == K"ErrorInvalidNumericConstant"
    @test onlytok("0o78p") == K"ErrorInvalidNumericConstant"
    @test toks("0o ") == ["0o"=>K"ErrorInvalidNumericConstant", " "=>K"Whitespace"]
end

@testset "float literals" begin
    @test onlytok("1.0") == K"Float"

    @test onlytok("1.0e0")  == K"Float"
    @test onlytok("1.0e-0") == K"Float"
    @test onlytok("1.0E0")  == K"Float"
    @test onlytok("1.0E-0") == K"Float"
    @test onlytok("1.0f0")  == K"Float32"
    @test onlytok("1.0f-0") == K"Float32"
    @test onlytok("1.e0")  == K"Float"
    @test onlytok("1.f0")  == K"Float32"

    @test onlytok("0e0")    == K"Float"
    @test onlytok("0e+0")   == K"Float"
    @test onlytok("0E0")    == K"Float"
    @test onlytok("201E+0") == K"Float"
    @test onlytok("2f+0")   == K"Float32"
    @test onlytok("2048f0") == K"Float32"

    # underscores
    @test onlytok("1_1.11")  == K"Float"
    @test onlytok("11.1_1")  == K"Float"
    @test onlytok("1_1.1_1") == K"Float"
    @test onlytok("1.2_3")   == K"Float"
    @test onlytok("3_2.5_2") == K"Float"
    @test toks("_1.1_1") == ["_1"=>K"Identifier", ".1_1"=>K"Float"]

    # juxtapositions with identifiers
    @test toks("3e2_2") == ["3e2"=>K"Float", "_2"=>K"Identifier"]
    @test toks("1e") == ["1"=>K"Integer", "e"=>K"Identifier"]

    # Floating point with \minus rather than -
    @test onlytok("1.0e−0") == K"Float"
    @test onlytok("1.0f−0") == K"Float32"
    @test onlytok("0x0p−2") == K"Float"

    # Errors
    @test onlytok("1._")   == K"ErrorInvalidNumericConstant"
    @test onlytok("1.1.")  == K"ErrorInvalidNumericConstant"
    @test onlytok("1e+")   == K"ErrorInvalidNumericConstant"
    @test onlytok("1.0e+") == K"ErrorInvalidNumericConstant"
    @test onlytok("1.e1.") == K"ErrorInvalidNumericConstant"
    @test onlytok("1e1.")  == K"ErrorInvalidNumericConstant"
    @test toks("1.e")   == ["1."=>K"ErrorAmbiguousNumericDotMultiply", "e"=>K"Identifier"]
    @test toks("3.2e2.2") == ["3.2e2."=>K"ErrorInvalidNumericConstant", "2"=>K"Integer"]
    @test toks("3e2.2") == ["3e2."=>K"ErrorInvalidNumericConstant", "2"=>K"Integer"]
    @test toks("1.2.f") == ["1.2."=>K"ErrorInvalidNumericConstant", "f"=>K"Identifier"]
end

@testset "numbers with trailing `.` " begin
    @test toks("1.")  == ["1."=>K"Float"]

    @test toks("1.)") == ["1."=>K"Float", ")"=>K")"]
    @test toks("1.]") == ["1."=>K"Float", "]"=>K"]"]
    @test toks("1.}") == ["1."=>K"Float", "}"=>K"}"]
    @test toks("1.,") == ["1."=>K"Float", ","=>K","]
    @test toks("1.;") == ["1."=>K"Float", ";"=>K";"]
    @test toks("1.#") == ["1."=>K"Float", "#"=>K"Comment"]

    # ellipses
    @test toks("1..")    == ["1"=>K"Integer",   ".."=>K".."]
    @test toks("1...")   == ["1"=>K"Integer",  "..."=>K"..."]
    @test toks(".1..")   == [".1"=>K"Float",    ".."=>K".."]
    @test toks("0x01..") == ["0x01"=>K"HexInt", ".."=>K".."]

    # Dotted operators and other dotted sufficies
    @test toks("1234 .+1") == ["1234"=>K"Integer", " "=>K"Whitespace", ".+"=>K"+", "1"=>K"Integer"]
    @test toks("1234.0+1") == ["1234.0"=>K"Float", "+"=>K"+", "1"=>K"Integer"]
    @test toks("1234.0 .+1") == ["1234.0"=>K"Float", " "=>K"Whitespace", ".+"=>K"+", "1"=>K"Integer"]
    @test toks("1234 .f(a)") == ["1234"=>K"Integer", " "=>K"Whitespace", "."=>K".",
                                 "f"=>K"Identifier", "("=>K"(", "a"=>K"Identifier", ")"=>K")"]
    @test toks("1234.0 .f(a)") == ["1234.0"=>K"Float", " "=>K"Whitespace", "."=>K".",
                                   "f"=>K"Identifier", "("=>K"(", "a"=>K"Identifier", ")"=>K")"]
    @test toks("1f0./1") == ["1f0"=>K"Float32", "./"=>K"/", "1"=>K"Integer"]

    # Dotted operators after numeric constants are ok
    @test toks("1e1.⫪")  == ["1e1"=>K"Float", ".⫪"=>K"⫪"]
    @test toks("1.1.⫪")  == ["1.1"=>K"Float", ".⫪"=>K"⫪"]
    @test toks("1e1.−")  == ["1e1"=>K"Float", ".−"=>K"-"]
    @test toks("1.1.−")  == ["1.1"=>K"Float", ".−"=>K"-"]
    # Non-dottable operators are not ok
    @test toks("1e1.\$")  == ["1e1."=>K"ErrorInvalidNumericConstant", "\$"=>K"$"]
    @test toks("1.1.\$")  == ["1.1."=>K"ErrorInvalidNumericConstant", "\$"=>K"$"]

    # Ambiguous dotted operators
    @test toks("1.+") == ["1."=>K"ErrorAmbiguousNumericConstant", "+"=>K"+"]
    @test toks("1.+ ") == ["1."=>K"ErrorAmbiguousNumericConstant", "+"=>K"+", " "=>K"Whitespace"]
    @test toks("1.⤋")  == ["1."=>K"ErrorAmbiguousNumericConstant", "⤋"=>K"⤋"]
    @test toks("1.⫪")  == ["1."=>K"ErrorAmbiguousNumericConstant", "⫪"=>K"⫪"]
    # non-dottable ops are the exception
    @test toks("1.:")  == ["1."=>K"Float", ":"=>K":"]
    @test toks("1.\$") == ["1."=>K"Float", "\$"=>K"$"]

    # Ambiguous - literal vs multiply by juxtaposition
    @test toks("1.x")  == ["1."=>K"ErrorAmbiguousNumericDotMultiply", "x"=>K"Identifier"]
    @test toks("1.(")  == ["1."=>K"ErrorAmbiguousNumericDotMultiply", "("=>K"("]
    @test toks("1.[")  == ["1."=>K"ErrorAmbiguousNumericDotMultiply", "["=>K"["]
    @test toks("1.{")  == ["1."=>K"ErrorAmbiguousNumericDotMultiply", "{"=>K"{"]
    @test toks("1.@")  == ["1."=>K"ErrorAmbiguousNumericDotMultiply", "@"=>K"@"]
    @test toks("1.\"") == ["1."=>K"ErrorAmbiguousNumericDotMultiply", "\""=>K"\""]
end

@testset "julia 0.6 types" begin
    @test onlytok("mutable")   == K"mutable"
    @test onlytok("primitive") == K"primitive"
    @test onlytok("struct")    == K"struct"
    @test onlytok("where")     == K"where"

    @test tok("mutable struct s{T} where T",  1).kind == K"mutable"
    @test tok("mutable struct s{T} where T",  3).kind == K"struct"
    @test tok("mutable struct s{T} where T", 10).kind == K"where"
end

@testset "CMDs" begin
    @test tok("`cmd`",1).kind == K"`"
    @test tok("`cmd`",2).kind == K"CmdString"
    @test tok("`cmd`",3).kind == K"`"
    @test tok("`cmd`",4).kind == K"EndMarker"
    @test tok("```cmd```", 1).kind == K"```"
    @test tok("```cmd```", 2).kind == K"CmdString"
    @test tok("```cmd```", 3).kind == K"```"
    @test tok("```cmd```", 4).kind == K"EndMarker"
    @test tok("```cmd````cmd`", 1).kind == K"```"
    @test tok("```cmd````cmd`", 2).kind == K"CmdString"
    @test tok("```cmd````cmd`", 3).kind == K"```"
    @test tok("```cmd````cmd`", 4).kind == K"`"
    @test tok("```cmd````cmd`", 5).kind == K"CmdString"
    @test tok("```cmd````cmd`", 6).kind == K"`"
    @test tok("```cmd````cmd`", 7).kind == K"EndMarker"
end

@testset "where" begin
    @test tok("a where b", 3).kind == K"where"
end

@testset "IO position" begin
    io = IOBuffer("#1+1")
    skip(io, 1)
    @test length(collect(tokenize(io))) == 4
end

@testset "dotted and suffixed operators" begin

for opkind in Tokenize._nondot_symbolic_operator_kinds()
    op = string(opkind)
    strs = [
        1 => [ # unary
            "$(op)b",
            ".$(op)b",
        ],
        2 => [ # binary
            "a $op b",
            "a .$op b",
            "a $(op)₁ b",
            "a $(op)\U0304 b",
            "a .$(op)₁ b"
        ]
    ]

    for (arity, container) in strs
        for str in container
            expr = JuliaSyntax.fl_parse(str, raise = false)
            if VERSION < v"1.7" && str == "a .&& b"
                expr = Expr(Symbol(".&&"), :a, :b)
            end
            if expr isa Expr && (expr.head != :error && expr.head != :incomplete)
                tokens = collect(tokenize(str))
                exop = expr.head == :call ? expr.args[1] : expr.head
                #println(str)
                @test Symbol(Tokenize.untokenize(tokens[arity == 1 ? 1 : 3], str)) == exop
            else
                break
            end
        end
    end
end
end

@testset "Normalization of Unicode symbols" begin
    # https://github.com/JuliaLang/julia/pull/25157
    @test tok("\u00b7").kind == K"⋅"
    @test tok("\u0387").kind == K"⋅"
    @test tok(".\u00b7").dotop
    @test tok(".\u0387").dotop

    # https://github.com/JuliaLang/julia/pull/40948
    @test tok("−").kind == K"-"
    @test tok("−=").kind == K"-="
    @test tok(".−").dotop
end

@testset "perp" begin
    @test tok("1 ⟂ 2", 3).kind==K"⟂"
end

@testset "outer" begin
    @test tok("outer", 1).kind==K"outer"
end

@testset "invalid operator errors" begin
    @test toks("--")      == ["--"=>K"ErrorInvalidOperator"]
    @test toks("1**2") == ["1"=>K"Integer", "**"=>K"Error**", "2"=>K"Integer"]
    @test toks("a<---b") == ["a"=>K"Identifier", "<---"=>K"ErrorInvalidOperator", "b"=>K"Identifier"]
    @test toks("a..+b") == ["a"=>K"Identifier", "..+"=>K"ErrorInvalidOperator", "b"=>K"Identifier"]
    @test toks("a..−b") == ["a"=>K"Identifier", "..−"=>K"ErrorInvalidOperator", "b"=>K"Identifier"]
end

@testset "hat suffix" begin
    @test tok("ŝ", 1).kind==K"Identifier"
    @test untokenize(collect(tokenize("ŝ"))[1], "ŝ") == "ŝ"
end

@testset "suffixed op" begin
    s = "+¹"
    @test is_operator(tok(s, 1).kind)
    @test untokenize(collect(tokenize(s))[1], s) == s
end

@testset "circ arrow right op" begin
    s = "↻"
    @test collect(tokenize(s))[1].kind == K"↻"
end

@testset "invalid float" begin
    s = ".0."
    @test collect(tokenize(s))[1].kind == K"ErrorInvalidNumericConstant"
end

@testset "allow prime after end" begin
    @test tok("begin end'", 4).kind === K"'"
end

@testset "new ops" begin
    ops = [
        raw"= += -= *= /= //= \= ^= ÷= %= <<= >>= >>>= |= &= ⊻= ≔ ⩴ ≕ ~ := $="
        raw"=>"
        raw"?"
        raw"← → ↔ ↚ ↛ ↞ ↠ ↢ ↣ ↦ ↤ ↮ ⇎ ⇍ ⇏ ⇐ ⇒ ⇔ ⇴ ⇶ ⇷ ⇸ ⇹ ⇺ ⇻ ⇼ ⇽ ⇾ ⇿ ⟵ ⟶ ⟷ ⟹ ⟺ ⟻ ⟼ ⟽ ⟾ ⟿ ⤀ ⤁ ⤂ ⤃ ⤄ ⤅ ⤆ ⤇ ⤌ ⤍ ⤎ ⤏ ⤐ ⤑ ⤔ ⤕ ⤖ ⤗ ⤘ ⤝ ⤞ ⤟ ⤠ ⥄ ⥅ ⥆ ⥇ ⥈ ⥊ ⥋ ⥎ ⥐ ⥒ ⥓ ⥖ ⥗ ⥚ ⥛ ⥞ ⥟ ⥢ ⥤ ⥦ ⥧ ⥨ ⥩ ⥪ ⥫ ⥬ ⥭ ⥰ ⧴ ⬱ ⬰ ⬲ ⬳ ⬴ ⬵ ⬶ ⬷ ⬸ ⬹ ⬺ ⬻ ⬼ ⬽ ⬾ ⬿ ⭀ ⭁ ⭂ ⭃ ⭄ ⭇ ⭈ ⭉ ⭊ ⭋ ⭌ ￩ ￫ ⇜ ⇝ ↜ ↝ ↩ ↪ ↫ ↬ ↼ ↽ ⇀ ⇁ ⇄ ⇆ ⇇ ⇉ ⇋ ⇌ ⇚ ⇛ ⇠ ⇢ ↷ ↶ ↺ ↻ -->"
        raw"||"
        raw"&&"
        raw"> < >= ≥ <= ≤ == === ≡ != ≠ !== ≢ ∈ ∉ ∋ ∌ ⊆ ⊈ ⊂ ⊄ ⊊ ∝ ∊ ∍ ∥ ∦ ∷ ∺ ∻ ∽ ∾ ≁ ≃ ≂ ≄ ≅ ≆ ≇ ≈ ≉ ≊ ≋ ≌ ≍ ≎ ≐ ≑ ≒ ≓ ≖ ≗ ≘ ≙ ≚ ≛ ≜ ≝ ≞ ≟ ≣ ≦ ≧ ≨ ≩ ≪ ≫ ≬ ≭ ≮ ≯ ≰ ≱ ≲ ≳ ≴ ≵ ≶ ≷ ≸ ≹ ≺ ≻ ≼ ≽ ≾ ≿ ⊀ ⊁ ⊃ ⊅ ⊇ ⊉ ⊋ ⊏ ⊐ ⊑ ⊒ ⊜ ⊩ ⊬ ⊮ ⊰ ⊱ ⊲ ⊳ ⊴ ⊵ ⊶ ⊷ ⋍ ⋐ ⋑ ⋕ ⋖ ⋗ ⋘ ⋙ ⋚ ⋛ ⋜ ⋝ ⋞ ⋟ ⋠ ⋡ ⋢ ⋣ ⋤ ⋥ ⋦ ⋧ ⋨ ⋩ ⋪ ⋫ ⋬ ⋭ ⋲ ⋳ ⋴ ⋵ ⋶ ⋷ ⋸ ⋹ ⋺ ⋻ ⋼ ⋽ ⋾ ⋿ ⟈ ⟉ ⟒ ⦷ ⧀ ⧁ ⧡ ⧣ ⧤ ⧥ ⩦ ⩧ ⩪ ⩫ ⩬ ⩭ ⩮ ⩯ ⩰ ⩱ ⩲ ⩳ ⩵ ⩶ ⩷ ⩸ ⩹ ⩺ ⩻ ⩼ ⩽ ⩾ ⩿ ⪀ ⪁ ⪂ ⪃ ⪄ ⪅ ⪆ ⪇ ⪈ ⪉ ⪊ ⪋ ⪌ ⪍ ⪎ ⪏ ⪐ ⪑ ⪒ ⪓ ⪔ ⪕ ⪖ ⪗ ⪘ ⪙ ⪚ ⪛ ⪜ ⪝ ⪞ ⪟ ⪠ ⪡ ⪢ ⪣ ⪤ ⪥ ⪦ ⪧ ⪨ ⪩ ⪪ ⪫ ⪬ ⪭ ⪮ ⪯ ⪰ ⪱ ⪲ ⪳ ⪴ ⪵ ⪶ ⪷ ⪸ ⪹ ⪺ ⪻ ⪼ ⪽ ⪾ ⪿ ⫀ ⫁ ⫂ ⫃ ⫄ ⫅ ⫆ ⫇ ⫈ ⫉ ⫊ ⫋ ⫌ ⫍ ⫎ ⫏ ⫐ ⫑ ⫒ ⫓ ⫔ ⫕ ⫖ ⫗ ⫘ ⫙ ⫷ ⫸ ⫹ ⫺ ⊢ ⊣ ⟂ <: >:"
        raw"<|"
        raw"|>"
        raw": .. … ⁝ ⋮ ⋱ ⋰ ⋯"
        raw"$ + - ¦ | ⊕ ⊖ ⊞ ⊟ ++ ∪ ∨ ⊔ ± ∓ ∔ ∸ ≏ ⊎ ⊻ ⊽ ⋎ ⋓ ⧺ ⧻ ⨈ ⨢ ⨣ ⨤ ⨥ ⨦ ⨧ ⨨ ⨩ ⨪ ⨫ ⨬ ⨭ ⨮ ⨹ ⨺ ⩁ ⩂ ⩅ ⩊ ⩌ ⩏ ⩐ ⩒ ⩔ ⩖ ⩗ ⩛ ⩝ ⩡ ⩢ ⩣"
        raw"* / ⌿ ÷ % & ⋅ ∘ × \ ∩ ∧ ⊗ ⊘ ⊙ ⊚ ⊛ ⊠ ⊡ ⊓ ∗ ∙ ∤ ⅋ ≀ ⊼ ⋄ ⋆ ⋇ ⋉ ⋊ ⋋ ⋌ ⋏ ⋒ ⟑ ⦸ ⦼ ⦾ ⦿ ⧶ ⧷ ⨇ ⨰ ⨱ ⨲ ⨳ ⨴ ⨵ ⨶ ⨷ ⨸ ⨻ ⨼ ⨽ ⩀ ⩃ ⩄ ⩋ ⩍ ⩎ ⩑ ⩓ ⩕ ⩘ ⩚ ⩜ ⩞ ⩟ ⩠ ⫛ ⊍ ▷ ⨝ ⟕ ⟖ ⟗"
        raw"//"
        raw"<< >> >>>"
        raw"^ ↑ ↓ ⇵ ⟰ ⟱ ⤈ ⤉ ⤊ ⤋ ⤒ ⤓ ⥉ ⥌ ⥍ ⥏ ⥑ ⥔ ⥕ ⥘ ⥙ ⥜ ⥝ ⥠ ⥡ ⥣ ⥥ ⥮ ⥯ ￪ ￬"
        raw"::"
        raw"."
    ]
    if VERSION >= v"1.6.0"
        push!(ops, raw"<-- <-->")
    end
    if VERSION >= v"1.7.0"
        append!(ops, [
            "−"
            "\u00b7 \u0387"
            "⫪ ⫫"
        ])
    end
    if VERSION >= v"1.10-DEV"
        push!(ops, "⥷ ⥺ ⟇")
    end
    allops = split(join(ops, " "), " ")
    @test all(s->Base.isoperator(Symbol(s)) == is_operator(first(collect(tokenize(s))).kind), allops)
end

const all_kws = Set([
    # Keywords
    "baremodule",
    "begin",
    "break",
    "catch",
    "const",
    "continue",
    "do",
    "else",
    "elseif",
    "end",
    "export",
    "finally",
    "for",
    "function",
    "global",
    "if",
    "import",
    "let",
    "local",
    "macro",
    "module",
    "public",
    "quote",
    "return",
    "struct",
    "try",
    "using",
    "while",
    # Contextual keywords
    "abstract",
    "as",
    "doc",
    "mutable",
    "outer",
    "primitive",
    "type",
    "var",
    # Literals
    "true",
    "false",
    # Word-like operators
    "in",
    "isa",
    "where",
])

function check_kw_hashes(iter)
    for cs in iter
        str = String([cs...])
        if Tokenize.simple_hash(str) in keys(Tokenize.kw_hash)
            @test str in all_kws
        end
    end
end

@testset "simple_hash" begin
    @test length(all_kws) == length(Tokenize.kw_hash)

    @testset "Length $len keywords" for len in 1:5
        check_kw_hashes(String([cs...]) for cs in Iterators.product(['a':'z' for _ in 1:len]...))
    end
end


@testset "UTF-8 BOM" begin
    @test kind.(collect(tokenize("\ufeff[1\ufeff2]"))) == [
        K"Whitespace",
        K"[",
        K"Integer",
        K"Whitespace",
        K"Integer",
        K"]",
        K"EndMarker"
    ]
end

@testset "lexer initialization" begin
    # Ranges of EndMarker
    @test (t = last(collect(tokenize("+"))); (t.startbyte, t.endbyte)) == (1,0)
    @test (t = last(collect(tokenize("+*"))); (t.startbyte, t.endbyte)) == (2,1)
end

@testset "invalid UTF-8 characters" begin
    @test onlytok("\x00") == K"ErrorUnknownCharacter"

    bad_chars = [
        first("\xe2")              # malformed
        first("\xc0\x9b")          # overlong
        first("\xf0\x83\x99\xae")  # overlong
    ]

    @testset "bad char $(repr(c))" for c in bad_chars
        @test Tokenize.is_identifier_char(c) == false
        @test Tokenize.is_identifier_start_char(c) == false
        @test Tokenize.is_never_id_char(c) == true
        @test Tokenize.is_dottable_operator_start_char(c) == false
        @test Tokenize.isopsuffix(c) == false
        @test Tokenize.is_operator_start_char(c) == false
        @test Tokenize.iswhitespace(c) == false
        @test Tokenize.ishex(c) == false
    end
end

@testset "unbalanced bidirectional unicode" begin
    open_embedding = ['\U202A', '\U202B', '\U202D', '\U202E']
    close_embedding = '\U202C'
    open_isolate = ['\U2066', '\U2067', '\U2068']
    close_isolate = '\U2069'
    close_all = '\n'

    all_bidi_codes = [open_embedding; close_embedding; open_isolate; close_isolate]

    bidi_pairs = [Iterators.product(open_embedding, [close_embedding, close_all])...,
                  Iterators.product(open_isolate,   [close_isolate, close_all])...]

    @testset "delimiter $kd" for (kd, chunk_kind) in [
            (K"\"",      K"String"),
            (K"\"\"\"",  K"String"),
            (K"`",       K"CmdString"),
            (K"```",     K"CmdString")
        ]
        d = string(kd)
        @testset "Single unbalanced codes" begin
            for c in all_bidi_codes
                @test toks("$d$c$d") ==
                    [d=>kd, "$c"=>K"ErrorBidiFormatting", d=>kd]
                @test toks("pfx$d$c$d") ==
                    ["pfx"=>K"Identifier", d=>kd, "$c"=>K"ErrorBidiFormatting", d=>kd]
            end
        end
        @testset "Balanced pairs" begin
            for (openc, closec) in bidi_pairs
                str = "$(openc)##$(closec)"
                @test toks("$d$str$d") ==
                    [d=>kd, str=>chunk_kind, d=>kd]
                @test toks("pfx$d$str$d") ==
                    ["pfx"=>K"Identifier", d=>kd, str=>chunk_kind, d=>kd]
            end
        end
    end

    @testset "multi line comments" begin
        @testset "Single unbalanced codes" begin
            for c in all_bidi_codes
                comment = "#=$c=#"
                @test toks(comment) == [comment=>K"ErrorBidiFormatting"]
            end
        end
        @testset "Balanced pairs" begin
            for (openc, closec) in bidi_pairs
                str = "#=$(openc)zz$(closec)=#"
                @test toks(str) == [str=>K"Comment"]
            end
        end
    end

    @testset "extended balanced/unbalanced bidi state" begin
        @testset "delimiter $kd" for (kd, chunk_kind) in [
                (K"\"",      K"String"),
                (K"\"\"\"",  K"String"),
                (K"`",       K"CmdString"),
                (K"```",     K"CmdString")
            ]
            d = string(kd)
            for balanced in [# Balanced pairs
                             "\u202a\u202bzz\u202c\u202c"
                             "\u2066\u2067zz\u2069\u2069"
                             # Newline is complete bidi state reset
                             "\u202a\u2067zz\n"
                             "\u202a\u202azz\n"
                             # \r\n and \n terminate a line
                             "\u202azz\r\n"
                             ]
                @test toks("$d$balanced$d") == [
                    d=>kd
                    balanced=>chunk_kind
                    d=>kd
                ]
            end
            for unbalanced in ["\u202azz\u202c\u202c"
                               "\u202a\u202bzz\u202c"
                               # \r does not terminate a bidi line
                               "\u202azz\r"
                              ]
                @test toks("$d$unbalanced$d") == [
                    d=>kd
                    unbalanced=>K"ErrorBidiFormatting"
                    d=>kd
                ]
            end
        end
    end

    # Interpolations reset bidi state
    @test toks("\"\u202a\$zz\n\"") == [
        "\""=>K"\""
        "\u202a"=>K"ErrorBidiFormatting"
        "\$"=>K"$"
        "zz"=>K"Identifier"
        "\n"=>K"String"
        "\""=>K"\""
    ]
    @testset "newline escaping" begin
        @test toks("\"a\u202a\\\n\"") == [
             "\""=>K"\""
             "a\u202a"=>K"String"
             "\\\n"=>K"Whitespace"
             "\""=>K"\""
        ]
        @test toks("\"a\u202a\\\r\n\"") == [
             "\""=>K"\""
             "a\u202a"=>K"String"
             "\\\r\n"=>K"Whitespace"
             "\""=>K"\""
        ]
        @test toks("\"a\u202a\\\r\"") == [
             "\""=>K"\""
             "a\u202a"=>K"ErrorBidiFormatting"
             "\\\r"=>K"Whitespace"
             "\""=>K"\""
        ]
    end

    @testset "delimiter '" begin
        for c in all_bidi_codes
            @test toks("'$c'") == ["'"=>K"'", "$c"=>K"Char", "'"=>K"'"]
        end
    end
end

@testset "dotop miscellanea" begin
    @test strtok("a .-> b")  ==  ["a", " ", ".-", ">", " ", "b", ""]
    @test strtok(".>: b")    ==  [".>:", " ", "b", ""]
    @test strtok(".<: b")    ==  [".<:", " ", "b", ""]
    @test strtok("a ||₁ b")  ==  ["a", " ", "||", "₁", " ", "b", ""]
    @test strtok("a ||̄ b")   ==  ["a", " ", "||", "̄", " ", "b", ""]
    @test strtok("a .||₁ b") ==  ["a", " ", ".||", "₁", " ", "b", ""]
    @test strtok("a &&₁ b")  ==  ["a", " ", "&&", "₁", " ", "b", ""]
    @test strtok("a &&̄ b")   ==  ["a", " ", "&&", "̄", " ", "b", ""]
    @test strtok("a .&&₁ b") ==  ["a", " ", ".&&", "₁", " ", "b", ""]
end

end
