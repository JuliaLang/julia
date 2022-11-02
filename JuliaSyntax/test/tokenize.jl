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
    Token

tok(str, i = 1) = collect(tokenize(str))[i]

strtok(str) = untokenize.(collect(tokenize(str)), str)

function toks(str)
    ts = [untokenize(t, str)=>kind(t) for t in tokenize(str)]
    @test ts[end] == (""=>K"EndMarker")
    pop!(ts)
    ts
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
    str = "𝘋 =2β"
    for s in [str, IOBuffer(str)]
        l = tokenize(s)
        kinds = [K"Identifier", K"Whitespace", K"=",
                 K"Integer", K"Identifier", K"EndMarker"]
        token_strs = ["𝘋", " ", "=", "2", "β", ""]
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


function test_roundtrip(str, kind, val)
    t = tok(str)
    @test t.kind == kind
    @test untokenize(t, str) == val
end

roundtrip(str) = join(untokenize.(collect(tokenize(str)), str))

@testset "tokenizing juxtaposed numbers and dotted operators/identifiers" begin
    test_roundtrip("1234 .+1",     K"Integer", "1234")
    test_roundtrip("1234.0+1",     K"Float",   "1234.0")
    test_roundtrip("1234.0 .+1",   K"Float",   "1234.0")
    test_roundtrip("1234.f(a)",    K"Float",   "1234.")
    test_roundtrip("1234 .f(a)",   K"Integer", "1234")
    test_roundtrip("1234.0.f(a)",  K"ErrorInvalidNumericConstant",   "1234.0.")
    test_roundtrip("1234.0 .f(a)", K"Float",   "1234.0")
end


@testset "lexing anon functions '->' " begin
    @test tok("a->b", 2).kind==K"->"
end

@testset "comments" begin
    toks = collect(tokenize("""
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
    @test kind.(toks) == kinds
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

@testset "errors" begin
    @test tok("#=   #=   =#", 1).kind == K"ErrorEofMultiComment"
    @test tok("aa **",        3).kind == K"ErrorInvalidOperator"
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

~(tok::Token, t::Tuple) = tok.kind == t[1] && untokenize(tok, t[3]) == t[2]

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

    @testset "invalid chars after identifier" begin
        str = """ "\$x෴" """
    ts = collect(tokenize(str))
        @test ts[4] ~ (K"Identifier" , "x" , str)
        @test ts[5] ~ (K"ErrorInvalidInterpolationTerminator" , ""  , str)
        @test ts[6] ~ (K"String"     , "෴" , str)
        @test is_error(ts[5].kind)
        @test ts[5].kind == K"ErrorInvalidInterpolationTerminator"
    end
end

@testset "inferred" begin
    l = tokenize("abc")
    @inferred Tokenize.next_token(l)
end

@testset "modifying function names (!) followed by operator" begin
    @test tok("a!=b",  2).kind == K"!="
    @test tok("a!!=b", 2).kind == K"!="
    @test tok("!=b",   1).kind == K"!="
end

@testset "lex integers" begin
    @test kind(tok("1234"))            == K"Integer"
    @test kind(tok("12_34"))           == K"Integer"
    @test kind(tok("_1234"))           == K"Identifier"
    @test kind(tok("1234_"))           == K"Integer"
    @test kind(tok("1234_", 2))        == K"Identifier"
    @test kind(tok("1234x"))           == K"Integer"
    @test kind(tok("1234x", 2))        == K"Identifier"
end

@testset "numbers with trailing `.` " begin
    @test tok("1.0").kind == K"Float"
    @test tok("1.a").kind == K"Float"
    @test tok("1.(").kind == K"Float"
    @test tok("1.[").kind == K"Float"
    @test tok("1.{").kind == K"Float"
    @test tok("1.)").kind == K"Float"
    @test tok("1.]").kind == K"Float"
    @test tok("1.{").kind == K"Float"
    @test tok("1.,").kind == K"Float"
    @test tok("1.;").kind == K"Float"
    @test tok("1.@").kind == K"Float"
    @test tok("1.#").kind == K"Float"
    @test tok("1.").kind == K"Float"
    @test tok("1.\"text\" ").kind == K"Float"

    @test toks("1..")    == ["1"=>K"Integer",   ".."=>K".."]
    @test toks(".1..")   == [".1"=>K"Float",    ".."=>K".."]
    @test toks("0x01..") == ["0x01"=>K"HexInt", ".."=>K".."]

    @test kind.(collect(tokenize("1f0./1"))) == [K"Float", K"/", K"Integer", K"EndMarker"]
end



@testset "lex octal" begin
    @test tok("0o0167").kind == K"OctInt"
end

@testset "lex float/bin/hex/oct w underscores" begin
    @test tok("1_1.11").kind           == K"Float"
    @test tok("11.1_1").kind           == K"Float"
    @test tok("1_1.1_1").kind           == K"Float"
    @test tok("_1.1_1", 1).kind           == K"Identifier"
    @test tok("_1.1_1", 2).kind           == K"Float"
    @test tok("0x0167_032").kind           == K"HexInt"
    @test tok("0b0101001_0100_0101").kind  == K"BinInt"
    @test tok("0o01054001_0100_0101").kind == K"OctInt"
    @test kind.(collect(tokenize("1.2."))) == [K"ErrorInvalidNumericConstant", K"EndMarker"]
    @test tok("1__2").kind == K"Integer"
    @test tok("1.2_3").kind == K"Float"
    @test tok("1.2_3", 2).kind == K"EndMarker"
    @test kind.(collect(tokenize("3e2_2"))) == [K"Float", K"Identifier", K"EndMarker"]
    @test kind.(collect(tokenize("1__2"))) == [K"Integer", K"Identifier", K"EndMarker"]
    @test kind.(collect(tokenize("0x2_0_2"))) == [K"HexInt", K"EndMarker"]
    @test kind.(collect(tokenize("0x2__2"))) == [K"HexInt", K"Identifier", K"EndMarker"]
    @test kind.(collect(tokenize("3_2.5_2"))) == [K"Float", K"EndMarker"]
    @test kind.(collect(tokenize("3.2e2.2"))) == [K"ErrorInvalidNumericConstant", K"Integer", K"EndMarker"]
    @test kind.(collect(tokenize("3e2.2"))) == [K"ErrorInvalidNumericConstant", K"Integer", K"EndMarker"]
    @test kind.(collect(tokenize("0b101__101"))) == [K"BinInt", K"Identifier", K"EndMarker"]
    @test tok("0x1p").kind == K"ErrorInvalidNumericConstant"
end

@testset "floating points" begin
    @test tok("1.0e0").kind  == K"Float"
    @test tok("1.0e-0").kind == K"Float"
    @test tok("1.0E0").kind  == K"Float"
    @test tok("1.0E-0").kind == K"Float"
    @test tok("1.0f0").kind  == K"Float"
    @test tok("1.0f-0").kind == K"Float"

    @test tok("0e0").kind    == K"Float"
    @test tok("0e+0").kind   == K"Float"
    @test tok("0E0").kind    == K"Float"
    @test tok("201E+0").kind == K"Float"
    @test tok("2f+0").kind   == K"Float"
    @test tok("2048f0").kind == K"Float"
    @test tok("1.:0").kind == K"Float"
    @test tok("0x00p2").kind == K"Float"
    @test tok("0x00P2").kind == K"Float"
    @test tok("0x0.00p23").kind == K"Float"
    @test tok("0x0.0ap23").kind == K"Float"
    @test tok("0x0.0_0p2").kind == K"Float"
    @test tok("0x0_0_0.0_0p2").kind == K"Float"
    @test tok("0x0p+2").kind == K"Float"
    @test tok("0x0p-2").kind == K"Float"

    # Floating point with \minus rather than -
    @test tok("1.0e−0").kind == K"Float"
    @test tok("1.0f−0").kind == K"Float"
    @test tok("0x0p−2").kind == K"Float"
end

@testset "1e1" begin
    @test tok("1e", 1).kind == K"Integer"
    @test tok("1e", 2).kind == K"Identifier"
end

@testset "jl06types" begin
    @test tok("mutable").kind   == K"mutable"
    @test tok("primitive").kind == K"primitive"
    @test tok("struct").kind    == K"struct"
    @test tok("where").kind     == K"where"
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

@testset "hex/bin/octal errors" begin
    @test tok("0x").kind == K"ErrorInvalidNumericConstant"
    @test tok("0b").kind == K"ErrorInvalidNumericConstant"
    @test tok("0o").kind == K"ErrorInvalidNumericConstant"
    @test tok("0x 2", 1).kind == K"ErrorInvalidNumericConstant"
    @test tok("0x.1p1").kind == K"Float"
end


@testset "dotted and suffixed operators" begin
ops = collect(values(Tokenize.UNICODE_OPS_REVERSE))

for op in ops
    op in (:isa, :in, :where, Symbol('\''), :?, :(:)) && continue
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

function test_error(tok, kind)
    @test is_error(tok.kind)
    @test tok.kind == kind
end

@testset "token errors" begin
    test_error(tok("1.2e2.3",1), K"ErrorInvalidNumericConstant")
    test_error(tok("1.2.",1),    K"ErrorInvalidNumericConstant")
    test_error(tok("1.2.f",1),   K"ErrorInvalidNumericConstant")
    test_error(tok("0xv",1),     K"ErrorInvalidNumericConstant")
    test_error(tok("0b3",1),     K"ErrorInvalidNumericConstant")
    test_error(tok("0op",1),     K"ErrorInvalidNumericConstant")
    test_error(tok("--",1),      K"ErrorInvalidOperator")
    test_error(tok("1**2",2),    K"ErrorInvalidOperator")
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

@testset "invalid float juxt" begin
    s = "1.+2"
    @test tok(s, 1).kind == K"error"
    @test is_operator(tok(s, 2).kind)
    test_roundtrip("1234.+1", K"error", "1234.")
    @test tok("1.+ ").kind == K"error"
    @test tok("1.⤋").kind  == K"error"
    @test tok("1.?").kind == K"error"
end

@testset "comments" begin
    s = "#=# text=#"
    @test length(collect(tokenize(s))) == 2
end

@testset "invalid hexadecimal" begin
    s = "0x."
    tok(s, 1).kind === K"error"
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

@testset "is_identifier[_start]_char" begin
    malformed = first("\xe2")
    @test Tokenize.is_identifier_char(malformed) == false
    @test Tokenize.is_identifier_start_char(malformed) == false
    @test Tokenize.is_never_id_char(malformed) == true
end

end
