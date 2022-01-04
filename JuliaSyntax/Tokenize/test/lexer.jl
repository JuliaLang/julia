using JuliaSyntax.Tokenize
using JuliaSyntax.Tokenize.Lexers
using Test

const T = Tokenize.Tokens

tok(str, i = 1) = collect(tokenize(str))[i]

@testset "tokens" begin
    for s in ["a", IOBuffer("a")]
        l = tokenize(s)
        @test Lexers.readchar(l) == 'a'

        # @test l.current_pos == 0
        l_old = l
        @test l == l_old
        @test Lexers.eof(l)
        @test Lexers.readchar(l) == Lexers.EOF_CHAR

        # @test l.current_pos == 0
    end
end # testset

@testset "tokenize unicode" begin
    str = "𝘋 =2β"
    for s in [str, IOBuffer(str)]
        l = tokenize(s)
        kinds = [T.IDENTIFIER, T.WHITESPACE, T.OP,
                T.INTEGER, T.IDENTIFIER, T.ENDMARKER]
        token_strs = ["𝘋", " ", "=", "2", "β", ""]
        for (i, n) in enumerate(l)
            @test T.kind(n) == kinds[i]
            @test untokenize(n)  == token_strs[i]
            @test T.startpos(n) == (1, i)
            @test T.endpos(n) == (1, i - 1 + length(token_strs[i]))
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
    # for t in Tokens.kind.(collect(tokenize(str)))
    #    print("T.", t, ",")
    # end
    # ```
    # and *check* it afterwards.

    kinds = [T.KEYWORD,T.WHITESPACE,T.IDENTIFIER,T.LBRACE,T.IDENTIFIER,
            T.OP,T.IDENTIFIER,T.RBRACE,T.LPAREN,T.IDENTIFIER,T.OP,
            T.LBRACE,T.IDENTIFIER,T.RBRACE,T.OP,T.INTEGER,T.RPAREN,

            T.NEWLINE_WS,T.AT_SIGN,T.IDENTIFIER,T.WHITESPACE,T.LPAREN,
            T.IDENTIFIER,T.OP,T.IDENTIFIER,T.COMMA,T.WHITESPACE,
            T.IDENTIFIER,T.OP,T.IDENTIFIER,T.RPAREN,T.SEMICOLON,

            T.NEWLINE_WS,T.KEYWORD,

            T.NEWLINE_WS,T.KEYWORD,
            T.NEWLINE_WS,T.IDENTIFIER,
            T.NEWLINE_WS,T.KEYWORD,
            T.NEWLINE_WS,T.IDENTIFIER,
            T.NEWLINE_WS,T.KEYWORD,

            T.NEWLINE_WS,T.AT_SIGN,T.IDENTIFIER,T.WHITESPACE,T.IDENTIFIER,
            T.OP,T.IDENTIFIER,

            T.NEWLINE_WS,T.IDENTIFIER,T.LSQUARE,T.LSQUARE,T.INTEGER,T.WHITESPACE,
            T.INTEGER,T.WHITESPACE,T.INTEGER,T.RSQUARE,T.RSQUARE,

            T.NEWLINE_WS,T.LSQUARE,T.INTEGER,T.OP,T.INTEGER,T.COMMA,T.INTEGER,
            T.SEMICOLON,T.INTEGER,T.COMMA,T.INTEGER,T.RSQUARE,

            T.NEWLINE_WS,T.DQUOTE,T.STRING,T.DQUOTE,T.SEMICOLON,T.WHITESPACE,T.CHAR,

            T.NEWLINE_WS,T.LPAREN,T.IDENTIFIER,T.OP,T.IDENTIFIER,T.RPAREN,T.OP,
            T.LPAREN,T.IDENTIFIER,T.OP,T.IDENTIFIER,T.RPAREN,

            T.NEWLINE_WS,T.COMMENT,

            T.NEWLINE_WS,T.COMMENT,

            T.NEWLINE_WS,T.INTEGER,T.OP,T.INTEGER,

            T.NEWLINE_WS,T.IDENTIFIER,T.OP,T.OP,T.IDENTIFIER,T.OP,

            T.NEWLINE_WS,T.IDENTIFIER,T.OP,T.OP,T.OP,T.IDENTIFIER,T.OP,T.OP,

            T.NEWLINE_WS,T.BACKTICK,T.STRING,T.BACKTICK,

            T.NEWLINE_WS,T.INTEGER,T.IDENTIFIER,T.LPAREN,T.INTEGER,T.RPAREN,

            T.NEWLINE_WS,T.LBRACE,T.RBRACE,

            T.NEWLINE_WS,T.ERROR,T.ENDMARKER]

    for (i, n) in enumerate(tokenize(str))
        @test Tokens.kind(n) == kinds[i]
    end
    for (i, n) in enumerate(tokenize(str, Tokens.RawToken))
        @test Tokens.kind(n) == kinds[i]
    end

    @testset "roundtrippability" begin
        @test join(untokenize.(collect(tokenize(str)))) == str
        @test untokenize(collect(tokenize(str))) == str
        @test untokenize(tokenize(str)) == str
        @test_throws ArgumentError untokenize("blabla")
    end

    @test all((t.endbyte - t.startbyte + 1)==sizeof(untokenize(t)) for t in tokenize(str))
end # testset

@testset "issue 5, '..'" begin
    @test Tokens.kind.(collect(tokenize("1.23..3.21"))) == [T.FLOAT,T.OP,T.FLOAT,T.ENDMARKER]
end

@testset "issue 17, >>" begin
    @test untokenize(tok(">> "))==">>"
end


@testset "test added operators" begin
    @test tok("1+=2",  2).kind == T.PLUS_EQ
    @test tok("1-=2",  2).kind == T.MINUS_EQ
    @test tok("1:=2",  2).kind == T.COLON_EQ
    @test tok("1*=2",  2).kind == T.STAR_EQ
    @test tok("1^=2",  2).kind == T.CIRCUMFLEX_EQ
    @test tok("1÷=2",  2).kind == T.DIVISION_EQ
    @test tok("1\\=2", 2).kind == T.BACKSLASH_EQ
    @test tok("1\$=2", 2).kind == T.EX_OR_EQ
    @test tok("1-->2", 2).kind == T.RIGHT_ARROW
    @test tok("1<--2", 2).kind == T.LEFT_ARROW
    @test tok("1<-->2", 2).kind == T.DOUBLE_ARROW
    @test tok("1>:2",  2).kind == T.ISSUPERTYPE
end

@testset "infix" begin
    @test tok("1 in 2",  3).kind == T.IN
    @test tok("1 in[1]", 3).kind == T.IN

    if VERSION >= v"0.6.0-dev.1471"
        @test tok("1 isa 2",  3).kind == T.ISA
        @test tok("1 isa[2]", 3).kind == T.ISA
    else
        @test tok("1 isa 2",  3).kind == T.IDENTIFIER
        @test tok("1 isa[2]", 3).kind == T.IDENTIFIER
    end
end

@testset "tokenizing true/false literals" begin
    @test tok("somtext true", 3).kind == T.TRUE
    @test tok("somtext false", 3).kind == T.FALSE
    @test tok("somtext tr", 3).kind == T.IDENTIFIER
    @test tok("somtext falsething", 3).kind == T.IDENTIFIER
end

@testset "tokenizing var identifiers" begin
    t = tok("var\"#1\"")
    @test t.kind == T.VAR_IDENTIFIER && untokenize(t) == "var\"#1\""
    t = tok("var\"  \"")
    @test t.kind == T.VAR_IDENTIFIER && untokenize(t) == "var\"  \""
end

@testset "tokenizing juxtaposed numbers and dotted operators/identifiers" begin
    @test (t->t.val=="1234"    && t.kind == Tokens.INTEGER )(tok("1234 .+1"))
    @test (t->t.val=="1234.0"  && t.kind == Tokens.FLOAT   )(tok("1234.0.+1"))
    @test (t->t.val=="1234.0"  && t.kind == Tokens.FLOAT   )(tok("1234.0 .+1"))
    @test (t->t.val=="1234."   && t.kind == Tokens.FLOAT   )(tok("1234.f(a)"))
    @test (t->t.val=="1234"    && t.kind == Tokens.INTEGER )(tok("1234 .f(a)"))
    @test (t->t.val=="1234.0." && t.kind == Tokens.ERROR   )(tok("1234.0.f(a)"))
    @test (t->t.val=="1234.0"  && t.kind == Tokens.FLOAT   )(tok("1234.0 .f(a)"))
end


@testset "lexing anon functions '->' " begin
    @test tok("a->b", 2).kind==Tokens.ANON_FUNC
end

@testset "comments" begin
    toks = collect(tokenize("""
       #
       \"\"\"
       f
       \"\"\"
       1
       """))

    kinds = [T.COMMENT, T.NEWLINE_WS,
             T.TRIPLE_DQUOTE, T.STRING, T.TRIPLE_DQUOTE, T.NEWLINE_WS,
             T.INTEGER, T.NEWLINE_WS,
             T.ENDMARKER]
    @test T.kind.(toks) == kinds
end


@testset "primes" begin
    tokens = collect(tokenize(
    """
    ImageMagick.save(fn, reinterpret(ARGB32, [0xf0884422]''))
    D = ImageMagick.load(fn)
    """))
    @test string(untokenize(tokens[16]))==string(untokenize(tokens[17]))=="'"
    @test tok("'a'").val == "'a'"
    @test tok("'a'").kind == Tokens.CHAR
    @test tok("''").val == "''"
    @test tok("''").kind == Tokens.CHAR
    @test tok("'''").val == "'''"
    @test tok("'''").kind == Tokens.CHAR
    @test tok("''''", 1).kind == Tokens.CHAR
    @test tok("''''", 2).kind == Tokens.PRIME
    @test tok("()'", 3).kind == Tokens.PRIME
    @test tok("{}'", 3).kind == Tokens.PRIME
    @test tok("[]'", 3).kind == Tokens.PRIME
end

@testset "keywords" begin
      for kw in    ["function",
                    "abstract",
                    "as",
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
                    #"false",
                    "finally",
                    "for",
                    "function",
                    "global",
                    "let",
                    "local",
                    "if",
                    "import",
                    "macro",
                    "module",
                    "mutable",
                    "primitive",
                    "quote",
                    "return",
                    "struct",
                    #"true",
                    "try",
                    "type",
                    "using",
                    "while"]

        @test T.kind(tok(kw)) == T.KEYWORD
    end
end

@testset "issue in PR #45" begin
    @test length(collect(tokenize("x)"))) == 3
end

@testset "errors" begin
    @test tok("#=   #=   =#",           1).kind == T.ERROR
    @test tok("'dsadsa",                1).kind == T.ERROR
    @test tok("aa **",                  3).kind == T.ERROR
end

@testset "xor_eq" begin
    @test tok("1 ⊻= 2", 3).kind==T.XOR_EQ
end

@testset "lex binary" begin
    @test tok("0b0101").kind==T.BIN_INT
end

@testset "show" begin
    io = IOBuffer()
    show(io, collect(tokenize("\"abc\nd\"ef"))[2])
    @test String(take!(io)) == "1,2-2,1          STRING         \"abc\\nd\""
end

~(tok::T.AbstractToken, t::Tuple) = tok.kind == t[1] && untokenize(tok) == t[2]

@testset "raw strings" begin
    ts = collect(tokenize(raw""" str"x $ \ y" """))
    @test ts[1] ~ (T.WHITESPACE , " "        )
    @test ts[2] ~ (T.IDENTIFIER , "str"      )
    @test ts[3] ~ (T.DQUOTE     , "\""       )
    @test ts[4] ~ (T.STRING     , "x \$ \\ y")
    @test ts[5] ~ (T.DQUOTE     , "\""       )
    @test ts[6] ~ (T.WHITESPACE , " "        )
    @test ts[7] ~ (T.ENDMARKER  , ""         )

    ts = collect(tokenize(raw"""`x $ \ y`"""))
    @test ts[1] ~ (T.BACKTICK  , "`"         )
    @test ts[2] ~ (T.STRING    , "x \$ \\ y" )
    @test ts[3] ~ (T.BACKTICK  , "`"         )
    @test ts[4] ~ (T.ENDMARKER , ""          )

    # str"\\"
    ts = collect(tokenize("str\"\\\\\""))
    @test ts[1] ~ (T.IDENTIFIER , "str"  )
    @test ts[2] ~ (T.DQUOTE     , "\""   )
    @test ts[3] ~ (T.STRING     , "\\\\" )
    @test ts[4] ~ (T.DQUOTE     , "\""   )
    @test ts[5] ~ (T.ENDMARKER  , ""     )

    # str"\\\""
    ts = collect(tokenize("str\"\\\\\\\"\""))
    @test ts[1] ~ (T.IDENTIFIER , "str"      )
    @test ts[2] ~ (T.DQUOTE     , "\""       )
    @test ts[3] ~ (T.STRING     , "\\\\\\\"" )
    @test ts[4] ~ (T.DQUOTE     , "\""       )
    @test ts[5] ~ (T.ENDMARKER  , ""         )
end

@testset "interpolation" begin
    @testset "basic" begin
        ts = collect(tokenize("\"\$x \$y\""))
        @test ts[1]  ~ (T.DQUOTE     , "\"")
        @test ts[2]  ~ (T.EX_OR      , "\$")
        @test ts[3]  ~ (T.IDENTIFIER , "x" )
        @test ts[4]  ~ (T.STRING     , " " )
        @test ts[5]  ~ (T.EX_OR      , "\$")
        @test ts[6]  ~ (T.IDENTIFIER , "y" )
        @test ts[7]  ~ (T.DQUOTE     , "\"")
        @test ts[8]  ~ (T.ENDMARKER  , ""  )
    end

    @testset "nested" begin
        str = """"str: \$(g("str: \$(h("str"))"))" """
        ts = collect(tokenize(str))
        @test length(ts) == 23
        @test ts[1]  ~ (T.DQUOTE    , "\""   )
        @test ts[2]  ~ (T.STRING    , "str: ")
        @test ts[3]  ~ (T.EX_OR     , "\$"   )
        @test ts[4]  ~ (T.LPAREN    , "("    )
        @test ts[5]  ~ (T.IDENTIFIER, "g"    )
        @test ts[6]  ~ (T.LPAREN    , "("    )
        @test ts[7]  ~ (T.DQUOTE    , "\""   )
        @test ts[8]  ~ (T.STRING    , "str: ")
        @test ts[9]  ~ (T.EX_OR     , "\$"   )
        @test ts[10] ~ (T.LPAREN    , "("    )
        @test ts[11] ~ (T.IDENTIFIER, "h"    )
        @test ts[12] ~ (T.LPAREN    , "("    )
        @test ts[13] ~ (T.DQUOTE    , "\""   )
        @test ts[14] ~ (T.STRING    , "str"  )
        @test ts[15] ~ (T.DQUOTE    , "\""   )
        @test ts[16] ~ (T.RPAREN    , ")"    )
        @test ts[17] ~ (T.RPAREN    , ")"    )
        @test ts[18] ~ (T.DQUOTE    , "\""   )
        @test ts[19] ~ (T.RPAREN    , ")"    )
        @test ts[20] ~ (T.RPAREN    , ")"    )
        @test ts[21] ~ (T.DQUOTE    , "\""   )
        @test ts[22] ~ (T.WHITESPACE, " "    )
        @test ts[23] ~ (T.ENDMARKER , ""     )
    end

    @testset "duplicate \$" begin
        ts = collect(tokenize("\"\$\$\""))
        @test ts[1]  ~ (T.DQUOTE     , "\"")
        @test ts[2]  ~ (T.EX_OR      , "\$")
        @test ts[3]  ~ (T.EX_OR      , "\$")
        @test ts[4]  ~ (T.DQUOTE     , "\"")
        @test ts[5]  ~ (T.ENDMARKER  , ""  )
    end

    @testset "Unmatched parens" begin
        # issue 73: https://github.com/JuliaLang/Tokenize.jl/issues/73
        ts = collect(tokenize("\"\$(fdsf\""))
        @test ts[1] ~ (T.DQUOTE     , "\""   )
        @test ts[2] ~ (T.EX_OR      , "\$"   )
        @test ts[3] ~ (T.LPAREN     , "("    )
        @test ts[4] ~ (T.IDENTIFIER , "fdsf" )
        @test ts[5] ~ (T.DQUOTE     , "\""   )
        @test ts[6] ~ (T.ENDMARKER  , ""     )
    end

    @testset "Unicode" begin
        # issue 178: https://github.com/JuliaLang/Tokenize.jl/issues/178
        ts = collect(tokenize(""" "\$uₕx \$(uₕx - ux)" """))
        @test ts[ 1] ~ (T.WHITESPACE , " "   )
        @test ts[ 2] ~ (T.DQUOTE     , "\""  )
        @test ts[ 3] ~ (T.EX_OR      , "\$"  )
        @test ts[ 4] ~ (T.IDENTIFIER , "uₕx" )
        @test ts[ 5] ~ (T.STRING     , " "   )
        @test ts[ 6] ~ (T.EX_OR      , "\$"  )
        @test ts[ 7] ~ (T.LPAREN     , "("   )
        @test ts[ 8] ~ (T.IDENTIFIER , "uₕx" )
        @test ts[ 9] ~ (T.WHITESPACE , " "   )
        @test ts[10] ~ (T.MINUS      , "-"   )
        @test ts[11] ~ (T.WHITESPACE , " "   )
        @test ts[12] ~ (T.IDENTIFIER , "ux"  )
        @test ts[13] ~ (T.RPAREN     , ")"   )
        @test ts[14] ~ (T.DQUOTE     , "\""  )
        @test ts[15] ~ (T.WHITESPACE , " "   )
        @test ts[16] ~ (T.ENDMARKER  , ""    )
    end

    @testset "var\"...\" disabled in interpolations" begin
        ts = collect(tokenize(""" "\$var"x" " """))
        @test ts[ 1] ~ (T.WHITESPACE , " "   )
        @test ts[ 2] ~ (T.DQUOTE     , "\""  )
        @test ts[ 3] ~ (T.EX_OR      , "\$"  )
        @test ts[ 4] ~ (T.IDENTIFIER , "var" )
        @test ts[ 5] ~ (T.DQUOTE     , "\""  )
        @test ts[ 6] ~ (T.IDENTIFIER , "x"   )
        @test ts[ 7] ~ (T.DQUOTE     , "\""  )
        @test ts[ 8] ~ (T.STRING     , " "   )
        @test ts[ 9] ~ (T.DQUOTE     , "\""  )
        @test ts[10] ~ (T.WHITESPACE , " "   )
        @test ts[11] ~ (T.ENDMARKER  , ""    )
    end

    @testset "invalid chars after identifier" begin
        ts = collect(tokenize(""" "\$x෴" """))
        @test ts[4] ~ (T.IDENTIFIER , "x" )
        @test ts[5] ~ (T.ERROR      , ""  )
        @test ts[6] ~ (T.STRING     , "෴" )
        @test ts[5].token_error == Tokens.INVALID_INTERPOLATION_TERMINATOR
    end
end

@testset "inferred" begin
    l = tokenize("abc")
    @inferred Tokenize.Lexers.next_token(l)
    l = tokenize("abc", Tokens.RawToken)
    @inferred Tokenize.Lexers.next_token(l)
end

@testset "modifying function names (!) followed by operator" begin
    @test tok("a!=b",  2).kind == Tokens.NOT_EQ
    @test tok("a!!=b", 2).kind == Tokens.NOT_EQ
    @test tok("!=b",   1).kind == Tokens.NOT_EQ
end

@testset "lex integers" begin
    @test tok("1234").kind            == T.INTEGER
    @test tok("12_34").kind           == T.INTEGER
    @test tok("_1234").kind           == T.IDENTIFIER
    @test tok("1234_").kind           == T.INTEGER
    @test tok("1234_", 2).kind        == T.IDENTIFIER
    @test tok("1234x").kind           == T.INTEGER
    @test tok("1234x", 2).kind        == T.IDENTIFIER
end

@testset "floats with trailing `.` " begin
    @test tok("1.0").kind == Tokens.FLOAT
    @test tok("1.a").kind == Tokens.FLOAT
    @test tok("1.(").kind == Tokens.FLOAT
    @test tok("1.[").kind == Tokens.FLOAT
    @test tok("1.{").kind == Tokens.FLOAT
    @test tok("1.)").kind == Tokens.FLOAT
    @test tok("1.]").kind == Tokens.FLOAT
    @test tok("1.{").kind == Tokens.FLOAT
    @test tok("1.,").kind == Tokens.FLOAT
    @test tok("1.;").kind == Tokens.FLOAT
    @test tok("1.@").kind == Tokens.FLOAT
    @test tok("1.").kind == Tokens.FLOAT
    @test tok("1.\"text\" ").kind == Tokens.FLOAT

    @test tok("1..").kind  == Tokens.INTEGER
    @test T.kind.(collect(tokenize("1f0./1"))) == [T.FLOAT, T.OP, T.INTEGER, T.ENDMARKER]
end



@testset "lex octal" begin
    @test tok("0o0167").kind == T.OCT_INT
end

@testset "lex float/bin/hex/oct w underscores" begin
    @test tok("1_1.11").kind           == T.FLOAT
    @test tok("11.1_1").kind           == T.FLOAT
    @test tok("1_1.1_1").kind           == T.FLOAT
    @test tok("_1.1_1", 1).kind           == T.IDENTIFIER
    @test tok("_1.1_1", 2).kind           == T.FLOAT
    @test tok("0x0167_032").kind           == T.HEX_INT
    @test tok("0b0101001_0100_0101").kind  == T.BIN_INT
    @test tok("0o01054001_0100_0101").kind == T.OCT_INT
    @test T.kind.(collect(tokenize("1.2."))) == [T.ERROR, T.ENDMARKER]
    @test tok("1__2").kind == T.INTEGER
    @test tok("1.2_3").kind == T.FLOAT
    @test tok("1.2_3", 2).kind == T.ENDMARKER
    @test T.kind.(collect(tokenize("3e2_2"))) == [T.FLOAT, T.IDENTIFIER, T.ENDMARKER]
    @test T.kind.(collect(tokenize("1__2"))) == [T.INTEGER, T.IDENTIFIER, T.ENDMARKER]
    @test T.kind.(collect(tokenize("0x2_0_2"))) == [T.HEX_INT, T.ENDMARKER]
    @test T.kind.(collect(tokenize("0x2__2"))) == [T.HEX_INT, T.IDENTIFIER, T.ENDMARKER]
    @test T.kind.(collect(tokenize("3_2.5_2"))) == [T.FLOAT, T.ENDMARKER]
    @test T.kind.(collect(tokenize("3.2e2.2"))) == [T.ERROR, T.INTEGER, T.ENDMARKER]
    @test T.kind.(collect(tokenize("3e2.2"))) == [T.ERROR, T.INTEGER, T.ENDMARKER]
    @test T.kind.(collect(tokenize("0b101__101"))) == [T.BIN_INT, T.IDENTIFIER, T.ENDMARKER]
end

@testset "floating points" begin
    @test tok("1.0e0").kind  == Tokens.FLOAT
    @test tok("1.0e-0").kind == Tokens.FLOAT
    @test tok("1.0E0").kind  == Tokens.FLOAT
    @test tok("1.0E-0").kind == Tokens.FLOAT
    @test tok("1.0f0").kind  == Tokens.FLOAT
    @test tok("1.0f-0").kind == Tokens.FLOAT

    @test tok("0e0").kind    == Tokens.FLOAT
    @test tok("0e+0").kind   == Tokens.FLOAT
    @test tok("0E0").kind    == Tokens.FLOAT
    @test tok("201E+0").kind == Tokens.FLOAT
    @test tok("2f+0").kind   == Tokens.FLOAT
    @test tok("2048f0").kind == Tokens.FLOAT
    @test tok("1.:0").kind == Tokens.FLOAT
    @test tok("0x00p2").kind == Tokens.FLOAT
    @test tok("0x00P2").kind == Tokens.FLOAT
    @test tok("0x0.00p23").kind == Tokens.FLOAT
    @test tok("0x0.0ap23").kind == Tokens.FLOAT
    @test tok("0x0.0_0p2").kind == Tokens.FLOAT
    @test tok("0x0_0_0.0_0p2").kind == Tokens.FLOAT
    @test tok("0x0p+2").kind == Tokens.FLOAT
    @test tok("0x0p-2").kind == Tokens.FLOAT
end

@testset "1e1" begin
    @test tok("1e", 1).kind == Tokens.INTEGER
    @test tok("1e", 2).kind == Tokens.IDENTIFIER
end

@testset "jl06types" begin
    @test tok("mutable").kind   == Tokens.MUTABLE
    @test tok("primitive").kind == Tokens.PRIMITIVE
    @test tok("struct").kind    == Tokens.STRUCT
    @test tok("where").kind     == Tokens.WHERE
    @test tok("mutable struct s{T} where T",  1).kind == Tokens.MUTABLE
    @test tok("mutable struct s{T} where T",  3).kind == Tokens.STRUCT
    @test tok("mutable struct s{T} where T", 10).kind == Tokens.WHERE
end

@testset "CMDs" begin
    @test tok("`cmd`",1).kind == T.BACKTICK
    @test tok("`cmd`",2).kind == T.STRING
    @test tok("`cmd`",3).kind == T.BACKTICK
    @test tok("`cmd`",4).kind == T.ENDMARKER
    @test tok("```cmd```", 1).kind == T.TRIPLE_BACKTICK
    @test tok("```cmd```", 2).kind == T.STRING
    @test tok("```cmd```", 3).kind == T.TRIPLE_BACKTICK
    @test tok("```cmd```", 4).kind == T.ENDMARKER
    @test tok("```cmd````cmd`", 1).kind == T.TRIPLE_BACKTICK
    @test tok("```cmd````cmd`", 2).kind == T.STRING
    @test tok("```cmd````cmd`", 3).kind == T.TRIPLE_BACKTICK
    @test tok("```cmd````cmd`", 4).kind == T.BACKTICK
    @test tok("```cmd````cmd`", 5).kind == T.STRING
    @test tok("```cmd````cmd`", 6).kind == T.BACKTICK
    @test tok("```cmd````cmd`", 7).kind == T.ENDMARKER
end

@testset "where" begin
    @test tok("a where b", 3).kind == T.WHERE
end

@testset "IO position" begin
    io = IOBuffer("#1+1")
    skip(io, 1)
    @test length(collect(tokenize(io))) == 4
end

@testset "hex/bin/octal errors" begin
    @test tok("0x").kind == T.ERROR
    @test tok("0b").kind == T.ERROR
    @test tok("0o").kind == T.ERROR
    @test tok("0x 2", 1).kind == T.ERROR
    @test tok("0x.1p1").kind == T.FLOAT
end


@testset "dotted and suffixed operators" begin
ops = collect(values(Tokenize.Tokens.UNICODE_OPS_REVERSE))

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
            expr = Meta.parse(str, raise = false)
            if expr isa Expr && (expr.head != :error && expr.head != :incomplete)
                tokens = collect(tokenize(str))
                exop = expr.head == :call ? expr.args[1] : expr.head
                @test Symbol(Tokenize.Tokens.untokenize(tokens[arity == 1 ? 1 : 3])) == exop
            else
                break
            end
        end
    end
end
end

@testset "perp" begin
    @test tok("1 ⟂ 2", 3).kind==T.PERP
end

@testset "outer" begin
    @test tok("outer", 1).kind==T.OUTER
end

@testset "dot startpos" begin
    @test Tokenize.Tokens.startpos(tok("./")) == (1,1)
    @test Tokenize.Tokens.startbyte(tok(".≤")) == 0
end

@testset "token errors" begin
    @test tok("1.2e2.3",1).token_error === Tokens.INVALID_NUMERIC_CONSTANT
    @test tok("1.2.",1).token_error === Tokens.INVALID_NUMERIC_CONSTANT
    @test tok("1.2.f",1).token_error === Tokens.INVALID_NUMERIC_CONSTANT
    @test tok("0xv",1).token_error === Tokens.INVALID_NUMERIC_CONSTANT
    @test tok("0b3",1).token_error === Tokens.INVALID_NUMERIC_CONSTANT
    @test tok("0op",1).token_error === Tokens.INVALID_NUMERIC_CONSTANT
    @test tok("--",1).token_error === Tokens.INVALID_OPERATOR
    @test tok("1**2",2).token_error === Tokens.INVALID_OPERATOR
end

@testset "hat suffix" begin
    @test tok("ŝ", 1).kind==Tokens.IDENTIFIER
    @test untokenize(collect(tokenize("ŝ", Tokens.RawToken))[1], "ŝ") == "ŝ"
end

@testset "suffixed op" begin
    s = "+¹"
    @test Tokens.isoperator(tok(s, 1).kind)
    @test untokenize(collect(tokenize(s, Tokens.RawToken))[1], s) == s
end

@testset "invalid float juxt" begin
    s = "1.+2"
    @test tok(s, 1).kind == Tokens.ERROR
    @test Tokens.isoperator(tok(s, 2).kind)
    @test (t->t.val=="1234."    && t.kind == Tokens.ERROR )(tok("1234.+1")) # requires space before '.'
    @test tok("1.+ ").kind == Tokens.ERROR
    @test tok("1.⤋").kind  == Tokens.ERROR
    @test tok("1.?").kind == Tokens.ERROR
end

@testset "comments" begin
    s = "#=# text=#"
    @test length(collect(tokenize(s, Tokens.RawToken))) == 2
end

@testset "invalid hexadecimal" begin
    s = "0x."
    tok(s, 1).kind === Tokens.ERROR
end

@testset "circ arrow right op" begin
    s = "↻"
    @test collect(tokenize(s, Tokens.RawToken))[1].kind == Tokens.CIRCLE_ARROW_RIGHT
end

@testset "invalid float" begin
    s = ".0."
    @test collect(tokenize(s, Tokens.RawToken))[1].kind == Tokens.ERROR
end

@testset "allow prime after end" begin
    @test tok("begin end'", 4).kind === Tokens.PRIME
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
    @test all(s->Base.isoperator(Symbol(s)) == Tokens.isoperator(first(collect(tokenize(s))).kind), allops)
end

const all_kws = Set(["abstract",
    "as",
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
    "mutable",
    "outer",
    "primitive",
    "quote",
    "return",
    "struct",
    "try",
    "type",
    "using",
    "while",
    "in",
    "isa",
    "where",
    "true",
    "false",
])

function check_kw_hashes(iter)
    for cs in iter
        str = String([cs...])
        if Lexers.simple_hash(str) in keys(Tokenize.Lexers.kw_hash)
            @test str in all_kws
        end
    end
end

@testset "simple_hash" begin
    @test length(all_kws) == length(Tokenize.Lexers.kw_hash)

    @testset "Length $len keywords" for len in 1:5
        check_kw_hashes(String([cs...]) for cs in Iterators.product(['a':'z' for _ in 1:len]...))
    end
end
