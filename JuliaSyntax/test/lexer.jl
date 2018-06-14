using Tokenize
using Tokenize.Lexers
using Test

const T = Tokenize.Tokens

tok(str, i = 1) = collect(tokenize(str))[i]

@testset "tokens" begin
    for s in ["a", IOBuffer("a")]
        l = tokenize(s)
        @test Lexers.readchar(l) == 'a'

        @test l.current_pos == 0
        l_old = l
        @test l == l_old
        @test Lexers.eof(l)
        @test Lexers.readchar(l) == Lexers.EOF_CHAR

        @test l.current_pos == 0
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

            T.WHITESPACE,T.AT_SIGN,T.IDENTIFIER,T.WHITESPACE,T.LPAREN,
            T.IDENTIFIER,T.OP,T.IDENTIFIER,T.COMMA,T.WHITESPACE,
            T.IDENTIFIER,T.OP,T.IDENTIFIER,T.RPAREN,T.SEMICOLON,

            T.WHITESPACE,T.KEYWORD,

            T.WHITESPACE,T.KEYWORD,
            T.WHITESPACE,T.IDENTIFIER,
            T.WHITESPACE,T.KEYWORD,
            T.WHITESPACE,T.IDENTIFIER,
            T.WHITESPACE,T.KEYWORD,

            T.WHITESPACE,T.AT_SIGN,T.IDENTIFIER,T.WHITESPACE,T.IDENTIFIER,
            T.OP,T.IDENTIFIER,

            T.WHITESPACE,T.IDENTIFIER,T.LSQUARE,T.LSQUARE,T.INTEGER,T.WHITESPACE,
            T.INTEGER,T.WHITESPACE,T.INTEGER,T.RSQUARE,T.RSQUARE,

            T.WHITESPACE,T.LSQUARE,T.INTEGER,T.OP,T.INTEGER,T.COMMA,T.INTEGER,
            T.SEMICOLON,T.INTEGER,T.COMMA,T.INTEGER,T.RSQUARE,

            T.WHITESPACE,T.STRING,T.SEMICOLON,T.WHITESPACE,T.CHAR,

            T.WHITESPACE,T.LPAREN,T.IDENTIFIER,T.OP,T.IDENTIFIER,T.RPAREN,T.OP,
            T.LPAREN,T.IDENTIFIER,T.OP,T.IDENTIFIER,T.RPAREN,

            T.WHITESPACE,T.COMMENT,

            T.WHITESPACE,T.COMMENT,

            T.WHITESPACE,T.INTEGER,T.OP,T.INTEGER,

            T.WHITESPACE,T.IDENTIFIER,T.OP,T.OP,T.IDENTIFIER,T.OP,

            T.WHITESPACE,T.IDENTIFIER,T.OP,T.OP,T.OP,T.IDENTIFIER,T.OP,T.OP,

            T.WHITESPACE,T.CMD,

            T.WHITESPACE,T.INTEGER,T.IDENTIFIER,T.LPAREN,T.INTEGER,T.RPAREN,

            T.WHITESPACE,T.LBRACE,T.RBRACE,

            T.WHITESPACE,T.ERROR,T.ENDMARKER]

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


@testset "tokenizing juxtaposed numbers and dotted operators/identifiers" begin
    @test (t->t.val=="1234"    && t.kind == Tokens.INTEGER )(tok("1234.+1"))
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

    kinds = [T.COMMENT, T.WHITESPACE,
             T.TRIPLE_STRING, T.WHITESPACE,
             T.INTEGER, T.WHITESPACE,
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
                    "baremodule",
                    "begin",
                    "bitstype",
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
                    "immutable",
                    "import",
                    "importall",
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
                    "typealias",
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
    @test tok("aa \"   ",               3).kind == T.ERROR
    @test tok("aa \"\"\" \"dsad\" \"\"",3).kind == T.ERROR

end

@testset "xor_eq" begin
    @test tok("1 ⊻= 2", 3).kind==T.XOR_EQ
end

@testset "lex binary" begin
    @test tok("0b0101").kind==T.BIN_INT
end

@testset "show" begin
    io = IOBuffer()
    show(io, collect(tokenize("\"abc\nd\"ef"))[1])
    @test String(take!(io)) == "1,1-2,2          STRING         \"\\\"abc\\nd\\\"\""
end


@testset "interpolation" begin
    ts = collect(tokenize(""""str: \$(g("str: \$(h("str"))"))" """))
    @test length(ts)==3
    @test ts[1].kind == Tokens.STRING
    ts = collect(tokenize("""\"\$\""""))
    @test ts[1].kind == Tokens.STRING
    # issue 73:
    t_err = tok("\"\$(fdsf\"")
    @test t_err.kind == Tokens.ERROR
    @test t_err.token_error == Tokens.EOF_STRING
    @test Tokenize.Tokens.startpos(t_err) == (1,1)
    @test Tokenize.Tokens.endpos(t_err) == (1,8)
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

    @test tok("1.+ ").kind == Tokens.INTEGER
    @test tok("1.⤋").kind  == Tokens.INTEGER
    @test tok("1..").kind  == Tokens.INTEGER
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
    @test tok("1.?").kind == Tokens.FLOAT
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
    @test tok("`cmd`").kind == T.CMD
    @test tok("```cmd```", 1).kind == T.TRIPLE_CMD
    @test tok("```cmd```", 2).kind == T.ENDMARKER
    @test tok("```cmd````cmd`", 1).kind == T.TRIPLE_CMD
    @test tok("```cmd````cmd`", 2).kind == T.CMD
end

@testset "where" begin
    @test tok("a where b", 3).kind == T.WHERE
end

@testset "IO position" begin
    io = IOBuffer("#1+1")
    skip(io, 1)
    @test length(collect(tokenize(io))) == 4
end

@testset "complicated interpolations" begin
    @test length(collect(tokenize("\"\$(())\""))) == 2
    @test length(collect(tokenize("\"\$(#=inline ) comment=#\"\")\""))) == 2
    @test length(collect(tokenize("\"\$(string(`inline ')' cmd`)\"\")\""))) == 2
    # These would require special interpolation support in the parse (Base issue #3150).
    # If that gets implemented, thses should all be adjust to `== 2`
    @test length(collect(tokenize("`\$((``))`"))) == 3
    @test length(collect(tokenize("`\$(#=inline ) comment=#``)`"))) == 3
    @test length(collect(tokenize("`\$(\"inline ) string\"*string(``))`"))) == 3
end


@testset "hex/bin/octal errors" begin
@test tok("0x").kind == T.ERROR
@test tok("0b").kind == T.ERROR
@test tok("0o").kind == T.ERROR
@test tok("0x 2", 1).kind == T.ERROR
@test tok("0x.1p1").kind == T.FLOAT
end


@testset "dotted and suffixed operators" begin
ops = collect(values(Main.Tokenize.Tokens.UNICODE_OPS_REVERSE))

for op in ops
    op in (:isa, :in, :where, Symbol('\''), :?, :(:)) && continue
    str1 = "$(op)b"
    str2 = ".$(op)b"
    str3 = "a $op b"
    str4 = "a .$op b"
    str5 = "a $(op)₁ b"
    str6 = "a .$(op)₁ b"
    ex1 = Meta.parse(str1, raise = false)
    ex2 = Meta.parse(str2, raise = false)
    ex3 = Meta.parse(str3, raise = false)
    ex4 = Meta.parse(str4, raise = false)
    ex5 = Meta.parse(str5, raise = false)
    ex6 = Meta.parse(str6, raise = false)
    if ex1.head != :error # unary
        t1 = collect(tokenize(str1))
        exop1 = ex1.head == :call ? ex1.args[1] : ex1.head
        @test Symbol(Tokenize.Tokens.untokenize(t1[1])) == exop1
        if ex2.head != :error
            t2 = collect(tokenize(str2))
            exop2 = ex2.head == :call ? ex2.args[1] : ex2.head
            @test Symbol(Tokenize.Tokens.untokenize(t2[1])) == exop2
        end
    elseif ex3.head != :error # binary
        t3 = collect(tokenize(str3))
        exop3 = ex3.head == :call ? ex3.args[1] : ex3.head
        @test Symbol(Tokenize.Tokens.untokenize(t3[3])) == exop3
        if ex4.head != :error
            t4 = collect(tokenize(str4))
            exop4 = ex4.head == :call ? ex4.args[1] : ex4.head
            @test Symbol(Tokenize.Tokens.untokenize(t4[3])) == exop4
        elseif ex5.head != :error
            t5 = collect(tokenize(str5))
            exop5 = ex5.head == :call ? ex5.args[1] : ex5.head
            @test Symbol(Tokenize.Tokens.untokenize(t5[3])) == exop5
        elseif ex6.head != :error
            t6 = collect(tokenize(str6))
            exop6 = ex6.head == :call ? ex6.args[1] : ex6.head
            @test Symbol(Tokenize.Tokens.untokenize(t6[3])) == exop6
        end
    end
end
end

@testset "perp" begin 
    @test tok("1 ⟂ 2", 3).kind==T.PERP 
end
