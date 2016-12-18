using Tokenize
using Tokenize.Lexers
using Base.Test

for s in ["a", IOBuffer("a")]
    # IOBuffer indexing starts at 0, string indexing at 1
    # difference is only relevant for internals
    ob1 = isa(s, IOBuffer) ? 1 : 0

    l = tokenize(s)
    @test Lexers.readchar(l) == 'a'
    @test Lexers.prevpos(l) == 1 - ob1

    @test l.current_pos == 2 - ob1
    l_old = l
    @test Lexers.prevchar(l) == 'a'
    @test l == l_old
    @test Lexers.eof(l)
    @test Lexers.readchar(l) == Lexers.EOF_CHAR

    Lexers.backup!(l)
    @test Lexers.prevpos(l) == -1
    @test l.current_pos == 2 - ob1
end

# correctly tokenizes simple unicode expressions:
str = "𝘋 =2β"
for s in [str, IOBuffer(str)]
    l = tokenize(s)
    kinds = [Tokens.IDENTIFIER, Tokens.WHITESPACE, Tokens.OP,
             Tokens.INTEGER, Tokens.IDENTIFIER, Tokens.ENDMARKER]
    token_strs = ["𝘋", " ", "=", "2", "β", ""]
    for (i, n) in enumerate(l)
        @test Tokens.kind(n) == kinds[i]
        @test untokenize(n)  == token_strs[i]
        @test Tokens.startpos(n) == (1, i)
        @test Tokens.endpos(n) == (1, i - 1 + length(token_strs[i]))
    end
end

const T = Tokenize.Tokens

# correctly tokenizes a complex piece of code
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

# test roundtrippability
@test join(untokenize.(collect(tokenize(str)))) == str

# test #5
@test Tokens.kind.(collect(tokenize("1.23..3.21"))) == [T.FLOAT,T.OP,T.FLOAT,T.ENDMARKER]
