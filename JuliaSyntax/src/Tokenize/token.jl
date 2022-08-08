module Tokens

using ...JuliaSyntax: Kind, @K_str

import Base.eof

export Token

include("token_kinds.jl")


iskeyword(k::Kind) = K"BEGIN_KEYWORDS" < k < K"END_KEYWORDS"
isliteral(k::Kind) = K"BEGIN_LITERAL" < k < K"END_LITERAL"
isoperator(k::Kind) = K"BEGIN_OPS" < k < K"END_OPS"
iserror(k::Kind) = K"BEGIN_ERRORS" < k < K"END_ERRORS"
iscontextualkeyword(k::Kind) = K"BEGIN_CONTEXTUAL_KEYWORDS" < k < K"END_CONTEXTUAL_KEYWORDS"

function iswordoperator(k::Kind)
    # Keyword-like operators
    k == K"in" ||
    k == K"isa" ||
    k == K"where"
end

# Error kind => description
TOKEN_ERROR_DESCRIPTION = Dict{Kind, String}(
    K"ErrorEofMultiComment" => "unterminated multi-line comment #= ... =#",
    K"ErrorEofChar" => "unterminated character literal",
    K"ErrorInvalidNumericConstant" => "invalid numeric constant",
    K"ErrorInvalidOperator" => "invalid operator",
    K"ErrorInvalidInterpolationTerminator" => "interpolated variable ends with invalid character; use `\$(...)` instead",
    K"error" => "unknown error",
)

struct Token
    kind::Kind
    # Offsets into a string or buffer
    startbyte::Int # The byte where the token start in the buffer
    endbyte::Int # The byte where the token ended in the buffer
    dotop::Bool
    suffix::Bool
end
function Token(kind::Kind, startbyte::Int, endbyte::Int)
    Token(kind, startbyte, endbyte, false, false)
end
Token() = Token(K"error", 0, 0, false, false)

const EMPTY_TOKEN = Token()

exactkind(t::Token) = t.kind

startbyte(t::Token) = t.startbyte
endbyte(t::Token) = t.endbyte


function untokenize(t::Token, str::String)
    String(codeunits(str)[1 .+ (t.startbyte:t.endbyte)])
end

function Base.show(io::IO, t::Token)
    print(io, rpad(string(startbyte(t), "-", endbyte(t)), 11, " "))
    print(io, rpad(exactkind(t), 15, " "))
end

end # module
