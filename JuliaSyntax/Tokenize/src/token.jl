module Tokens

import Base.eof

export Token

include("token_kinds.jl")


iskeyword(k::Kind) = begin_keywords < k < end_keywords
isliteral(k::Kind) = begin_literal < k < end_literal
isoperator(k::Kind) = begin_ops < k < end_ops

iscontextualkeyword(k::Kind) = begin_contextual_keywords < k < end_contextual_keywords

function iswordoperator(k::Kind)
    # Keyword-like operators
    k == Tokens.IN ||
    k == Tokens.ISA ||
    k == Tokens.WHERE
end

# Create string => keyword kind
const KEYWORDS = Dict{String, Kind}()

function _add_kws()
    for k in instances(Kind)
        if iskeyword(k)
            KEYWORDS[lowercase(string(k))] = k
        end
    end
end
_add_kws()

# TODO: more
@enum(TokenError,
    NO_ERR,
    EOF_MULTICOMMENT,
    EOF_CHAR,
    INVALID_NUMERIC_CONSTANT,
    INVALID_OPERATOR,
    INVALID_INTERPOLATION_TERMINATOR,
    UNKNOWN,
)

# Error kind => description
TOKEN_ERROR_DESCRIPTION = Dict{TokenError, String}(
    EOF_MULTICOMMENT => "unterminated multi-line comment #= ... =#",
    EOF_CHAR => "unterminated character literal",
    INVALID_NUMERIC_CONSTANT => "invalid numeric constant",
    INVALID_OPERATOR => "invalid operator",
    INVALID_INTERPOLATION_TERMINATOR => "interpolated variable ends with invalid character; use `\$(...)` instead",
    UNKNOWN => "unknown",
)

struct Token
    kind::Kind
    # Offsets into a string or buffer
    startbyte::Int # The byte where the token start in the buffer
    endbyte::Int # The byte where the token ended in the buffer
    token_error::TokenError
    dotop::Bool
    suffix::Bool
end
function Token(kind::Kind, startbyte::Int, endbyte::Int)
    Token(kind, startbyte, endbyte, NO_ERR, false, false)
end
Token() = Token(ERROR, 0, 0, UNKNOWN, false, false)

const EMPTY_TOKEN = Token()

function kind(t::Token)
    isoperator(t.kind) && return OP
    iskeyword(t.kind) && return KEYWORD
    return t.kind
end
exactkind(t::Token) = t.kind

startbyte(t::Token) = t.startbyte
endbyte(t::Token) = t.endbyte


function untokenize(t::Token, str::String)
    String(codeunits(str)[1 .+ (t.startbyte:t.endbyte)])
end

function Base.show(io::IO, t::Token)
    print(io, rpad(string(startbyte(t), "-", endbyte(t)), 11, " "))
    print(io, rpad(kind(t), 15, " "))
end

end # module
