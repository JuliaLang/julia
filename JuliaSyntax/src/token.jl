module Tokens

import Base.eof

export Token

include("token_kinds.jl")


iskeyword(k::Kind) = begin_keywords < k < end_keywords
isliteral(k::Kind) = begin_literal < k < end_literal
isoperator(k::Kind) = begin_ops < k < end_ops

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
    EOF_STRING,
    EOF_CHAR,
    EOF_CMD,
    INVALID_NUMERIC_CONSTANT,
    INVALID_OPERATOR,
    UNKNOWN,
)

# Error kind => description
TOKEN_ERROR_DESCRIPTION = Dict{TokenError, String}(
    EOF_MULTICOMMENT => "unterminated multi-line comment #= ... =#",
    EOF_STRING => "unterminated string literal",
    EOF_CHAR => "unterminated character literal",
    EOF_CMD => "unterminated cmd literal",
    INVALID_NUMERIC_CONSTANT => "invalid numeric constant",
    INVALID_OPERATOR => "invalid operator",
    UNKNOWN => "unknown",
)

abstract type AbstractToken end

struct Token <: AbstractToken
    kind::Kind
    # Offsets into a string or buffer
    startpos::Tuple{Int, Int} # row, col where token starts /end, col is a string index
    endpos::Tuple{Int, Int}
    startbyte::Int # The byte where the token start in the buffer
    endbyte::Int # The byte where the token ended in the buffer
    val::String # The actual string of the token
    token_error::TokenError
    dotop::Bool
    suffix::Bool
end
function Token(kind::Kind, startposition::Tuple{Int, Int}, endposition::Tuple{Int, Int},
    startbyte::Int, endbyte::Int, val::String)
Token(kind, startposition, endposition, startbyte, endbyte, val, NO_ERR, false, false)
end
Token() = Token(ERROR, (0,0), (0,0), 0, 0, "", UNKNOWN, false, false)

struct RawToken <: AbstractToken
    kind::Kind
    # Offsets into a string or buffer
    startpos::Tuple{Int, Int} # row, col where token starts /end, col is a string index
    endpos::Tuple{Int, Int}
    startbyte::Int # The byte where the token start in the buffer
    endbyte::Int # The byte where the token ended in the buffer
    token_error::TokenError
    dotop::Bool
    suffix::Bool
end
function RawToken(kind::Kind, startposition::Tuple{Int, Int}, endposition::Tuple{Int, Int},
    startbyte::Int, endbyte::Int)
RawToken(kind, startposition, endposition, startbyte, endbyte, NO_ERR, false, false)
end
RawToken() = RawToken(ERROR, (0,0), (0,0), 0, 0, UNKNOWN, false, false)


const _EMPTY_TOKEN = Token()
const _EMPTY_RAWTOKEN = RawToken()
EMPTY_TOKEN(::Type{Token}) = _EMPTY_TOKEN
EMPTY_TOKEN(::Type{RawToken}) = _EMPTY_RAWTOKEN

function kind(t::AbstractToken)
    isoperator(t.kind) && return OP
    iskeyword(t.kind) && return KEYWORD
    return t.kind
end
exactkind(t::AbstractToken) = t.kind
startpos(t::AbstractToken) = t.startpos
endpos(t::AbstractToken) = t.endpos
startbyte(t::AbstractToken) = t.startbyte
endbyte(t::AbstractToken) = t.endbyte
function untokenize(t::Token)
    if t.kind == IDENTIFIER || isliteral(t.kind) || t.kind == COMMENT || t.kind == WHITESPACE || t.kind == ERROR
        return t.val
    elseif iskeyword(t.kind)
        return lowercase(string(t.kind))
    elseif isoperator(t.kind)
        if t.dotop
            str = string(".", UNICODE_OPS_REVERSE[t.kind]) 
        else 
            str = string(UNICODE_OPS_REVERSE[t.kind]) 
        end 
        return string(str, t.val) 
    elseif t.kind == LPAREN
        return "("
    elseif t.kind == LSQUARE
        return "["
    elseif t.kind == LBRACE
        return "{"
    elseif t.kind == RPAREN
        return ")"
    elseif t.kind == RSQUARE
        return "]"
    elseif t.kind == RBRACE
        return "}"
    elseif t.kind == AT_SIGN
        return "@"
    elseif t.kind == COMMA
        return ","
    elseif t.kind == SEMICOLON
        return ";"
    else
        return ""
    end
end

function untokenize(t::RawToken, str::String)
    String(codeunits(str)[1 .+ (t.startbyte:t.endbyte)])
end

function untokenize(ts)
    if !(eltype(ts) <: AbstractToken)
        throw(ArgumentError("element type of iterator has to be Token"))
    end
    io = IOBuffer()
    for tok in ts
        write(io, untokenize(tok))
    end
    return String(take!(io))
end


function Base.show(io::IO, t::Token)
    start_r, start_c = startpos(t)
    end_r, end_c = endpos(t)
    str = kind(t) == ENDMARKER ? "" : escape_string(untokenize(t))
    print(io, rpad(string(start_r, ",", start_c, "-", end_r, ",", end_c), 17, " "))
    print(io, rpad(kind(t), 15, " "))
    print(io, "\"", str, "\"")
end

Base.print(io::IO, t::Token) = print(io, untokenize(t))

function Base.show(io::IO, t::RawToken)
    start_r, start_c = startpos(t)
    end_r, end_c = endpos(t)
    print(io, rpad(string(start_r, ",", start_c, "-", end_r, ",", end_c), 17, " "))
    print(io, rpad(kind(t), 15, " "))
end

end # module
