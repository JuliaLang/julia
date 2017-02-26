module Tokens

using Compat
import Compat.String
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
  UNKNOWN,
)

# Error kind => description
TOKEN_ERROR_DESCRIPTION = Dict{TokenError, String}(
  EOF_MULTICOMMENT => "unterminated multi-line comment #= ... =#",
  EOF_STRING => "unterminated string literal",
  EOF_CHAR => "unterminated character literal",
  EOF_CMD => "unterminated cmd literal",
  UNKNOWN => "unknown",
)

immutable Token
    kind::Kind
    # Offsets into a string or buffer
    startpos::Tuple{Int, Int} # row, col where token starts /end, col is a string index
    endpos::Tuple{Int, Int}
    startbyte::Int64 # The byte where the token start in the buffer
    endbyte::Int64 # The byte where the token ended in the buffer
    val::Compat.UTF8String # The actual string of the token
    token_error::TokenError
end

function Token(kind::Kind, startposition::Tuple{Int, Int}, endposition::Tuple{Int, Int},
               startbyte::Int64, endbyte::Int64, val::String)
    Token(kind, startposition, endposition, startbyte, endbyte, val, NO_ERR)
end
Token() = Token(ERROR, (0,0), (0,0), 0, 0, "", UNKNOWN)

function kind(t::Token)
    isoperator(t.kind) && return OP
    iskeyword(t.kind) && return KEYWORD
    return t.kind
end
exactkind(t::Token) = t.kind
startpos(t::Token) = t.startpos
endpos(t::Token) = t.endpos
untokenize(t::Token) = t.val
function untokenize(ts)
    if eltype(ts) != Token
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

eof(t::Token) = t.kind == Eof

end # module
