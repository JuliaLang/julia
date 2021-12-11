using Tokenize.Tokens: Kind, isliteral, iskeyword, isoperator

include("token_kinds.jl")

"""
    K"s"

The full kind of a string "s".  For example, K")" is the kind of the
right parenthesis token.

Naming rules:
* Kinds which correspond to exactly one textural form are represented with that
  text. This includes keywords like K"for" and operators like K"*".
* Kinds which represent many textural forms have UpperCamelCase names. This
  includes kinds like K"Identifier" and K"Comment".
* Kinds which exist merely as delimiters are all uppercase
"""
macro K_str(str)
    get(_str_to_kind, str) do
        error("unknown token kind K$(repr(str))")
    end
end

kind(k::Kind) = k
kind(raw::TzTokens.RawToken) = TzTokens.exactkind(raw)

# Predicates for operator precedence
is_prec_assignment(t)  = K"BEGIN_ASSIGNMENTS" < kind(t) < K"END_ASSIGNMENTS"
is_prec_pair(t)        = K"BEGIN_PAIRARROW"   < kind(t) < K"END_PAIRARROW"
is_prec_conditional(t) = K"BEGIN_CONDITIONAL" < kind(t) < K"END_CONDITIONAL"
is_prec_arrow(t)       = K"BEGIN_ARROW"       < kind(t) < K"END_ARROW"
is_prec_lazy_or(t)     = K"BEGIN_LAZYOR"      < kind(t) < K"END_LAZYOR"
is_prec_lazy_and(t)    = K"BEGIN_LAZYAND"     < kind(t) < K"END_LAZYAND"
is_prec_comparison(t)  = K"BEGIN_COMPARISON"  < kind(t) < K"END_COMPARISON"
is_prec_pipe(t)        = K"BEGIN_PIPE"        < kind(t) < K"END_PIPE"
is_prec_colon(t)       = K"BEGIN_COLON"       < kind(t) < K"END_COLON"
is_prec_plus(t)        = K"BEGIN_PLUS"        < kind(t) < K"END_PLUS"
is_prec_bitshift(t)    = K"BEGIN_BITSHIFTS"   < kind(t) < K"END_BITSHIFTS"
is_prec_times(t)       = K"BEGIN_TIMES"       < kind(t) < K"END_TIMES"
is_prec_rational(t)    = K"BEGIN_RATIONAL"    < kind(t) < K"END_RATIONAL"
is_prec_power(t)       = K"BEGIN_POWER"       < kind(t) < K"END_POWER"
is_prec_decl(t)        = K"BEGIN_DECL"        < kind(t) < K"END_DECL"
is_prec_where(t)       = K"BEGIN_WHERE"       < kind(t) < K"END_WHERE"
is_prec_dot(t)         = K"BEGIN_DOT"         < kind(t) < K"END_DOT"
is_prec_unicode_ops(t) = K"BEGIN_UNICODE_OPS" < kind(t) < K"END_UNICODE_OPS"

is_prec_pipe_lt(t)     = kind(t) == K"<|"
is_prec_pipe_gt(t)     = kind(t) == K"|>"

# Operators which are boty unary and binary
function is_both_unary_and_binary(t)
    # TODO: Do we need to check dotop as well here?
    kind(t) in (K"$", K"&", K"~",             # <- dotop disallowed?
                K"+", K"-", K"⋆", K"±", K"∓") # dotop allowed
end

function is_number(t)
    kind(t) in (K"Integer", K"BinInt", K"HexInt", K"OctInt", K"Float")
end

function is_whitespace(t)
    kind(t) in (K"Whitespace", K"NewlineWs")
end

"""
Get the "binding power" (precedence level) of an operator kind
"""
function binding_power(k::Kind)
    return k < K"END_ASSIGNMENTS" ? 1  :
           k < K"END_CONDITIONAL" ? 2  :
           k < K"END_ARROW"       ? 3  :
           k < K"END_LAZYOR"      ? 4  :
           k < K"END_LAZYAND"     ? 5  :
           k < K"END_COMPARISON"  ? 6  :
           k < K"END_PIPE"        ? 7  :
           k < K"END_COLON"       ? 8  :
           k < K"END_PLUS"        ? 9  :
           k < K"END_BITSHIFTS"   ? 10 :
           k < K"END_TIMES"       ? 11 :
           k < K"END_RATIONAL"    ? 12 :
           k < K"END_POWER"       ? 13 :
           k < K"END_DECL"        ? 14 :
           k < K"END_WHERE"       ? 15 :
           k < K"END_DOT"         ? 16 :
           k < K"END_OPS"         ? 17 : # ?? unary ops
           error("Not an operator")
end

function _kind_str(k::Kind)
    u = untokenize(k)
    return !isnothing(u)                           ? u            :
           k in (K"Identifier", K"VarIdentifier")  ? "Identifier" :
           isliteral(k)                            ? "Literal"    :
           k == K"Comment"                         ? "Comment"    :
           k == K"Whitespace"                      ? "Whitespace" :
           k == K"NewlineWs"                       ? "NewlineWs"  :
           lowercase(string(k))
end

"""
Return the string representation of a token kind, or `nothing` if the kind
represents a class of tokens like K"Identifier".
"""
function untokenize(k::Kind)
    get(_kind_to_str_unique, k, nothing)
end

