using .Tokenize.Tokens: Kind

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

"""
A set of kinds which can be used with the `in` operator.  For example

    k in KSet"+ - *"
"""
macro KSet_str(str)
    kinds = [get(_str_to_kind, s) do
         error("unknown token kind KSet\"$(repr(str)[2:end-1])\"")
    end
    for s in split(str)]

    quote
        ($(kinds...),)
    end
end

kind(k::Kind) = k
kind(raw::TzTokens.Token) = TzTokens.exactkind(raw)

# Some renaming for naming consistency
is_literal(k) = TzTokens.isliteral(kind(k))
is_keyword(k) = TzTokens.iskeyword(kind(k))
is_error(k::Kind) = TzTokens.iserror(k)
is_contextual_keyword(k) = TzTokens.iscontextualkeyword(kind(k))
is_operator(k) = TzTokens.isoperator(kind(k))
is_word_operator(k) = TzTokens.iswordoperator(kind(k))

# Predicates for operator precedence
# FIXME: Review how precedence depends on dottedness, eg
# https://github.com/JuliaLang/julia/pull/36725
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

#=
# Sholuld we optimize membership a bit by unrolling?
@generated function in(k::Kind, t::NTuple{N,Kind}) where {N}
    ex = :(k === t[1])
    for i = 2:N
        ex = :($ex || k === t[$i])
    end
    quote
        $ex
    end
end
=#

function is_syntax_kind(t)
    K"BEGIN_SYNTAX_KINDS" < kind(t) < K"END_SYNTAX_KINDS"
end

function is_identifier(k)
    kind(k) == K"Identifier"
end

function is_macro_name(k)
    K"BEGIN_MACRO_NAMES" < kind(k) < K"END_MACRO_NAMES"
end

function is_number(t)
    kind(t) in (K"Integer", K"BinInt", K"HexInt", K"OctInt", K"Float")
end

function is_string_delim(t)
    kind(t) in (K"\"", K"\"\"\"")
end

function is_radical_op(t)
    kind(t) in (K"√", K"∛", K"∜")
end

function is_whitespace(t)
    kind(t) in (K"Whitespace", K"NewlineWs")
end

"""
Return the string representation of a token kind, or `nothing` if the kind
represents a class of tokens like K"Identifier".

When `unique=true` only return a string when the kind uniquely defines the
corresponding input token, otherwise return `nothing`.  When `unique=false`,
return the name of the kind.

TODO: Replace `untokenize()` with `Base.string()`?
"""
function untokenize(k::Kind; unique=true)
    get(unique ? _kind_to_str_unique : _kind_to_str, k, nothing)
end
