module Tokenize

export tokenize, untokenize

using ..JuliaSyntax: JuliaSyntax, Kind, @K_str, @KSet_str, @callsite_inline,
    generic_operators_by_level, PrecedenceLevel, PREC_NONE, PREC_ASSIGNMENT,
    PREC_PAIRARROW, PREC_CONDITIONAL, PREC_ARROW, PREC_LAZYOR, PREC_LAZYAND,
    PREC_COMPARISON, PREC_PIPE_LT, PREC_PIPE_GT, PREC_COLON, PREC_PLUS,
    PREC_BITSHIFT, PREC_TIMES, PREC_RATIONAL, PREC_POWER, PREC_DECL,
    PREC_WHERE, PREC_DOT, PREC_QUOTE, PREC_UNICODE_OPS, PREC_COMPOUND_ASSIGN

import ..JuliaSyntax: kind,
    is_literal, is_contextual_keyword, is_word_operator, is_operator

#-------------------------------------------------------------------------------
# Character-based predicates for tokenization
import Base.Unicode

const EOF_CHAR = typemax(Char)

function is_identifier_char(c::Char)
    c == EOF_CHAR && return false
    isvalid(c) || return false
    return Base.is_id_char(c)
end

function is_identifier_start_char(c::Char)
    c == EOF_CHAR && return false
    isvalid(c) || return false
    c == '🢲' && return false  # First divergence from Base.is_id_start_char
    return Base.is_id_start_char(c)
end

function is_invisible_char(c::Char)
    # These are the chars considered invisible by the reference parser.
    # TODO: There's others we could add? See for example
    # https://invisible-characters.com/
    return c == '\u00ad' || # soft hyphen
           c == '\u200b' || # zero width space
           c == '\u200c' || # zero width non-joiner
           c == '\u200d' || # zero width joiner
           c == '\u200e' || # left-to-right mark
           c == '\u200f' || # right-to-left mark
           c == '\u2060' || # word joiner
           c == '\u2061'    # function application
    # https://github.com/JuliaLang/julia/issues/49850
    # c == '\u115f' || # Hangul Choseong filler
end

# Chars that we will never allow to be part of a valid non-operator identifier
function is_never_id_char(ch::Char)
    isvalid(ch) || return true
    cat = Unicode.category_code(ch)
    c = UInt32(ch)
    return (
        # spaces and control characters:
        (cat >= Unicode.UTF8PROC_CATEGORY_ZS && cat <= Unicode.UTF8PROC_CATEGORY_CS) ||

        # ASCII and Latin1 non-connector punctuation
        (c < 0xff &&
         cat >= Unicode.UTF8PROC_CATEGORY_PD && cat <= Unicode.UTF8PROC_CATEGORY_PO) ||

        c == UInt32('`') ||

        # mathematical brackets
        (c >= 0x27e6 && c <= 0x27ef) ||
        # angle, corner, and lenticular brackets
        (c >= 0x3008 && c <= 0x3011) ||
        # tortoise shell, square, and more lenticular brackets
        (c >= 0x3014 && c <= 0x301b) ||
        # fullwidth parens
        (c == 0xff08 || c == 0xff09) ||
        # fullwidth square brackets
        (c == 0xff3b || c == 0xff3d)
    )
end

readchar(io::IO) = eof(io) ? EOF_CHAR : read(io, Char)

function _char_in_set_expr(varname, firstchars)
    codes = sort!(UInt32.(unique(firstchars)))
    terms = []
    i = 1
    while i <= length(codes)
        j = i
        while j < length(codes) && codes[j+1] == codes[j]+1
            j += 1
        end
        if i == j
            push!(terms, :($varname == $(codes[i])))
        else
            push!(terms, :($(codes[i]) <= $varname <= $(codes[j])))
        end
        i = j+1
    end
    foldr((t1,t2)->:($t1 || $t2), terms)
end

@eval function is_operator_start_char(c)
   if c == EOF_CHAR || !isvalid(c)
       return false
   end
   # Check if character is a known operator char or in our unicode ops
   # dictionary. The second tuple lists the unicode operators which don't
   # appear in `_unicode_ops` because they have their own kinds (and explicit
   # branches in `_next_token`).
   return c in ('!', '#', '$', '%', '&', '*', '+', '-', '−', '/', ':', '<', '=', '>', '?', '@', '\\', '^', '|', '~', '÷', '⊻', '\'') ||
          c in ('∈', '≔', '⩴', '≕', '¬', '√', '∛', '∜') ||
          haskey(_unicode_ops, c)
end

# Checks whether a Char is an operator which can be prefixed with a dot `.`
function is_dottable_operator_start_char(c)
    return c != '?' && c != '$' && c != ':' && c != '\'' && c != '#' && c != '@' && is_operator_start_char(c)
end

@eval function isopsuffix(c::Char)
    c == EOF_CHAR && return false
    isvalid(c) || return false
    u = UInt32(c)
    if (u < 0xa1 || u > 0x10ffff)
        return false
    end
    cat = Base.Unicode.category_code(u)
    if (cat == Base.Unicode.UTF8PROC_CATEGORY_MN ||
        cat == Base.Unicode.UTF8PROC_CATEGORY_MC ||
        cat == Base.Unicode.UTF8PROC_CATEGORY_ME)
        return true
    end
    # Additional allowed cases
    return $(_char_in_set_expr(:u,
        collect("²³¹ʰʲʳʷʸˡˢˣ˱˲ᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁᵂᵃᵅᵇᵈᵉᵋᵍᵏᵐᵒᵖᵗᵘᵛᵝᵞᵟᵠᵡᵢᵣᵤᵥᵦᵧᵨᵩᵪᶜᶠᶥᶦᶫᶰᶲᶸᶻᶿ′″‴‵‶‷⁗⁰ⁱ⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ⁿ₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ₐₑₒₓₔₕₖₗₘₙₚₛₜⱼⱽꜛꜜꜝ")))
end

function optakessuffix(k)
    # Most operators can take suffix except for specific ones
    is_operator(k) &&
    !(
        K"BEGIN_ASSIGNMENTS" <= k <= K"END_ASSIGNMENTS" ||
        k == K"?"   ||
        k == K"<:"  ||
        k == K">:"  ||
        k == K"&&"  ||
        k == K"||"  ||
        k == K"in"  ||
        k == K"isa" ||
        k == K":"   ||
        k == K"$"   ||
        k == K"::"  ||
        k == K"where" ||
        k == K"."   ||
        k == K"!"   ||
        k == K".'"  ||
        k == K"->"  ||
        K"¬" <= k <= K"∜"
    )
end

const _unicode_ops = let
    # Map single-character unicode operators to their precedence levels
    ops = Dict{Char, PrecedenceLevel}()

    # Add operators from generic_operators_by_level
    for (prec, chars) in generic_operators_by_level
        for c in chars
            ops[c] = prec
        end
    end

    ops
end

#-------------------------------------------------------------------------------
# Tokens

struct RawToken
    kind::Kind
    # Offsets into a string or buffer
    startbyte::Int # The byte where the token start in the buffer
    endbyte::Int # The byte where the token ended in the buffer
    op_precedence::PrecedenceLevel # If K"Operator", the operator's precedence level
end
function RawToken(kind::Kind, startbyte::Int, endbyte::Int)
    RawToken(kind, startbyte, endbyte, PREC_NONE)
end
RawToken() = RawToken(K"error", 0, 0, PREC_NONE)

const EMPTY_TOKEN = RawToken()

kind(t::RawToken) = t.kind

startbyte(t::RawToken) = t.startbyte
endbyte(t::RawToken) = t.endbyte


function untokenize(t::RawToken, str::String)
    String(codeunits(str)[1 .+ (t.startbyte:t.endbyte)])
end

function Base.show(io::IO, t::RawToken)
    print(io, rpad(string(startbyte(t), "-", endbyte(t)), 11, " "))
    print(io, rpad(kind(t), 15, " "))
end

#-------------------------------------------------------------------------------
# Lexer

@inline ishex(c::Char) = isdigit(c) || ('a' <= c <= 'f') || ('A' <= c <= 'F')
@inline isbinary(c::Char) = c == '0' || c == '1'
@inline isoctal(c::Char) =  '0' ≤ c ≤ '7'
@inline iswhitespace(c::Char) = (isvalid(c) && Base.isspace(c)) || c === '\ufeff'

struct StringState
    triplestr::Bool
    raw::Bool
    delim::Char
    paren_depth::Int
end

"""
`Lexer` reads from an input stream and emits a single token each time
`next_token` is called.

Ideally a lexer is stateless but some state is needed here for:
* Disambiguating cases like x' (adjoint) vs 'x' (character literal)
* Tokenizing code within string interpolations
"""
mutable struct Lexer{IO_t <: IO}
    io::IO_t

    token_startpos::Int

    last_token::Kind
    string_states::Vector{StringState}
    chars::Tuple{Char,Char,Char,Char}
    charspos::Tuple{Int,Int,Int,Int}
end

function Lexer(io::IO)
    c1 = ' '
    p1 = position(io)
    if eof(io)
        c2, p2 = EOF_CHAR, p1
        c3, p3 = EOF_CHAR, p1
        c4, p4 = EOF_CHAR, p1
    else
        c2 = read(io, Char)
        p2 = position(io)
        if eof(io)
            c3, p3 = EOF_CHAR, p2
            c4, p4 = EOF_CHAR, p2
        else
            c3 = read(io, Char)
            p3 = position(io)
            if eof(io)
                c4, p4 = EOF_CHAR, p3
            else
                c4 = read(io, Char)
                p4 = position(io)
            end
        end
    end
    Lexer(io, position(io),
                  K"error", Vector{StringState}(),
                  (c1,c2,c3,c4), (p1,p2,p3,p4))
end
Lexer(str::AbstractString) = Lexer(IOBuffer(str))

"""
    tokenize(x)

Returns an `Iterable` containing the tokenized input. Can be reverted by e.g.
`join(untokenize.(tokenize(x)))`.
"""
tokenize(x) = Lexer(x)

# Iterator interface
Base.IteratorSize(::Type{<:Lexer}) = Base.SizeUnknown()
Base.IteratorEltype(::Type{<:Lexer}) = Base.HasEltype()
Base.eltype(::Type{<:Lexer}) = RawToken


function Base.iterate(l::Lexer)
    l.token_startpos = position(l)

    t = next_token(l)
    return t, t.kind == K"EndMarker"
end

function Base.iterate(l::Lexer, isdone::Any)
    isdone && return nothing
    t = next_token(l)
    return t, t.kind == K"EndMarker"
end

function Base.show(io::IO, l::Lexer)
    print(io, typeof(l), " at position: ", position(l))
end

"""
    startpos(l::Lexer)

Return the latest `RawToken`'s starting position.
"""
startpos(l::Lexer) = l.token_startpos

"""
    startpos!(l::Lexer, i::Integer)

Set a new starting position.
"""
startpos!(l::Lexer, i::Integer) = l.token_startpos = i

"""
    peekchar(l::Lexer)

Returns the next character without changing the lexer's state.
"""
peekchar(l::Lexer) = l.chars[2]

"""
dpeekchar(l::Lexer)

Returns the next two characters without changing the lexer's state.
"""
dpeekchar(l::Lexer) = l.chars[2], l.chars[3]

"""
peekchar3(l::Lexer)

Returns the next three characters without changing the lexer's state.
"""
peekchar3(l::Lexer) = l.chars[2], l.chars[3], l.chars[4]

"""
    position(l::Lexer)

Returns the current position.
"""
Base.position(l::Lexer) = l.charspos[1]

"""
    eof(l::Lexer)

Determine whether the end of the lexer's underlying buffer has been reached.
"""
Base.eof(l::Lexer) = eof(l.io)

Base.seek(l::Lexer, pos) = seek(l.io, pos)

"""
    start_token!(l::Lexer)

Updates the lexer's state such that the next  `RawToken` will start at the current
position.
"""
function start_token!(l::Lexer)
    l.token_startpos = l.charspos[1]
end

"""
    readchar(l::Lexer)

Returns the next character and increments the current position.
"""
function readchar(l::Lexer)
    c = readchar(l.io)
    l.chars = (l.chars[2], l.chars[3], l.chars[4], c)
    l.charspos = (l.charspos[2], l.charspos[3], l.charspos[4], position(l.io))
    return l.chars[1]
end

"""
    accept(l::Lexer, f::Union{Function, Char, Vector{Char}, String})

Consumes the next character `c` if either `f::Function(c)` returns true, `c == f`
for `c::Char` or `c in f` otherwise. Returns `true` if a character has been
consumed and `false` otherwise.
"""
@inline function accept(l::Lexer, f::Union{Function, Char, Vector{Char}, String})
    c = peekchar(l)
    if isa(f, Function)
        ok = f(c)
    elseif isa(f, Char)
        ok = c == f
    else
        ok = c in f
    end
    ok && readchar(l)
    return ok
end

"""
    accept_batch(l::Lexer, f)

Consumes all following characters until `accept(l, f)` is `false`.
"""
@inline function accept_batch(l::Lexer, f)
    ok = false
    while accept(l, f)
        ok = true
    end
    return ok
end

"""
    emit(l::Lexer, kind::Kind)

Returns a `RawToken` of kind `kind` with contents `str` and starts a new `RawToken`.
"""
function emit(l::Lexer, kind::Kind)
    tok = RawToken(kind, startpos(l), position(l) - 1, PREC_NONE)

    l.last_token = kind
    return tok
end

function emit_operator(l::Lexer, kind::Kind, precedence::PrecedenceLevel, take_suffix=optakessuffix(kind))
    if take_suffix
        while isopsuffix(peekchar(l))
            readchar(l)
            kind = K"Operator"
        end
    end
    tok = RawToken(kind, startpos(l), position(l) - 1, precedence)

    l.last_token = kind
    return tok
end

# Check whether the operator just lexed forms a compound assignment like `+=`,
# ie whether the input continues with the single token `=`. Peeking one
# character is not enough: the other tokens starting with `=` (`==`, `===` and
# `=>`) do not form compound assignments, eg `a +== b` is `a + (== b)`.
function compound_assign_follows(l::Lexer)
    pc, ppc = dpeekchar(l)
    return pc == '=' && ppc != '=' && ppc != '>'
end

# Emit `kind` with precedence `prec`, unless the operator is immediately followed
# by `=`, in which case it forms a compound assignment like `+=` and is emitted as
# a `K"Operator"` with `PREC_COMPOUND_ASSIGN`.
function emit_operator_or_compound_assign(l::Lexer, kind::Kind, prec::PrecedenceLevel)
    if compound_assign_follows(l)
        return emit_operator(l, K"Operator", PREC_COMPOUND_ASSIGN)
    end
    return emit_operator(l, kind, prec)
end

function emit_trivia(l::Lexer, kind::Kind)
    tok = RawToken(kind, startpos(l), position(l) - 1, PREC_NONE)
    l.last_token = kind
    return tok
end

"""
    next_token(l::Lexer)

Returns the next `RawToken`.
"""
function next_token(l::Lexer, start = true)
    start && start_token!(l)
    if !isempty(l.string_states)
        return lex_string_chunk(l)
    else
        return _next_token(l, readchar(l))
    end
end

function _next_token(l::Lexer, c)
    if c == EOF_CHAR
        return emit(l, K"EndMarker")
    elseif iswhitespace(c)
        return lex_whitespace(l, c)
    elseif c == '['
        return emit(l, K"[")
    elseif c == ']'
        return emit(l, K"]")
    elseif c == '{'
        return emit(l, K"{")
    elseif c == ';'
        return emit(l, K";")
    elseif c == '}'
        return emit(l, K"}")
    elseif c == '('
        return emit(l, K"(")
    elseif c == ')'
        return emit(l, K")")
    elseif c == ','
        return emit(l, K",")
    elseif c == '*'
        return lex_star(l);
    elseif c == '^'
        return lex_circumflex(l);
    elseif c == '@'
        return emit(l, K"@")
    elseif c == '?'
        return emit(l, K"?")
    elseif c == '$'
        return lex_dollar(l);
    elseif c == '⊻'
        return lex_xor(l);
    elseif c == '~'
        return emit(l, K"~")
    elseif c == '#'
        return lex_comment(l)
    elseif c == '='
        return lex_equal(l)
    elseif c == '!'
        return lex_exclaim(l)
    elseif c == '>'
        return lex_greater(l)
    elseif c == '<'
        return lex_less(l)
    elseif c == ':'
        return lex_colon(l)
    elseif c == '|'
        return lex_bar(l)
    elseif c == '&'
        return lex_amper(l)
    elseif c == '\''
        return lex_prime(l)
    elseif c == '÷'
        return lex_division(l)
    elseif c == '"'
        return lex_quote(l);
    elseif c == '%'
        return lex_percent(l);
    elseif c == '/'
        return lex_forwardslash(l);
    elseif c == '\\'
        return lex_backslash(l);
    elseif c == '.'
        return lex_dot(l);
    elseif c == '+'
        return lex_plus(l);
    elseif c == '-'
        return lex_minus(l);
    elseif c == '`'
        return lex_backtick(l);
    elseif c == '−' # \minus '−' treated as hyphen '-'
        return emit_operator_or_compound_assign(l, K"-", PREC_PLUS)
    elseif c == '∈'
        return emit_operator(l, K"∈", PREC_COMPARISON)
    elseif c == '⋆'
        return emit_operator(l, K"⋆", PREC_TIMES)
    elseif c == '±'
        return emit_operator(l, K"±", PREC_PLUS)
    elseif c == '∓'
        return emit_operator(l, K"∓", PREC_PLUS)
    elseif c == '¬'
        return emit(l, K"¬")
    elseif c == '√'
        return emit(l, K"√")
    elseif c == '∛'
        return emit(l, K"∛")
    elseif c == '∜'
        return emit(l, K"∜")
    elseif c == '≔'
        return emit_operator(l, K"≔", PREC_ASSIGNMENT)
    elseif c == '⩴'
        return emit_operator(l, K"⩴", PREC_ASSIGNMENT)
    elseif c == '≕'
        return emit_operator(l, K"≕", PREC_ASSIGNMENT)
    elseif haskey(_unicode_ops, c)
        return emit_operator(l, K"Operator", _unicode_ops[c])
    elseif is_identifier_start_char(c)
        return lex_identifier(l, c)
    elseif isdigit(c)
        return lex_digit(l, K"Integer")
    else
        return emit(l,
            !isvalid(c)           ? K"ErrorInvalidUTF8"   :
            is_invisible_char(c)  ? K"ErrorInvisibleChar" :
            is_identifier_char(c) ? K"ErrorIdentifierStart" :
            K"ErrorUnknownCharacter")
    end
end

# UAX #9: Unicode Bidirectional Algorithm
# https://unicode.org/reports/tr9/
# Very partial implementation - just enough to check correct nesting in strings
# and multiline comments.
function update_bidi_state((embedding_nesting, isolate_nesting), c)
    if c == '\n'
        embedding_nesting = 0
        isolate_nesting = 0
    elseif c == '\U202A' || c == '\U202B' || c == '\U202D' || c == '\U202E' # LRE RLE LRO RLO
        embedding_nesting += 1
    elseif c == '\U202C' # PDF
        embedding_nesting -= 1
    elseif c == '\U2066' || c == '\U2067' || c == '\U2068' # LRI RLI FSI
        isolate_nesting += 1
    elseif c == '\U2069' # PDI
        isolate_nesting -= 1
    end
    return (embedding_nesting, isolate_nesting)
end

# We're inside a string; possibly reading the string characters, or maybe in
# Julia code within an interpolation.
function lex_string_chunk(l)
    state = last(l.string_states)
    if state.paren_depth > 0
        # Read normal Julia code inside an interpolation but track nesting of
        # parentheses.
        # TODO: This stateful tracking should probably, somehow, be done by the
        # parser instead? Especially for recovery of unbalanced parens inside
        # interpolations?
        c = readchar(l)
        if c == '('
            l.string_states[end] = StringState(state.triplestr, state.raw, state.delim,
                                               state.paren_depth + 1)
            return emit(l, K"(")
        elseif c == ')'
            l.string_states[end] = StringState(state.triplestr, state.raw, state.delim,
                                               state.paren_depth - 1)
            return emit(l, K")")
        else
            return _next_token(l, c)
        end
    end
    pc = peekchar(l)
    if l.last_token == K"$"
        pc = peekchar(l)
        # Interpolated symbol or expression
        if pc == '('
            readchar(l)
            l.string_states[end] = StringState(state.triplestr, state.raw, state.delim,
                                               state.paren_depth + 1)
            return emit(l, K"(")
        elseif is_identifier_start_char(pc)
            return lex_identifier(l, readchar(l))
        else
            # Getting here is a syntax error - fall through to reading string
            # characters and let the parser deal with it.
        end
    elseif l.last_token == K"Identifier" &&
            !(pc == EOF_CHAR || is_operator_start_char(pc) || is_never_id_char(pc))
        # Only allow certain characters after interpolated vars
        # https://github.com/JuliaLang/julia/pull/25234
        readchar(l)
        return emit(l, K"ErrorInvalidInterpolationTerminator")
    end
    if pc == EOF_CHAR
        return emit(l, K"EndMarker")
    elseif !state.raw && pc == '$'
        # Start interpolation
        readchar(l)
        return emit(l, K"$")
    elseif !state.raw && pc == '\\' && (pc2 = dpeekchar(l)[2];
                                        pc2 == '\r' || pc2 == '\n')
        # Process escaped newline as whitespace
        readchar(l)
        readchar(l)
        if pc2 == '\r' && peekchar(l) == '\n'
            readchar(l)
        end
        while (pc = peekchar(l); pc == ' ' || pc == '\t')
            readchar(l)
        end
        return emit(l, K"Whitespace")
    elseif pc == state.delim && string_terminates(l, state.delim, state.triplestr)
        if state.delim == '\'' && l.last_token == K"'" && dpeekchar(l)[2] == '\''
            # Handle '''
            readchar(l)
            return emit(l, K"Char")
        end
        # Terminate string
        pop!(l.string_states)
        readchar(l)
        if state.triplestr
            readchar(l); readchar(l)
            return emit(l, state.delim == '"' ?
                        K"\"\"\"" : K"```")
        else
            return emit(l, state.delim == '"' ? K"\"" :
                           state.delim == '`' ? K"`"  : K"'")
        end
    end
    # Read a chunk of string characters
    init_bidi_state = (0,0)
    bidi_state = init_bidi_state
    valid = true
    if state.raw
        # Raw strings treat all characters as literals with the exception that
        # the closing quotes can be escaped with an odd number of \ characters.
        while true
            pc = peekchar(l)
            if string_terminates(l, state.delim, state.triplestr) || pc == EOF_CHAR
                break
            elseif state.triplestr && (pc == '\n' || pc == '\r')
                # triple quoted newline splitting
                readchar(l)
                if pc == '\n'
                    bidi_state = init_bidi_state
                elseif pc == '\r' && peekchar(l) == '\n'
                    bidi_state = init_bidi_state
                    readchar(l)
                end
                break
            end
            c = readchar(l)
            if c == '\\'
                n = 1
                while peekchar(l) == '\\'
                    readchar(l)
                    n += 1
                end
                if peekchar(l) == state.delim && !iseven(n)
                    readchar(l)
                end
            end
            bidi_state = update_bidi_state(bidi_state, c)
            valid &= isvalid(c)
        end
    else
        while true
            pc = peekchar(l)
            if pc == '$' || pc == EOF_CHAR
                break
            elseif state.triplestr && (pc == '\n' || pc == '\r')
                # triple quoted newline splitting
                readchar(l)
                if pc == '\n'
                    bidi_state = init_bidi_state
                elseif pc == '\r' && peekchar(l) == '\n'
                    readchar(l)
                    bidi_state = init_bidi_state
                end
                break
            elseif pc == state.delim && string_terminates(l, state.delim, state.triplestr)
                break
            elseif pc == '\\'
                # Escaped newline
                _, pc2, pc3 = peekchar3(l)
                if pc2 == '\r' || pc2 == '\n'
                    if pc2 == '\n' || pc3 == '\n'
                        bidi_state = init_bidi_state
                    end
                    break
                end
            end
            c = readchar(l)
            if c == '\\'
                c = readchar(l)
                c == EOF_CHAR && break
            end
            bidi_state = update_bidi_state(bidi_state, c)
            valid &= isvalid(c)
        end
    end
    outk = !valid                        ? K"ErrorInvalidUTF8"    :
           state.delim == '\''           ? K"Char"                :
           bidi_state != init_bidi_state ? K"ErrorBidiFormatting" :
           state.delim == '"'            ? K"String"              :
           state.delim == '`'            ? K"CmdString"           :
           (@assert(state.delim in KSet"' \" `"); K"error")
    return emit(l, outk)
end

# Lex whitespace, a whitespace char `c` has been consumed
function lex_whitespace(l::Lexer, c)
    k = K"Whitespace"
    while true
        if c == '\n'
            k = K"NewlineWs"
        end
        pc, ppc = dpeekchar(l)
        # stop on non whitespace and limit to a single newline in a token
        if !iswhitespace(pc) ||
                (k == K"NewlineWs" && (pc == '\n' || (pc == '\r' && ppc == '\n')))
            break
        end
        c = readchar(l)
    end
    return emit_trivia(l, k)
end

function lex_comment(l::Lexer)
    if peekchar(l) != '='
        valid = true
        while true
            pc, ppc = dpeekchar(l)
            if pc == '\n' || (pc == '\r' && ppc == '\n') || pc == EOF_CHAR
                return valid ? emit_trivia(l, K"Comment") :
                               emit(l, K"ErrorInvalidUTF8")
            end
            valid &= isvalid(pc)
            readchar(l)
        end
    else
        c = readchar(l) # consume the '='
        init_bidi_state = (0,0)
        bidi_state = init_bidi_state
        skip = true  # true => c was part of the prev comment marker pair
        nesting = 1
        valid = true
        while true
            if c == EOF_CHAR
                return emit(l, K"ErrorEofMultiComment")
            end
            nc = readchar(l)
            bidi_state = update_bidi_state(bidi_state, nc)
            valid &= isvalid(nc)
            if skip
                skip = false
            else
                if c == '#' && nc == '='
                    nesting += 1
                    skip = true
                elseif c == '=' && nc == '#'
                    nesting -= 1
                    skip = true
                    if nesting == 0
                        outk = !valid ? K"ErrorInvalidUTF8" :
                               bidi_state != init_bidi_state ? K"ErrorBidiFormatting" :
                               K"Comment"
                        return emit(l, outk)
                    end
                end
            end
            c = nc
        end
    end
end

# Lex a greater char, a '>' has been consumed
function lex_greater(l::Lexer)
    if accept(l, '>')
        # >> >>> >>= >>>=
        accept(l, '>')
        return emit_operator_or_compound_assign(l, K"Operator", PREC_BITSHIFT)
    elseif accept(l, '=')
        return emit_operator(l, K"Operator", PREC_COMPARISON) # >=
    elseif accept(l, ':')
        return emit(l, K">:")
    else
        return emit_operator(l, K">", PREC_COMPARISON)
    end
end

# Lex a less char, a '<' has been consumed
function lex_less(l::Lexer)
    if accept(l, '<')
        # << or <<=
        return emit_operator_or_compound_assign(l, K"Operator", PREC_BITSHIFT)
    elseif accept(l, '=')
        return emit_operator(l, K"Operator", PREC_COMPARISON) # <=
    elseif accept(l, ':')
        return emit(l, K"<:")
    elseif accept(l, '|')
        return emit_operator(l, K"Operator", PREC_PIPE_LT) # <|
    elseif dpeekchar(l) == ('-', '-')
        readchar(l); readchar(l)
        if accept(l, '-')
            return emit(l, K"ErrorInvalidOperator")
        else
            if accept(l, '>')
                return emit_operator(l, K"Operator", PREC_ARROW) # <-->
            elseif accept(l, '-')
                return emit(l, K"ErrorInvalidOperator")
            else
                return emit_operator(l, K"Operator", PREC_ARROW) # <--
            end
        end
    else
        return emit_operator(l, K"<", PREC_COMPARISON)
    end
end

# Lex all tokens that start with an = character.
# An '=' char has been consumed
function lex_equal(l::Lexer)
    if accept(l, '=')
        accept(l, '=')
        return emit_operator(l, K"Operator", PREC_COMPARISON) # ==, ===
    elseif accept(l, '>')
        return emit_operator(l, K"Operator", PREC_PAIRARROW)
    else
        return emit(l, K"=")
    end
end

# Lex a colon, a ':' has been consumed
function lex_colon(l::Lexer)
    if accept(l, ':')
        return emit(l, K"::")
    elseif accept(l, '=')
        return emit(l, K":=")
    else
        return emit_operator(l, K":", PREC_COLON)
    end
end

function lex_exclaim(l::Lexer)
    if accept(l, '=')
        if accept(l, '=')
            return emit_operator(l, K"Operator", PREC_COMPARISON) # !==
        else
            return emit_operator(l, K"Operator", PREC_COMPARISON) # !=
        end
    else
        return emit(l, K"!")
    end
end

function lex_percent(l::Lexer)
    return emit_operator_or_compound_assign(l, K"Operator", PREC_TIMES)
end

function lex_bar(l::Lexer)
    if accept(l, '>')
        return emit_operator(l, K"Operator", PREC_PIPE_GT) # |>
    elseif accept(l, '|')
        return emit(l, K"||")
    else
        return emit_operator_or_compound_assign(l, K"Operator", PREC_PLUS)
    end
end

function lex_plus(l::Lexer)
    if accept(l, '+')
        return emit_operator(l, K"++", PREC_PLUS)
    end
    return emit_operator_or_compound_assign(l, K"+", PREC_PLUS)
end

function lex_minus(l::Lexer)
    if accept(l, '-')
        if accept(l, '>')
            return emit_operator(l, K"-->", PREC_ARROW)
        else
            return emit(l, K"ErrorInvalidOperator") # "--" is an invalid operator
        end
    elseif l.last_token != K"." && accept(l, '>')
        return emit_operator(l, K"->", PREC_ARROW)
    end
    return emit_operator_or_compound_assign(l, K"-", PREC_PLUS)
end

function lex_star(l::Lexer)
    if accept(l, '*')
        return emit(l, K"Error**") # "**" is an invalid operator use ^
    end
    return emit_operator_or_compound_assign(l, K"*", PREC_TIMES)
end

function lex_circumflex(l::Lexer)
    return emit_operator_or_compound_assign(l, K"Operator", PREC_POWER) # ^
end

function lex_division(l::Lexer)
    return emit_operator_or_compound_assign(l, K"Operator", PREC_TIMES) # /
end

function lex_dollar(l::Lexer)
    return emit_operator_or_compound_assign(l, K"$", PREC_PLUS)
end

function lex_xor(l::Lexer)
    return emit_operator_or_compound_assign(l, K"Operator", PREC_PLUS)
end

function accept_number(l::Lexer, f::F) where {F}
    lexed_number = false
    while true
        pc, ppc = dpeekchar(l)
        if pc == '_' && !f(ppc)
            return lexed_number
        elseif f(pc) || pc == '_'
            readchar(l)
        else
            return lexed_number
        end
        lexed_number = true
    end
end

# A digit has been consumed
function lex_digit(l::Lexer, kind)
    accept_number(l, isdigit)
    pc,ppc = dpeekchar(l)
    if pc == '.'
        if ppc == '.'
            # Number followed by K"."
            return emit(l, kind)
        elseif kind === K"Float"
            # If we enter the function with kind == K"Float" then a '.' has been parsed.
            readchar(l)
            return emit(l, K"ErrorInvalidNumericConstant")
        elseif is_dottable_operator_start_char(ppc)
            readchar(l)
            return emit(l, K"ErrorAmbiguousNumericConstant") # `1.+`
        end
        readchar(l)

        kind = K"Float"
        accept(l, '_') && return emit(l, K"ErrorInvalidNumericConstant") # `1._`
        had_fraction_digs = accept_number(l, isdigit)
        pc, ppc = dpeekchar(l)
        if (pc == 'e' || pc == 'E' || pc == 'f') && (isdigit(ppc) || ppc == '+' || ppc == '-' || ppc == '−')
            kind = pc == 'f' ? K"Float32" : K"Float"
            readchar(l)
            accept(l, "+-−")
            if accept_batch(l, isdigit)
                pc,ppc = dpeekchar(l)
                if pc === '.' && !is_dottable_operator_start_char(ppc)
                    readchar(l)
                    return emit(l, K"ErrorInvalidNumericConstant") # `1.e1.`
                end
            else
                return emit(l, K"ErrorInvalidNumericConstant") # `1.e`
            end
        elseif pc == '.' && ppc != '.' && !is_dottable_operator_start_char(ppc)
            readchar(l)
            return emit(l, K"ErrorInvalidNumericConstant") # `1.1.`
        elseif !had_fraction_digs && (is_identifier_start_char(pc) ||
                                      pc == '(' || pc == '[' || pc == '{' ||
                                      pc == '@' || pc == '`' || pc == '"')
            return emit(l, K"ErrorAmbiguousNumericDotMultiply") # `1.(` `1.x`
        end
    elseif (pc == 'e' || pc == 'E' || pc == 'f') && (isdigit(ppc) || ppc == '+' || ppc == '-' || ppc == '−')
        kind = pc == 'f' ? K"Float32" : K"Float"
        readchar(l)
        accept(l, "+-−")
        if accept_batch(l, isdigit)
            pc,ppc = dpeekchar(l)
            if pc === '.' && !is_dottable_operator_start_char(ppc)
                accept(l, '.')
                return emit(l, K"ErrorInvalidNumericConstant") # `1e1.`
            end
        else
            return emit(l, K"ErrorInvalidNumericConstant") # `1e+`
        end
    elseif position(l) - startpos(l) == 1 && l.chars[1] == '0'
        kind == K"Integer"
        is_bin_oct_hex_int = false
        if pc == 'x'
            kind = K"HexInt"
            isfloat = false
            readchar(l)
            had_digits = accept_number(l, ishex)
            pc,ppc = dpeekchar(l)
            if pc == '.' && ppc != '.'
                readchar(l)
                had_digits |= accept_number(l, ishex)
                isfloat = true
            end
            if accept(l, "pP")
                kind = K"Float"
                accept(l, "+-−")
                if !accept_number(l, isdigit) || !had_digits
                    return emit(l, K"ErrorInvalidNumericConstant") # `0x1p` `0x.p0`
                end
                # Check for invalid trailing decimal point
                # https://github.com/JuliaLang/julia/issues/60189
                pc = peekchar(l)
                if pc == '.'
                    accept_batch(l, c->(c == '.' || isdigit(c)))
                    # `0x1p3.` `0x1p3.2` `0x1.5p2.3`
                    return emit(l, K"ErrorInvalidNumericConstant")
                end
            elseif isfloat
                return emit(l, K"ErrorHexFloatMustContainP") # `0x.` `0x1.0`
            end
            is_bin_oct_hex_int = !isfloat
        elseif pc == 'b'
            readchar(l)
            had_digits = accept_number(l, isbinary)
            kind = K"BinInt"
            is_bin_oct_hex_int = true
        elseif pc == 'o'
            readchar(l)
            had_digits = accept_number(l, isoctal)
            kind = K"OctInt"
            is_bin_oct_hex_int = true
        end
        if is_bin_oct_hex_int
            pc = peekchar(l)
            @assert @isdefined(had_digits)
            if !had_digits || isdigit(pc) || is_identifier_start_char(pc)
                accept_batch(l, c->isdigit(c) || is_identifier_start_char(c))
                # `0x` `0xg` `0x_` `0x-`
                # `0b123` `0o78p` `0xenomorph` `0xaα`
                return emit(l, K"ErrorInvalidNumericConstant")
            end
        end
    end
    return emit(l, kind)
end

function lex_prime(l)
    if l.last_token == K"Identifier"         ||
         is_contextual_keyword(l.last_token) ||
         is_word_operator(l.last_token)      ||
         l.last_token == K"."                ||
         l.last_token ==  K")"               ||
         l.last_token ==  K"]"               ||
         l.last_token ==  K"}"               ||
         l.last_token == K"'"                ||
         l.last_token == K"end"              ||
         is_literal(l.last_token)
        # FIXME ^ This doesn't cover all cases - probably needs involvement
        # from the parser state.
        return emit_operator(l, K"'", PREC_QUOTE)
    else
        push!(l.string_states, StringState(false, true, '\'', 0))
        return emit(l, K"'")
    end
end

function lex_amper(l::Lexer)
    if accept(l, '&')
        return emit(l, K"&&")
    else
        return emit_operator_or_compound_assign(l, K"&", PREC_TIMES)
    end
end

# Parse a token starting with a quote.
# A '"' has been consumed
function lex_quote(l::Lexer)
    raw = l.last_token == K"Identifier" ||
          is_contextual_keyword(l.last_token) ||
          is_word_operator(l.last_token)
    pc, dpc = dpeekchar(l)
    triplestr = pc == '"' && dpc == '"'
    push!(l.string_states, StringState(triplestr, raw, '"', 0))
    if triplestr
        readchar(l)
        readchar(l)
        emit(l, K"\"\"\"")
    else
        emit(l, K"\"")
    end
end

function string_terminates(l, delim::Char, triplestr::Bool)
    if triplestr
        c1, c2, c3 = peekchar3(l)
        c1 === delim && c2 === delim && c3 === delim
    else
        peekchar(l) === delim
    end
end

# Parse a token starting with a forward slash.
# A '/' has been consumed
function lex_forwardslash(l::Lexer)
    prec = accept(l, '/') ? PREC_RATIONAL : PREC_TIMES
    return emit_operator_or_compound_assign(l, K"Operator", prec) # // or /
end

function lex_backslash(l::Lexer)
    return emit_operator_or_compound_assign(l, K"Operator", PREC_TIMES)
end

function lex_dot(l::Lexer)
    if l.last_token == K"@"
        if accept(l, '.')
            if !accept(l, '.') && is_dottable_operator_start_char(peekchar(l))
                readchar(l)
                return emit(l, K"ErrorInvalidOperator")
            end
        end
        # Emit `.`, `..` and `...` as identifiers after `@`
        emit(l, K"Identifier")
    elseif l.last_token != K"." && Base.isdigit(peekchar(l))
        # Only start a numeric constant if the previous token wasn't a dot
        return lex_digit(l, K"Float")
    else
        return emit(l, K".")
    end
end

# A ` has been consumed
function lex_backtick(l::Lexer)
    pc, dpc = dpeekchar(l)
    triplestr = pc == '`' && dpc == '`'
    # Backticks always contain raw strings only. See discussion on bug
    # https://github.com/JuliaLang/julia/issues/3150
    raw = true
    push!(l.string_states, StringState(triplestr, raw, '`', 0))
    if triplestr
        readchar(l)
        readchar(l)
        emit(l, K"```")
    else
        emit(l, K"`")
    end
end

const MAX_KW_LENGTH = 10
const ascii_is_identifier_char = Bool[is_identifier_char(Char(b)) for b=0x00:0x7f]
function lex_identifier(l::Lexer, c)
    h = simple_hash(c, UInt64(0))
    n = 1
    ascii = isascii(c)
    graphemestate = Ref(Int32(ascii)) # all ASCII id chars are UTF8PROC_BOUNDCLASS_OTHER
    graphemestate_peek = Ref(zero(Int32))
    while true
        pc, ppc = dpeekchar(l)
        ascii = ascii && isascii(pc)
        if ascii # fast path
            pc_byte = pc % UInt8
            @inbounds if (pc_byte == UInt8('!') && ppc == '=') || !ascii_is_identifier_char[pc_byte+1]
                break
            end
        elseif @callsite_inline Unicode.isgraphemebreak!(graphemestate, c, pc)
            if (pc == '!' && ppc == '=') || !is_identifier_char(pc)
                break
            end
        elseif pc in ('\u200c','\u200d') # ZWNJ/ZWJ control characters
            # ZWJ/ZWNJ only within grapheme sequences, not at end
            graphemestate_peek[] = graphemestate[]
            if @callsite_inline Unicode.isgraphemebreak!(graphemestate_peek, pc, ppc)
                break
            end
        end
        c = readchar(l)
        h = simple_hash(c, h)
        n += 1
    end

    if n > MAX_KW_LENGTH
        return emit(l, K"Identifier")
    elseif h == _true_hash || h == _false_hash
        return emit(l, K"Bool")
    else
        k = get(_kw_hash, h, K"Identifier")
        return emit(l, k)
    end
end

# This creates a hash for chars in [A-z] using 6 bit per char.
# Requires an additional input-length check somewhere, because
# this only works up to ~10 chars.
@inline function simple_hash(c::Char, h::UInt64)
    bytehash = (clamp(c - 'A' + 1, -1, 60) % UInt8) & 0x3f
    h << 6 + bytehash
end

function simple_hash(str)
    ind = 1
    h = UInt64(0)
    L = min(lastindex(str), MAX_KW_LENGTH)
    while ind <= L
        h = simple_hash(str[ind], h)
        ind = nextind(str, ind)
    end
    h
end

kws = [
K"baremodule",
K"begin",
K"break",
K"catch",
K"const",
K"continue",
K"do",
K"else",
K"elseif",
K"end",
K"export",
K"finally",
K"for",
K"function",
K"global",
K"if",
K"import",
K"let",
K"local",
K"macro",
K"module",
K"public",
K"quote",
K"return",
K"struct",
K"try",
K"typegroup",
K"using",
K"while",
K"in",
K"isa",
K"where",

K"abstract",
K"as",
K"doc",
K"goto",
K"mutable",
K"outer",
K"primitive",
K"type",
K"var",
K"VERSION"
]

const _true_hash = simple_hash("true")
const _false_hash = simple_hash("false")
const _kw_hash = Dict(simple_hash(string(kw)) => kw for kw in kws)

end # module
