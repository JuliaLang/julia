module Lexers

include("utilities.jl")

import ..Tokens
import ..Tokens: AbstractToken, Token, RawToken, Kind, TokenError, UNICODE_OPS, EMPTY_TOKEN, isliteral

import ..Tokens: FUNCTION, ABSTRACT, IDENTIFIER, BAREMODULE, BEGIN, BITSTYPE, BREAK, CATCH, CONST, CONTINUE,
                 DO, ELSE, ELSEIF, END, EXPORT, FALSE, FINALLY, FOR, FUNCTION, GLOBAL, LET, LOCAL, IF, IMMUTABLE,
                 IMPORT, IMPORTALL, MACRO, MODULE, QUOTE, RETURN, TRUE, TRY, TYPE, TYPEALIAS, USING, WHILE, ISA, IN,
                 MUTABLE, PRIMITIVE, STRUCT, WHERE


export tokenize

@inline ishex(c::Char) = isdigit(c) || ('a' <= c <= 'f') || ('A' <= c <= 'F')
@inline isbinary(c::Char) = c == '0' || c == '1'
@inline isoctal(c::Char) =  '0' ≤ c ≤ '7'
@inline iswhitespace(c::Char) = Base.isspace(c)

mutable struct Lexer{IO_t <: IO, T <: AbstractToken}
    io::IO_t
    io_startpos::Int

    token_start_row::Int
    token_start_col::Int
    token_startpos::Int

    current_row::Int
    current_col::Int
    current_pos::Int

    last_token::Tokens.Kind
    charstore::IOBuffer
    current_char::Char
    doread::Bool
    dotop::Bool
end

Lexer(io::IO_t, T::Type{TT} = Token) where {IO_t,TT <: AbstractToken} = Lexer{IO_t,T}(io, position(io), 1, 1, position(io), 1, 1, position(io), Tokens.ERROR, IOBuffer(), ' ', false, false)
Lexer(str::AbstractString, T::Type{TT} = Token) where TT <: AbstractToken = Lexer(IOBuffer(str), T)

@inline token_type(l::Lexer{IO_t, TT}) where {IO_t, TT} = TT

"""
    tokenize(x, T = Token)

Returns an `Iterable` containing the tokenized input. Can be reverted by e.g.
`join(untokenize.(tokenize(x)))`. Setting `T` chooses the type of token
produced by the lexer (`Token` or `RawToken`).
"""
tokenize(x, ::Type{Token}) = Lexer(x, Token)
tokenize(x, ::Type{RawToken}) = Lexer(x, RawToken)
tokenize(x) = Lexer(x, Token)

# Iterator interface
Base.IteratorSize(::Type{Lexer{IO_t,T}}) where {IO_t,T} = Base.SizeUnknown()
Base.IteratorEltype(::Type{Lexer{IO_t,T}}) where {IO_t,T} = Base.HasEltype()
Base.eltype(::Type{Lexer{IO_t,T}}) where {IO_t,T} = T


function Base.start(l::Lexer)
    seekstart(l)
    l.token_startpos = position(l)
    l.token_start_row = 1
    l.token_start_col = 1

    l.current_row = 1
    l.current_col = 1
    l.current_pos = l.io_startpos
    false
end

function Base.next(l::Lexer, ::Any)
    t = next_token(l)
    return t, t.kind == Tokens.ENDMARKER
end

Base.done(::Lexer, isdone) = isdone

function Base.show(io::IO, l::Lexer)
    print(io, typeof(l), " at position: ", position(l))
end

"""
    startpos(l::Lexer)

Return the latest `Token`'s starting position.
"""
startpos(l::Lexer) = l.token_startpos

"""
    startpos!(l::Lexer, i::Integer)

Set a new starting position.
"""
startpos!(l::Lexer, i::Integer) = l.token_startpos = i

Base.seekstart(l::Lexer) = seek(l.io, l.io_startpos)

"""
    seek2startpos!(l::Lexer)

Sets the lexer's current position to the beginning of the latest `Token`.
"""
seek2startpos!(l::Lexer) = seek(l, startpos(l))

"""
    peekchar(l::Lexer)

Returns the next character without changing the lexer's state.
"""
peekchar(l::Lexer) = peekchar(l.io)

"""
dpeekchar(l::Lexer)

Returns the next two characters without changing the lexer's state.
"""
dpeekchar(l::Lexer) = dpeekchar(l.io)

"""
    position(l::Lexer)

Returns the current position.
"""
Base.position(l::Lexer) = Base.position(l.io)

"""
    eof(l::Lexer)

Determine whether the end of the lexer's underlying buffer has been reached.
"""
eof(l::Lexer) = eof(l.io)

Base.seek(l::Lexer, pos) = seek(l.io, pos)

"""
    start_token!(l::Lexer)

Updates the lexer's state such that the next  `Token` will start at the current
position.
"""
function start_token!(l::Lexer)
    l.token_startpos = position(l)
    l.token_start_row = l.current_row
    l.token_start_col = l.current_col
end

"""
    readchar(l::Lexer)

Returns the next character and increments the current position.
"""
function readchar end

function readchar(l::Lexer{I}) where {I <: IO}
    l.current_char = readchar(l.io)
    if l.doread
        write(l.charstore, l.current_char)
    end
    if l.current_char == '\n'
        l.current_row += 1
        l.current_col = 1
    elseif !eof(l.current_char)
        l.current_col += 1
    end
    return l.current_char
end

readon(l::Lexer{I,RawToken}) where {I <: IO} = l.current_char
function readon(l::Lexer{I,Token}) where {I <: IO}
    if l.charstore.size != 0
        take!(l.charstore)
    end
    write(l.charstore, l.current_char)
    l.doread = true
    return l.current_char
end

readoff(l::Lexer{I,RawToken}) where {I <: IO} = l.current_char
function readoff(l::Lexer{I,Token})  where {I <: IO}
    l.doread = false
    return l.current_char
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
    emit(l::Lexer, kind::Kind, err::TokenError=Tokens.NO_ERR)

Returns a `Token` of kind `kind` with contents `str` and starts a new `Token`.
"""
function emit(l::Lexer{IO_t,Token}, kind::Kind, err::TokenError = Tokens.NO_ERR) where IO_t
    if (kind == Tokens.IDENTIFIER || isliteral(kind) || kind == Tokens.COMMENT || kind == Tokens.WHITESPACE)
        str = String(take!(l.charstore))
    elseif kind == Tokens.ERROR
        str = String(l.io.data[(l.token_startpos + 1):position(l.io)])
    elseif optakessuffix(kind)
        str = ""
        while isopsuffix(peekchar(l))
            str = string(str, readchar(l))
        end
    else
        str = ""
    end
    if l.dotop
        tok = Token(kind, (l.token_start_row, l.token_start_col-1),
                (l.current_row, l.current_col - 1),
                startpos(l)-1, position(l) - 1,
                str, err, true)
        l.dotop = false
    else
        tok = Token(kind, (l.token_start_row, l.token_start_col),
                (l.current_row, l.current_col - 1),
                startpos(l), position(l) - 1,
                str, err,false)
    end
    l.last_token = kind
    readoff(l)
    return tok
end

function emit(l::Lexer{IO_t,RawToken}, kind::Kind, err::TokenError = Tokens.NO_ERR) where IO_t
    if optakessuffix(kind)
        while isopsuffix(peekchar(l))
            readchar(l)
        end
    end

    if l.dotop
        tok = RawToken(kind, (l.token_start_row, l.token_start_col),
        (l.current_row, l.current_col - 1),
        startpos(l), position(l) - 1, err, true)
        l.dotop = false
    else
        tok = RawToken(kind, (l.token_start_row, l.token_start_col),
        (l.current_row, l.current_col - 1),
        startpos(l), position(l) - 1, err, false)
    end

    l.last_token = kind
    readoff(l)
    return tok
end

"""
    emit_error(l::Lexer, err::TokenError=Tokens.UNKNOWN)

Returns an `ERROR` token with error `err` and starts a new `Token`.
"""
function emit_error(l::Lexer, err::TokenError = Tokens.UNKNOWN)
    return emit(l, Tokens.ERROR, err)
end


"""
    next_token(l::Lexer)

Returns the next `Token`.
"""
function next_token(l::Lexer)
    start_token!(l)
    c = readchar(l)
    if eof(c);
        return emit(l, Tokens.ENDMARKER)
    elseif iswhitespace(c)
        readon(l)
        return lex_whitespace(l)
    elseif c == '['
        return emit(l, Tokens.LSQUARE)
    elseif c == ']'
        return emit(l, Tokens.RSQUARE)
    elseif c == '{'
        return emit(l, Tokens.LBRACE)
    elseif c == ';'
        return emit(l, Tokens.SEMICOLON)
    elseif c == '}'
        return emit(l, Tokens.RBRACE)
    elseif c == '('
        return emit(l, Tokens.LPAREN)
    elseif c == ')'
        return emit(l, Tokens.RPAREN)
    elseif c == ','
        return emit(l, Tokens.COMMA)
    elseif c == '*'
        return lex_star(l);
    elseif c == '^'
        return lex_circumflex(l);
    elseif c == '@'
        return emit(l, Tokens.AT_SIGN)
    elseif c == '?'
        return emit(l, Tokens.CONDITIONAL)
    elseif c == '$'
        return lex_dollar(l);
    elseif c == '⊻'
        return lex_xor(l);
    elseif c == '~'
        return emit(l, Tokens.APPROX)
    elseif c == '#'
        readon(l)
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
        readon(l)
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
        readon(l)
        return lex_cmd(l);
    elseif is_identifier_start_char(c)
        readon(l)
        return lex_identifier(l, c)
    elseif isdigit(c)
        readon(l)
        return lex_digit(l, Tokens.INTEGER)
    elseif (k = get(UNICODE_OPS, c, Tokens.ERROR)) != Tokens.ERROR
        return emit(l, k)
    else
        emit_error(l)
    end
end


# Lex whitespace, a whitespace char has been consumed
function lex_whitespace(l::Lexer)
    accept_batch(l, iswhitespace)
    return emit(l, Tokens.WHITESPACE)
end

function lex_comment(l::Lexer, doemit=true)
    if peekchar(l) != '='
        while true
            pc = peekchar(l)
            if pc == '\n' || eof(pc)
                return doemit ? emit(l, Tokens.COMMENT) : EMPTY_TOKEN(token_type(l))
            end
            readchar(l)
        end
    else
        c = readchar(l) # consume the '='
        n_start, n_end = 1, 0
        while true
            if eof(c)
                return doemit ? emit_error(l, Tokens.EOF_MULTICOMMENT) : EMPTY_TOKEN(token_type(l))
            end
            nc = readchar(l)
            if c == '#' && nc == '='
                n_start += 1
            elseif c == '=' && nc == '#'
                n_end += 1
            end
            if n_start == n_end
                return doemit ? emit(l, Tokens.COMMENT) : EMPTY_TOKEN(token_type(l))
            end
            c = nc
        end
    end
end

# Lex a greater char, a '>' has been consumed
function lex_greater(l::Lexer)
    if accept(l, '>') # >>
        if accept(l, '>') # >>>
            if accept(l, '=') # >>>=
                return emit(l, Tokens.UNSIGNED_BITSHIFT_EQ)
            else # >>>?, ? not a =
                return emit(l, Tokens.UNSIGNED_BITSHIFT)
            end
        elseif accept(l, '=') # >>=
            return emit(l, Tokens.RBITSHIFT_EQ)
        else # '>>'
            return emit(l, Tokens.RBITSHIFT)
        end
    elseif accept(l, '=') # >=
        return emit(l, Tokens.GREATER_EQ)
    elseif accept(l, ':') # >:
        return emit(l, Tokens.ISSUPERTYPE)
    else  # '>'
        return emit(l, Tokens.GREATER)
    end
end

# Lex a less char, a '<' has been consumed
function lex_less(l::Lexer)
    if accept(l, '<') # <<
        if accept(l, '=') # <<=
            return emit(l, Tokens.LBITSHIFT_EQ)
        else # '<<?', ? not =, ' '
            return emit(l, Tokens.LBITSHIFT)
        end
    elseif accept(l, '=') # <=
        return emit(l, Tokens.LESS_EQ)
    elseif accept(l, ':')
        return emit(l, Tokens.ISSUBTYPE)
    elseif accept(l, '|') # <|
        return emit(l, Tokens.LPIPE)
    else
        return emit(l, Tokens.LESS) # '<'
    end
end

# Lex all tokens that start with an = character.
# An '=' char has been consumed
function lex_equal(l::Lexer)
    if accept(l, '=') # ==
        if accept(l, '=') # ===
            emit(l, Tokens.EQEQEQ)
        else
            emit(l, Tokens.EQEQ)
        end
    elseif accept(l, '>') # =>
        emit(l, Tokens.PAIR_ARROW)
    else
        emit(l, Tokens.EQ)
    end
end

# Lex a colon, a ':' has been consumed
function lex_colon(l::Lexer)
    if accept(l, ':') # '::'
        return emit(l, Tokens.DECLARATION)
    elseif accept(l, '=') # ':='
        return emit(l, Tokens.COLON_EQ)
    else
        return emit(l, Tokens.COLON)
    end
end

function lex_exclaim(l::Lexer)
    if accept(l, '=') # !=
        if accept(l, '=') # !==
            return emit(l, Tokens.NOT_IS)
        else # !=
            return emit(l, Tokens.NOT_EQ)
        end
    else
        return emit(l, Tokens.NOT)
    end
end

function lex_percent(l::Lexer)
    if accept(l, '=')
        return emit(l, Tokens.REM_EQ)
    else
        return emit(l, Tokens.REM)
    end
end

function lex_bar(l::Lexer)
    if accept(l, '=') # |=
        return emit(l, Tokens.OR_EQ)
    elseif accept(l, '>') # |>
        return emit(l, Tokens.RPIPE)
    elseif accept(l, '|') # ||
        return emit(l, Tokens.LAZY_OR)
    else
        emit(l, Tokens.OR) # '|'
    end
end

function lex_plus(l::Lexer)
    if accept(l, '+')
        return emit(l, Tokens.PLUSPLUS)
    elseif accept(l, '=')
        return emit(l, Tokens.PLUS_EQ)
    end
    return emit(l, Tokens.PLUS)
end

function lex_minus(l::Lexer)
    if accept(l, '-')
        if accept(l, '>')
            return emit(l, Tokens.RIGHT_ARROW)
        else
            return emit_error(l) # "--" is an invalid operator
        end
    elseif accept(l, '>')
        return emit(l, Tokens.ANON_FUNC)
    elseif accept(l, '=')
        return emit(l, Tokens.MINUS_EQ)
    end
    return emit(l, Tokens.MINUS)
end

function lex_star(l::Lexer)
    if accept(l, '*')
        return emit_error(l) # "**" is an invalid operator use ^
    elseif accept(l, '=')
        return emit(l, Tokens.STAR_EQ)
    end
    return emit(l, Tokens.STAR)
end

function lex_circumflex(l::Lexer)
    if accept(l, '=')
        return emit(l, Tokens.CIRCUMFLEX_EQ)
    end
    return emit(l, Tokens.CIRCUMFLEX_ACCENT)
end

function lex_division(l::Lexer)
    if accept(l, '=')
        return emit(l, Tokens.DIVISION_EQ)
    end
    return emit(l, Tokens.DIVISION_SIGN)
end

function lex_dollar(l::Lexer)
    if accept(l, '=')
        return emit(l, Tokens.EX_OR_EQ)
    end
    return emit(l, Tokens.EX_OR)
end

function lex_xor(l::Lexer)
    if accept(l, '=')
        return emit(l, Tokens.XOR_EQ)
    end
    return emit(l, Tokens.XOR)
end

function accept_number(l::Lexer, f::F) where {F}
    while true
        pc, ppc = dpeekchar(l)
        if pc == '_' && !f(ppc)
            return
        elseif f(pc) || pc == '_'
            readchar(l)
        else
            return
        end
    end
end

# A digit has been consumed
function lex_digit(l::Lexer, kind)
    accept_number(l, isdigit)
    pc,ppc = dpeekchar(l)
    if pc == '.'
        if ppc == '.'
            return emit(l, kind)
        elseif (!(isdigit(ppc) ||
            iswhitespace(ppc) ||
            is_identifier_start_char(ppc)
            || ppc == '('
            || ppc == ')'
            || ppc == '['
            || ppc == ']'
            || ppc == '{'
            || ppc == '}'
            || ppc == ','
            || ppc == ';'
            || ppc == '@'
            || ppc == '`'
            || ppc == '"'
            || ppc == ':'
            || ppc == '?'
            || eof(ppc)))
            kind = Tokens.INTEGER

            return emit(l, kind)
        end
        readchar(l)

        kind = Tokens.FLOAT
        accept_number(l, isdigit)
        pc, ppc = dpeekchar(l)
        if (pc == 'e' || pc == 'E' || pc == 'f') && (isdigit(ppc) || ppc == '+' || ppc == '-')
            kind = Tokens.FLOAT
            readchar(l)
            accept(l, "+-")
            if accept_batch(l, isdigit)
                if accept(l, '.') # 1.2e2.3 -> [ERROR, 3]
                    return emit_error(l)
                end
            else
                return emit_error(l)
            end
        elseif pc == '.' && (is_identifier_start_char(ppc) || eof(ppc))
            readchar(l)
            return emit_error(l)
        end

    elseif (pc == 'e' || pc == 'E' || pc == 'f') && (isdigit(ppc) || ppc == '+' || ppc == '-')
        kind = Tokens.FLOAT
        readchar(l)
        accept(l, "+-")
        if accept_batch(l, isdigit)
            if accept(l, '.') # 1.2e2.3 -> [ERROR, 3]
                return emit_error(l)
            end
        else
            return emit_error(l)
        end
    elseif position(l) - startpos(l) == 1 && l.current_char == '0'
        kind == Tokens.INTEGER
        if pc == 'x'
            kind = Tokens.HEX_INT
            readchar(l)
            !(ishex(ppc) || ppc =='.') && return emit_error(l)
            accept_number(l, ishex)
            if accept(l, '.')
                accept_number(l, ishex)
            end
            if accept(l, "pP")
                kind = Tokens.FLOAT
                accept(l, "+-")
                accept_number(l, isdigit)
            end
        elseif pc == 'b'
            !isbinary(ppc) && return emit_error(l)
            readchar(l)
            accept_number(l, isbinary)
            kind = Tokens.BIN_INT
        elseif pc == 'o'
            !isoctal(ppc) && return emit_error(l)
            readchar(l)
            accept_number(l, isoctal)
            kind = Tokens.OCT_INT
        end
    end
    return emit(l, kind)
end

function lex_prime(l)
    if l.last_token == Tokens.IDENTIFIER ||
        l.last_token == Tokens.DOT ||
        l.last_token ==  Tokens.RPAREN ||
        l.last_token ==  Tokens.RSQUARE ||
        l.last_token ==  Tokens.RBRACE ||
        l.last_token == Tokens.PRIME || isliteral(l.last_token)
        return emit(l, Tokens.PRIME)
    else
        readon(l)
        if accept(l, '\'')
            if accept(l, '\'')
                return emit(l, Tokens.CHAR)
            else
                # Empty char literal
                # Arguably this should be an error here, but we generally
                # look at the contents of the char literal in the parser,
                # so we defer erroring until there.
                return emit(l, Tokens.CHAR)
            end
        end
        while true
            c = readchar(l)
            if eof(c)
                return emit_error(l, Tokens.EOF_CHAR)
            elseif c == '\\'
                if eof(readchar(l))
                    return emit_error(l, Tokens.EOF_CHAR)
                end
            elseif c == '\''
                return emit(l, Tokens.CHAR)
            end
        end
    end
end

function lex_amper(l::Lexer)
    if accept(l, '&')
        return emit(l, Tokens.LAZY_AND)
    elseif accept(l, "=")
        return emit(l, Tokens.AND_EQ)
    else
        return emit(l, Tokens.AND)
    end
end

# Parse a token starting with a quote.
# A '"' has been consumed
function lex_quote(l::Lexer, doemit=true)
    if accept(l, '"') # ""
        if accept(l, '"') # """
            if read_string(l, Tokens.TRIPLE_STRING)
                return doemit ? emit(l, Tokens.TRIPLE_STRING) : EMPTY_TOKEN(token_type(l))
            else
                return doemit ? emit_error(l, Tokens.EOF_STRING) : EMPTY_TOKEN(token_type(l))
            end
        else # empty string
            return doemit ? emit(l, Tokens.STRING) : EMPTY_TOKEN(token_type(l))
        end
    else # "?, ? != '"'
        if read_string(l, Tokens.STRING)
            return doemit ? emit(l, Tokens.STRING) : EMPTY_TOKEN(token_type(l))
        else
            return doemit ? emit_error(l, Tokens.EOF_STRING) : EMPTY_TOKEN(token_type(l))
        end
    end
end

function string_terminated(l, c, kind::Tokens.Kind)
    if (kind == Tokens.STRING || kind == Tokens.TRIPLE_STRING) && c == '"'
        if kind == Tokens.STRING
            return true
        else
            if accept(l, "\"") && accept(l, "\"")
                return true
            end
        end
    elseif (kind == Tokens.CMD || kind == Tokens.TRIPLE_CMD) && c == '`'
        if kind == Tokens.CMD
            return true
        else
            if accept(l, "\`") && accept(l, "\`")
                return true
            end
        end
    end
    return false
end

# We just consumed a ", """, `, or ```
function read_string(l::Lexer, kind::Tokens.Kind)
    while true
        c = readchar(l)
        if c == '\\'
            eof(readchar(l)) && return false
            continue
        end
        if string_terminated(l, c, kind)
            return true
        elseif eof(c)
            return false
        end
        if c == '$'
            c = readchar(l)
            if string_terminated(l, c, kind)
                return true
            elseif eof(c)
                return false
            elseif c == '('
                o = 1
                while o > 0
                    c = readchar(l)
                    eof(c) && return false
                    if c == '('
                        o += 1
                    elseif c == ')'
                        o -= 1
                    elseif c == '"'
                        lex_quote(l, false)
                    elseif c == '`'
                        lex_cmd(l, false)
                    elseif c == '#'
                        lex_comment(l, false)
                    end
                end
            end
        end
    end
end

# Parse a token starting with a forward slash.
# A '/' has been consumed
function lex_forwardslash(l::Lexer)
    if accept(l, "/") # //
        if accept(l, "=") # //=
            return emit(l, Tokens.FWDFWD_SLASH_EQ)
        else
            return emit(l, Tokens.FWDFWD_SLASH)
        end
    elseif accept(l, "=") # /=
        return emit(l, Tokens.FWD_SLASH_EQ)
    else
        return emit(l, Tokens.FWD_SLASH)
    end
end

function lex_backslash(l::Lexer)
    if accept(l, '=')
        return emit(l, Tokens.BACKSLASH_EQ)
    end
    return emit(l, Tokens.BACKSLASH)
end

# TODO .op
function lex_dot(l::Lexer)
    if accept(l, '.')
        if accept(l, '.')
            return emit(l, Tokens.DDDOT)
        else
            return emit(l, Tokens.DDOT)
        end
    elseif Base.isdigit(peekchar(l))
        readon(l)
        return lex_digit(l, Tokens.FLOAT)
    else
        pc, dpc = dpeekchar(l)
        if dotop1(pc)
            l.dotop = true
            return next_token(l)
        elseif pc =='+'
            l.dotop = true
            readchar(l)
            return lex_plus(l)
        elseif pc =='-'
            l.dotop = true
            readchar(l)
            return lex_minus(l)
        elseif pc =='*'
            l.dotop = true
            readchar(l)
            return lex_star(l)
        elseif pc =='/'
            l.dotop = true
            readchar(l)
            return lex_forwardslash(l)
        elseif pc =='\\'
            l.dotop = true
            readchar(l)
            return lex_backslash(l)
        elseif pc =='^'
            l.dotop = true
            readchar(l)
            return lex_circumflex(l)
        elseif pc =='<'
            l.dotop = true
            readchar(l)
            return lex_less(l)
        elseif pc =='>'
            l.dotop = true
            readchar(l)
            return lex_greater(l)
        elseif pc =='&' 
            l.dotop = true
            readchar(l)
            if accept(l, "=")
                return emit(l, Tokens.AND_EQ)
            else
                return emit(l, Tokens.AND)
            end
        elseif pc =='%'
            l.dotop = true
            readchar(l)
            return lex_percent(l)
        elseif pc == '=' && dpc != '>'
            l.dotop = true
            readchar(l)
            return lex_equal(l)
        elseif pc == '|' && dpc != '|'
            l.dotop = true
            readchar(l)
            return lex_bar(l)
        elseif pc == '!' && dpc == '='
            l.dotop = true
            readchar(l)
            return lex_exclaim(l)
        elseif pc == '⊻'
            l.dotop = true
            readchar(l)
            return lex_xor(l)
        elseif pc == '÷'
            l.dotop = true
            readchar(l)
            return lex_division(l)
        elseif pc == '=' && dpc == '>'
            l.dotop = true
            readchar(l)
            return lex_equal(l)
        else
            return emit(l, Tokens.DOT)
        end
    end
end

# A ` has been consumed
# N.B.: cmds do not currently have special parser interpolation support
function lex_cmd(l::Lexer, doemit=true)
    kind = Tokens.CMD
    if accept(l, '`') # ``
        if accept(l, '`') # ```
            kind = Tokens.TRIPLE_CMD
        else # empty cmd
            return doemit ? emit(l, Tokens.CMD) : EMPTY_TOKEN(token_type(l))
        end
    end
    while true
        c = readchar(l)
        eof(c) && return (doemit ? emit_error(l, Tokens.EOF_CMD) : EMPTY_TOKEN(token_type(l)))
        string_terminated(l, c, kind) && return (doemit ? emit(l, kind) : EMPTY_TOKEN(token_type(l)))
    end
end

function tryread(l, str, k, c)
    for s in str
        c = peekchar(l)
        if c != s
            if !is_identifier_char(c)
                return emit(l, IDENTIFIER)
            end
            return readrest(l, c)
        else
            readchar(l)
        end
    end
    if is_identifier_char(peekchar(l))
        return readrest(l, c)
    end
    return emit(l, k)
end

function readrest(l, c)
    while true
        pc, ppc = dpeekchar(l)
        if !is_identifier_char(pc) || (pc == '!' && ppc == '=')
            break
        end
        c = readchar(l)
    end

    return emit(l, IDENTIFIER)
end


function _doret(l, c)
    if !is_identifier_char(c)
        return emit(l, IDENTIFIER)
    else
        return readrest(l, c)
    end
end

function lex_identifier(l, c)
    if c == 'a'
        return tryread(l, ('b', 's', 't', 'r', 'a', 'c', 't'), ABSTRACT, c)
    elseif c == 'b'
        c = peekchar(l)
        if c == 'a'
            c = readchar(l)
            return tryread(l, ('r', 'e', 'm', 'o', 'd', 'u', 'l', 'e'), BAREMODULE, c)
        elseif c == 'e'
            c = readchar(l)
            return tryread(l, ('g', 'i', 'n'), BEGIN, c)
        elseif c == 'i'
            c = readchar(l)
            return tryread(l, ('t', 's', 't', 'y', 'p', 'e'), BITSTYPE, c)
        elseif c == 'r'
            c = readchar(l)
            return tryread(l, ('e', 'a', 'k'), BREAK, c)
        else
            return _doret(l, c)
        end
    elseif c == 'c'
        c = peekchar(l)
        if c == 'a'
            c = readchar(l)
            return tryread(l, ('t', 'c', 'h'), CATCH, c)
        elseif c == 'o'
            readchar(l)
            c = peekchar(l)
            if c == 'n'
                readchar(l)
                c = peekchar(l)
                if c == 's'
                    readchar(l)
                    c = peekchar(l)
                    return tryread(l, ('t',), CONST, c)
                elseif c == 't'
                    readchar(l)
                    c = peekchar(l)
                    return tryread(l, ('i', 'n', 'u', 'e'), CONTINUE, c)
                else
                    return _doret(l, c)
                end
            else
                return _doret(l, c)
            end
        else
            return _doret(l, c)
        end
    elseif c == 'd'
        return tryread(l, ('o'), DO, c)
    elseif c == 'e'
        c = peekchar(l)
        if c == 'l'
            readchar(l)
            c = peekchar(l)
            if c == 's'
                readchar(l)
                c = peekchar(l)
                if c == 'e'
                    readchar(l)
                    c = peekchar(l)
                    if !is_identifier_char(c)
                        return emit(l, ELSE)
                    elseif c == 'i'
                        c = readchar(l)
                        return tryread(l, ('f'), ELSEIF ,c)
                    else
                        return _doret(l, c)
                    end
                else
                    return _doret(l, c)
                end
            else
                return _doret(l, c)
            end
        elseif c == 'n'
            c = readchar(l)
            return tryread(l, ('d'), END, c)
        elseif c == 'x'
            c = readchar(l)
            return tryread(l, ('p', 'o', 'r', 't'), EXPORT, c)
        else
            return _doret(l, c)
        end
    elseif c == 'f'
        c = peekchar(l)
        if c == 'a'
            c = readchar(l)
            return tryread(l, ('l', 's', 'e'), FALSE, c)
        elseif c == 'i'
            c = readchar(l)
            return tryread(l, ('n', 'a', 'l', 'l', 'y'), FINALLY, c)
        elseif c == 'o'
            c = readchar(l)
            return tryread(l, ('r'), FOR, c)
        elseif c == 'u'
            c = readchar(l)
            return tryread(l, ('n', 'c', 't', 'i', 'o', 'n'), FUNCTION, c)
        else
            return _doret(l, c)
        end
    elseif c == 'g'
        return tryread(l, ('l', 'o', 'b', 'a', 'l'), GLOBAL, c)
    elseif c == 'i'
        c = peekchar(l)
        if c == 'f'
            readchar(l)
            c = peekchar(l)
            if !is_identifier_char(c)
                return emit(l, IF)
            else
                return readrest(l, c)
            end
        elseif c == 'm'
            readchar(l)
            c = peekchar(l)
            if c == 'm'
                readchar(l)
                return tryread(l, ('u', 't', 'a', 'b', 'l', 'e'), IMMUTABLE, c)
            elseif c == 'p'
                readchar(l)
                c = peekchar(l)
                if c == 'o'
                    readchar(l)
                    c = peekchar(l)
                    if c == 'r'
                        readchar(l)
                        c = peekchar(l)
                        if c == 't'
                            readchar(l)
                            c = peekchar(l)
                            if !is_identifier_char(c)
                                return emit(l, IMPORT)
                            elseif c == 'a'
                                c = readchar(l)
                                return tryread(l, ('l','l'), IMPORTALL, c)
                            else
                                return _doret(l, c)
                            end
                        else
                            return _doret(l, c)
                        end
                    else
                        return _doret(l, c)
                    end
                else
                    return _doret(l, c)
                end
            else
                return _doret(l, c)
            end
        elseif c == 'n'
            readchar(l)
            c = peekchar(l)
            if !is_identifier_char(c)
                return emit(l, IN)
            else
                return readrest(l, c)
            end
        elseif (@static VERSION >= v"0.6.0-dev.1471" ? true : false) && c == 's'
            c = readchar(l)
            return tryread(l, ('a'), ISA, c)
        else
            return _doret(l, c)
        end
    elseif c == 'l'
        c = peekchar(l)
        if c == 'e'
            readchar(l)
            return tryread(l, ('t'), LET, c)
        elseif c == 'o'
            readchar(l)
            return tryread(l, ('c', 'a', 'l'), LOCAL, c)
        else
            return _doret(l, c)
        end
    elseif c == 'm'
        c = peekchar(l)
        if c == 'a'
            c = readchar(l)
            return tryread(l, ('c', 'r', 'o'), MACRO, c)
        elseif c == 'o'
            c = readchar(l)
            return tryread(l, ('d', 'u', 'l', 'e'), MODULE, c)
        elseif c == 'u'
            c = readchar(l)
            return tryread(l, ('t', 'a', 'b', 'l', 'e'), MUTABLE, c)
        else
            return _doret(l, c)
        end
    elseif c == 'p'
        return tryread(l, ('r', 'i', 'm', 'i', 't', 'i', 'v', 'e'), PRIMITIVE, c)
    elseif c == 'q'
        return tryread(l, ('u', 'o', 't', 'e'), QUOTE, c)
    elseif c == 'r'
        return tryread(l, ('e', 't', 'u', 'r', 'n'), RETURN, c)
    elseif c == 's'
        return tryread(l, ('t', 'r', 'u', 'c', 't'), STRUCT, c)
    elseif c == 't'
        c = peekchar(l)
        if c == 'r'
            readchar(l)
            c = peekchar(l)
            if c == 'u'
                c = readchar(l)
                return tryread(l, ('e'), TRUE, c)
            elseif c == 'y'
                readchar(l)
                c = peekchar(l)
                if !is_identifier_char(c)
                    return emit(l, TRY)
                else
                    c = readchar(l)
                    return _doret(l, c)
                end
            else
                return _doret(l, c)
            end
        elseif c == 'y'
            readchar(l)
            c = peekchar(l)
            if c == 'p'
                readchar(l)
                c = peekchar(l)
                if c == 'e'
                    readchar(l)
                    c = peekchar(l)
                    if !is_identifier_char(c)
                        return emit(l, TYPE)
                    elseif c == 'a'
                        c = readchar(l)
                        return tryread(l, ('l', 'i', 'a', 's'), TYPEALIAS, c)
                    else
                        c = readchar(l)
                        return _doret(l, c)
                    end
                else
                    return _doret(l, c)
                end
            else
                return _doret(l, c)
            end
        else
            return _doret(l, c)
        end
    elseif c == 'u'
        return tryread(l, ('s', 'i', 'n', 'g'), USING, c)
    elseif c == 'w'
        c = peekchar(l)
        if c == 'h'
            readchar(l)
            c = peekchar(l)
            if c == 'e'
                c = readchar(l)
                return tryread(l, ('r', 'e'), WHERE, c)
            elseif c == 'i'
                c = readchar(l)
                return tryread(l, ('l', 'e'), WHILE, c)
            else
                return _doret(l, c)
            end
        else
            return _doret(l, c)
        end
    else
        return _doret(l, c)
    end
end

end # module
