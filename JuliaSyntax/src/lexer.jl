module Lexers

include("utilities.jl")

import ..Tokens
import ..Tokens: Token, Kind, TokenError, UNICODE_OPS, EMPTY_TOKEN, isliteral

import ..Tokens: FUNCTION, ABSTRACT, IDENTIFIER, BAREMODULE, BEGIN, BITSTYPE, BREAK, CATCH, CONST, CONTINUE,
                 DO, ELSE, ELSEIF, END, EXPORT, FALSE, FINALLY, FOR, FUNCTION, GLOBAL, LET, LOCAL, IF, IMMUTABLE,
                 IMPORT, IMPORTALL, MACRO, MODULE, QUOTE, RETURN, TRUE, TRY, TYPE, TYPEALIAS, USING, WHILE, ISA, IN,
                 MUTABLE, PRIMITIVE, STRUCT, WHERE


export tokenize

ishex(c::Char) = isdigit(c) || ('a' <= c <= 'f') || ('A' <= c <= 'F')
isbinary(c::Char) = c == '0' || c == '1'
isoctal(c::Char) =  '0' ≤ c ≤ '7'
iswhitespace(c::Char) = Base.UTF8proc.isspace(c)

mutable struct Lexer{IO_t <: IO}
    io::IO_t
    io_startpos::Int

    token_start_row::Int
    token_start_col::Int

    prevpos::Int
    token_startpos::Int

    current_row::Int
    current_col::Int
    current_pos::Int

    last_token::Tokens.Kind
    charstore::IOBuffer
end

Lexer(io) = Lexer(io, position(io), 1, 1, -1, position(io), 1, 1, position(io), Tokens.ERROR, IOBuffer())
Lexer(str::AbstractString) = Lexer(IOBuffer(str))

"""
    tokenize(x)

Returns an `Iterable` containing the tokenized input. Can be reverted by e.g.
`join(untokenize.(tokenize(x)))`.
"""
tokenize(x) = Lexer(x)

# Iterator interface
Base.iteratorsize(::Type{Lexer{IO_t}}) where {IO_t} = Base.SizeUnknown()
Base.iteratoreltype(::Type{Lexer{IO_t}}) where {IO_t} = Base.HasEltype()
Base.eltype(::Type{Lexer{IO_t}}) where {IO_t} = Token

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

"""
    prevpos(l::Lexer)

Return the lexer's previous position.
"""
prevpos(l::Lexer) = l.prevpos

"""
    prevpos!(l::Lexer, i::Integer)

Set the lexer's previous position.
"""
prevpos!(l::Lexer, i::Integer) = l.prevpos = i

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
    prevchar(l::Lexer)

Returns the previous character. Does not change the lexer's state.
"""
function prevchar(l::Lexer)
    backup!(l)
    return readchar(l)
end

"""
    readchar(l::Lexer)

Returns the next character and increments the current position.
"""
function readchar end

function readchar(l::Lexer{I}) where {I <: IO}
    prevpos!(l, position(l))
    c = readchar(l.io)
    return c
end

"""
    backup!(l::Lexer)

Decrements the current position and sets the previous position to `-1`, unless
the previous position already is `-1`.
"""
function backup!(l::Lexer)
    prevpos(l) == -1 && error("prevpos(l) == -1\n Cannot backup! multiple times.")
    seek(l, prevpos(l))
    prevpos!(l, -1)
end

"""
    accept(l::Lexer, f::Union{Function, Char, Vector{Char}, String})

Consumes the next character `c` if either `f::Function(c)` returns true, `c == f`
for `c::Char` or `c in f` otherwise. Returns `true` if a character has been
consumed and `false` otherwise.
"""
function accept(l::Lexer, f::Union{Function, Char, Vector{Char}, String})
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
function accept_batch(l::Lexer, f)
    ok = false
    while accept(l, f)
        ok = true
    end
    return ok
end

"""
    emit(l::Lexer, kind::Kind,
         str::String=extract_tokenstring(l), err::TokenError=Tokens.NO_ERR)

Returns a `Token` of kind `kind` with contents `str` and starts a new `Token`.
"""
function emit(l::Lexer, kind::Kind,
              str::String = extract_tokenstring(l), err::TokenError = Tokens.NO_ERR)
    tok = Token(kind, (l.token_start_row, l.token_start_col),
                (l.current_row, l.current_col - 1),
                startpos(l), position(l) - 1,
                str, err)
    l.last_token = kind
    start_token!(l)
    return tok
end

"""
    emit_error(l::Lexer, err::TokenError=Tokens.UNKNOWN)

Returns an `ERROR` token with error `err` and starts a new `Token`.
"""
function emit_error(l::Lexer, err::TokenError = Tokens.UNKNOWN)
    return emit(l, Tokens.ERROR, extract_tokenstring(l), err)
end

"""
    extract_tokenstring(l::Lexer)

Returns all characters since the start of the current `Token` as a `String`.
"""
function extract_tokenstring(l::Lexer)
    charstore = l.charstore
    curr_pos = position(l)
    seek2startpos!(l)

    while position(l) < curr_pos
        c = readchar(l)
        l.current_col += 1
        if c == '\n'
            l.current_row += 1
            l.current_col = 1
         end
        write(charstore, c)
    end
    str = String(take!(charstore))
    return str
end

"""
    next_token(l::Lexer)

Returns the next `Token`.
"""
function next_token(l::Lexer)
    c = readchar(l)

    if eof(c); return emit(l, Tokens.ENDMARKER)
    elseif iswhitespace(c); return lex_whitespace(l)
    elseif c == '['; return emit(l, Tokens.LSQUARE)
    elseif c == ']'; return emit(l, Tokens.RSQUARE)
    elseif c == '{'; return emit(l, Tokens.LBRACE)
    elseif c == ';'; return emit(l, Tokens.SEMICOLON)
    elseif c == '}'; return emit(l, Tokens.RBRACE)
    elseif c == '('; return emit(l, Tokens.LPAREN)
    elseif c == ')'; return emit(l, Tokens.RPAREN)
    elseif c == ','; return emit(l, Tokens.COMMA)
    elseif c == '*'; return lex_star(l);
    elseif c == '^'; return lex_circumflex(l);
    elseif c == '@'; return emit(l, Tokens.AT_SIGN)
    elseif c == '?'; return emit(l, Tokens.CONDITIONAL)
    elseif c == '$'; return lex_dollar(l);
    elseif c == '⊻'; return lex_xor(l);
    elseif c == '~'; return emit(l, Tokens.APPROX)
    elseif c == '#'; return lex_comment(l)
    elseif c == '='; return lex_equal(l)
    elseif c == '!'; return lex_exclaim(l)
    elseif c == '>'; return lex_greater(l)
    elseif c == '<'; return lex_less(l)
    elseif c == ':'; return lex_colon(l)
    elseif c == '|'; return lex_bar(l)
    elseif c == '&'; return lex_amper(l)
    elseif c == '\''; return lex_prime(l)
    elseif c == '÷'; return lex_division(l)
    elseif c == '"'; return lex_quote(l);
    elseif c == '%'; return lex_percent(l);
    elseif c == '/'; return lex_forwardslash(l);
    elseif c == '\\'; return lex_backslash(l);
    elseif c == '.'; return lex_dot(l);
    elseif c == '+'; return lex_plus(l);
    elseif c == '-'; return lex_minus(l);
    elseif c == '`'; return lex_cmd(l);
    elseif is_identifier_start_char(c); return lex_identifier(l, c)
    elseif isdigit(c); return lex_digit(l)
    elseif (k = get(UNICODE_OPS, c, Tokens.ERROR)) != Tokens.ERROR; return emit(l, k)
    else emit_error(l)
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
            c = readchar(l)
            if c == '\n' || eof(c)
                backup!(l)
                return doemit ? emit(l, Tokens.COMMENT) : EMPTY_TOKEN
            end
        end
    else
        c = readchar(l) # consume the '='
        n_start, n_end = 1, 0
        while true
            if eof(c)
                return doemit ? emit_error(l, Tokens.EOF_MULTICOMMENT) : EMPTY_TOKEN
            end
            nc = readchar(l)
            if c == '#' && nc == '='
                n_start += 1
            elseif c == '=' && nc == '#'
                n_end += 1
            end
            if n_start == n_end
                return doemit ? emit(l, Tokens.COMMENT) : EMPTY_TOKEN
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

function accept_number{F}(l::Lexer, f::F)
    !f(peekchar(l)) && return false
    while true
        if !accept(l, f)
            if accept(l, '_')
                if !f(peekchar(l))
                    backup!(l)
                    return true
                end
            else
                return true
            end
        end
    end
end

# A digit has been consumed
function lex_digit(l::Lexer)
    backup!(l)
    longest, kind = position(l), Tokens.ERROR

    # accept_batch(l, isdigit)
    accept_number(l, isdigit)

    if accept(l, '.')
        if peekchar(l) == '.' # 43.. -> [43, ..]
            backup!(l)
            return emit(l, Tokens.INTEGER)
        elseif !(isdigit(peekchar(l)) ||
            iswhitespace(peekchar(l)) ||
            is_identifier_start_char(peekchar(l))
            || peekchar(l) == '('
            || peekchar(l) == ')'
            || peekchar(l) == '['
            || peekchar(l) == ']'
            || peekchar(l) == '{'
            || peekchar(l) == '}'
            || peekchar(l) == ','
            || peekchar(l) == ';'
            || peekchar(l) == '@'
            || peekchar(l) == '`'
            || peekchar(l) == '"'
            || peekchar(l) == ':'
            || peekchar(l) == '?'
            || eof(l))
            backup!(l)
            return emit(l, Tokens.INTEGER)
        end
        accept_number(l, isdigit)
        if accept(l, '.')
            if peekchar(l) == '.' # 1.23.. -> [1.23, ..]
                backup!(l)
                return emit(l, Tokens.FLOAT)
            elseif !(isdigit(peekchar(l)) || iswhitespace(peekchar(l)) ||
                        is_identifier_start_char(peekchar(l)) || eof(peekchar(l))) # {1.23a, 1.23␣, 1.23EOF} -> [1.23, ?]
                backup!(l)
                return emit(l, Tokens.FLOAT)
            else # 3213.313.3123 is an error
                return emit_error(l)
            end
        elseif position(l) > longest # 323213.3232 candidate
            longest, kind = position(l), Tokens.FLOAT
        end
        if accept(l, "eEf") # 1313.[0-9]*e
            accept(l, "+-")
            if accept_batch(l, isdigit)
                if accept(l, '.' ) # 1.2e2.3 -> [ERROR, 3]
                    return emit_error(l)
                elseif position(l) > longest
                    longest, kind = position(l), Tokens.FLOAT
                end
            end
        end
    elseif accept(l, "eEf")
        accept(l, "+-")
        if accept_batch(l, isdigit)
            if accept(l, '.') # 1e2.3 -> [ERROR, 3]
                return emit_error(l)
            elseif position(l) > longest
                longest, kind = position(l), Tokens.FLOAT
            end
        else
            backup!(l)
            return emit(l, Tokens.INTEGER)
        end
    elseif position(l) > longest
        longest, kind = position(l), Tokens.INTEGER
    end

    seek2startpos!(l)

    # 0x[0-9A-Fa-f]+
    if accept(l, '0')
        if accept(l, 'x')
            if accept_number(l, ishex) && position(l) > longest
                longest, kind = position(l), Tokens.INTEGER
            end
        elseif accept(l, 'b')
            if accept_number(l, isbinary) && position(l) > longest
                longest, kind = position(l), Tokens.INTEGER
            end
        elseif accept(l, 'o')
            if accept_number(l, isoctal) && position(l) > longest
                longest, kind = position(l), Tokens.INTEGER
            end
        end
    end

    seek(l, longest)

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
                return doemit ? emit(l, Tokens.TRIPLE_STRING) : EMPTY_TOKEN
            else
                return doemit ? emit_error(l, Tokens.EOF_STRING) : EMPTY_TOKEN
            end
        else # empty string
            return doemit ? emit(l, Tokens.STRING) : EMPTY_TOKEN
        end
    else # "?, ? != '"'
        if read_string(l, Tokens.STRING)
            return doemit ? emit(l, Tokens.STRING) : EMPTY_TOKEN
        else
            return doemit ? emit_error(l, Tokens.EOF_STRING) : EMPTY_TOKEN
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
        return lex_digit(l)
    else
        return emit(l, Tokens.DOT)
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
            return doemit ? emit(l, Tokens.CMD) : EMPTY_TOKEN
        end
    end
    while true
        c = readchar(l)
        eof(c) && return (doemit ? emit_error(l, Tokens.EOF_CMD) : EMPTY_TOKEN)
        string_terminated(l, c, kind) && return (doemit ? emit(l, kind) : EMPTY_TOKEN)
    end
end

function tryread(l, str, k, c)
    for s in str
        c = readchar(l)
        if c != s
            if !is_identifier_char(c)
                backup!(l)
                return emit(l, IDENTIFIER)
            end
            return readrest(l, c)
        end
    end
    if is_identifier_char(peekchar(l))
        return readrest(l, c)
    end
    return emit(l, k)
end

function readrest(l, c)
    while is_identifier_char(c)
        if c == '!' && peekchar(l) == '='
            backup!(l)
            break
        elseif !is_identifier_char(peekchar(l))
            break
        end
        c = readchar(l)
    end

    return emit(l, IDENTIFIER)
end


function _doret(l, c)
    if !is_identifier_char(c)
        backup!(l)
        return emit(l, IDENTIFIER)
    else
        return readrest(l, c)
    end
end

function lex_identifier(l, c)
    if c == 'a'
        return tryread(l, ('b', 's', 't', 'r', 'a', 'c', 't'), ABSTRACT, c)
    elseif c == 'b'
        c = readchar(l)
        if c == 'a'
            return tryread(l, ('r', 'e', 'm', 'o', 'd', 'u', 'l', 'e'), BAREMODULE, c)
        elseif c == 'e'
            return tryread(l, ('g', 'i', 'n'), BEGIN, c)
        elseif c == 'i'
            return tryread(l, ('t', 's', 't', 'y', 'p', 'e'), BITSTYPE, c)
        elseif c == 'r'
            return tryread(l, ('e', 'a', 'k'), BREAK, c)
        else
            return _doret(l, c)
        end
    elseif c == 'c'
        c = readchar(l)
        if c == 'a'
            return tryread(l, ('t', 'c', 'h'), CATCH, c)
        elseif c == 'o'
            c = readchar(l)
            if c == 'n'
                c = readchar(l)
                if c == 's'
                    return tryread(l, ('t',), CONST, c)
                elseif c == 't'
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
        c = readchar(l)
        if c == 'l'
            c = readchar(l)
            if c == 's'
                c = readchar(l)
                if c == 'e'
                    c = readchar(l)
                    if !is_identifier_char(c)
                        backup!(l)
                        return emit(l, ELSE)
                    elseif c == 'i'
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
            return tryread(l, ('d'), END, c)
        elseif c == 'x'
            return tryread(l, ('p', 'o', 'r', 't'), EXPORT, c)
        else
            return _doret(l, c)
        end
    elseif c == 'f'
        c = readchar(l)
        if c == 'a'
            return tryread(l, ('l', 's', 'e'), FALSE, c)
        elseif c == 'i'
            return tryread(l, ('n', 'a', 'l', 'l', 'y'), FINALLY, c)
        elseif c == 'o'
            return tryread(l, ('r'), FOR, c)
        elseif c == 'u'
            return tryread(l, ('n', 'c', 't', 'i', 'o', 'n'), FUNCTION, c)
        else
            return _doret(l, c)
        end
    elseif c == 'g'
        return tryread(l, ('l', 'o', 'b', 'a', 'l'), GLOBAL, c)
    elseif c == 'i'
        c = readchar(l)
        if c == 'f'
            c = readchar(l)
            if !is_identifier_char(c)
                backup!(l)
                return emit(l, IF)
            else
                return readrest(l, c)
            end
        elseif c == 'm'
            c = readchar(l)
            if c == 'm'
                return tryread(l, ('u', 't', 'a', 'b', 'l', 'e'), IMMUTABLE, c)
            elseif c == 'p'
                c = readchar(l)
                if c == 'o'
                    c = readchar(l)
                    if c == 'r'
                        c = readchar(l)
                        if c == 't'
                            c = readchar(l)
                            if !is_identifier_char(c)
                                backup!(l)
                                return emit(l, IMPORT)
                            elseif c == 'a'
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
            c = readchar(l)
            if !is_identifier_char(c)
                backup!(l)
                return emit(l, IN)
            else
                return readrest(l, c)
            end
        elseif (@static VERSION >= v"0.6.0-dev.1471" ? true : false) && c == 's'
            return tryread(l, ('a'), ISA, c)
        else
            return _doret(l, c)
        end
    elseif c == 'l'
        c = readchar(l)
        if c == 'e'
            return tryread(l, ('t'), LET, c)
        elseif c == 'o'
            return tryread(l, ('c', 'a', 'l'), LOCAL, c)
        else
            return _doret(l, c)
        end
    elseif c == 'm'
        c = readchar(l)
        if c == 'a'
            return tryread(l, ('c', 'r', 'o'), MACRO, c)
        elseif c == 'o'
            return tryread(l, ('d', 'u', 'l', 'e'), MODULE, c)
        elseif c == 'u'
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
        c = readchar(l)
        if c == 'r'
            c = readchar(l)
            if c == 'u'
                return tryread(l, ('e'), TRUE, c)
            elseif c == 'y'
                c = readchar(l)
                if !is_identifier_char(c)
                    backup!(l)
                    return emit(l, TRY)
                else
                    return _doret(l, c)
                end
            else
                return _doret(l, c)
            end
        elseif c == 'y'
            c = readchar(l)
            if c == 'p'
                c = readchar(l)
                if c == 'e'
                    c = readchar(l)
                    if !is_identifier_char(c)
                        backup!(l)
                        return emit(l, TYPE)
                    elseif c == 'a'
                        return tryread(l, ('l', 'i', 'a', 's'), TYPEALIAS, c)
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
    elseif c == 'u'
        return tryread(l, ('s', 'i', 'n', 'g'), USING, c)
    elseif c == 'w'
        c = readchar(l)
        if c == 'h'
            c = readchar(l)
            if c == 'e'
                return tryread(l, ('r', 'e'), WHERE, c)
            elseif c == 'i'
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
