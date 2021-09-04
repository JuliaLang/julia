module JuliaSyntax

#-------------------------------------------------------------------------------
# Token stream utilities

import Tokenize
using Tokenize.Tokens: RawToken

"""
We define a token type which is more suited to parsing than the basic token
types from Tokenize.
"""
struct SyntaxToken
    raw::RawToken
    leading_trivia::RawToken
end

#=
function Base.show(io::IO, mime::MIME"text/plain", token)
    show(io, mime, RawToken())
end
=#

kind(tok::SyntaxToken) = tok.raw.kind

const EMPTY_RAW_TOKEN = RawToken()
const EMPTY_TOKEN = SyntaxToken(RawToken(), RawToken())

"""
TokenStream wraps the lexer from Tokenize.jl with a short putback buffer and
condenses syntactically irrelevant whitespace tokens into "syntax trivia" which
are attached to other tokens.
"""
mutable struct TokenStream
    lexer::Tokenize.Lexers.Lexer{IOBuffer,RawToken}
    # We buffer up to two tokens here, with `next2` taken before `next1`. It
    # suffices to support only a single putback token (which always goes into
    # `next2`). The presence of a valid token in `next2` does not imply there's
    # one in `next1`.
    next1::SyntaxToken
    next2::SyntaxToken
    hasnext1::Bool
    hasnext2::Bool
end

function TokenStream(code)
    lexer = Tokenize.tokenize(code, RawToken)
    TokenStream(lexer, EMPTY_TOKEN, EMPTY_TOKEN, false, false)
end

function Base.show(io::IO, mime::MIME"text/plain", ts::TokenStream)
    print(io, TokenStream, ":\n  lexer = ")
    show(io, mime, ts.lexer)
    if ts.hasnext2
        print(io, "\n  next2  = ", ts.next2)
    end
    if ts.hasnext1
        print(io, "\n  next1  = ", ts.next1)
    end
end

# Iterator interface
Base.IteratorSize(::Type{TokenStream}) = Base.SizeUnknown()
Base.IteratorEltype(::Type{TokenStream}) = Base.HasEltype()
Base.eltype(::Type{TokenStream}) = SyntaxToken

function Base.iterate(ts::TokenStream, end_state=false)
    end_state && return nothing
    t = take_token!(ts)
    return t, kind(t) == Tokens.ENDMARKER
end

function _read_raw_token(lexer::Tokenize.Lexers.Lexer)
    c = Tokenize.Lexers.peekchar(lexer)
    if isspace(c)
        # We lex whitespace slightly differently from Tokenize.jl, as newlines
        # are syntactically significant
        if Tokenize.Lexers.accept(lexer, '\n')
            return Tokenize.Lexers.emit(lexer, Tokens.NEWLINE_WS)
        else
            Tokenize.Lexers.readon(lexer)
            Tokenize.Lexers.accept_batch(lexer, c->isspace(c) && c != '\n')
            return Tokenize.Lexers.emit(lexer, Tokens.WHITESPACE)
        end
    else
        return Tokenize.Lexers.next_token(lexer) 
    end
end

function _read_token(lexer::Tokenize.Lexers.Lexer)
    # No token - do the actual work of taking a token from the lexer
    leading_trivia = EMPTY_RAW_TOKEN
    raw = _read_raw_token(lexer) 
    if Tokens.exactkind(raw) == Tokens.WHITESPACE
        leading_trivia = raw
        raw = _read_raw_token(lexer) 
    end
    return SyntaxToken(raw, leading_trivia)
end

# Return next token in the stream, but don't remove it.
function peek_token(ts::TokenStream)
    ts.hasnext2 && return ts.next2
    ts.hasnext1 && return ts.next1
    ts.next1 = _read_token(ts.lexer)
    ts.hasnext1 = true
    return ts.next1
end

# Like peek_token, but 
# * EOF becomes an error
# * Newlines tokens are gobbled (TODO!)
function require_token(ts::TokenStream)
    tok = peek_token(ts)
    if kind(tok) == Tokens.ENDMARKER
        error("incomplete: premature end of input")
    end
    return tok
end

# Remove next token from from the stream and return it.
function take_token!(ts::TokenStream)
    if ts.hasnext2
        ts.hasnext2 = false
        return ts.next2
    end
    if ts.hasnext1
        ts.hasnext1 = false
        return ts.next1
    end
    # This line is a departure from the scheme parser, which requires
    # peek_token to be called
    return _read_token(ts.lexer)
end

function put_back!(ts::TokenStream, tok::RawToken)
    ts.hasnext2 || error("Cannot put back two tokens")
    ts.next2 = tok
end

function had_space(ts::TokenStream)
end

#-------------------------------------------------------------------------------

"""
ParseState carries parser context as we recursively descend into the parse
tree. For example, normally `x -y` means `(x) - (y)`, but when parsing matrix
literals we're in "whitespace sensitive" mode, and `[x -y]` means [(x) (-y)].
"""
struct ParseState
    tokens::TokenStream

    # Disable range colon for parsing ternary conditional operator
    range_colon_enabled::Bool
    # In space-sensitive mode "x -y" is 2 expressions, not a subtraction
    space_sensitive::Bool
    # Seeing `for` stops parsing macro arguments and makes a generator
    for_generator::Bool
    # Treat 'end' like a normal symbol instead of a reserved word
    end_symbol::Bool
    # Treat newline like ordinary whitespace instead of as a potential separator
    whitespace_newline::Bool
    # Enable parsing `where` with high precedence
    where_enabled::Bool
end

# Normal context
function ParseState(tokens::TokenStream)
    ParseState(tokens, true, false, true, false, false, false)
end

function ParseState(ps::ParseState; range_colon_enabled=nothing,
                    space_sensitive=nothing, for_generator=nothing,
                    end_symbol=nothing, whitespace_newline=nothing,
                    where_enabled=nothing)
    ParseState(ps.tokens,
        range_colon_enabled === nothing ? ps.range_colon_enabled : range_colon_enabled,
        space_sensitive === nothing ? ps.space_sensitive : space_sensitive,
        for_generator === nothing ? ps.for_generator : for_generator,
        end_symbol === nothing ? ps.end_symbol : end_symbol,
        whitespace_newline === nothing ? ps.whitespace_newline : whitespace_newline,
        where_enabled === nothing ? ps.where_enabled : where_enabled)
end

take_token!(ps::ParseState) = take_token!(ps.tokens)
require_token(ps::ParseState) = require_token(ps.tokens)
peek_token(ps::ParseState) = peek_token(ps.tokens)
put_back!(ps::ParseState, tok::RawToken) = put_back!(ps.tokens, tok)


#-------------------------------------------------------------------------------
# Parser

function is_closing_token(ps::ParseState, tok)
    k = kind(tok)
    return k in (Tokens.ELSE, Tokens.ELSEIF, Tokens.CATCH, Tokens.FINALLY,
                 Tokens.COMMA, Tokens.LPAREN, Tokens.RSQUARE, Tokens.RBRACE,
                 Tokens.SEMICOLON, Tokens.ENDMARKER) ||
        k == Tokens.END && !ps.end_symbol
end

function has_whitespace_prefix(tok::SyntaxToken)
    tok.leading_trivia.kind == Tokens.WHITESPACE
end


# Parse numbers, identifiers, parenthesized expressions, lists, vectors, etc.
function parse_atom(ps::ParseState; checked::Bool=true)
    tok = require_token(ps)
    tok_kind = kind(tok)
    if tok_kind == Tokens.COLON # symbol/expression quote
        take_token!(ps)
        next = peek_token(ps)
        if is_closing_token(ps, next) && (kind(next) != Tokens.KEYWORD ||
                                          has_whitespace_prefix(next))
            return Symbol(":") # FIXME: CST NODE ???
        elseif has_whitespace_prefix(next)
            error("whitespace not allowed after \":\" used for quoting")
        elseif kind(next) == Tokens.NEWLINE_WS
            error("newline not allowed after \":\" used for quoting")
        else
            # Being inside quote makes `end` non-special again. issue #27690
            ps1 = ParseState(ps, end_symbol=false)
            return Expr(:quote, parse_atom(ps1, checked=false))
        end
    elseif tok_kind == Tokens.EQ # misplaced =
        error("unexpected `=`")
    elseif tok_kind == Tokens.IDENTIFIER
        if checked
            # FIXME: Check identifier names
        end
        take_token!(ps)
    else
        return :heloooo
    end
end

end
