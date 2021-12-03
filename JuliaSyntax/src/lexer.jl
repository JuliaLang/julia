#-------------------------------------------------------------------------------
"""
`SyntaxToken` covers a contiguous range of the source text which contains a
token *relevant for parsing*, with a possibly-irrelevant prefix of "token
trivia". Trivial tokens include
* Whitespace
* Comments

Note that "triviality" of tokens is context-dependent in general. For example,
the parentheses in `(1+2)*3` are important for parsing but are irrelevant after
the abstract syntax tree is constructed.
"""
struct SyntaxToken
    # TODO: Could use a more stripped down version of RawToken which only
    # stores byte offsets?
    leading_trivia::RawToken
    raw::RawToken
end

function Base.show(io::IO, t::SyntaxToken)
    fullrange = string(lpad(t.leading_trivia.startbyte+1, 3), ":", rpad(t.raw.endbyte+1, 3))

    range = string(lpad(t.raw.startbyte+1, 3), ":", rpad(t.raw.endbyte+1, 3))
    print(io, rpad(string(fullrange, "│", range), 17, " "), rpad(kind(t), 15, " "))
end


kind(tok::SyntaxToken) = tok.raw.kind

# summary_kind(tok::SyntaxToken) = TzTokens.kind(tok.raw)

const EMPTY_RAW_TOKEN = RawToken()
const EMPTY_TOKEN = SyntaxToken(RawToken(), RawToken())


#-------------------------------------------------------------------------------
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

# TODO: replace TokenStream with "ParseStream"/"ParserIO"/? interface
#
# This would be an I/O interface for the parser
# - Input: Provides input tokens to the parser
# - Output: Accepts tree output events from the parser
#
# Such an interface can be used to decouple parsing from the input and output
# representations as is done in rust-analyzer's TreeSink. Part of the point of
# this is to have a place to preserve whitespace trivia outside the parser. (
# The rust TextTreeSink is oddly named, as it appears to be used for both
# getting tokens and emitting nodes... see
# https://github.com/rust-analyzer/rust-analyzer/blob/4691a0647b2c96cc475d8bbe7c31fe194d1443e7/crates/syntax/src/parsing/text_tree_sink.rs )
#
# struct ParseStream
# end

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
    return t, kind(t) == K"ENDMARKER"
end

function _read_raw_token(lexer::Tokenize.Lexers.Lexer)
    c = Tokenize.Lexers.peekchar(lexer)
    if isspace(c)
        Tokenize.Lexers.start_token!(lexer)
        # We lex whitespace slightly differently from Tokenize.jl, as newlines
        # are syntactically significant
        if Tokenize.Lexers.accept(lexer, '\n')
            return Tokenize.Lexers.emit(lexer, K"NEWLINE_WS")
        else
            Tokenize.Lexers.readon(lexer)
            Tokenize.Lexers.accept_batch(lexer, c->isspace(c) && c != '\n')
            return Tokenize.Lexers.emit(lexer, K"WHITESPACE")
        end
    else
        return Tokenize.Lexers.next_token(lexer)
    end
end

function _read_token(lexer::Tokenize.Lexers.Lexer)
    # No token - do the actual work of taking a token from the lexer
    raw = _read_raw_token(lexer)
    if TzTokens.exactkind(raw) in (K"WHITESPACE", K"COMMENT")
        # TODO: *Combine* comments with whitespace here to get a single leading
        # trivia item per real token.
        leading_trivia = raw
        raw = _read_raw_token(lexer)
    else
        leading_trivia = RawToken(K"ERROR", (0,0), (0,0),
                                  raw.startbyte, raw.startbyte-1,
                                  TzTokens.NO_ERR, false, false)
    end
    return SyntaxToken(leading_trivia, raw)
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
    if kind(tok) == K"ENDMARKER"
        error("incomplete: premature end of input")
    end
    return tok
end

# Remove next token from the stream and return it.
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

is_prec_assignment(tok) = K"BEGIN_ASSIGNMENTS" < kind(tok) < K"END_ASSIGNMENTS"

