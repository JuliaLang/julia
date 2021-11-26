module JuliaSyntax

#-------------------------------------------------------------------------------
# Token stream utilities

import Tokenize
using Tokenize.Tokens: RawToken
const TzTokens = Tokenize.Tokens

include("token_kinds.jl")

"""
We define a token type which is more suited to parsing than the basic token
types from Tokenize.
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

function had_space(ts::TokenStream)
end

is_prec_assignment(tok) = K"BEGIN_ASSIGNMENTS" < kind(tok) < K"END_ASSIGNMENTS"


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

include("syntax_tree.jl")

function is_closing_token(ps::ParseState, tok)
    k = kind(tok)
    return k in (K"else", K"elseif", K"catch", K"finally",
                 K",", K")", K"]", K"}", K";",
                 K"ENDMARKER") || (k == K"END" && !ps.end_symbol)
end

function has_whitespace_prefix(tok::SyntaxToken)
    tok.leading_trivia.kind == K"WHITESPACE"
end

function TODO(str)
    error("TODO: $str")
end

# Parse numbers, identifiers, parenthesized expressions, lists, vectors, etc.
function parse_atom(ps::ParseState; checked::Bool=true)::SyntaxNode
    tok = require_token(ps)
    tok_kind = kind(tok)
    # TODO: Reorder these to put most likely tokens first
    if tok_kind == K":" # symbol/expression quote
        take_token!(ps)
        next = peek_token(ps)
        if is_closing_token(ps, next) && (kind(next) != K"KEYWORD" ||
                                          has_whitespace_prefix(next))
            return SyntaxNode(tok)
        elseif has_whitespace_prefix(next)
            error("whitespace not allowed after \":\" used for quoting")
        elseif kind(next) == K"NEWLINE_WS"
            error("newline not allowed after \":\" used for quoting")
        else
            # Being inside quote makes `end` non-special again. issue #27690
            ps1 = ParseState(ps, end_symbol=false)
            return SyntaxNode(K"quote", parse_atom(ps1, checked=false))
        end
    elseif tok_kind == K"=" # misplaced =
        error("unexpected `=`")
    elseif tok_kind == K"IDENTIFIER"
        if checked
            TODO("Checked identifier names")
        end
        take_token!(ps)
        return SyntaxNode(tok)
    elseif tok_kind == K"VAR_IDENTIFIER"
        take_token!(ps)
        return SyntaxNode(tok)
    elseif tok_kind == K"(" # parens or tuple
        take_token!(ps)
        return parse_paren(ps, checked)
    elseif tok_kind == K"[" # cat expression
        take_token!(ps)
        TODO("""parse_cat(ps, K"]", ps.end_symbol)""")
    elseif tok_kind == K"{" # cat expression
        take_token!(ps)
        TODO("""parse_cat(ps, K"}", ps.end_symbol)""")
    elseif tok_kind == K"`"
        TODO("(macrocall (core @cmd) ...)")
        # return Expr(:macrocall, Expr(:core, Symbol("@cmd")),
    elseif isliteral(tok_kind)
        take_token!(ps)
        return SyntaxNode(tok)
    elseif is_closing_token(tok)
        error("unexpected: $tok")
    else
        error("invalid syntax: `$tok`")
    end
end

# parse `a@b@c@...` for some @
#
# `is_separator` - predicate
# `head` the expression head to yield in the result, e.g. "a;b" => (block a b)
# `is_closer` - predicate to identify tokens that stop parsing
#               however, this doesn't consume the closing token, just looks at it
function parse_Nary(ps::ParseState, down::Function, is_separator::Function,
                    result_kind, is_closer::Function)
end

# flisp: parse-docstring
# Parse statement with possible docstring
function parse_statement_with_doc(ps::ParseState)
    parse_eq(ps)
    # TODO: Detect docstrings
end

#-------------------------------------------------------------------------------

# the principal non-terminals follow, in increasing precedence order

#function parse_block(ps::ParseState, down=parse_eq)
#end

# flisp: parse-stmts
# `;` at the top level produces a sequence of top level expressions
function parse_statements(ps::ParseState)
    parse_Nary(ps, parse_statement)
end

# flisp: parse-eq
function parse_eq(ps::ParseState)
    parse_assignment(ps, parse_comma)
end

# flisp: parse-eq*
# parse_eq_2 is used where commas are special, for example in an argument list
# function parse_eq_2

function parse_assignment(ps::ParseState, down)
    ex = down(ps)
    t = peek_token(ps)
    if !is_prec_assignment(t)
        return ex
    end
    take_token!(ps)
    if kind(t) == K"~"
        # ~ is the only non-syntactic assignment-precedence operator
        TODO("Turn ~ into a call node")
    else
        SyntaxNode
    end
end

#-------------------------------------------------------------------------------

function parse(code)
    tokens = JuliaSyntax.TokenStream(code)
    parse_statements(tokens)
end

end
