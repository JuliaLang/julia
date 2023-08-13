# The main parser API.
#
# This is defined separately from parser.jl so that:
# * parser.jl doesn't need to refer to any tree data structures
# * It's clear which parts are the public API

struct ParseError <: Exception
    source::SourceFile
    diagnostics::Vector{Diagnostic}
    incomplete_tag::Symbol # Used only for Base Expr(:incomplete) support
end

function ParseError(stream::ParseStream; incomplete_tag=:none, kws...)
    source = SourceFile(stream; kws...)
    ParseError(source, stream.diagnostics, incomplete_tag)
end

function Base.showerror(io::IO, err::ParseError)
    println(io, "ParseError:")
    # Only show the first parse error for now - later errors are often
    # misleading due to the way recovery works
    i = findfirst(is_error, err.diagnostics)
    if isnothing(i)
        i = lastindex(err.diagnostics)
    end
    show_diagnostics(io, err.diagnostics[1:i], err.source)
end

"""
    parse!(stream::ParseStream; rule=:all)

Parse Julia source code from a [`ParseStream`](@ref) object. Output tree data
structures may be extracted from `stream` with the [`build_tree`](@ref) function.

`rule` may be any of
* `:all` (default) — parse a whole "file" of top level statements. In this
  mode, the parser expects to fully consume the input.
* `:statement` — parse a single statement, or statements separated by semicolons.
* `:atom` — parse a single syntax "atom": a literal, identifier, or
  parenthesized expression.
"""
function parse!(stream::ParseStream; rule::Symbol=:all)
    if rule == :toplevel
        Base.depwarn("Use of rule == :toplevel in parse!() is deprecated. use `rule=:all` instead.", :parse!)
        rule = :all
    end
    ps = ParseState(stream)
    if rule === :all
        parse_toplevel(ps)
    elseif rule === :statement
        parse_stmts(ps)
    elseif rule === :atom
        parse_atom(ps)
    else
        throw(ArgumentError("Unknown grammar rule $rule"))
    end
    validate_tokens(stream)
    stream
end

"""
    parse!(TreeType, io::IO; rule=:all, version=VERSION)

Parse Julia source code from a seekable `IO` object. The output is a tuple
`(tree, diagnostics)`. When `parse!` returns, the stream `io` is positioned
directly after the last byte which was consumed during parsing.
"""
function parse!(::Type{TreeType}, io::IO;
                rule::Symbol=:all, version=VERSION, kws...) where {TreeType}
    stream = ParseStream(io; version=version)
    parse!(stream; rule=rule)
    tree = build_tree(TreeType, stream; kws...)
    seek(io, last_byte(stream))
    tree, stream.diagnostics
end

function _parse(rule::Symbol, need_eof::Bool, ::Type{T}, text, index=1; version=VERSION,
                ignore_trivia=true, filename=nothing, first_line=1, ignore_errors=false,
                ignore_warnings=ignore_errors, kws...) where {T}
    stream = ParseStream(text, index; version=version)
    if ignore_trivia && rule != :all
        bump_trivia(stream, skip_newlines=true)
    end
    parse!(stream; rule=rule)
    if need_eof
        if (ignore_trivia  && peek(stream, skip_newlines=true) != K"EndMarker") ||
           (!ignore_trivia && (peek(stream, skip_newlines=false, skip_whitespace=false) != K"EndMarker"))
            emit_diagnostic(stream, error="unexpected text after parsing $rule")
        end
    end
    if (!ignore_errors && any_error(stream.diagnostics)) ||
          (!ignore_warnings && !isempty(stream.diagnostics))
        throw(ParseError(stream, filename=filename, first_line=first_line))
    end
    tree = build_tree(T, stream; filename=filename, first_line=first_line, kws...)
    tree, last_byte(stream) + 1
end

_parse_docs = """
    # Parse a single expression/statement
    parsestmt(TreeType, text, [index];
              version=VERSION,
              ignore_trivia=true,
              filename=nothing,
              ignore_errors=false,
              ignore_warnings=ignore_errors)

    # Parse all statements at top level (file scope)
    parseall(...)

    # Parse a single syntax atom
    parseatom(...)

Parse Julia source code string `text` into a data structure of type `TreeType`.
`parsestmt` parses a single Julia statement, `parseall` parses top level statements
at file scope and `parseatom` parses a single Julia identifier or other "syntax
atom".

If `text` is passed without `index`, all the input text must be consumed and a
tree data structure is returned. When an integer byte `index` is passed, a
tuple `(tree, next_index)` will be returned containing the next index in `text`
to resume parsing. By default whitespace and comments before and after valid
code are ignored but you can turn this off by setting `ignore_trivia=false`.

`version` (default `VERSION`) may be used to set the syntax version to
any Julia version `>= v"1.0"`. We aim to parse all Julia syntax which has been
added after v"1.0", emitting an error if it's not compatible with the requested
`version`.

Pass `filename` to set any file name information embedded within the output
tree, if applicable. This will also annotate errors and warnings with the
source file name.

A `ParseError` will be thrown if any errors or warnings occurred during
parsing. To avoid exceptions due to warnings, use `ignore_warnings=true`. To
also avoid exceptions due to errors, use `ignore_errors=true`.
"""

"$_parse_docs"
parsestmt(::Type{T}, text::AbstractString; kws...) where {T} = _parse(:statement, true, T, text; kws...)[1]

"$_parse_docs"
parseall(::Type{T}, text::AbstractString; kws...) where {T} = _parse(:all, true, T, text; kws...)[1]

"$_parse_docs"
parseatom(::Type{T}, text::AbstractString; kws...) where {T} = _parse(:atom, true, T, text; kws...)[1]

parsestmt(::Type{T}, text::AbstractString, index::Integer; kws...) where {T} = _parse(:statement, false, T, text, index; kws...)
parseall(::Type{T}, text::AbstractString, index::Integer; kws...) where {T} = _parse(:all, false, T, text, index; kws...)
parseatom(::Type{T}, text::AbstractString, index::Integer; kws...) where {T} = _parse(:atom, false, T, text, index; kws...)

#-------------------------------------------------------------------------------
# Tokens interface
"""
Token type resulting from calling `tokenize(text)`

Use
* `kind(tok)` to get the token kind
* `untokenize(tok, text)` to retreive the text
* Predicates like `is_error(tok)` to query token categories and flags
"""
struct Token
    head::SyntaxHead
    range::UnitRange{UInt32}
end

Token() = Token(SyntaxHead(K"None", EMPTY_FLAGS), 0:0)

head(t::Token) = t.head

"""
    tokenize(text)

Returns the tokenized UTF-8 encoded `text` as a vector of `Token`s. The
text for the token can be retreived by using `untokenize()`. The full text can be
reconstructed with, for example, `join(untokenize.(tokenize(text), text))`.

This interface works on UTF-8 encoded string or buffer data only.
"""
function tokenize(text)
    ps = ParseStream(text)
    parse!(ps, rule=:all)
    ts = ps.tokens
    output_tokens = Token[]
    for i = 2:length(ts)
        if kind(ts[i]) == K"TOMBSTONE"
            continue
        end
        r = ts[i-1].next_byte:ts[i].next_byte-1
        push!(output_tokens, Token(head(ts[i]), r))
    end
    output_tokens
end

function untokenize(token::Token, text::AbstractString)
    text[first(token.range):thisind(text, last(token.range))]
end

function untokenize(token::Token, text::Vector{UInt8})
    text[token.range]
end

@deprecate parse parsestmt
