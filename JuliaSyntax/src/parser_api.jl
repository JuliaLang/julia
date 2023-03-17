# The main parser API.
#
# This is defined separately from parser.jl so that:
# * parser.jl doesn't need to refer to any tree data structures
# * It's clear which parts are the public API

struct ParseError <: Exception
    source::SourceFile
    diagnostics::Vector{Diagnostic}
end

function ParseError(stream::ParseStream; kws...)
    source = SourceFile(sourcetext(stream); kws...)
    ParseError(source, stream.diagnostics)
end

function Base.showerror(io::IO, err::ParseError, bt; backtrace=false)
    println(io, "ParseError:")
    show_diagnostics(io, err.diagnostics, err.source)
end

function Base.showerror(io::IO, err::ParseError)
    println(io, "ParseError:")
    show_diagnostics(io, err.diagnostics, err.source)
end

Base.display_error(io::IO, err::ParseError, bt) = Base.showerror(io, err, bt)


"""
    parse!(stream::ParseStream; rule=:toplevel)

Parse Julia source code from a [`ParseStream`](@ref) object. Output tree data
structures may be extracted from `stream` with the [`build_tree`](@ref) function.

`rule` may be any of
* `:toplevel` (default) — parse a whole "file" of top level statements. In this
  mode, the parser expects to fully consume the input.
* `:statement` — parse a single statement, or statements separated by semicolons.
* `:atom` — parse a single syntax "atom": a literal, identifier, or
  parenthesized expression.
"""
function parse!(stream::ParseStream; rule::Symbol=:toplevel)
    ps = ParseState(stream)
    if rule === :toplevel
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
    parse!(TreeType, io::IO; rule=:toplevel, version=VERSION)

Parse Julia source code from a seekable `IO` object. The output is a tuple
`(tree, diagnostics)`. When `parse!` returns, the stream `io` is positioned
directly after the last byte which was consumed during parsing.
"""
function parse!(::Type{TreeType}, io::IO;
                rule::Symbol=:toplevel, version=VERSION, kws...) where {TreeType}
    stream = ParseStream(io; version=version)
    parse!(stream; rule=rule)
    tree = build_tree(TreeType, stream; kws...)
    seek(io, last_byte(stream))
    tree, stream.diagnostics
end

function _parse(rule::Symbol, need_eof::Bool, ::Type{T}, text, index=1; version=VERSION,
                ignore_trivia=true, filename=nothing, first_line=1, ignore_errors=false,
                ignore_warnings=ignore_errors) where {T}
    stream = ParseStream(text, index; version=version)
    if ignore_trivia && rule != :toplevel
        bump_trivia(stream, skip_newlines=true)
        empty!(stream)
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
    # TODO: Figure out a more satisfying solution to the wrap_toplevel_as_kind
    # mess that we've got here.
    # * It's kind of required for GreenNode, as GreenNode only records spans,
    #   not absolute positions.
    # * Dropping it would be ok for SyntaxNode and Expr...
    tree = build_tree(T, stream; wrap_toplevel_as_kind=K"toplevel", filename=filename, first_line=first_line)
    tree, last_byte(stream) + 1
end

_parse_docs = """
    parse(TreeType, text, [index];
          version=VERSION,
          ignore_trivia=true,
          filename=nothing,
          ignore_errors=false,
          ignore_warnings=ignore_errors)

    # Or, with the same arguments
    parseall(...)
    parseatom(...)

Parse Julia source code string `text` into a data structure of type `TreeType`.
`parse` parses a single Julia statement, `parseall` parses top level statements
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

parse(::Type{T}, text::AbstractString; kws...) where {T} = _parse(:statement, true, T, text; kws...)[1]
parseall(::Type{T}, text::AbstractString; kws...) where {T} = _parse(:toplevel, true, T, text; kws...)[1]
parseatom(::Type{T}, text::AbstractString; kws...) where {T} = _parse(:atom, true, T, text; kws...)[1]

@eval @doc $_parse_docs parse
@eval @doc $_parse_docs parseall
@eval @doc $_parse_docs parseatom

parse(::Type{T}, text::AbstractString, index::Integer; kws...) where {T} = _parse(:statement, false, T, text, index; kws...)
parseall(::Type{T}, text::AbstractString, index::Integer; kws...) where {T} = _parse(:toplevel, false, T, text, index; kws...)
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
    parse!(ps, rule=:toplevel)
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
