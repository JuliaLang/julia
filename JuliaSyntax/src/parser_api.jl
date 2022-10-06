# The main parser API.
#
# This is defined separately from parser.jl so that:
# * parser.jl doesn't need to refer to any tree data structures
# * It's clear which parts are the public API

struct ParseError <: Exception
    source::SourceFile
    diagnostics::Vector{Diagnostic}
end

function ParseError(stream::ParseStream; filename=nothing)
    source = SourceFile(sourcetext(stream), filename=filename)
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
                ignore_trivia=true, filename=nothing, ignore_warnings=false) where {T}
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
    if any_error(stream.diagnostics) || (!ignore_warnings && !isempty(stream.diagnostics))
        throw(ParseError(stream, filename=filename))
    end
    # TODO: Figure out a more satisfying solution to the wrap_toplevel_as_kind
    # mess that we've got here.
    # * It's kind of required for GreenNode, as GreenNode only records spans,
    #   not absolute positions.
    # * Dropping it would be ok for SyntaxNode and Expr...
    tree = build_tree(T, stream; wrap_toplevel_as_kind=K"toplevel", filename=filename)
    tree, last_byte(stream) + 1
end

_parse_docs = """
    parse(TreeType, text, [index];
          version=VERSION,
          ignore_trivia=true,
          filename=nothing,
          ignore_warnings=false)

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
parsing. To avoid exceptions due to warnings, use `ignore_warnings=true`.
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

