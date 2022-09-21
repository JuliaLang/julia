# The main parser API.
#
# This is defined separately from parser.jl so that:
# * parser.jl doesn't need to refer to any tree data structures
# * It's clear which parts are the public API
#
# What should the general parsing API look like? Some points to consider:
#
# * After parsing atoms or statements or most other internal rules, it's
#   usual to start in the middle of the input text and end somewhere else in
#   the middle of the input text. So we should taken an index for the start of
#   parsing and supply an index back to the caller after parsing.
#
# * `parseall` is a special case where we expect to consume all the input.
#   Perhaps this is the API which throws an error if we don't consume it all,
#   and doesn't accept an index as input?
#
# * The ParseStream is the fundamental interface which wraps the code string
#   and index up together for input and contains the output events, diagnostics
#   and current stream position after parsing. The user should potentially be
#   able to use this directly. It does, however assume a Julia-compatible token
#   stream.
#
# * It could be useful to support an IO-based interface so that users can parse
#   Julia code intermixed with other DSLs. Documenter.jl and string macros come
#   to mind as examples which could use this. A tricky part is deciding where
#   the input ends: For string macros this is done by the parser, but for
#   Documenter it's probably just done beforehand according to the Markdown
#   code block rules.
#
# * The API should have an interface where a simple string is passed in. How
#   does SourceFile relate to this?
#
# * It's neat for `parse` to be overloadable to produce various output data
#   structures; GreenNode, SyntaxNode, Expr, (etc?) in the same way that
#   Base.parse can be used for non-Julia code.  (Heh... though
#   `Base.parse(Expr, "...")` would also make a certain amount of sense.)
#
# * What's the no-copy API look like? A String can be put into an IOBuffer via
#   unsafe_wrap(Vector{UInt8}, str) ... A SubString likewise. Also there's the
#   `codeunits` function to hold a GC-safe view of string data as an array (but
#   we can't use a Vector{UInt8})

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
    # Input and output:
    stream = parse(stream::ParseStream; kws...)
    (tree, diagnostics)        = parse(TreeType, io::IOBuffer; kws...)
    (tree, diagnostics, index) = parse(TreeType, str::AbstractString, [index::Integer]; kws...)
    # Keywords
    parse(...; rule=:toplevel, version=VERSION, ignore_trivia=true)

Parse Julia source code from `input`, returning the output in a format
compatible with `input`:

* When `input` is a `ParseStream`, the stream itself is returned and the
  `ParseStream` interface can be used to process the output.
* When `input` is a seekable `IO` subtype, the output is `(tree, diagnostics)`.
  The buffer `position` will be set to the next byte of input.
* When `input` is an `AbstractString, Integer`, or `Vector{UInt8}, Integer` the
  output is `(tree, diagnostics, index)`, where `index` (default 1) is the next
  byte of input.

`rule` may be any of
* `toplevel` (default) — parse a whole "file" of top level statements. In this
  mode, the parser expects to fully consume the input.
* `statement` — parse a single statement, or statements separated by semicolons.
* `atom` — parse a single syntax "atom": a literal, identifier, or
  parenthesized expression.

`version` (default `VERSION`) may be used to set the syntax version to
any Julia version `>= v"1.0"`. We aim to parse all Julia syntax which has been
added after v"1.0", emitting an error if it's not compatible with the requested
`version`.

See also [`parseall`](@ref) for a simpler but less powerful interface.
"""
function parse(stream::ParseStream; rule::Symbol=:toplevel)
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

function parse(::Type{T}, io::IO;
               rule::Symbol=:toplevel, version=VERSION, kws...) where {T}
    stream = ParseStream(io; version=version)
    parse(stream; rule=rule)
    tree = build_tree(T, stream; kws...)
    seek(io, last_byte(stream))
    tree, stream.diagnostics
end

# Generic version of parse for all other cases where an index must be passed
# back - ie strings and buffers
function parse(::Type{T}, input...;
               rule::Symbol=:toplevel, version=VERSION, kws...) where {T}
    stream = ParseStream(input...; version=version)
    parse(stream; rule=rule)
    tree = build_tree(T, stream; kws...)
    tree, stream.diagnostics, last_byte(stream) + 1
end


"""
    parseall(TreeType, input...;
             rule=:toplevel,
             version=VERSION,
             ignore_trivia=true)

Experimental convenience interface to parse `input` as Julia code, emitting an
error if the entire input is not consumed. `input` can be a string or any other
valid input to the `ParseStream` constructor. By default `parseall` will ignore
whitespace and comments before and after valid code but you can turn this off
by setting `ignore_trivia=false`.

A `ParseError` will be thrown if any errors occurred during parsing.

See [`parse`](@ref) for a more complete and powerful interface to the parser,
as well as a description of the `version` and `rule` keywords.
"""
function parseall(::Type{T}, input...; rule=:toplevel, version=VERSION,
                  ignore_trivia=true, filename=nothing) where {T}
    stream = ParseStream(input...; version=version)
    if ignore_trivia && rule != :toplevel
        bump_trivia(stream, skip_newlines=true)
        empty!(stream)
    end
    parse(stream; rule=rule)
    if (ignore_trivia  && peek(stream, skip_newlines=true) != K"EndMarker") ||
       (!ignore_trivia && (peek(stream, skip_newlines=false, skip_whitespace=false) != K"EndMarker"))
        emit_diagnostic(stream, error="unexpected text after parsing $rule")
    end
    if any_error(stream.diagnostics)
        throw(ParseError(stream, filename=filename))
    end
    # TODO: Figure out a more satisfying solution to the wrap_toplevel_as_kind
    # mess that we've got here.
    # * It's kind of required for GreenNode, as GreenNode only records spans,
    #   not absolute positions.
    # * Dropping it would be ok for SyntaxNode and Expr...
    tree = build_tree(T, stream; wrap_toplevel_as_kind=K"toplevel", filename=filename)
    if !isempty(stream.diagnostics)
        # Crudely format any warnings to the current logger.
        buf = IOBuffer()
        show_diagnostics(IOContext(buf, stdout), stream,
                         SourceFile(sourcetext(stream, steal_textbuf=true), filename=filename))
        @warn Text(String(take!(buf)))
    end
    tree
end

