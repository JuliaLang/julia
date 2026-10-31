# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
TOML.jl is a Julia standard library for parsing and writing TOML v1.1 files.
This module provides functions to parse TOML strings and files into Julia data structures
and to serialize Julia data structures to TOML format.
"""
module TOML

using Dates

module Internals
    # The parser is defined in Base
    using Base.TOML: Parser, Printer, parse, tryparse, ParserError, reinit!,
                     Comments, CommentBlock
    # Put the error instances in this module
    for errtype in instances(Base.TOML.ErrorType)
        @eval using Base.TOML: $(Symbol(errtype))
    end
end

# https://github.com/JuliaLang/julia/issues/36605
_readstring(f::AbstractString) = isfile(f) ? read(f, String) : error(repr(f), ": No such file")

"""
    Parser()

Constructor for a TOML `Parser`.  Note that in most cases one does not need to
explicitly create a `Parser` but instead one directly uses
[`TOML.parsefile`](@ref) or [`TOML.parse`](@ref).  Using an explicit parser
will however reuse some internal data structures which can be beneficial for
performance if a larger number of small files are parsed.
"""
struct Parser
    _p::Internals.Parser{Dates}
end

# Dates-enabled constructors
Parser() = Parser(Internals.Parser{Dates}())
Parser(io::IO) = Parser(Internals.Parser{Dates}(io))
Parser(str::String; filepath=nothing) = Parser(Internals.Parser{Dates}(str; filepath))

"""
    Comments()

A container for the comments of a TOML document. Pass an empty `Comments`
object via the `comments` keyword argument of [`TOML.parsefile`](@ref),
[`TOML.tryparsefile`](@ref), [`TOML.parse`](@ref) or [`TOML.tryparse`](@ref)
to capture the comments of the parsed document into it, and pass it back via
the `comments` keyword argument of [`TOML.print`](@ref) to write the comments
out again. This allows modifying a TOML file without losing its comments:

```julia
comments = TOML.Comments()
data = TOML.parsefile("Project.toml"; comments)
# ... modify data ...
open("Project.toml", "w") do io
    TOML.print(io, data; comments)
end
```

Comments are associated with the items of the document (see the TOML stdlib
documentation for the precise rules): a block of whole-line comments directly
above an item and a comment on the same line as an item are attached to that
item and are printed with it (and dropped with it, if the item is removed from
the data). Whole-line comments separated from the following item by a blank
line are "floating": they are associated with the surrounding table and are
printed at the top of it.

!!! compat "Julia 1.14"
    This type requires Julia 1.14 or later.
"""
const Comments = Internals.Comments

"""
    parsefile(f::AbstractString; comments=nothing)
    parsefile(p::Parser, f::AbstractString; comments=nothing)

Parse file `f` and return the resulting table (dictionary). Throw a
[`ParserError`](@ref) upon failure.

If a [`TOML.Comments`](@ref) object is passed via the `comments` keyword
argument, it is emptied and the comments of the document are captured into it.

!!! compat "Julia 1.14"
    The `comments` keyword argument requires Julia 1.14 or later.

See also [`TOML.tryparsefile`](@ref).
"""
parsefile(f::AbstractString; comments::Union{Comments, Nothing}=nothing) =
    Internals.parse(Internals.Parser{Dates}(_readstring(f); filepath=abspath(f), comments))
parsefile(p::Parser, f::AbstractString; comments::Union{Comments, Nothing}=nothing) =
    Internals.parse(Internals.reinit!(p._p, _readstring(f); filepath=abspath(f), comments))

"""
    tryparsefile(f::AbstractString; comments=nothing)
    tryparsefile(p::Parser, f::AbstractString; comments=nothing)

Parse file `f` and return the resulting table (dictionary). Return a
[`ParserError`](@ref) upon failure.

If a [`TOML.Comments`](@ref) object is passed via the `comments` keyword
argument, it is emptied and the comments of the document are captured into it.

!!! compat "Julia 1.14"
    The `comments` keyword argument requires Julia 1.14 or later.

See also [`TOML.parsefile`](@ref).
"""
tryparsefile(f::AbstractString; comments::Union{Comments, Nothing}=nothing) =
    Internals.tryparse(Internals.Parser{Dates}(_readstring(f); filepath=abspath(f), comments))
tryparsefile(p::Parser, f::AbstractString; comments::Union{Comments, Nothing}=nothing) =
    Internals.tryparse(Internals.reinit!(p._p, _readstring(f); filepath=abspath(f), comments))

"""
    parse(x::Union{AbstractString, IO}; comments=nothing)
    parse(p::Parser, x::Union{AbstractString, IO}; comments=nothing)

Parse the string or stream `x`, and return the resulting table (dictionary).
Throw a [`ParserError`](@ref) upon failure.

If a [`TOML.Comments`](@ref) object is passed via the `comments` keyword
argument, it is emptied and the comments of the document are captured into it.

!!! compat "Julia 1.14"
    The `comments` keyword argument requires Julia 1.14 or later.

See also [`TOML.tryparse`](@ref).
"""
parse(p::Parser) = Internals.parse(p._p)
parse(str::AbstractString; comments::Union{Comments, Nothing}=nothing) =
    Internals.parse(Internals.Parser{Dates}(String(str); comments))
parse(p::Parser, str::AbstractString; comments::Union{Comments, Nothing}=nothing) =
    Internals.parse(Internals.reinit!(p._p, String(str); comments))
parse(io::IO; comments::Union{Comments, Nothing}=nothing) = parse(read(io, String); comments)
parse(p::Parser, io::IO; comments::Union{Comments, Nothing}=nothing) = parse(p, read(io, String); comments)

"""
    tryparse(x::Union{AbstractString, IO}; comments=nothing)
    tryparse(p::Parser, x::Union{AbstractString, IO}; comments=nothing)

Parse the string or stream `x`, and return the resulting table (dictionary).
Return a [`ParserError`](@ref) upon failure.

If a [`TOML.Comments`](@ref) object is passed via the `comments` keyword
argument, it is emptied and the comments of the document are captured into it.

!!! compat "Julia 1.14"
    The `comments` keyword argument requires Julia 1.14 or later.

See also [`TOML.parse`](@ref).
"""
tryparse(p::Parser) = Internals.tryparse(p._p)
tryparse(str::AbstractString; comments::Union{Comments, Nothing}=nothing) =
    Internals.tryparse(Internals.Parser{Dates}(String(str); comments))
tryparse(p::Parser, str::AbstractString; comments::Union{Comments, Nothing}=nothing) =
    Internals.tryparse(Internals.reinit!(p._p, String(str); comments))
tryparse(io::IO; comments::Union{Comments, Nothing}=nothing) = tryparse(read(io, String); comments)
tryparse(p::Parser, io::IO; comments::Union{Comments, Nothing}=nothing) = tryparse(p, read(io, String); comments)

"""
    ParserError

Type that is returned from [`tryparse`](@ref) and [`tryparsefile`](@ref)
when parsing fails. It contains (among others) the following fields:

- `pos`, the position in the string when the error happened
- `table`, the result that so far was successfully parsed
- `type`, an error type, different for different types of errors
"""
const ParserError = Internals.ParserError


"""
    print([to_toml::Function], io::IO [=stdout], data::AbstractDict; sorted=false, by=identity, inline_tables::IdSet{<:AbstractDict}, comments=nothing)

Write `data` as TOML syntax to the stream `io`. If the keyword argument `sorted` is set to `true`,
sort tables according to the function given by the keyword argument `by`. If the keyword argument
`inline_tables` is given, it should be a set of tables that should be printed "inline".

If a [`TOML.Comments`](@ref) object (as populated by the parsing functions) is
passed via the `comments` keyword argument, the comments in it are printed with
the items they are associated with. Comments associated with items that are not
present in `data` are ignored.

!!! compat "Julia 1.11"
    The `inline_tables` keyword argument is supported by Julia 1.11 or later.

!!! compat "Julia 1.14"
    The `comments` keyword argument requires Julia 1.14 or later.

The following data types are supported: `AbstractDict`, `AbstractVector`, `AbstractString`, `Integer`, `AbstractFloat`, `Bool`,
`Dates.DateTime`, `Dates.Time`, `Dates.Date`. Note that the integers and floats
need to be convertible to `Int64` and `Float64` respectively. For other data types,
pass the function `to_toml` that takes the data types and returns a value of a
supported type.
"""
const print = Internals.Printer.print

public Parser, parsefile, tryparsefile, parse, tryparse, ParserError, print, Comments

# These methods are private Base interfaces, but we do our best to support them over
# the TOML stdlib types anyway to minimize downstream breakage.
Base.TOMLCache(p::Parser) = Base.TOMLCache(p._p, Dict{String, Base.CachedTOMLDict}())
Base.TOMLCache(p::Parser, d::Base.CachedTOMLDict) = Base.TOMLCache(p._p, d)
Base.TOMLCache(p::Parser, d::Dict{String, Dict{String, Any}}) = Base.TOMLCache(p._p, d)

Internals.reinit!(p::Parser, str::String; filepath::Union{Nothing, String}=nothing,
                  comments::Union{Comments, Nothing}=nothing) =
    Internals.reinit!(p._p, str; filepath, comments)
Internals.parse(p::Parser) = Internals.parse(p._p)
Internals.tryparse(p::Parser) = Internals.tryparse(p._p)

include("precompile.jl")

end
