# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
TOML.jl is a Julia standard library for parsing and writing TOML v1.0 files.
This module provides functions to parse TOML strings and files into Julia data structures
and to serialize Julia data structures to TOML format.
"""
module TOML

using Dates

module Internals
    # The parser is defined in Base
    using Base.TOML: Parser, parse, tryparse, ParserError, isvalid_barekey_char, reinit!
    # Put the error instances in this module
    for errtype in instances(Base.TOML.ErrorType)
        @eval using Base.TOML: $(Symbol(errtype))
    end
    # We put the printing functionality in a separate module since It
    # defines a function `print` and we don't want that to collide with normal
    # usage of `(Base.)print` in other files
    module Printer
        include("print.jl")
    end
end

# https://github.com/JuliaLang/julia/issues/36605
_readstring(f::AbstractString) = isfile(f) ? read(f, String) : error(repr(f), ": No such file")

"""
    Parser()

Constructor for a TOML `Parser`.  Note that in most cases one does not need to
explicitly create a `Parser` but instead one directly use use
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
    parsefile(f::AbstractString)
    parsefile(p::Parser, f::AbstractString)

Parse file `f` and return the resulting table (dictionary). Throw a
[`ParserError`](@ref) upon failure.

See also [`TOML.tryparsefile`](@ref).
"""
parsefile(f::AbstractString) =
    Internals.parse(Internals.Parser{Dates}(_readstring(f); filepath=abspath(f)))
parsefile(p::Parser, f::AbstractString) =
    Internals.parse(Internals.reinit!(p._p, _readstring(f); filepath=abspath(f)))

"""
    tryparsefile(f::AbstractString)
    tryparsefile(p::Parser, f::AbstractString)

Parse file `f` and return the resulting table (dictionary). Return a
[`ParserError`](@ref) upon failure.

See also [`TOML.parsefile`](@ref).
"""
tryparsefile(f::AbstractString) =
    Internals.tryparse(Internals.Parser{Dates}(_readstring(f); filepath=abspath(f)))
tryparsefile(p::Parser, f::AbstractString) =
    Internals.tryparse(Internals.reinit!(p._p, _readstring(f); filepath=abspath(f)))

"""
    parse(x::Union{AbstractString, IO})
    parse(p::Parser, x::Union{AbstractString, IO})

Parse the string  or stream `x`, and return the resulting table (dictionary).
Throw a [`ParserError`](@ref) upon failure.

See also [`TOML.tryparse`](@ref).
"""
parse(p::Parser) = Internals.parse(p._p)
parse(str::AbstractString) =
    Internals.parse(Internals.Parser{Dates}(String(str)))
parse(p::Parser, str::AbstractString) =
    Internals.parse(Internals.reinit!(p._p, String(str)))
parse(io::IO) = parse(read(io, String))
parse(p::Parser, io::IO) = parse(p, read(io, String))

"""
    tryparse(x::Union{AbstractString, IO})
    tryparse(p::Parser, x::Union{AbstractString, IO})

Parse the string or stream `x`, and return the resulting table (dictionary).
Return a [`ParserError`](@ref) upon failure.

See also [`TOML.parse`](@ref).
"""
tryparse(p::Parser) = Internals.tryparse(p._p)
tryparse(str::AbstractString) =
    Internals.tryparse(Internals.Parser{Dates}(String(str)))
tryparse(p::Parser, str::AbstractString) =
    Internals.tryparse(Internals.reinit!(p._p, String(str)))
tryparse(io::IO) = tryparse(read(io, String))
tryparse(p::Parser, io::IO) = tryparse(p, read(io, String))

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
    print([to_toml::Function], io::IO [=stdout], data::AbstractDict; sorted=false, by=identity, inline_tables::IdSet{<:AbstractDict})

Write `data` as TOML syntax to the stream `io`. If the keyword argument `sorted` is set to `true`,
sort tables according to the function given by the keyword argument `by`. If the keyword argument
`inline_tables` is given, it should be a set of tables that should be printed "inline".

The following data types are supported: `AbstractDict`, `AbstractVector`, `AbstractString`, `Integer`, `AbstractFloat`, `Bool`,
`Dates.DateTime`, `Dates.Time`, `Dates.Date`. Note that the integers and floats
need to be convertible to `Float64` and `Int64` respectively. For other data types,
pass the function `to_toml` that takes the data types and returns a value of a
supported type.
"""
const print = Internals.Printer.print

public Parser, parsefile, tryparsefile, parse, tryparse, ParserError, print

# These methods are private Base interfaces, but we do our best to support them over
# the TOML stdlib types anyway to minimize downstream breakage.
Base.TOMLCache(p::Parser) = Base.TOMLCache(p._p, Dict{String, Base.CachedTOMLDict}())
Base.TOMLCache(p::Parser, d::Base.CachedTOMLDict) = Base.TOMLCache(p._p, d)
Base.TOMLCache(p::Parser, d::Dict{String, Dict{String, Any}}) = Base.TOMLCache(p._p, d)

Internals.reinit!(p::Parser, str::String; filepath::Union{Nothing, String}=nothing) =
    Internals.reinit!(p._p, str; filepath)
Internals.parse(p::Parser) = Internals.parse(p._p)
Internals.tryparse(p::Parser) = Internals.tryparse(p._p)

end
