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
    using Base.TOML: Parser, Printer, parse, tryparse, ParserError, reinit!
    # Put the error instances in this module
    for errtype in instances(Base.TOML.ErrorType)
        @eval using Base.TOML: $(Symbol(errtype))
    end
end

# https://github.com/JuliaLang/julia/issues/36605
_readstring(f::AbstractString) = isfile(f) ? read(f, String) : error(repr(f), ": No such file")

"""
    Parser()
    Parser{dicttype}()

Constructor for a TOML `Parser`.  Note that in most cases one does not need to
explicitly create a `Parser` but instead one directly uses
[`TOML.parsefile`](@ref) or [`TOML.parse`](@ref).  Using an explicit parser
will however reuse some internal data structures which can be beneficial for
performance if a larger number of small files are parsed.

The type parameter `dicttype` can be used to make the parser return tables of
a dictionary type other than `Dict{String, Any}`, for example an
`OrderedDict{String, Any}` to preserve the order of the keys in the file.

!!! compat "Julia 1.14"
    The `dicttype` type parameter requires Julia 1.14 or later.
"""
struct Parser{D <: AbstractDict{String, Any}}
    _p::Internals.Parser{Dates, D}
end

const TOMLDict = Dict{String, Any}

# Dates-enabled constructors
Parser{D}() where {D<:AbstractDict{String, Any}} = Parser(Internals.Parser{Dates, D}())
Parser{D}(io::IO) where {D<:AbstractDict{String, Any}} = Parser(Internals.Parser{Dates, D}(io))
Parser{D}(str::String; filepath=nothing) where {D<:AbstractDict{String, Any}} =
    Parser(Internals.Parser{Dates, D}(str; filepath))
Parser() = Parser{TOMLDict}()
Parser(io::IO) = Parser{TOMLDict}(io)
Parser(str::String; filepath=nothing) = Parser{TOMLDict}(str; filepath)

"""
    parsefile(f::AbstractString; dicttype=Dict{String, Any})
    parsefile(p::Parser, f::AbstractString)

Parse file `f` and return the resulting table (dictionary). Throw a
[`ParserError`](@ref) upon failure. The keyword argument `dicttype` can be
used to return tables of a dictionary type other than `Dict{String, Any}`,
for example an `OrderedDict{String, Any}` to preserve the order of the keys
in the file.

!!! compat "Julia 1.14"
    The `dicttype` keyword argument requires Julia 1.14 or later.

See also [`TOML.tryparsefile`](@ref).
"""
parsefile(f::AbstractString; dicttype::Type{D}=TOMLDict) where {D<:AbstractDict{String, Any}} =
    Internals.parse(Internals.Parser{Dates, D}(_readstring(f); filepath=abspath(f)))
parsefile(p::Parser, f::AbstractString) =
    Internals.parse(Internals.reinit!(p._p, _readstring(f); filepath=abspath(f)))

"""
    tryparsefile(f::AbstractString; dicttype=Dict{String, Any})
    tryparsefile(p::Parser, f::AbstractString)

Parse file `f` and return the resulting table (dictionary). Return a
[`ParserError`](@ref) upon failure. The keyword argument `dicttype` can be
used to return tables of a dictionary type other than `Dict{String, Any}`,
for example an `OrderedDict{String, Any}` to preserve the order of the keys
in the file.

!!! compat "Julia 1.14"
    The `dicttype` keyword argument requires Julia 1.14 or later.

See also [`TOML.parsefile`](@ref).
"""
tryparsefile(f::AbstractString; dicttype::Type{D}=TOMLDict) where {D<:AbstractDict{String, Any}} =
    Internals.tryparse(Internals.Parser{Dates, D}(_readstring(f); filepath=abspath(f)))
tryparsefile(p::Parser, f::AbstractString) =
    Internals.tryparse(Internals.reinit!(p._p, _readstring(f); filepath=abspath(f)))

"""
    parse(x::Union{AbstractString, IO}; dicttype=Dict{String, Any})
    parse(p::Parser, x::Union{AbstractString, IO})

Parse the string or stream `x`, and return the resulting table (dictionary).
Throw a [`ParserError`](@ref) upon failure. The keyword argument `dicttype`
can be used to return tables of a dictionary type other than
`Dict{String, Any}`, for example an `OrderedDict{String, Any}` to preserve
the order of the keys in the input.

!!! compat "Julia 1.14"
    The `dicttype` keyword argument requires Julia 1.14 or later.

See also [`TOML.tryparse`](@ref).
"""
parse(p::Parser) = Internals.parse(p._p)
parse(str::AbstractString; dicttype::Type{D}=TOMLDict) where {D<:AbstractDict{String, Any}} =
    Internals.parse(Internals.Parser{Dates, D}(String(str)))
parse(p::Parser, str::AbstractString) =
    Internals.parse(Internals.reinit!(p._p, String(str)))
parse(io::IO; dicttype::Type{D}=TOMLDict) where {D<:AbstractDict{String, Any}} =
    parse(read(io, String); dicttype)
parse(p::Parser, io::IO) = parse(p, read(io, String))

"""
    tryparse(x::Union{AbstractString, IO}; dicttype=Dict{String, Any})
    tryparse(p::Parser, x::Union{AbstractString, IO})

Parse the string or stream `x`, and return the resulting table (dictionary).
Return a [`ParserError`](@ref) upon failure. The keyword argument `dicttype`
can be used to return tables of a dictionary type other than
`Dict{String, Any}`, for example an `OrderedDict{String, Any}` to preserve
the order of the keys in the input.

!!! compat "Julia 1.14"
    The `dicttype` keyword argument requires Julia 1.14 or later.

See also [`TOML.parse`](@ref).
"""
tryparse(p::Parser) = Internals.tryparse(p._p)
tryparse(str::AbstractString; dicttype::Type{D}=TOMLDict) where {D<:AbstractDict{String, Any}} =
    Internals.tryparse(Internals.Parser{Dates, D}(String(str)))
tryparse(p::Parser, str::AbstractString) =
    Internals.tryparse(Internals.reinit!(p._p, String(str)))
tryparse(io::IO; dicttype::Type{D}=TOMLDict) where {D<:AbstractDict{String, Any}} =
    tryparse(read(io, String); dicttype)
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

!!! compat "Julia 1.11"
    The `inline_tables` keyword argument is supported by Julia 1.11 or later.

The following data types are supported: `AbstractDict`, `AbstractVector`, `AbstractString`, `Integer`, `AbstractFloat`, `Bool`,
`Dates.DateTime`, `Dates.Time`, `Dates.Date`. Note that the integers and floats
need to be convertible to `Int64` and `Float64` respectively. For other data types,
pass the function `to_toml` that takes the data types and returns a value of a
supported type.
"""
const print = Internals.Printer.print

public Parser, parsefile, tryparsefile, parse, tryparse, ParserError, print

# These methods are private Base interfaces, but we do our best to support them over
# the TOML stdlib types anyway to minimize downstream breakage.
Base.TOMLCache(p::Parser{TOMLDict}) = Base.TOMLCache(p._p, Dict{String, Base.CachedTOMLDict}())
Base.TOMLCache(p::Parser{TOMLDict}, d::Base.CachedTOMLDict) = Base.TOMLCache(p._p, d)
Base.TOMLCache(p::Parser{TOMLDict}, d::Dict{String, Dict{String, Any}}) = Base.TOMLCache(p._p, d)

Internals.reinit!(p::Parser, str::String; filepath::Union{Nothing, String}=nothing) =
    Internals.reinit!(p._p, str; filepath)
Internals.parse(p::Parser) = Internals.parse(p._p)
Internals.tryparse(p::Parser) = Internals.tryparse(p._p)

include("precompile.jl")

end
