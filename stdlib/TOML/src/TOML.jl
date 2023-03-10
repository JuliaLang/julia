# This file is a part of Julia. License is MIT: https://julialang.org/license

module TOML

module Internals
    # The parser is defined in Base
    using Base.TOML: TOMLDict, Parser, parse, tryparse, ParserError, isvalid_barekey_char, reinit!
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
readstring(f::AbstractString) = isfile(f) ? read(f, String) : error(repr(f), ": No such file")

"""
    Parser()

Constructor for a TOML `Parser`.  Note that in most cases one does not need to
explicitly create a `Parser` but instead one directly use use
[`TOML.parsefile`](@ref) or [`TOML.parse`](@ref).  Using an explicit parser
will however reuse some internal data structures which can be beneficial for
performance if a larger number of small files are parsed.
"""
const Parser = Internals.Parser

"""
    TOMLDict

Default dictionary type for TOML files. Note that in most cases one does not need to specify a different dictionary type, ohwever [`TOML.parsefile`](@ref), and [`TOML.parse`](@ref) allow you to specify your own custom dictionary type, as long as it is `<: AbstractDict{String, Any}`
"""
const TOMLDict = Internals.TOMLDict

"""
    parsefile(f::AbstractString; dicttype)
    parsefile(p::Parser, f::AbstractString; dicttype)

Parse file `f` and return the resulting table (dictionary, or `dictype <: AbstractDict{String, Any}`). Throw a
[`ParserError`](@ref) upon failure.

See also [`TOML.tryparsefile`](@ref).
"""
parsefile(f::AbstractString; dicttype=TOMLDict) =
    Internals.parse(Parser(readstring(f); filepath=abspath(f), root=dicttype()))
parsefile(p::Parser, f::AbstractString; dicttype=TOMLDict) =
    Internals.parse(Internals.reinit!(p, readstring(f); filepath=abspath(f), root=dicttype()))


"""
    tryparsefile(f::AbstractString; dicttype)
    tryparsefile(p::Parser, f::AbstractString; dicttype)

Parse file `f` and return the resulting table (dictionary, or `dicttype <: AbstractDict{String, Any}`). Return a
[`ParserError`](@ref) upon failure.

See also [`TOML.parsefile`](@ref).
"""
tryparsefile(f::AbstractString; dicttype=TOMLDict) =
    Internals.tryparse(Parser(readstring(f); filepath=abspath(f), root=dicttype()))
tryparsefile(p::Parser, f::AbstractString; ditctype=TOMLDict) =
    Internals.tryparse(Internals.reinit!(p, readstring(f); filepath=abspath(f), root=dicttype()))

"""
    parse(x::Union{AbstractString, IO}; dicttype)
    parse(p::Parser, x::Union{AbstractString, IO}; dicttype)

Parse the string  or stream `x`, and return the resulting table (dictionary, or `dicttype <: AbstractDict{String, Any}`).
Throw a [`ParserError`](@ref) upon failure.

See also [`TOML.tryparse`](@ref).
"""
parse(str::AbstractString; dicttype=TOMLDict) =
    Internals.parse(Parser(String(str); root=dicttype()))
parse(p::Parser, str::AbstractString; dicttype=TOMLDict) =
    Internals.parse(Internals.reinit!(p, String(str); root=dicttype()))
parse(io::IO; dicttype=TOMLDict) = parse(read(io, String); dicttype=dicttype)
parse(p::Parser, io::IO; dicttype=TOMLDict) = parse(p, read(io, String); dicttype=dicttype)

"""
    tryparse(x::Union{AbstractString, IO}; dicttype)
    tryparse(p::Parser, x::Union{AbstractString, IO}; dicttype)

Parse the string or stream `x`, and return the resulting table (dictionary, or `dicttype <: AbstractDict{String, Any}`).
Return a [`ParserError`](@ref) upon failure.

See also [`TOML.parse`](@ref).
"""
tryparse(str::AbstractString; dicttype=TOMLDict) =
    Internals.tryparse(Parser(String(str); root=dicttype()))
tryparse(p::Parser, str::AbstractString; dicttype=TOMLDict) =
    Internals.tryparse(Internals.reinit!(p, String(str); root=dicttype()))
tryparse(io::IO; dicttype=TOMLDict) = tryparse(read(io, String); dicttype=dicttype)
tryparse(p::Parser, io::IO; dicttype=TOMLDict) = tryparse(p, read(io, String); dicttype=dicttype)

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
    print([to_toml::Function], io::IO [=stdout], data::AbstractDict; sorted=false, by=identity)

Write `data` as TOML syntax to the stream `io`. If the keyword argument `sorted` is set to `true`,
sort tables according to the function given by the keyword argument `by`.

The following data types are supported: `AbstractDict`, `AbstractVector`, `AbstractString`, `Integer`, `AbstractFloat`, `Bool`,
`Dates.DateTime`, `Dates.Time`, `Dates.Date`. Note that the integers and floats
need to be convertible to `Float64` and `Int64` respectively. For other data types,
pass the function `to_toml` that takes the data types and returns a value of a
supported type.
"""
const print = Internals.Printer.print

end
