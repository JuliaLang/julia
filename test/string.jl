# This file is a part of Julia. License is MIT: http://julialang.org/license

## generic string uses only endof and next; used for testing ##

immutable CharString <: DirectIndexString
    chars::Vector{Char}
end
Base.convert(::Type{CharString}, s::AbstractString) = CharString(collect(s))
Base.endof(s::CharString) = length(s.chars)
Base.next(s::CharString, i::Int) = next(s.chars, i)

immutable GenericString <: AbstractString
    string::AbstractString
end
Base.convert(::Type{GenericString}, s::AbstractString) = GenericString(s)
Base.endof(s::GenericString) = endof(s.string)
Base.next(s::GenericString, i::Int) = next(s.string, i)

include("strings/basic.jl")
include("strings/types.jl")
include("strings/search.jl")
include("strings/util.jl")
include("strings/io.jl")
