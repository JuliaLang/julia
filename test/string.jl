# This file is a part of Julia. License is MIT: http://julialang.org/license

## generic string uses only endof and next; used for testing ##

immutable GenericString <: AbstractString
    string::AbstractString
end

Base.endof(s::GenericString) = endof(s.string)
Base.next(s::GenericString, i::Int) = next(s.string, i)

include("strings/basic.jl")
include("strings/types.jl")
include("strings/search.jl")
include("strings/util.jl")
include("strings/io.jl")
