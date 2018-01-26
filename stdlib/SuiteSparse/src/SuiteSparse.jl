# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

module SuiteSparse

import Base: \
import LinearAlgebra: ldiv!, rdiv!

## Functions to switch to 0-based indexing to call external sparse solvers

# Convert from 1-based to 0-based indices
function decrement!(A::AbstractArray{T}) where T<:Integer
    for i in 1:length(A); A[i] -= oneunit(T) end
    A
end
decrement(A::AbstractArray{<:Integer}) = decrement!(copy(A))

# Convert from 0-based to 1-based indices
function increment!(A::AbstractArray{T}) where T<:Integer
    for i in 1:length(A); A[i] += oneunit(T) end
    A
end
increment(A::AbstractArray{<:Integer}) = increment!(copy(A))

if Base.USE_GPL_LIBS
    include("umfpack.jl")
    include("cholmod.jl")
    include("spqr.jl")
    include("deprecated.jl")
end

end # module SuiteSparse
