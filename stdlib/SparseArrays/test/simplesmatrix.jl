# This file is a part of Julia. License is MIT: https://julialang.org/license

struct SimpleSMatrix{N,M,T} <: AbstractMatrix{T}
    m::Matrix{T}
end

SimpleSMatrix{N,M}(m::AbstractMatrix{T}) where {N,M,T} =
    size(m) == (N, M) ? SimpleSMatrix{N,M,T}(m) : throw(error("Wrong matrix size"))

Base.:*(a::SimpleSMatrix{N,O}, b::SimpleSMatrix{O,M}) where {N,O,M} =
    SimpleSMatrix{N,M}(a.m * b.m)

Base.:*(a::LinearAlgebra.Adjoint{<:Any, <:SimpleSMatrix{O,N}}, b::SimpleSMatrix{O,M}) where {N,O,M} =
    SimpleSMatrix{N,M}(adjoint(a.parent.m) * b.m)

Base.:*(a::SimpleSMatrix{N,O}, b::LinearAlgebra.Adjoint{<:Any, <:SimpleSMatrix{M,O}}) where {N,O,M} =
    SimpleSMatrix{N,M}(a.m * adjoint(b.parent.m))

Base.:+(a::SimpleSMatrix{N,M}, b::SimpleSMatrix{N,M}) where {N,M} =
    SimpleSMatrix{N,M}(a.m + b.m)

Base.:+(a::LinearAlgebra.Adjoint{<:Any, <:SimpleSMatrix{M,N}}, b::SimpleSMatrix{N,M}) where {N,M} =
    SimpleSMatrix{N,M}(adjoint(a.parent.m) + b.m)

Base.:+(a::LinearAlgebra.Adjoint{<:Any, <:SimpleSMatrix}, b::LinearAlgebra.Adjoint{<:Any, <:SimpleSMatrix}) =
    (a' + b')'

Base.:+(a::SimpleSMatrix{N,M}, b::LinearAlgebra.Adjoint{<:Any, <:SimpleSMatrix{M,N}}) where {N,M} =
    SimpleSMatrix{N,M}(a.m + adjoint(b.parent.m))

Base.:-(a::SimpleSMatrix{N,M}, b::SimpleSMatrix{N,M}) where {N,M} =
    SimpleSMatrix{N,M}(a.m - b.m)

Base.:-(a::LinearAlgebra.Adjoint{<:Any, <:SimpleSMatrix{M,N}}, b::SimpleSMatrix{N,M}) where {N,M} =
    SimpleSMatrix{N,M}(adjoint(a.parent.m) - b.m)

Base.:-(a::LinearAlgebra.Adjoint{<:Any, <:SimpleSMatrix}, b::LinearAlgebra.Adjoint{<:Any, <:SimpleSMatrix}) =
    (a' - b')'

Base.:-(a::SimpleSMatrix{N,M}, b::LinearAlgebra.Adjoint{<:Any, <:SimpleSMatrix{M,N}}) where {N,M} =
    SimpleSMatrix{N,M}(a.m - adjoint(b.parent.m))

Base.size(a::SimpleSMatrix{N,M}) where {N,M} = (N, M)

Base.length(a::SimpleSMatrix{N,M}) where {N,M} = N * M

Base.zero(::Type{S}) where {N,M,T,S<:SimpleSMatrix{N,M,T}} = SimpleSMatrix{N,M}(zeros(T, N, M))

Base.getindex(s::SimpleSMatrix, inds...) = getindex(s.m, inds...)

Base.convert(::Type{S}, value::Matrix) where {N,M,T,S<:SimpleSMatrix{N,M,T}} =
    SimpleSMatrix{N,M}(T.(value))
