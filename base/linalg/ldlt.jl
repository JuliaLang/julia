# This file is a part of Julia. License is MIT: http://julialang.org/license

immutable LDLt{T,S<:AbstractMatrix} <: Factorization{T}
    data::S
end

size(S::LDLt) = size(S.data)
size(S::LDLt, i::Integer) = size(S.data, i)

convert{T,S}(::Type{LDLt{T,S}}, F::LDLt) = LDLt{T,S}(convert(S, F.data))
# NOTE: the annotaion <:AbstractMatrix shouldn't be necessary, it is introduced
#       to avoid an ambiguity warning (see issue #6383)
convert{T,S,U<:AbstractMatrix}(::Type{LDLt{T}}, F::LDLt{S,U}) = convert(LDLt{T,U}, F)

convert{T}(::Type{Factorization{T}}, F::LDLt{T}) = F
convert{T,S,U}(::Type{Factorization{T}}, F::LDLt{S,U}) = convert(LDLt{T,U}, F)

# SymTridiagonal
"""
    ldltfact!(S::SymTridiagonal) -> LDLt

Same as [`ldltfact`](@ref), but saves space by overwriting the input `A`, instead of creating a copy.
"""
function ldltfact!{T<:Real}(S::SymTridiagonal{T})
    n = size(S,1)
    d = S.dv
    e = S.ev
    @inbounds @simd for i = 1:n-1
        e[i] /= d[i]
        d[i+1] -= abs2(e[i])*d[i]
    end
    return LDLt{T,SymTridiagonal{T}}(S)
end

"""
    ldltfact(S::SymTridiagonal) -> LDLt

Compute an `LDLt` factorization of a real symmetric tridiagonal matrix such that `A = L*Diagonal(d)*L'`
where `L` is a unit lower triangular matrix and `d` is a vector. The main use of an `LDLt`
factorization `F = ldltfact(A)` is to solve the linear system of equations `Ax = b` with `F\\b`.
"""
function ldltfact{T}(M::SymTridiagonal{T})
    S = typeof(zero(T)/one(T))
    return S == T ? ldltfact!(copy(M)) : ldltfact!(convert(SymTridiagonal{S}, M))
end

factorize(S::SymTridiagonal) = ldltfact(S)

function A_ldiv_B!{T}(S::LDLt{T,SymTridiagonal{T}}, B::AbstractVecOrMat{T})
    n, nrhs = size(B, 1), size(B, 2)
    if size(S,1) != n
        throw(DimensionMismatch("Matrix has dimensions $(size(S)) but right hand side has first dimension $n"))
    end
    d = S.data.dv
    l = S.data.ev
    @inbounds begin
        for i = 2:n
            li1 = l[i-1]
            @simd for j = 1:nrhs
                B[i,j] -= li1*B[i-1,j]
            end
        end
        dn = d[n]
        @simd for j = 1:nrhs
            B[n,j] /= dn
        end
        for i = n-1:-1:1
            di = d[i]
            li = l[i]
            @simd for j = 1:nrhs
                B[i,j] /= di
                B[i,j] -= li*B[i+1,j]
            end
        end
    end
    return B
end

# Conversion methods
function convert(::Type{SymTridiagonal}, F::LDLt)
    e = copy(F.data.ev)
    d = copy(F.data.dv)
    e .*= d[1:end-1]
    d[2:end] += e .* F.data.ev
    SymTridiagonal(d, e)
end
convert(::Type{AbstractMatrix}, F::LDLt) = convert(SymTridiagonal, F)
convert(::Type{AbstractArray}, F::LDLt) = convert(AbstractMatrix, F)
convert(::Type{Matrix}, F::LDLt) = convert(Array, convert(AbstractArray, F))
convert(::Type{Array}, F::LDLt) = convert(Matrix, F)
full(F::LDLt) = convert(AbstractArray, F)
