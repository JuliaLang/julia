# This file is a part of Julia. License is MIT: http://julialang.org/license

## Matrix factorizations and decompositions

abstract Factorization{T}

eltype{T}(::Type{Factorization{T}}) = T
transpose(F::Factorization) = error("transpose not implemented for $(typeof(F))")
ctranspose(F::Factorization) = error("ctranspose not implemented for $(typeof(F))")

macro assertposdef(A, info)
   :(($info)==0 ? $A : throw(PosDefException($info)))
end

macro assertnonsingular(A, info)
   :(($info)==0 ? $A : throw(SingularException($info)))
end


### General promotion rules
convert{T}(::Type{Factorization{T}}, F::Factorization{T}) = F
inv{T}(F::Factorization{T}) = A_ldiv_B!(F, eye(T, size(F,1)))

# With a real lhs and complex rhs with the same precision, we can reinterpret
# the complex rhs as a real rhs with twice the number of columns
function (\){T<:BlasReal}(F::Factorization{T}, B::AbstractVector{Complex{T}})
    c2r = reshape(transpose(reinterpret(T, B, (2, length(B)))), size(B, 1), 2*size(B, 2))
    x = A_ldiv_B!(F, c2r)
    return reinterpret(Complex{T}, transpose(reshape(x, div(length(x), 2), 2)), (size(F,2),))
end
function (\){T<:BlasReal}(F::Factorization{T}, B::AbstractMatrix{Complex{T}})
    c2r = reshape(transpose(reinterpret(T, B, (2, length(B)))), size(B, 1), 2*size(B, 2))
    x = A_ldiv_B!(F, c2r)
    return reinterpret(Complex{T}, transpose(reshape(x, div(length(x), 2), 2)), (size(F,2), size(B,2)))
end

function (\){TF<:Number,TB<:Number,N}(F::Factorization{TF}, B::AbstractArray{TB,N})
    TFB = typeof(one(TF)/one(TB))
    A_ldiv_B!(convert(Factorization{TFB}, F), copy_oftype(B, TFB))
end

function Ac_ldiv_B{TF<:Number,TB<:Number,N}(F::Factorization{TF}, B::AbstractArray{TB,N})
    TFB = typeof(one(TF)/one(TB))
    Ac_ldiv_B!(convert(Factorization{TFB}, F), copy_oftype(B, TFB))
end

function At_ldiv_B{TF<:Number,TB<:Number,N}(F::Factorization{TF}, B::AbstractArray{TB,N})
    TFB = typeof(one(TF)/one(TB))
    At_ldiv_B!(convert(Factorization{TFB}, F), copy_oftype(B, TFB))
end
