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
function \{TF<:Number,TB<:Number,N}(F::Factorization{TF}, B::AbstractArray{TB,N})
    TFB = typeof(one(TF)/one(TB))
    A_ldiv_B!(convert(Factorization{TFB}, F), TB == TFB ? copy(B) : convert(AbstractArray{TFB,N}, B))
end

function Ac_ldiv_B{TF<:Number,TB<:Number,N}(F::Factorization{TF}, B::AbstractArray{TB,N})
    TFB = typeof(one(TF)/one(TB))
    Ac_ldiv_B!(convert(Factorization{TFB}, F), TB == TFB ? copy(B) : convert(AbstractArray{TFB,N}, B))
end

function At_ldiv_B{TF<:Number,TB<:Number,N}(F::Factorization{TF}, B::AbstractArray{TB,N})
    TFB = typeof(one(TF)/one(TB))
    At_ldiv_B!(convert(Factorization{TFB}, F), TB == TFB ? copy(B) : convert(AbstractArray{TFB,N}, B))
end
