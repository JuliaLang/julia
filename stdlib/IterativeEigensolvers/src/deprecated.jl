# This file is a part of Julia. License is MIT: https://julialang.org/license

@eval IterativeEigensolvers begin
    Base.A_mul_B!(y::StridedVector{T}, A::AtA_or_AAt{T}, x::StridedVector{T}) where {T} = Base.LinAlg.mul!(y, A, x)
    Base.A_mul_B!(y::StridedVector{T}, A::SVDAugmented{T}, x::StridedVector{T}) where {T} = Base.LinAlg.mul!(y, A, x)
end
