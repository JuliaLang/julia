# This file is a part of Julia. License is MIT: https://julialang.org/license

LinearAlgebra.A_mul_B!(y::StridedVector{T}, A::AtA_or_AAt{T}, x::StridedVector{T}) where {T} = LinearAlgebra.mul!(y, A, x)
LinearAlgebra.A_mul_B!(y::StridedVector{T}, A::SVDAugmented{T}, x::StridedVector{T}) where {T} = LinearAlgebra.mul!(y, A, x)
