# This file is a part of Julia. License is MIT: http://julialang.org/license

import Base: copy, ctranspose, getindex, show, transpose, one, zero, inv
import Base.LinAlg: SingularException

immutable UniformScaling{T<:Number}
    λ::T
end

const I = UniformScaling(1)

eltype{T}(::Type{UniformScaling{T}}) = T
ndims(J::UniformScaling) = 2
getindex(J::UniformScaling, i::Integer,j::Integer) = ifelse(i==j,J.λ,zero(J.λ))

show(io::IO, J::UniformScaling) = print(io, "$(typeof(J))\n$(J.λ)*I")
copy(J::UniformScaling) = UniformScaling(J.λ)

transpose(J::UniformScaling) = J
ctranspose(J::UniformScaling) = UniformScaling(conj(J.λ))

one{T}(::Type{UniformScaling{T}}) = UniformScaling(one(T))
one{T}(J::UniformScaling{T}) = one(UniformScaling{T})
zero{T}(::Type{UniformScaling{T}}) = UniformScaling(zero(T))
zero{T}(J::UniformScaling{T}) = zero(UniformScaling{T})

(+)(J1::UniformScaling, J2::UniformScaling) = UniformScaling(J1.λ+J2.λ)
(+){T}(B::BitArray{2},J::UniformScaling{T}) = Array(B) + J
(+)(J::UniformScaling, B::BitArray{2})      = J + Array(B)
(+)(J::UniformScaling, A::AbstractMatrix)   = A + J
(+)(J::UniformScaling, x::Number)           = J.λ + x
(+)(x::Number, J::UniformScaling)           = x + J.λ

(-)(J::UniformScaling)                      = UniformScaling(-J.λ)
(-)(J1::UniformScaling, J2::UniformScaling) = UniformScaling(J1.λ-J2.λ)
(-)(B::BitArray{2}, J::UniformScaling)      = Array(B) - J
(-)(J::UniformScaling, B::BitArray{2})      = J - Array(B)
(-)(J::UniformScaling, x::Number)           = J.λ - x
(-)(x::Number, J::UniformScaling)           = x - J.λ

for (t1, t2) in ((:UnitUpperTriangular, :UpperTriangular),
                 (:UnitLowerTriangular, :LowerTriangular))
    for op in (:+,:-)
        @eval begin
            ($op)(UL::$t2, J::UniformScaling) = ($t2)(($op)(UL.data, J))

            function ($op)(UL::$t1, J::UniformScaling)
                ULnew = copy_oftype(UL.data, promote_type(eltype(UL), eltype(J)))
                for i = 1:size(ULnew, 1)
                    ULnew[i,i] = ($op)(1, J.λ)
                end
                return ($t2)(ULnew)
            end
        end
    end
end

function (-)(J::UniformScaling, UL::Union{UpperTriangular,UnitUpperTriangular})
    ULnew = similar(full(UL), promote_type(eltype(J), eltype(UL)))
    n = size(ULnew, 1)
    ULold = UL.data
    for j = 1:n
        for i = 1:j - 1
            ULnew[i,j] = -ULold[i,j]
        end
        if isa(UL, UnitUpperTriangular)
            ULnew[j,j] = J.λ - 1
        else
            ULnew[j,j] = J.λ - ULold[j,j]
        end
    end
    return UpperTriangular(ULnew)
end
function (-)(J::UniformScaling, UL::Union{LowerTriangular,UnitLowerTriangular})
    ULnew = similar(full(UL), promote_type(eltype(J), eltype(UL)))
    n = size(ULnew, 1)
    ULold = UL.data
    for j = 1:n
        if isa(UL, UnitLowerTriangular)
            ULnew[j,j] = J.λ - 1
        else
            ULnew[j,j] = J.λ - ULold[j,j]
        end
        for i = j + 1:n
            ULnew[i,j] = -ULold[i,j]
        end
    end
    return LowerTriangular(ULnew)
end

function (+){TA,TJ}(A::AbstractMatrix{TA}, J::UniformScaling{TJ})
    n = checksquare(A)
    B = similar(A, promote_type(TA,TJ))
    copy!(B,A)
    @inbounds for i = 1:n
        B[i,i] += J.λ
    end
    B
end

function (-){TA,TJ<:Number}(A::AbstractMatrix{TA}, J::UniformScaling{TJ})
    n = checksquare(A)
    B = similar(A, promote_type(TA,TJ))
    copy!(B, A)
    @inbounds for i = 1:n
        B[i,i] -= J.λ
    end
    B
end
function (-){TA,TJ<:Number}(J::UniformScaling{TJ}, A::AbstractMatrix{TA})
    n = checksquare(A)
    B = convert(AbstractMatrix{promote_type(TJ,TA)}, -A)
    @inbounds for j = 1:n
        B[j,j] += J.λ
    end
    B
end

inv(J::UniformScaling) = UniformScaling(inv(J.λ))

*(J1::UniformScaling, J2::UniformScaling) = UniformScaling(J1.λ*J2.λ)
*(B::BitArray{2}, J::UniformScaling) = *(Array(B), J::UniformScaling)
*(J::UniformScaling, B::BitArray{2}) = *(J::UniformScaling, Array(B))
*(A::AbstractMatrix, J::UniformScaling) = J.λ == 1 ? A : J.λ*A
*(J::UniformScaling, A::AbstractVecOrMat) = J.λ == 1 ? A : J.λ*A

*(x::Number, J::UniformScaling) = UniformScaling(x*J.λ)
*(J::UniformScaling, x::Number) = UniformScaling(J.λ*x)

/(J1::UniformScaling, J2::UniformScaling) = J2.λ == 0 ? throw(SingularException(1)) : UniformScaling(J1.λ/J2.λ)
/(J::UniformScaling, A::AbstractMatrix) = scale!(J.λ, inv(A))
/(A::AbstractMatrix, J::UniformScaling) = J.λ == 0 ? throw(SingularException(1)) : A/J.λ

/(J::UniformScaling, x::Number) = UniformScaling(J.λ/x)

\(J1::UniformScaling, J2::UniformScaling) = J1.λ == 0 ? throw(SingularException(1)) : UniformScaling(J1.λ\J2.λ)
\{T<:Number}(A::Union{Bidiagonal{T},AbstractTriangular{T}}, J::UniformScaling) = scale!(inv(A), J.λ)
\(J::UniformScaling, A::AbstractVecOrMat) = J.λ == 0 ? throw(SingularException(1)) : J.λ\A
\(A::AbstractMatrix, J::UniformScaling) = scale!(inv(A), J.λ)

\(x::Number, J::UniformScaling) = UniformScaling(x\J.λ)

.*(x::Number,J::UniformScaling) = UniformScaling(x*J.λ)
.*(J::UniformScaling,x::Number) = UniformScaling(J.λ*x)

./(J::UniformScaling,x::Number) = UniformScaling(J.λ/x)

==(J1::UniformScaling,J2::UniformScaling) = (J1.λ == J2.λ)
