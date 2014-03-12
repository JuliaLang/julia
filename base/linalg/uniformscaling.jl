import Base: +, -, *, /, copy, ctranspose, getindex, showarray, transpose
import Base.LinAlg: SingularException
immutable UniformScaling{T<:Number} <: AbstractMatrix{T}
		λ::T
end

const I = UniformScaling(1)
const 𝟙 = I

getindex(J::UniformScaling, i::Integer,j::Integer) = ifelse(i==j,J.λ,zero(J.λ))

showarray(io::IO,J::UniformScaling;kw...) = print(io,"$(typeof(J))\n$(J.λ)*I")
copy(J::UniformScaling) = UniformScaling(J.λ)

transpose(J::UniformScaling) = J
ctranspose(J::UniformScaling) = UniformScaling(conj(J.λ))

+(J1::UniformScaling,J2::UniformScaling)  = UniformScaling(J1.λ+J2.λ)
+{T}(B::BitArray{2},J::UniformScaling{T}) = bitunpack(B) + J
+(J::UniformScaling,B::BitArray{2}) 	  = J + bitunpack(B)
function +{TA,TJ}(A::AbstractMatrix{TA},J::UniformScaling{TJ})
    n = chksquare(A)
    B = similar(A,promote_type(TA,TJ))
    copy!(B,A)
    @inbounds for i = 1:n
        B[i,i] += J.λ
    end
    B
end
+(J::UniformScaling,A::AbstractMatrix) = A + J

-(J1::UniformScaling,J2::UniformScaling) = UniformScaling(J1.λ-J2.λ)
-(B::BitArray{2},J::UniformScaling) 	 = bitunpack(B) - J
-(J::UniformScaling,B::BitArray{2}) 	 = J - bitunpack(B)
function -{TA,TJ<:Number}(A::AbstractMatrix{TA},J::UniformScaling{TJ})
    n = chksquare(A)
    B = similar(A,promote_type(TA,TJ))
    copy!(B,A)
    @inbounds for i = 1:n
        B[i,i] -= J.λ
    end
    B
end
function -{TA,TJ<:Number}(J::UniformScaling{TJ},A::AbstractMatrix{TA})
	n = chksquare(A)
	B = -A
	@inbounds for i = 1:n
		B[i,i] += J.λ
	end
	B
end

*(J1::UniformScaling,J2::UniformScaling) = UniformScaling(J1.λ*J2.λ)
*(B::BitArray{2},J::UniformScaling) = *(bitunpack(B),J::UniformScaling)
*(J::UniformScaling,B::BitArray{2}) = *(J::UniformScaling,bitunpack(B))
*(S::SparseMatrixCSC,J::UniformScaling) = J.λ == 1 ? S : J.λ*S
*{Tv,Ti}(J::UniformScaling,S::SparseMatrixCSC{Tv,Ti}) = J.λ == 1 ? S : S*J.λ
*(A::AbstractMatrix,J::UniformScaling) = J.λ == 1 ? A : J.λ*A
*(J::UniformScaling,A::AbstractVecOrMat) = J.λ == 1 ? A : J.λ*A

*(x::Number,J::UniformScaling) = UniformScaling(x*J.λ)
*(J::UniformScaling,x::Number) = UniformScaling(J.λ*x)

/(J1::UniformScaling,J2::UniformScaling) = J2.λ == 0 ? throw(SingularException(1)) : UniformScaling(J1.λ/J2.λ)
/(J::UniformScaling,A::AbstractMatrix) = J.λ*inv(A)
/(A::AbstractMatrix,J::UniformScaling) = J.λ == 0 ? throw(SingularException(1)) : A/J.λ

/(J::UniformScaling,x::Number) = UniformScaling(J.λ/x)

\(J1::UniformScaling,J2::UniformScaling) = J1.λ == 0 ? throw(SingularException(1)) : UniformScaling(J1.λ\J2.λ)
\{T<:Number}(A::Union(Bidiagonal{T},Triangular{T}),J::UniformScaling) = inv(A)*J.λ
\(J::UniformScaling,A::AbstractVecOrMat) = J.λ == 0 ? throw(SingularException(1)) : J.λ\A
\(A::AbstractMatrix,J::UniformScaling) = inv(A)*J.λ

\(x::Number,J::UniformScaling) = UniformScaling(x\J.λ)
