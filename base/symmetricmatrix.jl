import Base.show, Base.size, Base.convert
# import Base.+, Base.(.+), Base.-, Base.(.-), Base.*, Base.(.*), Base./, Base.(./)
import Base.A_mul_B, Base.Ac_mul_B, Base.A_mul_Bc
import Base.eig

type SymmetricMatrix{T} <: AbstractMatrix{T}
	data::Matrix{T}
	upper::Bool
end

SymmetricMatrix(A) = SymmetricMatrix(A, true)
convert{T}(::Type{Matrix{T}}, A::SymmetricMatrix{T}) = symmetrize!(copy(A.data), A.upper)
promote_rule{T}(::Type{Matrix{T}},::Type{SymmetricMatrix{T}}) = Matrix{T}

print_matrix(io, A::SymmetricMatrix) = print_matrix(io, convert(Matrix{eltype(A)}, A))
size(A::SymmetricMatrix) = size(A.data)
size(A::SymmetricMatrix, d) = size(A.data, d)
issym(A::SymmetricMatrix) = true
eltype{T}(A::SymmetricMatrix{T}) = T
copy(A::SymmetricMatrix) = SymmetricMatrix(copy(A.data), A.upper)
transpose(A::SymmetricMatrix) = A
one{T}(A::SymmetricMatrix{T}) = SymmetricMatrix(eye(T, size(A, 1)))

Ac_mul_A{T<:Union(Float32,Float64)}(A::StridedMatrix{T}) = At_mul_A(A)
Ac_mul_A{T<:Union(Complex64, Complex128)}(A::StridedMatrix{T}) = herk_wrapper('C', A)
At_mul_A{T<:LapackType}(A::StridedMatrix{T}) = SymmetricMatrix(syrk_wrapper('T', A), true)
A_mul_Ac{T<:Union(Float32,Float64)}(A::StridedMatrix{T}) = A_mul_At(A)
A_mul_Ac{T<:Union(Complex64, Complex128)}(A::StridedMatrix{T}) = herk_wrapper('N', A)
A_mul_At{T<:LapackType}(A::StridedMatrix{T}) = SymmetricMatrix(syrk_wrapper('N', A), true)

function ref(A::SymmetricMatrix, i::Integer)
	n = size(A, 1)
	icol = div(i - 1,n) + 1
	irow = mod(i - 1,n) + 1
	if A.upper
		if icol >= irow
			return ref(A.data, irow, icol)
		else
			return ref(A.data, icol, irow)
		end
	else
		if icol >= irow
			return ref(A.data, icol, irow)
		else
			return ref(A.data, irow, icol)
		end
	end
end
			
function ref(A::SymmetricMatrix, i::Integer, j::Integer)
	if A.upper 
		if i <= j
			return ref(A.data, i, j)
		else 
			return ref(A.data, j, i)
		end
	else
		if i <= j
			return ref(A.data, j, i)
		else 
			return ref(A.data, i, j)
		end
	end
end

# Scalar
for f in (:+,:.+,:-,:.-,:*,:.*,:/,:./)
	@eval begin
		function ($f){T}(A::SymmetricMatrix{T}, x::Number)
			Ac = copy(A)
			n = size(A, 1)
			for i = 1:n
				for j = (A.upper ? (1:i) : (i:n))
					Ac.data[j,i] = ($f)(Ac.data[j,i],x)
				end
			end
			Ac
		end
	end
end

for f in (:+,:.+,:-,:.-,:*,:.*,:\,:.\)
	@eval begin
		function ($f){T}(x::Number, A::SymmetricMatrix{T})
			Ac = copy(A)
			n = size(A, 1)
			for i = 1:n
				for j = (A.upper ? (1:i) : (i:n))
					Ac.data[j,i] = ($f)(x,Ac.data[j,i])
				end
			end
			Ac
		end
	end
end

# BLAS
function (*){T<:LapackType}(A::SymmetricMatrix{T}, B::StridedVector{T})
    if length(B) != size(A,2)
        error("Dimension mismatch: size(A,2) = $(size(A,2)) and length(B,1) = $(length(B))")
    end
    BLAS.symv(A.upper ? 'U' : 'L', 1., A.data, B)
end

function (*){T<:LapackType}(A::SymmetricMatrix{T}, B::StridedMatrix{T})
    if size(B,1) != size(A,2)
        error("Dimension mismatch: size(A,2) = $(size(A,2)) and size(B,1) = $(size(B,1))")
    end
    BLAS.symm('L', A.upper ? 'U' : 'L', 1., A.data, B)
end

function (*){T<:LapackType}(A::StridedMatrix{T}, B::SymmetricMatrix{T})
    if size(B,1) != size(A,2)
        error("Dimension mismatch: size(A,2) = $(size(A,2)) and size(B,1) = $(size(B,1))")
    end
    BLAS.symm('R', B.upper ? 'U' : 'L', 1., B.data, A)
end

# A_mul_B(A::SymmetricMatrix, B) = (*)(A,B)
# A_mul_B(A, B::SymmetricMatrix) = (*)(A,B)
Ac_mul_B{T<:Union(Float32,Float64)}(A::SymmetricMatrix{T}, B) = (*)(A,B)
Ac_mul_B{T<:Union(Complex64,Complex128)}(A::SymmetricMatrix{T}, B) = (*)(conj(A),B)
A_mul_Bc{T<:Union(Float32,Float64)}(A, B::SymmetricMatrix{T}) = (*)(A,B)
A_mul_Bc{T<:Union(Complex64,Complex128)}(A, B::SymmetricMatrix{T}) = (*)(A,conj(B))

# LAPACK
eig(A::SymmetricMatrix, vecs::Bool) = LAPACK.syevr!(vecs ? 'V' : 'N', 'A', A.upper ? 'U' : 'L', copy(A.data), 0.0, 0.0, 0, 0, -1.0)

function \(A::SymmetricMatrix, B::StridedVecOrMat)
	ans, _, _, info = LAPACK.sysv!(A.upper ? 'U' : 'L', copy(A.data), copy(B))
    if info > 0; error("Singular system"); end
    return isa(B, Vector) ? ans[:,1] : ans
end

inv(A::SymmetricMatrix) = inv(BunchKaufman(A.data, A.upper))