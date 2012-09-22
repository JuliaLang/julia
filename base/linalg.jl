## linalg.jl: Basic Linear Algebra interface specifications and
## specialized matrix types

#
# This file mostly contains commented functions which are supposed
# to be defined in type-specific linalg_<type>.jl files.
#
# It defines functions in cases where sufficiently few assumptions about
# storage can be made.

#Ac_mul_B(x::AbstractVector, y::AbstractVector)
#At_mul_B{T<:Real}(x::AbstractVector{T}, y::AbstractVector{T})

#dot(x::AbstractVector, y::AbstractVector)

#cross(a::AbstractVector, b::AbstractVector)

#(*){T,S}(A::AbstractMatrix{T}, B::AbstractVector{S})
#(*){T,S}(A::AbstractVector{S}, B::AbstractMatrix{T})
#(*){T,S}(A::AbstractMatrix{T}, B::AbstractMatrix{S})

function axpy{TA<:Number, T<:LapackScalar}(alpha::TA, x::Array{T}, y::Array{T})
    if length(x) != length(y)
        error("Inputs should be of the same length")
    end
    Blas.axpy!(length(x), convert(T, alpha), x, 1, y, 1)
    return y
end

function axpy{TA<:Number, T<:LapackScalar, TI<:Integer}(alpha::TA, x::Array{T}, rx::Union(Range1{TI},Range{TI}), y::Array{T}, ry::Union(Range1{TI},Range{TI}))
    if length(rx) != length(ry)
        error("Ranges should be of the same length")
    end
    if min(rx) < 1 || max(rx) > length(x) || min(ry) < 1 || max(ry) > length(y)
        throw(BoundsError())
    end
    Blas.axpy!(length(rx), convert(T, alpha), pointer(x)+(first(rx)-1)*sizeof(T), step(rx), pointer(y)+(first(ry)-1)*sizeof(T), step(ry))
    return y
end

function copy_to{T<:LapackScalar}(dest::Ptr{T}, src::Ptr{T}, n::Integer)
    if n < 200
        Blas.copy!(n, src, 1, dest, 1)
    else
        ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint), dest, src, n*sizeof(T))
    end
    dest
end

function copy_to{T<:LapackScalar}(dest::Array{T}, src::Array{T})
    n = numel(src)
    if n > numel(dest)
        throw(BoundsError())
    end
    if n < 200
        Blas.copy!(n, src, 1, dest, 1)
    else
        ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint), dest, src, n*sizeof(T))
    end
    dest
end

function copy_to{T<:LapackScalar,TI<:Integer}(dest::Array{T}, rdest::Union(Range1{TI},Range{TI}), src::Array{T}, rsrc::Union(Range1{TI},Range{TI}))
    if min(rdest) < 1 || max(rdest) > length(dest) || min(rsrc) < 1 || max(rsrc) > length(src)
        throw(BoundsError())
    end
    if length(rdest) != length(rsrc)
        error("Ranges must be of the same length")
    end
    Blas.copy!(length(rsrc), pointer(src)+(first(rsrc)-1)*sizeof(T), step(rsrc),
              pointer(dest)+(first(rdest)-1)*sizeof(T), step(rdest))
    return dest
end

function dot{T<:Union(Vector{Float64}, Vector{Float32})}(x::T, y::T)
    length(x) != length(y) ? error("Inputs should be of same length") : true
    Blas.dot(length(x), x, 1, y, 1)
end

function dot{T<:Union(Float64, Float32), TI<:Integer}(x::Vector{T}, rx::Union(Range1{TI},Range{TI}), y::Vector{T}, ry::Union(Range1{TI},Range{TI}))
    length(rx) != length(ry) ? error("Ranges should be of same length") : true
    if min(rx) < 1 || max(rx) > length(x) || min(ry) < 1 || max(ry) > length(y)
        throw(BoundsError())
    end
    Blas.dot(length(rx), pointer(x)+(first(rx)-1)*sizeof(T), step(rx), pointer(y)+(first(ry)-1)*sizeof(T), step(ry))
end

norm{T<:LapackScalar}(x::Vector{T}) = Blas.nrm2(length(x), x, 1)

function norm{T<:LapackScalar, TI<:Integer}(x::Vector{T}, rx::Union(Range1{TI},Range{TI}))
    if min(rx) < 1 || max(rx) > length(x)
        throw(BoundsError())
    end
    Blas.nrm2(length(rx), pointer(x)+(first(rx)-1)*sizeof(T), step(rx))
end


(*){T<:LapackScalar}(A::StridedMatrix{T}, B::StridedMatrix{T}) = _jl_gemm('N', 'N', A, B)
A_mul_B{T<:LapackScalar}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = _jl_gemm(C, 'N', 'N', A, B)
A_mul_B{T,S,R}(C::StridedMatrix{R}, A::StridedMatrix{T}, B::StridedMatrix{S}) = _jl_generic_matmatmul(C, 'N', 'N', A, B)

function At_mul_B{T<:LapackScalar}(A::StridedMatrix{T},
                                   B::StridedMatrix{T})
    if is(A, B) && size(A,1)>=500
        _jl_syrk('T', A)
    else
        _jl_gemm('T', 'N', A, B)
    end
end
# TODO: syrk
At_mul_B{T<:LapackScalar}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = _jl_gemm(C, 'T', 'N', A, B)
At_mul_B{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S}) = _jl_generic_matmatmul('T', 'N', A, B)
At_mul_B{T,S,R}(C::StridedMatrix{R}, A::StridedMatrix{T}, B::StridedMatrix{S}) = _jl_generic_matmatmul(C, 'T', 'N', A, B)

function A_mul_Bt{T<:LapackScalar}(A::StridedMatrix{T},
                                   B::StridedMatrix{T})
    if is(A, B) && size(A,2)>=500
        _jl_syrk('N', A)
    else
        _jl_gemm('N', 'T', A, B)
    end
end
A_mul_Bt{T<:LapackScalar}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = _jl_gemm(C, 'N', 'T', A, B)
A_mul_Bt{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S}) = _jl_generic_matmatmul('N', 'T', A, B)
A_mul_Bt{T,S,R}(C::StridedMatrix{R}, A::StridedMatrix{T}, B::StridedMatrix{S}) = _jl_generic_matmatmul(C, 'N', 'T', A, B)


At_mul_Bt{T<:LapackScalar}(A::StridedMatrix{T}, B::StridedMatrix{T}) = _jl_gemm('T', 'T', A, B)
At_mul_Bt{T<:LapackScalar}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = _jl_gemm(C, 'T', 'T', A, B)
At_mul_Bt{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S}) = _jl_generic_matmatmul('T', 'T', A, B)
At_mul_Bt{T,S,R}(C::StridedMatrix{R}, A::StridedMatrix{T}, B::StridedMatrix{S}) = _jl_generic_matmatmul(C, 'T', 'T', A, B)

Ac_mul_B{T<:Union(Float64,Float32)}(A::StridedMatrix{T}, B::StridedMatrix{T}) = At_mul_B(A, B)
Ac_mul_B{T<:Union(Float64,Float32)}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = At_mul_B(C, A, B)
function Ac_mul_B{T<:Union(Complex128,Complex64)}(A::StridedMatrix{T},
                                                  B::StridedMatrix{T})
    if is(A, B) && size(A,1)>=500
        _jl_herk('C', A)
    else
        _jl_gemm('C', 'N', A, B)
    end
end
Ac_mul_B{T<:Union(Complex128,Complex64)}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = _jl_gemm('C', 'N', A, B)
Ac_mul_B{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S}) = _jl_generic_matmatmul('C', 'N', A, B)
Ac_mul_B{T,S,R}(C::StridedMatrix{R}, A::StridedMatrix{T}, B::StridedMatrix{S}) = _jl_generic_matmatmul(C, 'C', 'N', A, B)

A_mul_Bc{T<:Union(Float64,Float32)}(A::StridedMatrix{T}, B::StridedMatrix{T}) = A_mul_Bt(A, B)
A_mul_Bc{T<:Union(Float64,Float32)}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = A_mul_Bt(C, A, B)
function A_mul_Bc{T<:Union(Complex128,Complex64)}(A::StridedMatrix{T},
                                                  B::StridedMatrix{T})
    if is(A, B) && size(A,2)>=500
        _jl_herk('N', A)
    else
        _jl_gemm('N', 'C', A, B)
    end
end
A_mul_Bc{T<:Union(Complex128,Complex64)}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = _jl_gemm(C, 'N', 'C', A, B)
A_mul_Bc{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S}) = _jl_generic_matmatmul('N', 'C', A, B)
A_mul_Bc{T,S,R}(C::StridedMatrix{R}, A::StridedMatrix{T}, B::StridedMatrix{S}) = _jl_generic_matmatmul(C, 'N', 'C', A, B)

Ac_mul_Bc{T<:LapackScalar}(A::StridedMatrix{T}, B::StridedMatrix{T}) = _jl_gemm('C', 'C', A, B)
Ac_mul_Bc{T<:LapackScalar}(C::StridedMatrix{T}, A::StridedMatrix{T}, B::StridedMatrix{T}) = _jl_gemm(C, 'C', 'C', A, B)
Ac_mul_Bt{T,S}(A::StridedMatrix{T}, B::StridedMatrix{S}) = _jl_generic_matmatmul('C', 'C', A, B)
Ac_mul_Bt{T,S,R}(C::StridedMatrix{R}, A::StridedMatrix{T}, B::StridedMatrix{S}) = _jl_generic_matmatmul(C, 'C', 'C', A, B)

function _jl_copy_upper_to_lower(A::StridedMatrix)
    n = size(A, 1)
    for i = 1:n-1
        for j = i+1:n
            A[j, i] = A[i, j]
        end
    end
    A
end

function _jl_syrk{T<:LapackScalar}(tA, A::StridedMatrix{T})
    if tA == 'T'
        (nA, mA) = size(A)
        tAt = 'N'
    else
        (mA, nA) = size(A)
        tAt = 'T'
    end

    if mA == 2 && nA == 2; return matmul2x2(tA,tAt,A,A); end
    if mA == 3 && nA == 3; return matmul3x3(tA,tAt,A,A); end

    if stride(A, 1) != 1
        return _jl_generic_matmatmul(tA, tAt, A, A)
    end

    _jl_copy_upper_to_lower(Blas.syrk('U', tA, one(T), A))
end

function _jl_copy_upper_to_lower_conj(A::StridedMatrix)
    n = size(A, 1)
    for i = 1:n-1
        for j = i+1:n
            A[j, i] = conj(A[i, j])
        end
    end
    A
end

function _jl_herk{T<:LapackScalar}(tA, A::StridedMatrix{T})
    if tA == 'C'
        (nA, mA) = size(A)
        tAt = 'N'
    else
        (mA, nA) = size(A)
        tAt = 'C'
    end

    if mA == 2 && nA == 2; return matmul2x2(tA,tAt,A,A); end
    if mA == 3 && nA == 3; return matmul3x3(tA,tAt,A,A); end

    if stride(A, 1) != 1
        return _jl_generic_matmatmul(tA, tAt, A, A)
    end

    # Result array does not need to be initialized as long as beta==0
#    C = Array(T, mA, mA)

    _jl_copy_upper_to_lower_conj(Blas.herk('U', tA, one(T), A))
end



function _jl_gemm{T<:LapackScalar}(tA, tB,
                                   A::StridedMatrix{T},
                                   B::StridedMatrix{T})
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)
    C = Array(T, mA, nB)
    _jl_gemm(C, tA, tB, A, B)
end

function _jl_gemm{T<:LapackScalar}(C::StridedMatrix{T}, tA, tB,
                                   A::StridedMatrix{T},
                                   B::StridedMatrix{T})
    mA, nA = lapack_size(tA, A)
    mB, nB = lapack_size(tB, B)

    if nA != mB; error("*: argument shapes do not match"); end

    if mA == 0 || nA == 0 || nB == 0; return zeros(T, mA, nB); end
    if mA == 2 && nA == 2 && nB == 2; return matmul2x2(C,tA,tB,A,B); end
    if mA == 3 && nA == 3 && nB == 3; return matmul3x3(C,tA,tB,A,B); end

    if stride(A, 1) != 1 || stride(B, 1) != 1
        return _jl_generic_matmatmul(C, tA, tB, A, B)
    end

    Blas.gemm!(tA, tB, one(T), A, B, zero(T), C)
end

function (*){T<:LapackScalar}(A::StridedMatrix{T},
                              X::StridedVector{T})
    Y = similar(A, size(A,1))
    _jl_gemv(Y, 'N', A, X)
end

A_mul_B{T<:LapackScalar}(y::StridedVector{T}, A::StridedMatrix{T}, x::StridedVector{T}) = _jl_gemv(y, 'N', A, x)
    
A_mul_B(y::StridedVector, A::StridedMatrix, x::StridedVector) = _jl_generic_matvecmul(y, 'N', A, x)
    
function At_mul_B{T<:LapackScalar}(A::StridedMatrix{T}, x::StridedVector{T})
    y = similar(A, size(A, 2))
    _jl_gemv(y, 'T', A, x)
end

At_mul_B{T<:LapackScalar}(y::StridedVector{T}, A::StridedMatrix{T}, x::StridedVector{T}) = _jl_gemv(y, 'T', A, x)
    
At_mul_B(y::StridedVector, A::StridedMatrix, x::StridedVector) = _jl_generic_matvecmul(y, 'T', A, x)
    
function _jl_gemv{T<:LapackScalar}(y::StridedVector{T},
                                   tA,
                                   A::StridedMatrix{T},
                                   x::StridedVector{T})
    if stride(A, 1) != 1
        return _jl_generic_matvecmul(y, tA, A, x)
    end

    if tA != 'N'
        (nA, mA) = size(A)
    else
        (mA, nA) = size(A)
    end

    if nA != length(x); error("*: argument shapes do not match"); end
    if mA != length(y); error("*: output size is incorrect"); end

    Blas.gemv!(tA, one(T), A, x, zero(T), y)
end

triu(M::AbstractMatrix) = triu(M,0)
tril(M::AbstractMatrix) = tril(M,0)
#triu{T}(M::AbstractMatrix{T}, k::Integer)
#tril{T}(M::AbstractMatrix{T}, k::Integer)
triu!(M::AbstractMatrix) = triu!(M,0)
tril!(M::AbstractMatrix) = tril!(M,0)

#diff(a::AbstractVector)
#diff(a::AbstractMatrix, dim::Integer)
diff(a::AbstractMatrix) = diff(a, 1)

gradient(F::AbstractVector) = gradient(F, [1:length(F)])
gradient(F::AbstractVector, h::Real) = gradient(F, [h*(1:length(F))])
#gradient(F::AbstractVector, h::AbstractVector)

diag(A::AbstractVector) = error("Perhaps you meant to use diagm().")
#diag(A::AbstractMatrix)

#diagm{T}(v::Union(AbstractVector{T},AbstractMatrix{T}))

function norm(x::AbstractVector, p::Number)
    if p == Inf
        return max(abs(x))
    elseif p == -Inf
        return min(abs(x))
    else
        return sum(abs(x).^p).^(1/p)
    end
end

norm(x::AbstractVector) = sqrt(real(dot(x,x)))

function norm(A::AbstractMatrix, p)
    if size(A,1) == 1 || size(A,2) == 1
        return norm(reshape(A, numel(A)), p)
    elseif p == 1
        return max(sum(abs(A),1))
    elseif p == 2
        return max(svd(A)[2])
    elseif p == Inf
        max(sum(abs(A),2))
    elseif p == "fro"
        return sqrt(sum(diag(A'*A)))
    else
        error("invalid parameter to matrix norm")
    end
end

norm(A::AbstractMatrix) = norm(A, 2)
rank(A::AbstractMatrix, tol::Real) = sum(svdvals(A) .> tol)
function rank(A::AbstractMatrix)
    sv = svdvals(A)
    sum(sv .> max(size(A,1),size(A,2))*eps(sv[1]))
end

trace(A::AbstractMatrix) = sum(diag(A))

#kron(a::AbstractVector, b::AbstractVector)
#kron{T,S}(a::AbstractMatrix{T}, b::AbstractMatrix{S})

#det(a::AbstractMatrix)
inv(a::AbstractMatrix) = a \ one(a)
cond(a::AbstractMatrix, p) = norm(a, p) * norm(inv(a), p)
cond(a::AbstractMatrix) = cond(a, 2)

#issym(A::AbstractMatrix)
#ishermitian(A::AbstractMatrix)
#istriu(A::AbstractMatrix)
#istril(A::AbstractMatrix)

function linreg(x::AbstractVector, y::AbstractVector)
    M = [ones(length(x)) x]
    Mt = M'
    ((Mt*M)\Mt)*y
end

# weighted least squares
function linreg(x::AbstractVector, y::AbstractVector, w::AbstractVector)
    w = sqrt(w)
    M = [w w.*x]
    Mt = M'
    ((Mt*M)\Mt)*(w.*y)
end

# multiply by diagonal matrix as vector
#diagmm!(C::AbstractMatrix, A::AbstractMatrix, b::AbstractVector)

#diagmm!(C::AbstractMatrix, b::AbstractVector, A::AbstractMatrix)

diagmm!(A::AbstractMatrix, b::AbstractVector) = diagmm!(A,A,b)
diagmm!(b::AbstractVector, A::AbstractMatrix) = diagmm!(A,b,A)

#diagmm(A::AbstractMatrix, b::AbstractVector)
#diagmm(b::AbstractVector, A::AbstractMatrix)

#^(A::AbstractMatrix, p::Number)

#findmax(a::AbstractArray)
#findmin(a::AbstractArray)

#rref{T}(A::AbstractMatrix{T})
