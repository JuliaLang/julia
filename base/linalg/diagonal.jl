# This file is a part of Julia. License is MIT: http://julialang.org/license

## Diagonal matrices

immutable Diagonal{T} <: AbstractMatrix{T}
    diag::Vector{T}
end
Diagonal(A::AbstractMatrix) = Diagonal(diag(A))

convert{T}(::Type{Diagonal{T}}, D::Diagonal{T}) = D
convert{T}(::Type{Diagonal{T}}, D::Diagonal) = Diagonal{T}(convert(Vector{T}, D.diag))
convert{T}(::Type{AbstractMatrix{T}}, D::Diagonal) = convert(Diagonal{T}, D)
convert{T}(::Type{UpperTriangular}, A::Diagonal{T}) = UpperTriangular(A)
convert{T}(::Type{LowerTriangular}, A::Diagonal{T}) = LowerTriangular(A)

function similar{T}(D::Diagonal, ::Type{T}, d::Tuple{Int,Int})
    d[1] == d[2] || throw(ArgumentError("Diagonal matrix must be square"))
    return Diagonal{T}(Array(T,d[1]))
end

copy!(D1::Diagonal, D2::Diagonal) = (copy!(D1.diag, D2.diag); D1)

size(D::Diagonal) = (length(D.diag),length(D.diag))
size(D::Diagonal,d::Integer) = d<1 ? throw(ArgumentError("dimension must be ≥ 1, got $d")) : (d<=2 ? length(D.diag) : 1)

fill!(D::Diagonal, x) = (fill!(D.diag, x); D)

getindex{T<:Number}(D::Diagonal{T}, i::Integer, j::Integer) = i == j ? D.diag[i] : zero(eltype(D.diag))
getindex{T<:AbstractArray}(D::Diagonal{T}, i::Integer, j::Integer) = i == j ? D.diag[i] : zeros(eltype(D.diag), size(D.diag[i], 1), size(D.diag[j], 2))
full(D::Diagonal) = [D[i,j] for i = 1:size(D, 1), j = 1:size(D, 2)]

function getindex(D::Diagonal, i::Integer)
    n = length(D.diag)
    colm1, rowm1 = divrem(i - 1, n)
    return D[rowm1 + 1, colm1 + 1]
end

ishermitian{T<:Real}(D::Diagonal{T}) = true
ishermitian(D::Diagonal) = all([d .== ctranspose(d) for d in D.diag])
issym{T<:Number}(D::Diagonal{T}) = true
issym{T<:Number}(D::Diagonal{T}) = all(map(issym, D.diag))
isposdef(D::Diagonal) = all(map(isposdef, D.diag))

factorize{T<:Number}(D::Diagonal{T}) = D

tril!(D::Diagonal,i::Integer) = i == 0 ? D : zeros(D)
triu!(D::Diagonal,i::Integer) = i == 0 ? D : zeros(D)

(==)(Da::Diagonal, Db::Diagonal) = Da.diag == Db.diag

(+)(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag + Db.diag)
(-)(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag - Db.diag)

(*){T<:Number}(x::T, D::Diagonal) = Diagonal(x * D.diag)
(*){T<:Number}(D::Diagonal, x::T) = Diagonal(D.diag * x)
(/){T<:Number}(D::Diagonal, x::T) = Diagonal(D.diag / x)
(*)(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag .* Db.diag)
(*)(D::Diagonal, V::StridedVector) = D.diag .* V
(*)(A::StridedMatrix, D::Diagonal) = scale(A, D.diag)
(*)(D::Diagonal, A::StridedMatrix) = scale(D.diag, A)

A_mul_B!(A::Diagonal, B::AbstractMatrix) = scale!(A.diag, B)
At_mul_B!(A::Diagonal, B::AbstractMatrix)= scale!(A.diag, B)
Ac_mul_B!(A::Diagonal, B::AbstractMatrix)= scale!(conj(A.diag), B)

conj(D::Diagonal) = Diagonal(conj(D.diag))
transpose(D::Diagonal) = Diagonal(map(transpose, D.diag))
ctranspose(D::Diagonal) = Diagonal(map(ctranspose, D.diag))

diag(D::Diagonal) = D.diag
trace(D::Diagonal) = mapreduce(trace, +, D.diag)
det(D::Diagonal) = mapreduce(det, *, D.diag)
function logdet{T<:Complex}(D::Diagonal{T})
    l = mapreduce(logdet, +, D.diag)
    i = mod2pi(imag(l))
    twoπ = convert(typeof(i), 2π)
    return Complex(real(l), ifelse(i > π, i - twoπ, i))
end
logdet(D::Diagonal) = mapreduce(logdet, +, D.diag)

# identity matrices via eye(Diagonal{type},n)
eye{T<:Number}(::Type{Diagonal{T}}, n::Int) = Diagonal(ones(T,n))

expm(D::Diagonal) = Diagonal(map(expm, D.diag))
sqrtm(D::Diagonal) = Diagonal(map(sqrtm, D.diag))

#Linear solver
function A_ldiv_B!(D::Diagonal, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    m == length(D.diag) || throw(DimensionMismatch("diagonal matrix is $(length(D.diag)) by $(length(D.diag)) but right hand side has $m rows"))
    (m == 0 || n == 0) && return B
    for j = 1:n
        for i = 1:m
            di = D.diag[i]
            di == zero(di) && throw(SingularException(i))
            B[i,j] = di\B[i,j]
        end
    end
    return B
end
(\)(D::Diagonal, B::StridedVecOrMat) = A_ldiv_B!(D, copy_oftype(B, promote_type(eltype(D), eltype(B))))
(/)(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag ./ Db.diag)
(\)(Da::Diagonal, Db::Diagonal) = Diagonal(Db.diag ./ Da.diag)

function inv{T}(D::Diagonal{T})
    Di = similar(D.diag)
    for i = 1:length(D.diag)
        d = D.diag[i]
        d == zero(d) && throw(SingularException(i))
        Di[i] = inv(d)
    end
    Diagonal(Di)
end

function pinv{T}(D::Diagonal{T})
    Di = similar(D.diag)
    for i = 1:length(D.diag)
        isfinite(inv(D.diag[i])) ? Di[i] = inv(D.diag[i]) : Di[i] = zero(D.diag[i])
    end
    Diagonal(Di)
end
function pinv{T}(D::Diagonal{T}, tol::Real)
    Di = similar(D.diag)
    if length(D.diag) != 0
        maxabsD = maximum(abs(D.diag))
    end
    for i = 1:length(D.diag)
        if abs(D.diag[i]) > tol*maxabsD && isfinite(inv(D.diag[i]))
            Di[i] = inv(D.diag[i])
        else
            Di[i] = zero(D.diag[i])
        end
    end
    Diagonal(Di)
end
