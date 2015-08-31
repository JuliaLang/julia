# This file is a part of Julia. License is MIT: http://julialang.org/license

## Diagonal matrices

immutable Diagonal{T} <: AbstractMatrix{T}
    diag::Vector{T}
end
Diagonal(A::Matrix) = Diagonal(diag(A))

convert{T}(::Type{Diagonal{T}}, D::Diagonal{T}) = D
convert{T}(::Type{Diagonal{T}}, D::Diagonal) = Diagonal{T}(convert(Vector{T}, D.diag))
convert{T}(::Type{AbstractMatrix{T}}, D::Diagonal) = convert(Diagonal{T}, D)
convert{T}(::Type{UpperTriangular}, A::Diagonal{T}) = UpperTriangular(A)
convert{T}(::Type{LowerTriangular}, A::Diagonal{T}) = LowerTriangular(A)

function similar{T}(D::Diagonal, ::Type{T}, d::Tuple{Int,Int})
    if d[1] != d[2]
        throw(ArgumentError("diagonal matrix must be square"))
    end
    return Diagonal{T}(Array(T,d[1]))
end

copy!(D1::Diagonal, D2::Diagonal) = (copy!(D1.diag, D2.diag); D1)

size(D::Diagonal) = (length(D.diag),length(D.diag))

function size(D::Diagonal,d::Integer)
    if d<1
        throw(ArgumentError("dimension must be ≥ 1, got $d"))
    end
    return d<=2 ? length(D.diag) : 1
end

fill!(D::Diagonal, x) = (fill!(D.diag, x); D)

full(D::Diagonal) = diagm(D.diag)

getindex(D::Diagonal, i::Int, j::Int) = (checkbounds(D, i, j); unsafe_getindex(D, i, j))
unsafe_getindex{T}(D::Diagonal{T}, i::Int, j::Int) = i == j ? unsafe_getindex(D.diag, i) : zero(T)

setindex!(D::Diagonal, v, i::Int, j::Int) = (checkbounds(D, i, j); unsafe_setindex!(D, v, i, j))
function unsafe_setindex!(D::Diagonal, v, i::Int, j::Int)
    if i == j
        unsafe_setindex!(D.diag, v, i)
    elseif v != 0
        throw(ArgumentError("cannot set an off-diagonal index ($i, $j) to a nonzero value ($v)"))
    end
    D
end

ishermitian{T<:Real}(D::Diagonal{T}) = true
ishermitian(D::Diagonal) = all(D.diag .== real(D.diag))
issym(D::Diagonal) = true
isposdef(D::Diagonal) = all(D.diag .> 0)

factorize(D::Diagonal) = D

istriu(D::Diagonal) = true
istril(D::Diagonal) = true
function triu!(D::Diagonal,k::Integer=0)
    n = size(D,1)
    if abs(k) > n
        throw(ArgumentError("requested diagonal, $k, out of bounds in matrix of size ($n,$n)"))
    elseif k > 0
        fill!(D.diag,0)
    end
    return D
end

function tril!(D::Diagonal,k::Integer=0)
    n = size(D,1)
    if abs(k) > n
        throw(ArgumentError("requested diagonal, $k, out of bounds in matrix of size ($n,$n)"))
    elseif k < 0
        fill!(D.diag,0)
    end
    return D
end

==(Da::Diagonal, Db::Diagonal) = Da.diag == Db.diag
-(A::Diagonal)=Diagonal(-A.diag)
+(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag + Db.diag)
-(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag - Db.diag)

*{T<:Number}(x::T, D::Diagonal) = Diagonal(x * D.diag)
*{T<:Number}(D::Diagonal, x::T) = Diagonal(D.diag * x)
/{T<:Number}(D::Diagonal, x::T) = Diagonal(D.diag / x)
*(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag .* Db.diag)
*(D::Diagonal, V::Vector) = D.diag .* V
*(A::Matrix, D::Diagonal) = scale(A,D.diag)
*(D::Diagonal, A::Matrix) = scale(D.diag,A)

A_mul_B!(A::Diagonal,B::AbstractMatrix) = scale!(A.diag,B)
At_mul_B!(A::Diagonal,B::AbstractMatrix)= scale!(A.diag,B)
Ac_mul_B!(A::Diagonal,B::AbstractMatrix)= scale!(conj(A.diag),B)

/(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag ./ Db.diag )
function A_ldiv_B!{T}(D::Diagonal{T}, v::AbstractVector{T})
    if length(v) != length(D.diag)
        throw(DimensionMismatch("diagonal matrix is $(length(D.diag)) by $(length(D.diag)) but right hand side has $(length(v)) rows"))
    end
    for i=1:length(D.diag)
        d = D.diag[i]
        if d == zero(T)
            throw(SingularException(i))
        end
        v[i] *= inv(d)
    end
    v
end
function A_ldiv_B!{T}(D::Diagonal{T}, V::AbstractMatrix{T})
    if size(V,1) != length(D.diag)
        throw(DimensionMismatch("diagonal matrix is $(length(D.diag)) by $(length(D.diag)) but right hand side has $(size(V,1)) rows"))
    end
    for i=1:length(D.diag)
        d = D.diag[i]
        if d == zero(T)
            throw(SingularException(i))
        end
        V[i,:] *= inv(d)
    end
    V
end

conj(D::Diagonal) = Diagonal(conj(D.diag))
transpose(D::Diagonal) = D
ctranspose(D::Diagonal) = conj(D)

diag(D::Diagonal) = D.diag
trace(D::Diagonal) = sum(D.diag)
det(D::Diagonal) = prod(D.diag)
logdet{T<:Real}(D::Diagonal{T}) = sum(log(D.diag))
function logdet{T<:Complex}(D::Diagonal{T}) #Make sure branch cut is correct
    x = sum(log(D.diag))
    -pi<imag(x)<pi ? x : real(x)+(mod2pi(imag(x)+pi)-pi)*im
end
# identity matrices via eye(Diagonal{type},n)
eye{T}(::Type{Diagonal{T}}, n::Int) = Diagonal(ones(T,n))

expm(D::Diagonal) = Diagonal(exp(D.diag))
logm(D::Diagonal) = Diagonal(log(D.diag))
sqrtm(D::Diagonal) = Diagonal(sqrt(D.diag))

#Linear solver
function A_ldiv_B!(D::Diagonal, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != length(D.diag)
        throw(DimensionMismatch("diagonal matrix is $(length(D.diag)) by $(length(D.diag)) but right hand side has $m rows"))
    end
    (m == 0 || n == 0) && return B
    for j = 1:n
        for i = 1:m
            di = D.diag[i]
            if di == 0
                throw(SingularException(i))
            end
            B[i,j] /= di
        end
    end
    return B
end
\(D::Diagonal, B::StridedMatrix) = scale(1 ./ D.diag, B)
\(D::Diagonal, b::StridedVector) = reshape(scale(1 ./ D.diag, reshape(b, length(b), 1)), length(b))
\(Da::Diagonal, Db::Diagonal) = Diagonal(Db.diag ./ Da.diag)

function inv{T}(D::Diagonal{T})
    Di = similar(D.diag)
    for i = 1:length(D.diag)
        if D.diag[i] == zero(T)
            throw(SingularException(i))
        end
        Di[i] = inv(D.diag[i])
    end
    Diagonal(Di)
end

function pinv{T}(D::Diagonal{T})
    Di = similar(D.diag)
    for i = 1:length(D.diag)
        isfinite(inv(D.diag[i])) ? Di[i]=inv(D.diag[i]) : Di[i]=zero(T)
    end
    Diagonal(Di)
end
function pinv{T}(D::Diagonal{T}, tol::Real)
    Di = similar(D.diag)
    if( length(D.diag) != 0 ) maxabsD = maximum(abs(D.diag)) end
    for i = 1:length(D.diag)
        if( abs(D.diag[i]) > tol*maxabsD && isfinite(inv(D.diag[i])) )
            Di[i]=inv(D.diag[i])
        else
            Di[i]=zero(T)
        end
    end
    Diagonal(Di)
end

#Eigensystem
eigvals{T<:Number}(D::Diagonal{T}) = D.diag
eigvals(D::Diagonal) = [eigvals(x) for x in D.diag] #For block matrices, etc.
eigvecs(D::Diagonal) = eye(D)
eigfact(D::Diagonal) = Eigen(eigvals(D), eigvecs(D))

#Singular system
svdvals{T<:Number}(D::Diagonal{T}) = sort(abs(D.diag), rev = true)
svdvals(D::Diagonal) = [svdvals(v) for v in D.diag]
function svd{T<:Number}(D::Diagonal{T})
    S   = abs(D.diag)
    piv = sortperm(S, rev = true)
    U   = full(Diagonal(D.diag ./ S))
    Up  = hcat([U[:,i] for i = 1:length(D.diag)][piv]...)
    V   = eye(D)
    Vp  = hcat([V[:,i] for i = 1:length(D.diag)][piv]...)
    return (Up, S[piv], Vp)
end
function svdfact(D::Diagonal)
    U, s, V = svd(D)
    SVD(U, s, V')
end
