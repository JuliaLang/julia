## Diagonal matrices

type Diagonal{T} <: AbstractMatrix{T}
    diag::Vector{T}
end
Diagonal(A::Matrix) = Diagonal(diag(A))

convert{T<:Number,S<:Number}(::Type{Diagonal{T}}, A::Diagonal{S}) = T == S ? A : Diagonal(convert(Vector{T}, A.diag))

size(D::Diagonal) = (length(D.diag),length(D.diag))
size(D::Diagonal,d::Integer) = d<1 ? error("dimension out of range") : (d<=2 ? length(D.diag) : 1)

full(D::Diagonal) = diagm(D.diag)
getindex(D::Diagonal, i::Integer, j::Integer) = i == j ? D.diag[i] : zero(eltype(D.diag))

ishermitian(D::Diagonal) = true
issym(D::Diagonal) = true
isposdef(D::Diagonal) = all(D.diag .> 0)

==(Da::Diagonal, Db::Diagonal) = Da.diag == Db.diag

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
    length(v)==length(D.diag) || throw(DimensionMismatch(""))
    for i=1:length(D.diag)
        d = D.diag[i]
        d==zero(T) && throw(SingularException(i))
        v[i] *= inv(d)
    end
    v
end
function A_ldiv_B!{T}(D::Diagonal{T}, V::AbstractMatrix{T})
    size(V,1)==length(D.diag) || throw(DimensionMismatch(""))
    for i=1:length(D.diag)
        d = D.diag[i]
        d==zero(T) && throw(SingularException(i))
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
sqrtm(D::Diagonal) = Diagonal(sqrt(D.diag))

#Linear solver
function \{TD<:Number,TA<:Number}(D::Diagonal{TD}, A::AbstractArray{TA,1})
    m, n = size(A,2)==1 ? (size(A,1),1) : size(A)
    m==length(D.diag) || throw(DimensionMismatch(""))
    (m == 0 || n == 0) && return A
    C = Array(typeof(one(TD)/one(TA)),size(A))
    for j = 1:n
        for i = 1:m
            di = D.diag[i]
            di==0 && throw(SingularException(i))
            C[i,j] = A[i,j] / di
        end
    end
    return C
end
\(Da::Diagonal, Db::Diagonal) = Diagonal(Db.diag ./ Da.diag)

function inv{T}(D::Diagonal{T})
    Di = similar(D.diag)
    for i = 1:length(D.diag)
        D.diag[i]==zero(T) && throw(SingularException(i))
        Di[i]=inv(D.diag[i])
    end
    Diagonal(Di)
end

#Eigensystem
eigvals{T<:Number}(D::Diagonal{T}) = D.diag
eigvals(D::Diagonal) = [eigvals(x) for x in D.diag] #For block matrices, etc.
eigvecs(D::Diagonal) = eye(D)
eigfact(D::Diagonal) = Eigen(eigvals(D), eigvecs(D))

#Singular system
svdvals(D::Diagonal) = sort(D.diag, rev = true)
function svdfact(D::Diagonal, thin=true)
    S = abs(D.diag)
    piv = sortperm(S, rev=true)
    U = full(Diagonal(D.diag./S))
    Up= hcat([U[:,i] for i=1:length(D.diag)][piv]...)
    V = eye(D)
    Vp= hcat([V[:,i] for i=1:length(D.diag)][piv]...)
    SVD(Up, S[piv], Vp')
end
