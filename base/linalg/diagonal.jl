## Diagonal matrices

type Diagonal{T} <: AbstractMatrix{T}
    diag::Vector{T}
end
Diagonal(A::Matrix) = Diagonal(diag(A))

size(D::Diagonal) = (length(D.diag),length(D.diag))
size(D::Diagonal,d::Integer) = d<1 ? error("dimension out of range") : (d<=2 ? length(D.diag) : 1)

convert{T}(::Type{Matrix{T}}, D::Diagonal{T}) = diagm(D.diag)
convert{T}(::Type{SymTridiagonal{T}}, D::Diagonal{T}) = SymTridiagonal(D.diag,zeros(T,length(D.diag)-1))
convert{T}(::Type{Tridiagonal{T}}, D::Diagonal{T}) = Tridiagonal(zeros(T,length(D.diag)-1),D.diag,zeros(T,length(D.diag)-1))

full(D::Diagonal) = diagm(D.diag)
function show(io::IO, D::Diagonal)
    println(io, summary(D), ":")
    print(io, "diag: ")
    print_matrix(io, (D.diag)')
end

ishermitian(D::Diagonal) = true
issym(D::Diagonal) = true
isposdef(D::Diagonal) = all(D.diag .> 0)

==(Da::Diagonal, Db::Diagonal) = Da.diag == Db.diag

+(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag + Db.diag)
-(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag - Db.diag)

*(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag .* Db.diag)
*(D::Diagonal, V::Vector) = D.diag .* V
*(A::Matrix, D::Diagonal) = scale(A,D.diag)
*(D::Diagonal, A::Matrix) = scale(D.diag,A)

\(Da::Diagonal, Db::Diagonal) = Diagonal(Db.diag ./ Da.diag )
/(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag ./ Db.diag )
function A_ldiv_B!(D::Diagonal, v::Vector)
    for i=1:length(D.diag)
        d = D.diag[i]
        d==0 && throw(SingularException())
        v[i] /= d
    end
    v
end
function /(A::Matrix, D::Diagonal)
    m, n = size(A)
    (m == 0 || n == 0) && return A
    n==length(D.diag) && throw(DimensionMismatch(""))
    C = Array(promote_type(eltype(A),eltype(D.diag)),size(A))
    for j = 1:n
        dj = D.diag[j]
        dj==0 && throw(SingularException())
        for i = 1:m
            C[i,j] = A[i,j] / dj
        end
    end
    return C
end
\(D::Diagonal, A::Matrix) = A/D

conj(D::Diagonal) = Diagonal(conj(D.diag))
transpose(D::Diagonal) = D
ctranspose(D::Diagonal) = conj(D)

diag(D::Diagonal) = D.diag
det(D::Diagonal) = prod(D.diag)
logdet(D::Diagonal) = sum(log(D.diag))
function inv{T<:BlasFloat}(D::Diagonal{T})
    Di = similar(D.diag)
    for i = 1:length(D.diag)
        D.diag[i] == 0 && throw(SingularException(i))
        Di[i] = 1 / D.diag[i]
    end
    Diagonal(Di)
end
inv(D::Diagonal) = inv(Diagonal(float(D.diag)))

eigvals(D::Diagonal) = sort(D.diag)

expm(D::Diagonal) = Diagonal(exp(D.diag))
sqrtm(D::Diagonal) = Diagonal(sqrt(D.diag))

# identity matrices via eye(Diagonal{type},n)
eye{T}(::Type{Diagonal{T}}, n::Int) = Diagonal(ones(T,n))
