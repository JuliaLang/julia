#### Specialized matrix types ####

## Bidiagonal matrices


type Bidiagonal{T} <: AbstractMatrix{T}
    dv::Vector{T} # diagonal
    ev::Vector{T} # sub/super diagonal
    isupper::Bool # is upper bidiagonal (true) or lower (false)
    function Bidiagonal{T}(dv::Vector{T}, ev::Vector{T}, isupper::Bool)
        length(ev)==length(dv)-1 ? new(dv, ev, isupper) : throw(DimensionMismatch(""))
    end
end

Bidiagonal{T<:BlasFloat}(dv::Vector{T}, ev::Vector{T}, isupper::Bool)=Bidiagonal{T}(copy(dv), copy(ev), isupper)
Bidiagonal{T}(dv::Vector{T}, ev::Vector{T}) = error("Did you want an upper or lower Bidiagonal? Try again with an additional true (upper) or false (lower) argument.")

#Convert from BLAS uplo flag to boolean internal
function Bidiagonal{T<:BlasFloat}(dv::Vector{T}, ev::Vector{T}, uplo::BlasChar)
    if uplo=='U'
        isupper = true
    elseif uplo=='L'
        isupper = false
    else
        error("Bidiagonal can only be upper 'U' or lower 'L' but you said '$uplo''")
    end
    Bidiagonal{T}(copy(dv), copy(ev), isupper)
end

function Bidiagonal{Td<:Number,Te<:Number}(dv::Vector{Td}, ev::Vector{Te}, isupper::Bool)
    T = promote(Td,Te)
    Bidiagonal(convert(Vector{T}, dv), convert(Vector{T}, ev), isupper)
end

Bidiagonal(A::AbstractMatrix, isupper::Bool)=Bidiagonal(diag(A), diag(A, isupper?1:-1), isupper)

#Converting from Bidiagonal to dense Matrix
full{T}(M::Bidiagonal{T}) = convert(Matrix{T}, M)
convert{T}(::Type{Matrix{T}}, A::Bidiagonal{T})=diagm(A.dv) + diagm(A.ev, A.isupper?1:-1)
promote_rule{T}(::Type{Matrix{T}}, ::Type{Bidiagonal{T}})=Matrix{T}
promote_rule{T,S}(::Type{Matrix{T}}, ::Type{Bidiagonal{S}})=Matrix{promote_type(T,S)}

#Converting from Bidiagonal to Tridiagonal
Tridiagonal{T}(M::Bidiagonal{T}) = convert(Tridiagonal{T}, M)
function convert{T}(::Type{Tridiagonal{T}}, A::Bidiagonal{T})
    z = zeros(T, size(A)[1]-1)
    A.isupper ? Tridiagonal(A.ev, A.dv, z) : Tridiagonal(z, A.dv, A.ev)
end
promote_rule{T}(::Type{Tridiagonal{T}}, ::Type{Bidiagonal{T}})=Tridiagonal{T}
promote_rule{T,S}(::Type{Tridiagonal{T}}, ::Type{Bidiagonal{S}})=Tridiagonal{promote_type(T,S)}


function show(io::IO, M::Bidiagonal)
    println(io, summary(M), ":")
    print(io, " diag:")
    print_matrix(io, (M.dv)')
    print(io, M.isupper?"\nsuper:":"\n  sub:")
    print_matrix(io, (M.ev)')
end

size(M::Bidiagonal) = (length(M.dv), length(M.dv))
size(M::Bidiagonal, d::Integer) = d<1 ? error("dimension out of range") : (d<=2 ? length(M.dv) : 1)

#Elementary operations
copy(M::Bidiagonal) = Bidiagonal(copy(M.dv), copy(M.ev), copy(M.isupper))
round(M::Bidiagonal) = Bidiagonal(round(M.dv), round(M.ev), M.isupper)
iround(M::Bidiagonal) = Bidiagonal(iround(M.dv), iround(M.ev), M.isupper)

conj(M::Bidiagonal) = Bidiagonal(conj(M.dv), conj(M.ev), M.isupper)
transpose(M::Bidiagonal) = Bidiagonal(M.dv, M.ev, !M.isupper)
ctranspose(M::Bidiagonal) = Bidiagonal(conj(M.dv), conj(M.ev), !M.isupper)

function +(A::Bidiagonal, B::Bidiagonal)
    if A.isupper==B.isupper
        Bidiagonal(A.dv+B.dv, A.ev+B.ev, A.isupper)
    else
        apply(Tridiagonal, A.isupper ? (B.ev,A.dv+B.dv,A.ev) : (A.ev,A.dv+B.dv,B.ev))
    end
end

function -(A::Bidiagonal, B::Bidiagonal)
    if A.isupper==B.isupper
        Bidiagonal(A.dv-B.dv, A.ev-B.ev, A.isupper)
    else
        apply(Tridiagonal, A.isupper ? (-B.ev,A.dv-B.dv,A.ev) : (A.ev,A.dv-B.dv,-B.ev))
    end
end

-(A::Bidiagonal)=Bidiagonal(-A.dv,-A.ev)
*(A::Bidiagonal, B::Number) = Bidiagonal(A.dv*B, A.ev*B, A.isupper)
*(B::Number, A::Bidiagonal) = A*B
/(A::Bidiagonal, B::Number) = Bidiagonal(A.dv/B, A.ev/B, A.isupper)
==(A::Bidiagonal, B::Bidiagonal) = (A.dv==B.dv) && (A.ev==B.ev) && (A.isupper==B.isupper)

SpecialMatrix = Union(Diagonal, Bidiagonal, SymTridiagonal, Tridiagonal)
*(A::SpecialMatrix, B::SpecialMatrix)=full(A)*full(B)

# solver uses tridiagonal gtsv! 
function \{T<:BlasFloat}(M::Bidiagonal{T}, rhs::StridedVecOrMat{T})
    stride(rhs, 1)==1 || solve(M, rhs) #generic fallback
    z = zeros(size(M, 1) - 1)
    apply(LAPACK.gtsv!, M.isupper ? (z, copy(M.dv), copy(M.ev), copy(rhs)) : (copy(M.ev), copy(M.dv), z, copy(rhs)))
end


#######################
# Eigenvalues/vectors #
#######################

eigvals{T<:Number}(M::Bidiagonal{T}) = M.isupper ? M.dv : reverse(M.dv)
function eigvecs{T<:Number}(M::Bidiagonal{T})
  n = length(M.dv)
  Q=zeros(T, n, n)
  v=zeros(T, n)
  if M.isupper
    for i=1:n #index of eigenvector
      v[1] = convert(T, 1.0)
      for j=1:i-1 #Starting from j=i, eigenvector elements will be 0
        v[j+1] = (M.dv[i]-M.dv[j])/M.ev[j] * v[j]
      end
      v /= norm(v)
      Q[:,i] = v
    end
  else
    for i=n:-1:1 #index of eigenvector
      v[n] = convert(T, 1.0)
      for j=(n-1):-1:max(1,(i-1)) #Starting from j=i-1, eigenvector elements will be 0
        v[j] = (M.dv[i]-M.dv[j+1])/M.ev[j] * v[j+1]
      end
      v /= norm(v)
      Q[:,n+1-i] = v
    end
  end
  Q
end

eig{T<:Number}(M::Bidiagonal{T}) = eigvals(M), eigvecs(M)
eigfact{T<:Number}(M::Bidiagonal{T}) = Eigen{T,T}(eigvals(M), eigvecs(M))

###################
# Singular values #
###################
#Wrap bdsdc to compute singular values and vectors
svdvals{T<:Real}(M::Bidiagonal{T})=LAPACK.bdsdc!(M.isupper?'U':'L', 'N', copy(M.dv), copy(M.ev))
svd    {T<:Real}(M::Bidiagonal{T})=LAPACK.bdsdc!(M.isupper?'U':'L', 'I', copy(M.dv), copy(M.ev))

