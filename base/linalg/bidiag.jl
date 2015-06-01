# This file is a part of Julia. License is MIT: http://julialang.org/license

# Bidiagonal matrices
type Bidiagonal{T} <: AbstractMatrix{T}
    dv::Vector{T} # diagonal
    ev::Vector{T} # sub/super diagonal
    isupper::Bool # is upper bidiagonal (true) or lower (false)
    function Bidiagonal{T}(dv::Vector{T}, ev::Vector{T}, isupper::Bool)
        if length(ev)==length(dv)-1
            new(dv, ev, isupper)
        else
            throw(DimensionMismatch("Length of diagonal vector is $(length(dv)), length of off-diagonal vector is $(length(ev))"))
        end
    end
end
Bidiagonal{T}(dv::AbstractVector{T}, ev::AbstractVector{T}, isupper::Bool)=Bidiagonal{T}(copy(dv), copy(ev), isupper)
Bidiagonal{T}(dv::AbstractVector{T}, ev::AbstractVector{T}) = error("Did you want an upper or lower Bidiagonal? Try again with an additional true (upper) or false (lower) argument.")

#Convert from BLAS uplo flag to boolean internal
Bidiagonal(dv::AbstractVector, ev::AbstractVector, uplo::Char) = begin
    if uplo === 'U'
        isupper = true
    elseif uplo === 'L'
        isupper = false
    else
        throw(ArgumentError("Bidiagonal uplo argument must be upper 'U' or lower 'L', got $(repr(uplo))"))
    end
    Bidiagonal(copy(dv), copy(ev), isupper)
end
function Bidiagonal{Td,Te}(dv::AbstractVector{Td}, ev::AbstractVector{Te}, isupper::Bool)
    T = promote_type(Td,Te)
    Bidiagonal(convert(Vector{T}, dv), convert(Vector{T}, ev), isupper)
end

Bidiagonal(A::AbstractMatrix, isupper::Bool)=Bidiagonal(diag(A), diag(A, isupper?1:-1), isupper)

function getindex{T}(A::Bidiagonal{T}, i::Integer, j::Integer)
    (1<=i<=size(A,2) && 1<=j<=size(A,2)) || throw(BoundsError())
    i == j ? A.dv[i] : (A.isupper && (i == j-1)) || (!A.isupper && (i == j+1)) ? A.ev[min(i,j)] : zero(T)
end

#Converting from Bidiagonal to dense Matrix
full{T}(M::Bidiagonal{T}) = convert(Matrix{T}, M)
convert{T}(::Type{Matrix{T}}, A::Bidiagonal{T})=diagm(A.dv) + diagm(A.ev, A.isupper?1:-1)
convert{T}(::Type{Matrix}, A::Bidiagonal{T}) = convert(Matrix{T}, A)
promote_rule{T,S}(::Type{Matrix{T}}, ::Type{Bidiagonal{S}})=Matrix{promote_type(T,S)}

#Converting from Bidiagonal to Tridiagonal
Tridiagonal{T}(M::Bidiagonal{T}) = convert(Tridiagonal{T}, M)
function convert{T}(::Type{Tridiagonal{T}}, A::Bidiagonal{T})
    z = zeros(T, size(A)[1]-1)
    A.isupper ? Tridiagonal(z, A.dv, A.ev) : Tridiagonal(A.ev, A.dv, z)
end
promote_rule{T,S}(::Type{Tridiagonal{T}}, ::Type{Bidiagonal{S}})=Tridiagonal{promote_type(T,S)}

###################
# LAPACK routines #
###################

#Singular values
svdvals!{T<:BlasReal}(M::Bidiagonal{T}) = LAPACK.bdsdc!(M.isupper ? 'U' : 'L', 'N', M.dv, M.ev)[1]
function svd{T<:BlasReal}(M::Bidiagonal{T})
    d, e, U, Vt, Q, iQ = LAPACK.bdsdc!(M.isupper ? 'U' : 'L', 'I', copy(M.dv), copy(M.ev))
    return U, d, Vt'
end
function svdfact!(M::Bidiagonal, thin::Bool=true)
    d, e, U, Vt, Q, iQ = LAPACK.bdsdc!(M.isupper ? 'U' : 'L', 'I', M.dv, M.ev)
    SVD(U, d, Vt)
end

####################
# Generic routines #
####################

function show(io::IO, M::Bidiagonal)
    println(io, summary(M), ":")
    print(io, " diag:")
    print_matrix(io, (M.dv)')
    print(io, M.isupper?"\nsuper:":"\n  sub:")
    print_matrix(io, (M.ev)')
end

size(M::Bidiagonal) = (length(M.dv), length(M.dv))
size(M::Bidiagonal, d::Integer) = d<1 ? throw(ArgumentError("dimension must be ≥ 1, got $d")) : (d<=2 ? length(M.dv) : 1)

#Elementary operations
for func in (:conj, :copy, :round, :trunc, :floor, :ceil)
    @eval ($func)(M::Bidiagonal) = Bidiagonal(($func)(M.dv), ($func)(M.ev), M.isupper)
end
for func in (:round, :trunc, :floor, :ceil)
    @eval ($func){T<:Integer}(::Type{T}, M::Bidiagonal) = Bidiagonal(($func)(T,M.dv), ($func)(T,M.ev), M.isupper)
end

transpose(M::Bidiagonal) = Bidiagonal(M.dv, M.ev, !M.isupper)
ctranspose(M::Bidiagonal) = Bidiagonal(conj(M.dv), conj(M.ev), !M.isupper)

istriu(M::Bidiagonal) = M.isupper || all(M.ev .== 0)
istril(M::Bidiagonal) = !M.isupper || all(M.ev .== 0)

function diag{T}(M::Bidiagonal{T}, n::Integer=0)
    if n==0
        return M.dv
    elseif n==1
        return M.isupper ? M.ev : zeros(T, size(M,1)-1)
    elseif n==-1
        return M.isupper ? zeros(T, size(M,1)-1) : M.ev
    elseif -size(M,1)<n<size(M,1)
        return zeros(T, size(M,1)-abs(n))
    else
        throw(BoundsError("Matrix size is $(size(M)), n is $n"))
    end
end

function +(A::Bidiagonal, B::Bidiagonal)
    if A.isupper==B.isupper
        Bidiagonal(A.dv+B.dv, A.ev+B.ev, A.isupper)
    else
        Tridiagonal((A.isupper ? (B.ev,A.dv+B.dv,A.ev) : (A.ev,A.dv+B.dv,B.ev))...)
    end
end

function -(A::Bidiagonal, B::Bidiagonal)
    if A.isupper==B.isupper
        Bidiagonal(A.dv-B.dv, A.ev-B.ev, A.isupper)
    else
        Tridiagonal((A.isupper ? (-B.ev,A.dv-B.dv,A.ev) : (A.ev,A.dv-B.dv,-B.ev))...)
    end
end

-(A::Bidiagonal)=Bidiagonal(-A.dv,-A.ev)
*(A::Bidiagonal, B::Number) = Bidiagonal(A.dv*B, A.ev*B, A.isupper)
*(B::Number, A::Bidiagonal) = A*B
/(A::Bidiagonal, B::Number) = Bidiagonal(A.dv/B, A.ev/B, A.isupper)
==(A::Bidiagonal, B::Bidiagonal) = (A.dv==B.dv) && (A.ev==B.ev) && (A.isupper==B.isupper)

SpecialMatrix = Union(Diagonal, Bidiagonal, SymTridiagonal, Tridiagonal, AbstractTriangular)
*(A::SpecialMatrix, B::SpecialMatrix)=full(A)*full(B)

#Generic multiplication
for func in (:*, :Ac_mul_B, :A_mul_Bc, :/, :A_rdiv_Bc)
    @eval ($func){T}(A::Bidiagonal{T}, B::AbstractVector{T}) = ($func)(full(A), B)
end

#Linear solvers
A_ldiv_B!(A::Union(Bidiagonal, AbstractTriangular), b::AbstractVector) = naivesub!(A, b)
At_ldiv_B!(A::Union(Bidiagonal, AbstractTriangular), b::AbstractVector) = naivesub!(transpose(A), b)
Ac_ldiv_B!(A::Union(Bidiagonal, AbstractTriangular), b::AbstractVector) = naivesub!(ctranspose(A), b)
function A_ldiv_B!(A::Union(Bidiagonal, AbstractTriangular), B::AbstractMatrix)
    nA,mA = size(A)
    tmp = similar(B,size(B,1))
    n = size(B, 1)
    if nA != n
        throw(DimensionMismatch("Size of A is ($nA,$mA), corresponding dimension of B is $n"))
    end
    for i = 1:size(B,2)
        copy!(tmp, 1, B, (i - 1)*n + 1, n)
        A_ldiv_B!(A, tmp)
        copy!(B, (i - 1)*n + 1, tmp, 1, n) # Modify this when array view are implemented.
    end
    B
end
A_ldiv_B(A::Union(Bidiagonal, AbstractTriangular), B::AbstractMatrix) = A_ldiv_B!(A,copy(B))

for func in (:Ac_ldiv_B!, :At_ldiv_B!)
    @eval function ($func)(A::Union(Bidiagonal, AbstractTriangular), B::AbstractMatrix)
        nA,mA = size(A)
        tmp = similar(B,size(B,1))
        n = size(B, 1)
        if mA != n
            throw(DimensionMismatch("Size of A' is ($mA,$nA), corresponding dimension of B is $n"))
        end
        for i = 1:size(B,2)
            copy!(tmp, 1, B, (i - 1)*n + 1, n)
            ($func)(A, tmp)
            copy!(B, (i - 1)*n + 1, tmp, 1, n) # Modify this when array view are implemented.
        end
        B
    end
end
Ac_ldiv_B(A::Union(Bidiagonal, AbstractTriangular), B::AbstractMatrix) = Ac_ldiv_B!(A,copy(B))
At_ldiv_B(A::Union(Bidiagonal, AbstractTriangular), B::AbstractMatrix) = At_ldiv_B!(A,copy(B))

#Generic solver using naive substitution
function naivesub!{T}(A::Bidiagonal{T}, b::AbstractVector, x::AbstractVector = b)
    N = size(A, 2)
    if N != length(b) || N != length(x)
        throw(DimensionMismatch())
    end
    if !A.isupper #do forward substitution
        for j = 1:N
            x[j] = b[j]
            j > 1 && (x[j] -= A.ev[j-1] * x[j-1])
            x[j] /= A.dv[j] == zero(T) ? throw(SingularException(j)) : A.dv[j]
        end
    else #do backward substitution
        for j = N:-1:1
            x[j] = b[j]
            j < N && (x[j] -= A.ev[j] * x[j+1])
            x[j] /= A.dv[j] == zero(T) ? throw(SingularException(j)) : A.dv[j]
        end
    end
    x
end

function \{T,S}(A::Bidiagonal{T}, B::AbstractVecOrMat{S})
    TS = typeof(zero(T)*zero(S) + zero(T)*zero(S))
    TS == S ? A_ldiv_B!(A, copy(B)) : A_ldiv_B!(A, convert(AbstractArray{TS}, B))
end

factorize(A::Bidiagonal) = A

# Eigensystems
eigvals(M::Bidiagonal) = M.dv
function eigvecs{T}(M::Bidiagonal{T})
    n = length(M.dv)
    Q = Array(T, n, n)
    blks = [0; find(x -> x == 0, M.ev); n]
    if M.isupper
        for idx_block = 1:length(blks) - 1, i = blks[idx_block] + 1:blks[idx_block + 1] #index of eigenvector
            v=zeros(T, n)
            v[blks[idx_block] + 1] = one(T)
            for j = blks[idx_block] + 1:i - 1 #Starting from j=i, eigenvector elements will be 0
                v[j+1] = (M.dv[i] - M.dv[j])/M.ev[j] * v[j]
            end
            Q[:, i] = v/norm(v)
        end
    else
        for idx_block = 1:length(blks) - 1, i = blks[idx_block + 1]:-1:blks[idx_block] + 1 #index of eigenvector
            v = zeros(T, n)
            v[blks[idx_block+1]] = one(T)
            for j = (blks[idx_block+1] - 1):-1:max(1, (i - 1)) #Starting from j=i, eigenvector elements will be 0
                v[j] = (M.dv[i] - M.dv[j+1])/M.ev[j] * v[j+1]
            end
            Q[:,i] = v/norm(v)
        end
    end
    Q #Actually Triangular
end
eigfact(M::Bidiagonal) = Eigen(eigvals(M), eigvecs(M))

