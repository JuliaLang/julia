# This file is a part of Julia. License is MIT: https://julialang.org/license

# Linear algebra functions for dense matrices in column major format

## BLAS cutoff threshold constants

#TODO const DOT_CUTOFF = 128
const ASUM_CUTOFF = 32
const NRM2_CUTOFF = 32

# Generic cross-over constant based on benchmarking on a single thread with an i7 CPU @ 2.5GHz
# L1 cache: 32K, L2 cache: 256K, L3 cache: 6144K
# This constant should ideally be determined by the actual CPU cache size
const ISONE_CUTOFF = 2^21 # 2M

function isone(A::StridedMatrix)
    m, n = size(A)
    m != n && return false # only square matrices can satisfy x == one(x)
    if sizeof(A) < ISONE_CUTOFF
        _isone_triacheck(A, m)
    else
        _isone_cachefriendly(A, m)
    end
end

@inline function _isone_triacheck(A::StridedMatrix, m::Int)
    @inbounds for i in 1:m, j in i:m
        if i == j
            isone(A[i,i]) || return false
        else
            iszero(A[i,j]) && iszero(A[j,i]) || return false
        end
    end
    return true
end

# Inner loop over rows to be friendly to the CPU cache
@inline function _isone_cachefriendly(A::StridedMatrix, m::Int)
    @inbounds for i in 1:m, j in 1:m
        if i == j
            isone(A[i,i]) || return false
        else
            iszero(A[j,i]) || return false
        end
    end
    return true
end


"""
    isposdef!(A) -> Bool

Test whether a matrix is positive definite (and Hermitian) by trying to perform a
Cholesky factorization of `A`, overwriting `A` in the process.
See also [`isposdef`](@ref).

# Examples
```jldoctest
julia> A = [1. 2.; 2. 50.];

julia> isposdef!(A)
true

julia> A
2×2 Matrix{Float64}:
 1.0  2.0
 2.0  6.78233
```
"""
isposdef!(A::AbstractMatrix) =
    ishermitian(A) && isposdef(cholesky!(Hermitian(A); check = false))

"""
    isposdef(A) -> Bool

Test whether a matrix is positive definite (and Hermitian) by trying to perform a
Cholesky factorization of `A`.
See also [`isposdef!`](@ref)

# Examples
```jldoctest
julia> A = [1 2; 2 50]
2×2 Matrix{Int64}:
 1   2
 2  50

julia> isposdef(A)
true
```
"""
isposdef(A::AbstractMatrix) =
    ishermitian(A) && isposdef(cholesky(Hermitian(A); check = false))
isposdef(x::Number) = imag(x)==0 && real(x) > 0

function norm(x::StridedVector{T}, rx::Union{UnitRange{TI},AbstractRange{TI}}) where {T<:BlasFloat,TI<:Integer}
    if minimum(rx) < 1 || maximum(rx) > length(x)
        throw(BoundsError(x, rx))
    end
    GC.@preserve x BLAS.nrm2(length(rx), pointer(x)+(first(rx)-1)*sizeof(T), step(rx))
end

norm1(x::Union{Array{T},StridedVector{T}}) where {T<:BlasReal} =
    length(x) < ASUM_CUTOFF ? generic_norm1(x) : BLAS.asum(x)

norm2(x::Union{Array{T},StridedVector{T}}) where {T<:BlasFloat} =
    length(x) < NRM2_CUTOFF ? generic_norm2(x) : BLAS.nrm2(x)

"""
    triu!(M, k::Integer)

Return the upper triangle of `M` starting from the `k`th superdiagonal,
overwriting `M` in the process.

# Examples
```jldoctest
julia> M = [1 2 3 4 5; 1 2 3 4 5; 1 2 3 4 5; 1 2 3 4 5; 1 2 3 4 5]
5×5 Matrix{Int64}:
 1  2  3  4  5
 1  2  3  4  5
 1  2  3  4  5
 1  2  3  4  5
 1  2  3  4  5

julia> triu!(M, 1)
5×5 Matrix{Int64}:
 0  2  3  4  5
 0  0  3  4  5
 0  0  0  4  5
 0  0  0  0  5
 0  0  0  0  0
```
"""
function triu!(M::AbstractMatrix, k::Integer)
    require_one_based_indexing(M)
    m, n = size(M)
    for j in 1:min(n, m + k)
        for i in max(1, j - k + 1):m
            M[i,j] = zero(M[i,j])
        end
    end
    M
end

triu(M::Matrix, k::Integer) = triu!(copy(M), k)

"""
    tril!(M, k::Integer)

Return the lower triangle of `M` starting from the `k`th superdiagonal, overwriting `M` in
the process.

# Examples
```jldoctest
julia> M = [1 2 3 4 5; 1 2 3 4 5; 1 2 3 4 5; 1 2 3 4 5; 1 2 3 4 5]
5×5 Matrix{Int64}:
 1  2  3  4  5
 1  2  3  4  5
 1  2  3  4  5
 1  2  3  4  5
 1  2  3  4  5

julia> tril!(M, 2)
5×5 Matrix{Int64}:
 1  2  3  0  0
 1  2  3  4  0
 1  2  3  4  5
 1  2  3  4  5
 1  2  3  4  5
```
"""
function tril!(M::AbstractMatrix, k::Integer)
    require_one_based_indexing(M)
    m, n = size(M)
    for j in max(1, k + 1):n
        @inbounds for i in 1:min(j - k - 1, m)
            M[i,j] = zero(M[i,j])
        end
    end
    M
end
tril(M::Matrix, k::Integer) = tril!(copy(M), k)

"""
    fillband!(A::AbstractMatrix, x, l, u)

Fill the band between diagonals `l` and `u` with the value `x`.
"""
function fillband!(A::AbstractMatrix{T}, x, l, u) where T
    require_one_based_indexing(A)
    m, n = size(A)
    xT = convert(T, x)
    for j in 1:n
        for i in max(1,j-u):min(m,j-l)
            @inbounds A[i, j] = xT
        end
    end
    return A
end

diagind(m::Integer, n::Integer, k::Integer=0) =
    k <= 0 ? range(1-k, step=m+1, length=min(m+k, n)) : range(k*m+1, step=m+1, length=min(m, n-k))

"""
    diagind(M, k::Integer=0)

An `AbstractRange` giving the indices of the `k`th diagonal of the matrix `M`.

# Examples
```jldoctest
julia> A = [1 2 3; 4 5 6; 7 8 9]
3×3 Matrix{Int64}:
 1  2  3
 4  5  6
 7  8  9

julia> diagind(A,-1)
2:4:6
```
"""
function diagind(A::AbstractMatrix, k::Integer=0)
    require_one_based_indexing(A)
    diagind(size(A,1), size(A,2), k)
end

"""
    diag(M, k::Integer=0)

The `k`th diagonal of a matrix, as a vector.

See also: [`diagm`](@ref)

# Examples
```jldoctest
julia> A = [1 2 3; 4 5 6; 7 8 9]
3×3 Matrix{Int64}:
 1  2  3
 4  5  6
 7  8  9

julia> diag(A,1)
2-element Vector{Int64}:
 2
 6
```
"""
diag(A::AbstractMatrix, k::Integer=0) = A[diagind(A,k)]

"""
    diagm(kv::Pair{<:Integer,<:AbstractVector}...)
    diagm(m::Integer, n::Integer, kv::Pair{<:Integer,<:AbstractVector}...)

Construct a matrix from `Pair`s of diagonals and vectors.
Vector `kv.second` will be placed on the `kv.first` diagonal.
By default the matrix is square and its size is inferred
from `kv`, but a non-square size `m`×`n` (padded with zeros as needed)
can be specified by passing `m,n` as the first arguments.

`diagm` constructs a full matrix; if you want storage-efficient
versions with fast arithmetic, see [`Diagonal`](@ref), [`Bidiagonal`](@ref)
[`Tridiagonal`](@ref) and [`SymTridiagonal`](@ref).

# Examples
```jldoctest
julia> diagm(1 => [1,2,3])
4×4 Matrix{Int64}:
 0  1  0  0
 0  0  2  0
 0  0  0  3
 0  0  0  0

julia> diagm(1 => [1,2,3], -1 => [4,5])
4×4 Matrix{Int64}:
 0  1  0  0
 4  0  2  0
 0  5  0  3
 0  0  0  0
```
"""
diagm(kv::Pair{<:Integer,<:AbstractVector}...) = _diagm(nothing, kv...)
diagm(m::Integer, n::Integer, kv::Pair{<:Integer,<:AbstractVector}...) = _diagm((Int(m),Int(n)), kv...)
function _diagm(size, kv::Pair{<:Integer,<:AbstractVector}...)
    A = diagm_container(size, kv...)
    for p in kv
        inds = diagind(A, p.first)
        for (i, val) in enumerate(p.second)
            A[inds[i]] += val
        end
    end
    return A
end
function diagm_size(size::Nothing, kv::Pair{<:Integer,<:AbstractVector}...)
    mnmax = mapreduce(x -> length(x.second) + abs(Int(x.first)), max, kv; init=0)
    return mnmax, mnmax
end
function diagm_size(size::Tuple{Int,Int}, kv::Pair{<:Integer,<:AbstractVector}...)
    mmax = mapreduce(x -> length(x.second) - min(0,Int(x.first)), max, kv; init=0)
    nmax = mapreduce(x -> length(x.second) + max(0,Int(x.first)), max, kv; init=0)
    m, n = size
    (m ≥ mmax && n ≥ nmax) || throw(DimensionMismatch("invalid size=$size"))
    return m, n
end
function diagm_container(size, kv::Pair{<:Integer,<:AbstractVector}...)
    T = promote_type(map(x -> eltype(x.second), kv)...)
    # For some type `T`, `zero(T)` is not a `T` and `zeros(T, ...)` fails.
    U = promote_type(T, typeof(zero(T)))
    return zeros(U, diagm_size(size, kv...)...)
end
diagm_container(size, kv::Pair{<:Integer,<:BitVector}...) =
    falses(diagm_size(size, kv...)...)

"""
    diagm(v::AbstractVector)
    diagm(m::Integer, n::Integer, v::AbstractVector)

Construct a matrix with elements of the vector as diagonal elements.
By default, the matrix is square and its size is given by
`length(v)`, but a non-square size `m`×`n` can be specified
by passing `m,n` as the first arguments.

# Examples
```jldoctest
julia> diagm([1,2,3])
3×3 Matrix{Int64}:
 1  0  0
 0  2  0
 0  0  3
```
"""
diagm(v::AbstractVector) = diagm(0 => v)
diagm(m::Integer, n::Integer, v::AbstractVector) = diagm(m, n, 0 => v)

function tr(A::Matrix{T}) where T
    n = checksquare(A)
    t = zero(T)
    for i=1:n
        t += A[i,i]
    end
    t
end

"""
    kron!(C, A, B)

`kron!` is the in-place version of [`kron`](@ref). Computes `kron(A, B)` and stores the result in `C`
overwriting the existing value of `C`.

!!! tip
    Bounds checking can be disabled by [`@inbounds`](@ref), but you need to take care of the shape
    of `C`, `A`, `B` yourself.
"""
@inline function kron!(C::AbstractMatrix, A::AbstractMatrix, B::AbstractMatrix)
    require_one_based_indexing(A, B)
    @boundscheck (size(C) == (size(A,1)*size(B,1), size(A,2)*size(B,2))) || throw(DimensionMismatch())
    m = 0
    @inbounds for j = 1:size(A,2), l = 1:size(B,2), i = 1:size(A,1)
        Aij = A[i,j]
        for k = 1:size(B,1)
            C[m += 1] = Aij*B[k,l]
        end
    end
    return C
end

"""
    kron(A, B)

Kronecker tensor product of two vectors or two matrices.

For real vectors `v` and `w`, the Kronecker product is related to the outer product by
`kron(v,w) == vec(w * transpose(v))` or
`w * transpose(v) == reshape(kron(v,w), (length(w), length(v)))`.
Note how the ordering of `v` and `w` differs on the left and right
of these expressions (due to column-major storage).
For complex vectors, the outer product `w * v'` also differs by conjugation of `v`.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> B = [im 1; 1 -im]
2×2 Matrix{Complex{Int64}}:
 0+1im  1+0im
 1+0im  0-1im

julia> kron(A, B)
4×4 Matrix{Complex{Int64}}:
 0+1im  1+0im  0+2im  2+0im
 1+0im  0-1im  2+0im  0-2im
 0+3im  3+0im  0+4im  4+0im
 3+0im  0-3im  4+0im  0-4im

julia> v = [1, 2]; w = [3, 4, 5];

julia> w*transpose(v)
3×2 Matrix{Int64}:
 3   6
 4   8
 5  10

julia> reshape(kron(v,w), (length(w), length(v)))
3×2 Matrix{Int64}:
 3   6
 4   8
 5  10
```
"""
function kron(a::AbstractMatrix{T}, b::AbstractMatrix{S}) where {T,S}
    R = Matrix{promote_op(*,T,S)}(undef, size(a,1)*size(b,1), size(a,2)*size(b,2))
    return @inbounds kron!(R, a, b)
end

kron!(c::AbstractVecOrMat, a::AbstractVecOrMat, b::Number) = mul!(c, a, b)
kron!(c::AbstractVecOrMat, a::Number, b::AbstractVecOrMat) = mul!(c, a, b)

Base.@propagate_inbounds function kron!(c::AbstractVector, a::AbstractVector, b::AbstractVector)
    C = reshape(c, length(a)*length(b), 1)
    A = reshape(a ,length(a), 1)
    B = reshape(b, length(b), 1)
    kron!(C, A, B)
    return c
end

Base.@propagate_inbounds kron!(C::AbstractMatrix, a::AbstractMatrix, b::AbstractVector) = kron!(C, a, reshape(b, length(b), 1))
Base.@propagate_inbounds kron!(C::AbstractMatrix, a::AbstractVector, b::AbstractMatrix) = kron!(C, reshape(a, length(a), 1), b)

kron(a::Number, b::Union{Number, AbstractVecOrMat}) = a * b
kron(a::AbstractVecOrMat, b::Number) = a * b
kron(a::AbstractVector, b::AbstractVector) = vec(kron(reshape(a ,length(a), 1), reshape(b, length(b), 1)))
kron(a::AbstractMatrix, b::AbstractVector) = kron(a, reshape(b, length(b), 1))
kron(a::AbstractVector, b::AbstractMatrix) = kron(reshape(a, length(a), 1), b)

kron(a::AdjointAbsVec, b::AdjointAbsVec) = adjoint(kron(adjoint(a), adjoint(b)))
kron(a::AdjOrTransAbsVec, b::AdjOrTransAbsVec) = transpose(kron(transpose(a), transpose(b)))

# Matrix power
(^)(A::AbstractMatrix, p::Integer) = p < 0 ? power_by_squaring(inv(A), -p) : power_by_squaring(A, p)
function (^)(A::AbstractMatrix{T}, p::Integer) where T<:Integer
    # make sure that e.g. [1 1;1 0]^big(3)
    # gets promotes in a similar way as 2^big(3)
    TT = promote_op(^, T, typeof(p))
    return power_by_squaring(convert(AbstractMatrix{TT}, A), p)
end
function integerpow(A::AbstractMatrix{T}, p) where T
    TT = promote_op(^, T, typeof(p))
    return (TT == T ? A : copyto!(similar(A, TT), A))^Integer(p)
end
function schurpow(A::AbstractMatrix, p)
    if istriu(A)
        # Integer part
        retmat = A ^ floor(p)
        # Real part
        if p - floor(p) == 0.5
            # special case: A^0.5 === sqrt(A)
            retmat = retmat * sqrt(A)
        else
            retmat = retmat * powm!(UpperTriangular(float.(A)), real(p - floor(p)))
        end
    else
        S,Q,d = schur(complex(A))
        # Integer part
        R = S ^ floor(p)
        # Real part
        if p - floor(p) == 0.5
            # special case: A^0.5 === sqrt(A)
            R = R * sqrt(S)
        else
            R = R * powm!(UpperTriangular(float.(S)), real(p - floor(p)))
        end
        retmat = Q * R * Q'
    end

    # if A has nonpositive real eigenvalues, retmat is a nonprincipal matrix power.
    if isreal(retmat)
        return real(retmat)
    else
        return retmat
    end
end
function (^)(A::AbstractMatrix{T}, p::Real) where T
    n = checksquare(A)

    # Quicker return if A is diagonal
    if isdiag(A)
        TT = promote_op(^, T, typeof(p))
        retmat = copy_oftype(A, TT)
        for i in 1:n
            retmat[i, i] = retmat[i, i] ^ p
        end
        return retmat
    end

    # For integer powers, use power_by_squaring
    isinteger(p) && return integerpow(A, p)

    # If possible, use diagonalization
    if issymmetric(A)
        return (Symmetric(A)^p)
    end
    if ishermitian(A)
        return (Hermitian(A)^p)
    end

    # Otherwise, use Schur decomposition
    return schurpow(A, p)
end

"""
    ^(A::AbstractMatrix, p::Number)

Matrix power, equivalent to ``\\exp(p\\log(A))``

# Examples
```jldoctest
julia> [1 2; 0 3]^3
2×2 Matrix{Int64}:
 1  26
 0  27
```
"""
(^)(A::AbstractMatrix, p::Number) = exp(p*log(A))

# Matrix exponential

"""
    exp(A::AbstractMatrix)

Compute the matrix exponential of `A`, defined by

```math
e^A = \\sum_{n=0}^{\\infty} \\frac{A^n}{n!}.
```

For symmetric or Hermitian `A`, an eigendecomposition ([`eigen`](@ref)) is
used, otherwise the scaling and squaring algorithm (see [^H05]) is chosen.

[^H05]: Nicholas J. Higham, "The squaring and scaling method for the matrix exponential revisited", SIAM Journal on Matrix Analysis and Applications, 26(4), 2005, 1179-1193. [doi:10.1137/090768539](https://doi.org/10.1137/090768539)

# Examples
```jldoctest
julia> A = Matrix(1.0I, 2, 2)
2×2 Matrix{Float64}:
 1.0  0.0
 0.0  1.0

julia> exp(A)
2×2 Matrix{Float64}:
 2.71828  0.0
 0.0      2.71828
```
"""
exp(A::StridedMatrix{<:BlasFloat}) = exp!(copy(A))
exp(A::StridedMatrix{<:Union{Integer,Complex{<:Integer}}}) = exp!(float.(A))

"""
    ^(b::Number, A::AbstractMatrix)

Matrix exponential, equivalent to ``\\exp(\\log(b)A)``.

!!! compat "Julia 1.1"
    Support for raising `Irrational` numbers (like `ℯ`)
    to a matrix was added in Julia 1.1.

# Examples
```jldoctest
julia> 2^[1 2; 0 3]
2×2 Matrix{Float64}:
 2.0  6.0
 0.0  8.0

julia> ℯ^[1 2; 0 3]
2×2 Matrix{Float64}:
 2.71828  17.3673
 0.0      20.0855
```
"""
Base.:^(b::Number, A::AbstractMatrix) = exp!(log(b)*A)
# method for ℯ to explicitly elide the log(b) multiplication
Base.:^(::Irrational{:ℯ}, A::AbstractMatrix) = exp(A)

## Destructive matrix exponential using algorithm from Higham, 2008,
## "Functions of Matrices: Theory and Computation", SIAM
function exp!(A::StridedMatrix{T}) where T<:BlasFloat
    n = checksquare(A)
    if ishermitian(A)
        return copytri!(parent(exp(Hermitian(A))), 'U', true)
    end
    ilo, ihi, scale = LAPACK.gebal!('B', A)    # modifies A
    nA   = opnorm(A, 1)
    Inn    = Matrix{T}(I, n, n)
    ## For sufficiently small nA, use lower order Padé-Approximations
    if (nA <= 2.1)
        if nA > 0.95
            C = T[17643225600.,8821612800.,2075673600.,302702400.,
                     30270240.,   2162160.,    110880.,     3960.,
                           90.,         1.]
        elseif nA > 0.25
            C = T[17297280.,8648640.,1995840.,277200.,
                     25200.,   1512.,     56.,     1.]
        elseif nA > 0.015
            C = T[30240.,15120.,3360.,
                    420.,   30.,   1.]
        else
            C = T[120.,60.,12.,1.]
        end
        A2 = A * A
        P  = copy(Inn)
        U  = C[2] * P
        V  = C[1] * P
        for k in 1:(div(size(C, 1), 2) - 1)
            k2 = 2 * k
            P *= A2
            U += C[k2 + 2] * P
            V += C[k2 + 1] * P
        end
        U = A * U
        X = V + U
        LAPACK.gesv!(V-U, X)
    else
        s  = log2(nA/5.4)               # power of 2 later reversed by squaring
        if s > 0
            si = ceil(Int,s)
            A /= convert(T,2^si)
        end
        CC = T[64764752532480000.,32382376266240000.,7771770303897600.,
                1187353796428800.,  129060195264000.,  10559470521600.,
                    670442572800.,      33522128640.,      1323241920.,
                        40840800.,           960960.,           16380.,
                             182.,                1.]
        A2 = A * A
        A4 = A2 * A2
        A6 = A2 * A4
        U  = A * (A6 * (CC[14].*A6 .+ CC[12].*A4 .+ CC[10].*A2) .+
                  CC[8].*A6 .+ CC[6].*A4 .+ CC[4].*A2 .+ CC[2].*Inn)
        V  = A6 * (CC[13].*A6 .+ CC[11].*A4 .+ CC[9].*A2) .+
                   CC[7].*A6 .+ CC[5].*A4 .+ CC[3].*A2 .+ CC[1].*Inn

        X = V + U
        LAPACK.gesv!(V-U, X)

        if s > 0            # squaring to reverse dividing by power of 2
            for t=1:si; X *= X end
        end
    end

    # Undo the balancing
    for j = ilo:ihi
        scj = scale[j]
        for i = 1:n
            X[j,i] *= scj
        end
        for i = 1:n
            X[i,j] /= scj
        end
    end

    if ilo > 1       # apply lower permutations in reverse order
        for j in (ilo-1):-1:1; rcswap!(j, Int(scale[j]), X) end
    end
    if ihi < n       # apply upper permutations in forward order
        for j in (ihi+1):n;    rcswap!(j, Int(scale[j]), X) end
    end
    X
end

## Swap rows i and j and columns i and j in X
function rcswap!(i::Integer, j::Integer, X::StridedMatrix{<:Number})
    for k = 1:size(X,1)
        X[k,i], X[k,j] = X[k,j], X[k,i]
    end
    for k = 1:size(X,2)
        X[i,k], X[j,k] = X[j,k], X[i,k]
    end
end

"""
    log(A{T}::StridedMatrix{T})

If `A` has no negative real eigenvalue, compute the principal matrix logarithm of `A`, i.e.
the unique matrix ``X`` such that ``e^X = A`` and ``-\\pi < Im(\\lambda) < \\pi`` for all
the eigenvalues ``\\lambda`` of ``X``. If `A` has nonpositive eigenvalues, a nonprincipal
matrix function is returned whenever possible.

If `A` is symmetric or Hermitian, its eigendecomposition ([`eigen`](@ref)) is
used, if `A` is triangular an improved version of the inverse scaling and squaring method is
employed (see [^AH12] and [^AHR13]). For general matrices, the complex Schur form
([`schur`](@ref)) is computed and the triangular algorithm is used on the
triangular factor.

[^AH12]: Awad H. Al-Mohy and Nicholas J. Higham, "Improved inverse  scaling and squaring algorithms for the matrix logarithm", SIAM Journal on Scientific Computing, 34(4), 2012, C153-C169. [doi:10.1137/110852553](https://doi.org/10.1137/110852553)

[^AHR13]: Awad H. Al-Mohy, Nicholas J. Higham and Samuel D. Relton, "Computing the Fréchet derivative of the matrix logarithm and estimating the condition number", SIAM Journal on Scientific Computing, 35(4), 2013, C394-C410. [doi:10.1137/120885991](https://doi.org/10.1137/120885991)

# Examples
```jldoctest
julia> A = Matrix(2.7182818*I, 2, 2)
2×2 Matrix{Float64}:
 2.71828  0.0
 0.0      2.71828

julia> log(A)
2×2 Matrix{Float64}:
 1.0  0.0
 0.0  1.0
```
"""
function log(A::StridedMatrix)
    # If possible, use diagonalization
    if ishermitian(A)
        logHermA = log(Hermitian(A))
        return isa(logHermA, Hermitian) ? copytri!(parent(logHermA), 'U', true) : parent(logHermA)
    end

    # Use Schur decomposition
    n = checksquare(A)
    if istriu(A)
        return triu!(parent(log(UpperTriangular(complex(A)))))
    else
        if isreal(A)
            SchurF = schur(real(A))
        else
            SchurF = schur(A)
        end
        if !istriu(SchurF.T)
            SchurS = schur(complex(SchurF.T))
            logT = SchurS.Z * log(UpperTriangular(SchurS.T)) * SchurS.Z'
            return SchurF.Z * logT * SchurF.Z'
        else
            R = log(UpperTriangular(complex(SchurF.T)))
            return SchurF.Z * R * SchurF.Z'
        end
    end
end

"""
    sqrt(A::AbstractMatrix)

If `A` has no negative real eigenvalues, compute the principal matrix square root of `A`,
that is the unique matrix ``X`` with eigenvalues having positive real part such that
``X^2 = A``. Otherwise, a nonprincipal square root is returned.

If `A` is real-symmetric or Hermitian, its eigendecomposition ([`eigen`](@ref)) is
used to compute the square root.   For such matrices, eigenvalues λ that
appear to be slightly negative due to roundoff errors are treated as if they were zero
More precisely, matrices with all eigenvalues `≥ -rtol*(max |λ|)` are treated as semidefinite
(yielding a Hermitian square root), with negative eigenvalues taken to be zero.
`rtol` is a keyword argument to `sqrt` (in the Hermitian/real-symmetric case only) that
defaults to machine precision scaled by `size(A,1)`.

Otherwise, the square root is determined by means of the
Björck-Hammarling method [^BH83], which computes the complex Schur form ([`schur`](@ref))
and then the complex square root of the triangular factor.

[^BH83]:

    Åke Björck and Sven Hammarling, "A Schur method for the square root of a matrix",
    Linear Algebra and its Applications, 52-53, 1983, 127-140.
    [doi:10.1016/0024-3795(83)80010-X](https://doi.org/10.1016/0024-3795(83)80010-X)

# Examples
```jldoctest
julia> A = [4 0; 0 4]
2×2 Matrix{Int64}:
 4  0
 0  4

julia> sqrt(A)
2×2 Matrix{Float64}:
 2.0  0.0
 0.0  2.0
```
"""
function sqrt(A::StridedMatrix{<:Real})
    if issymmetric(A)
        return copytri!(parent(sqrt(Symmetric(A))), 'U')
    end
    n = checksquare(A)
    if istriu(A)
        return triu!(parent(sqrt(UpperTriangular(A))))
    else
        SchurF = schur(complex(A))
        R = triu!(parent(sqrt(UpperTriangular(SchurF.T)))) # unwrapping unnecessary?
        return SchurF.vectors * R * SchurF.vectors'
    end
end
function sqrt(A::StridedMatrix{<:Complex})
    if ishermitian(A)
        sqrtHermA = sqrt(Hermitian(A))
        return isa(sqrtHermA, Hermitian) ? copytri!(parent(sqrtHermA), 'U', true) : parent(sqrtHermA)
    end
    n = checksquare(A)
    if istriu(A)
        return triu!(parent(sqrt(UpperTriangular(A))))
    else
        SchurF = schur(A)
        R = triu!(parent(sqrt(UpperTriangular(SchurF.T)))) # unwrapping unnecessary?
        return SchurF.vectors * R * SchurF.vectors'
    end
end

function inv(A::StridedMatrix{T}) where T
    checksquare(A)
    S = typeof((one(T)*zero(T) + one(T)*zero(T))/one(T))
    AA = convert(AbstractArray{S}, A)
    if istriu(AA)
        Ai = triu!(parent(inv(UpperTriangular(AA))))
    elseif istril(AA)
        Ai = tril!(parent(inv(LowerTriangular(AA))))
    else
        Ai = inv!(lu(AA))
        Ai = convert(typeof(parent(Ai)), Ai)
    end
    return Ai
end

"""
    cos(A::AbstractMatrix)

Compute the matrix cosine of a square matrix `A`.

If `A` is symmetric or Hermitian, its eigendecomposition ([`eigen`](@ref)) is used to
compute the cosine. Otherwise, the cosine is determined by calling [`exp`](@ref).

# Examples
```jldoctest
julia> cos(fill(1.0, (2,2)))
2×2 Matrix{Float64}:
  0.291927  -0.708073
 -0.708073   0.291927
```
"""
function cos(A::AbstractMatrix{<:Real})
    if issymmetric(A)
        return copytri!(parent(cos(Symmetric(A))), 'U')
    end
    T = complex(float(eltype(A)))
    return real(exp!(T.(im .* A)))
end
function cos(A::AbstractMatrix{<:Complex})
    if ishermitian(A)
        return copytri!(parent(cos(Hermitian(A))), 'U', true)
    end
    T = complex(float(eltype(A)))
    X = exp!(T.(im .* A))
    @. X = (X + $exp!(T(-im*A))) / 2
    return X
end

"""
    sin(A::AbstractMatrix)

Compute the matrix sine of a square matrix `A`.

If `A` is symmetric or Hermitian, its eigendecomposition ([`eigen`](@ref)) is used to
compute the sine. Otherwise, the sine is determined by calling [`exp`](@ref).

# Examples
```jldoctest
julia> sin(fill(1.0, (2,2)))
2×2 Matrix{Float64}:
 0.454649  0.454649
 0.454649  0.454649
```
"""
function sin(A::AbstractMatrix{<:Real})
    if issymmetric(A)
        return copytri!(parent(sin(Symmetric(A))), 'U')
    end
    T = complex(float(eltype(A)))
    return imag(exp!(T.(im .* A)))
end
function sin(A::AbstractMatrix{<:Complex})
    if ishermitian(A)
        return copytri!(parent(sin(Hermitian(A))), 'U', true)
    end
    T = complex(float(eltype(A)))
    X = exp!(T.(im .* A))
    Y = exp!(T.(.-im .* A))
    @inbounds for i in eachindex(X)
        x, y = X[i]/2, Y[i]/2
        X[i] = Complex(imag(x)-imag(y), real(y)-real(x))
    end
    return X
end

"""
    sincos(A::AbstractMatrix)

Compute the matrix sine and cosine of a square matrix `A`.

# Examples
```jldoctest
julia> S, C = sincos(fill(1.0, (2,2)));

julia> S
2×2 Matrix{Float64}:
 0.454649  0.454649
 0.454649  0.454649

julia> C
2×2 Matrix{Float64}:
  0.291927  -0.708073
 -0.708073   0.291927
```
"""
function sincos(A::AbstractMatrix{<:Real})
    if issymmetric(A)
        symsinA, symcosA = sincos(Symmetric(A))
        sinA = copytri!(parent(symsinA), 'U')
        cosA = copytri!(parent(symcosA), 'U')
        return sinA, cosA
    end
    T = complex(float(eltype(A)))
    c, s = reim(exp!(T.(im .* A)))
    return s, c
end
function sincos(A::AbstractMatrix{<:Complex})
    if ishermitian(A)
        hermsinA, hermcosA = sincos(Hermitian(A))
        sinA = copytri!(parent(hermsinA), 'U', true)
        cosA = copytri!(parent(hermcosA), 'U', true)
        return sinA, cosA
    end
    T = complex(float(eltype(A)))
    X = exp!(T.(im .* A))
    Y = exp!(T.(.-im .* A))
    @inbounds for i in eachindex(X)
        x, y = X[i]/2, Y[i]/2
        X[i] = Complex(imag(x)-imag(y), real(y)-real(x))
        Y[i] = x+y
    end
    return X, Y
end

"""
    tan(A::AbstractMatrix)

Compute the matrix tangent of a square matrix `A`.

If `A` is symmetric or Hermitian, its eigendecomposition ([`eigen`](@ref)) is used to
compute the tangent. Otherwise, the tangent is determined by calling [`exp`](@ref).

# Examples
```jldoctest
julia> tan(fill(1.0, (2,2)))
2×2 Matrix{Float64}:
 -1.09252  -1.09252
 -1.09252  -1.09252
```
"""
function tan(A::AbstractMatrix)
    if ishermitian(A)
        return copytri!(parent(tan(Hermitian(A))), 'U', true)
    end
    S, C = sincos(A)
    S /= C
    return S
end

"""
    cosh(A::AbstractMatrix)

Compute the matrix hyperbolic cosine of a square matrix `A`.
"""
function cosh(A::AbstractMatrix)
    if ishermitian(A)
        return copytri!(parent(cosh(Hermitian(A))), 'U', true)
    end
    X = exp(A)
    @. X = (X + $exp!(float(-A))) / 2
    return X
end

"""
    sinh(A::AbstractMatrix)

Compute the matrix hyperbolic sine of a square matrix `A`.
"""
function sinh(A::AbstractMatrix)
    if ishermitian(A)
        return copytri!(parent(sinh(Hermitian(A))), 'U', true)
    end
    X = exp(A)
    @. X = (X - $exp!(float(-A))) / 2
    return X
end

"""
    tanh(A::AbstractMatrix)

Compute the matrix hyperbolic tangent of a square matrix `A`.
"""
function tanh(A::AbstractMatrix)
    if ishermitian(A)
        return copytri!(parent(tanh(Hermitian(A))), 'U', true)
    end
    X = exp(A)
    Y = exp!(float.(.-A))
    @inbounds for i in eachindex(X)
        x, y = X[i], Y[i]
        X[i] = x - y
        Y[i] = x + y
    end
    X /= Y
    return X
end

"""
    acos(A::AbstractMatrix)

Compute the inverse matrix cosine of a square matrix `A`.

If `A` is symmetric or Hermitian, its eigendecomposition ([`eigen`](@ref)) is used to
compute the inverse cosine. Otherwise, the inverse cosine is determined by using
[`log`](@ref) and [`sqrt`](@ref).  For the theory and logarithmic formulas used to compute
this function, see [^AH16_1].

[^AH16_1]: Mary Aprahamian and Nicholas J. Higham, "Matrix Inverse Trigonometric and Inverse Hyperbolic Functions: Theory and Algorithms", MIMS EPrint: 2016.4. [https://doi.org/10.1137/16M1057577](https://doi.org/10.1137/16M1057577)

# Examples
```jldoctest
julia> acos(cos([0.5 0.1; -0.2 0.3]))
2×2 Matrix{ComplexF64}:
  0.5-8.32667e-17im  0.1+0.0im
 -0.2+2.63678e-16im  0.3-3.46945e-16im
```
"""
function acos(A::AbstractMatrix)
    if ishermitian(A)
        acosHermA = acos(Hermitian(A))
        return isa(acosHermA, Hermitian) ? copytri!(parent(acosHermA), 'U', true) : parent(acosHermA)
    end
    SchurF = schur(complex(A))
    U = UpperTriangular(SchurF.T)
    R = triu!(parent(-im * log(U + im * sqrt(I - U^2))))
    return SchurF.Z * R * SchurF.Z'
end

"""
    asin(A::AbstractMatrix)

Compute the inverse matrix sine of a square matrix `A`.

If `A` is symmetric or Hermitian, its eigendecomposition ([`eigen`](@ref)) is used to
compute the inverse sine. Otherwise, the inverse sine is determined by using [`log`](@ref)
and [`sqrt`](@ref).  For the theory and logarithmic formulas used to compute this function,
see [^AH16_2].

[^AH16_2]: Mary Aprahamian and Nicholas J. Higham, "Matrix Inverse Trigonometric and Inverse Hyperbolic Functions: Theory and Algorithms", MIMS EPrint: 2016.4. [https://doi.org/10.1137/16M1057577](https://doi.org/10.1137/16M1057577)

# Examples
```jldoctest
julia> asin(sin([0.5 0.1; -0.2 0.3]))
2×2 Matrix{ComplexF64}:
  0.5-4.16334e-17im  0.1-5.55112e-17im
 -0.2+9.71445e-17im  0.3-1.249e-16im
```
"""
function asin(A::AbstractMatrix)
    if ishermitian(A)
        asinHermA = asin(Hermitian(A))
        return isa(asinHermA, Hermitian) ? copytri!(parent(asinHermA), 'U', true) : parent(asinHermA)
    end
    SchurF = schur(complex(A))
    U = UpperTriangular(SchurF.T)
    R = triu!(parent(-im * log(im * U + sqrt(I - U^2))))
    return SchurF.Z * R * SchurF.Z'
end

"""
    atan(A::AbstractMatrix)

Compute the inverse matrix tangent of a square matrix `A`.

If `A` is symmetric or Hermitian, its eigendecomposition ([`eigen`](@ref)) is used to
compute the inverse tangent. Otherwise, the inverse tangent is determined by using
[`log`](@ref).  For the theory and logarithmic formulas used to compute this function, see
[^AH16_3].

[^AH16_3]: Mary Aprahamian and Nicholas J. Higham, "Matrix Inverse Trigonometric and Inverse Hyperbolic Functions: Theory and Algorithms", MIMS EPrint: 2016.4. [https://doi.org/10.1137/16M1057577](https://doi.org/10.1137/16M1057577)

# Examples
```jldoctest
julia> atan(tan([0.5 0.1; -0.2 0.3]))
2×2 Matrix{ComplexF64}:
  0.5+1.38778e-17im  0.1-2.77556e-17im
 -0.2+6.93889e-17im  0.3-4.16334e-17im
```
"""
function atan(A::AbstractMatrix)
    if ishermitian(A)
        return copytri!(parent(atan(Hermitian(A))), 'U', true)
    end
    SchurF = schur(complex(A))
    U = im * UpperTriangular(SchurF.T)
    R = triu!(parent(log((I + U) / (I - U)) / 2im))
    return SchurF.Z * R * SchurF.Z'
end

"""
    acosh(A::AbstractMatrix)

Compute the inverse hyperbolic matrix cosine of a square matrix `A`.  For the theory and
logarithmic formulas used to compute this function, see [^AH16_4].

[^AH16_4]: Mary Aprahamian and Nicholas J. Higham, "Matrix Inverse Trigonometric and Inverse Hyperbolic Functions: Theory and Algorithms", MIMS EPrint: 2016.4. [https://doi.org/10.1137/16M1057577](https://doi.org/10.1137/16M1057577)
"""
function acosh(A::AbstractMatrix)
    if ishermitian(A)
        acoshHermA = acosh(Hermitian(A))
        return isa(acoshHermA, Hermitian) ? copytri!(parent(acoshHermA), 'U', true) : parent(acoshHermA)
    end
    SchurF = schur(complex(A))
    U = UpperTriangular(SchurF.T)
    R = triu!(parent(log(U + sqrt(U - I) * sqrt(U + I))))
    return SchurF.Z * R * SchurF.Z'
end

"""
    asinh(A::AbstractMatrix)

Compute the inverse hyperbolic matrix sine of a square matrix `A`.  For the theory and
logarithmic formulas used to compute this function, see [^AH16_5].

[^AH16_5]: Mary Aprahamian and Nicholas J. Higham, "Matrix Inverse Trigonometric and Inverse Hyperbolic Functions: Theory and Algorithms", MIMS EPrint: 2016.4. [https://doi.org/10.1137/16M1057577](https://doi.org/10.1137/16M1057577)
"""
function asinh(A::AbstractMatrix)
    if ishermitian(A)
        return copytri!(parent(asinh(Hermitian(A))), 'U', true)
    end
    SchurF = schur(complex(A))
    U = UpperTriangular(SchurF.T)
    R = triu!(parent(log(U + sqrt(I + U^2))))
    return SchurF.Z * R * SchurF.Z'
end

"""
    atanh(A::AbstractMatrix)

Compute the inverse hyperbolic matrix tangent of a square matrix `A`.  For the theory and
logarithmic formulas used to compute this function, see [^AH16_6].

[^AH16_6]: Mary Aprahamian and Nicholas J. Higham, "Matrix Inverse Trigonometric and Inverse Hyperbolic Functions: Theory and Algorithms", MIMS EPrint: 2016.4. [https://doi.org/10.1137/16M1057577](https://doi.org/10.1137/16M1057577)
"""
function atanh(A::AbstractMatrix)
    if ishermitian(A)
        return copytri!(parent(atanh(Hermitian(A))), 'U', true)
    end
    SchurF = schur(complex(A))
    U = UpperTriangular(SchurF.T)
    R = triu!(parent(log((I + U) / (I - U)) / 2))
    return SchurF.Z * R * SchurF.Z'
end

for (finv, f, finvh, fh, fn) in ((:sec, :cos, :sech, :cosh, "secant"),
                                 (:csc, :sin, :csch, :sinh, "cosecant"),
                                 (:cot, :tan, :coth, :tanh, "cotangent"))
    name = string(finv)
    hname = string(finvh)
    @eval begin
        @doc """
            $($name)(A::AbstractMatrix)

        Compute the matrix $($fn) of a square matrix `A`.
        """ ($finv)(A::AbstractMatrix{T}) where {T} = inv(($f)(A))
        @doc """
            $($hname)(A::AbstractMatrix)

        Compute the matrix hyperbolic $($fn) of square matrix `A`.
        """ ($finvh)(A::AbstractMatrix{T}) where {T} = inv(($fh)(A))
    end
end

for (tfa, tfainv, hfa, hfainv, fn) in ((:asec, :acos, :asech, :acosh, "secant"),
                                       (:acsc, :asin, :acsch, :asinh, "cosecant"),
                                       (:acot, :atan, :acoth, :atanh, "cotangent"))
    tname = string(tfa)
    hname = string(hfa)
    @eval begin
        @doc """
            $($tname)(A::AbstractMatrix)
        Compute the inverse matrix $($fn) of `A`. """ ($tfa)(A::AbstractMatrix{T}) where {T} = ($tfainv)(inv(A))
        @doc """
            $($hname)(A::AbstractMatrix)
        Compute the inverse matrix hyperbolic $($fn) of `A`. """ ($hfa)(A::AbstractMatrix{T}) where {T} = ($hfainv)(inv(A))
    end
end

"""
    factorize(A)

Compute a convenient factorization of `A`, based upon the type of the input matrix.
`factorize` checks `A` to see if it is symmetric/triangular/etc. if `A` is passed
as a generic matrix. `factorize` checks every element of `A` to verify/rule out
each property. It will short-circuit as soon as it can rule out symmetry/triangular
structure. The return value can be reused for efficient solving of multiple
systems. For example: `A=factorize(A); x=A\\b; y=A\\C`.

| Properties of `A`          | type of factorization                          |
|:---------------------------|:-----------------------------------------------|
| Positive-definite          | Cholesky (see [`cholesky`](@ref))  |
| Dense Symmetric/Hermitian  | Bunch-Kaufman (see [`bunchkaufman`](@ref)) |
| Sparse Symmetric/Hermitian | LDLt (see [`ldlt`](@ref))      |
| Triangular                 | Triangular                                     |
| Diagonal                   | Diagonal                                       |
| Bidiagonal                 | Bidiagonal                                     |
| Tridiagonal                | LU (see [`lu`](@ref))            |
| Symmetric real tridiagonal | LDLt (see [`ldlt`](@ref))      |
| General square             | LU (see [`lu`](@ref))            |
| General non-square         | QR (see [`qr`](@ref))            |

If `factorize` is called on a Hermitian positive-definite matrix, for instance, then `factorize`
will return a Cholesky factorization.

# Examples
```jldoctest
julia> A = Array(Bidiagonal(fill(1.0, (5, 5)), :U))
5×5 Matrix{Float64}:
 1.0  1.0  0.0  0.0  0.0
 0.0  1.0  1.0  0.0  0.0
 0.0  0.0  1.0  1.0  0.0
 0.0  0.0  0.0  1.0  1.0
 0.0  0.0  0.0  0.0  1.0

julia> factorize(A) # factorize will check to see that A is already factorized
5×5 Bidiagonal{Float64,Vector{Float64}}:
 1.0  1.0   ⋅    ⋅    ⋅
  ⋅   1.0  1.0   ⋅    ⋅
  ⋅    ⋅   1.0  1.0   ⋅
  ⋅    ⋅    ⋅   1.0  1.0
  ⋅    ⋅    ⋅    ⋅   1.0
```
This returns a `5×5 Bidiagonal{Float64}`, which can now be passed to other linear algebra functions
(e.g. eigensolvers) which will use specialized methods for `Bidiagonal` types.
"""
function factorize(A::StridedMatrix{T}) where T
    m, n = size(A)
    if m == n
        if m == 1 return A[1] end
        utri    = true
        utri1   = true
        herm    = true
        sym     = true
        for j = 1:n-1, i = j+1:m
            if utri1
                if A[i,j] != 0
                    utri1 = i == j + 1
                    utri = false
                end
            end
            if sym
                sym &= A[i,j] == A[j,i]
            end
            if herm
                herm &= A[i,j] == conj(A[j,i])
            end
            if !(utri1|herm|sym) break end
        end
        ltri = true
        ltri1 = true
        for j = 3:n, i = 1:j-2
            ltri1 &= A[i,j] == 0
            if !ltri1 break end
        end
        if ltri1
            for i = 1:n-1
                if A[i,i+1] != 0
                    ltri &= false
                    break
                end
            end
            if ltri
                if utri
                    return Diagonal(A)
                end
                if utri1
                    return Bidiagonal(diag(A), diag(A, -1), :L)
                end
                return LowerTriangular(A)
            end
            if utri
                return Bidiagonal(diag(A), diag(A, 1), :U)
            end
            if utri1
                if (herm & (T <: Complex)) | sym
                    try
                        return ldlt!(SymTridiagonal(diag(A), diag(A, -1)))
                    catch
                    end
                end
                return lu(Tridiagonal(diag(A, -1), diag(A), diag(A, 1)))
            end
        end
        if utri
            return UpperTriangular(A)
        end
        if herm
            cf = cholesky(A; check = false)
            if cf.info == 0
                return cf
            else
                return factorize(Hermitian(A))
            end
        end
        if sym
            return factorize(Symmetric(A))
        end
        return lu(A)
    end
    qr(A, Val(true))
end
factorize(A::Adjoint)   =   adjoint(factorize(parent(A)))
factorize(A::Transpose) = transpose(factorize(parent(A)))

## Moore-Penrose pseudoinverse

"""
    pinv(M; atol::Real=0, rtol::Real=atol>0 ? 0 : n*ϵ)
    pinv(M, rtol::Real) = pinv(M; rtol=rtol) # to be deprecated in Julia 2.0

Computes the Moore-Penrose pseudoinverse.

For matrices `M` with floating point elements, it is convenient to compute
the pseudoinverse by inverting only singular values greater than
`max(atol, rtol*σ₁)` where `σ₁` is the largest singular value of `M`.

The optimal choice of absolute (`atol`) and relative tolerance (`rtol`) varies
both with the value of `M` and the intended application of the pseudoinverse.
The default relative tolerance is `n*ϵ`, where `n` is the size of the smallest
dimension of `M`, and `ϵ` is the [`eps`](@ref) of the element type of `M`.

For inverting dense ill-conditioned matrices in a least-squares sense,
`rtol = sqrt(eps(real(float(one(eltype(M))))))` is recommended.

For more information, see [^issue8859], [^B96], [^S84], [^KY88].

# Examples
```jldoctest
julia> M = [1.5 1.3; 1.2 1.9]
2×2 Matrix{Float64}:
 1.5  1.3
 1.2  1.9

julia> N = pinv(M)
2×2 Matrix{Float64}:
  1.47287   -1.00775
 -0.930233   1.16279

julia> M * N
2×2 Matrix{Float64}:
 1.0          -2.22045e-16
 4.44089e-16   1.0
```

[^issue8859]: Issue 8859, "Fix least squares", [https://github.com/JuliaLang/julia/pull/8859](https://github.com/JuliaLang/julia/pull/8859)

[^B96]: Åke Björck, "Numerical Methods for Least Squares Problems",  SIAM Press, Philadelphia, 1996, "Other Titles in Applied Mathematics", Vol. 51. [doi:10.1137/1.9781611971484](http://epubs.siam.org/doi/book/10.1137/1.9781611971484)

[^S84]: G. W. Stewart, "Rank Degeneracy", SIAM Journal on Scientific and Statistical Computing, 5(2), 1984, 403-413. [doi:10.1137/0905030](http://epubs.siam.org/doi/abs/10.1137/0905030)

[^KY88]: Konstantinos Konstantinides and Kung Yao, "Statistical analysis of effective singular values in matrix rank determination", IEEE Transactions on Acoustics, Speech and Signal Processing, 36(5), 1988, 757-763. [doi:10.1109/29.1585](https://doi.org/10.1109/29.1585)
"""
function pinv(A::AbstractMatrix{T}; atol::Real = 0.0, rtol::Real = (eps(real(float(one(T))))*min(size(A)...))*iszero(atol)) where T
    m, n = size(A)
    Tout = typeof(zero(T)/sqrt(one(T) + one(T)))
    if m == 0 || n == 0
        return Matrix{Tout}(undef, n, m)
    end
    if istril(A)
        if istriu(A)
            maxabsA = maximum(abs.(diag(A)))
            tol = max(rtol*maxabsA, atol)
            B = zeros(Tout, n, m)
            for i = 1:min(m, n)
                if abs(A[i,i]) > tol
                    Aii = inv(A[i,i])
                    if isfinite(Aii)
                        B[i,i] = Aii
                    end
                end
            end
            return B
        end
    end
    SVD         = svd(A, full = false)
    tol         = max(rtol*maximum(SVD.S), atol)
    Stype       = eltype(SVD.S)
    Sinv        = zeros(Stype, length(SVD.S))
    index       = SVD.S .> tol
    Sinv[index] = one(Stype) ./ SVD.S[index]
    Sinv[findall(.!isfinite.(Sinv))] .= zero(Stype)
    return SVD.Vt' * (Diagonal(Sinv) * SVD.U')
end
function pinv(x::Number)
    xi = inv(x)
    return ifelse(isfinite(xi), xi, zero(xi))
end

## Basis for null space

"""
    nullspace(M; atol::Real=0, rtol::Real=atol>0 ? 0 : n*ϵ)
    nullspace(M, rtol::Real) = nullspace(M; rtol=rtol) # to be deprecated in Julia 2.0

Computes a basis for the nullspace of `M` by including the singular
vectors of `M` whose singular values have magnitudes greater than `max(atol, rtol*σ₁)`,
where `σ₁` is `M`'s largest singular value.

By default, the relative tolerance `rtol` is `n*ϵ`, where `n`
is the size of the smallest dimension of `M`, and `ϵ` is the [`eps`](@ref) of
the element type of `M`.

# Examples
```jldoctest
julia> M = [1 0 0; 0 1 0; 0 0 0]
3×3 Matrix{Int64}:
 1  0  0
 0  1  0
 0  0  0

julia> nullspace(M)
3×1 Matrix{Float64}:
 0.0
 0.0
 1.0

julia> nullspace(M, rtol=3)
3×3 Matrix{Float64}:
 0.0  1.0  0.0
 1.0  0.0  0.0
 0.0  0.0  1.0

julia> nullspace(M, atol=0.95)
3×1 Matrix{Float64}:
 0.0
 0.0
 1.0
```
"""
function nullspace(A::AbstractMatrix; atol::Real = 0.0, rtol::Real = (min(size(A)...)*eps(real(float(one(eltype(A))))))*iszero(atol))
    m, n = size(A)
    (m == 0 || n == 0) && return Matrix{eltype(A)}(I, n, n)
    SVD = svd(A, full=true)
    tol = max(atol, SVD.S[1]*rtol)
    indstart = sum(s -> s .> tol, SVD.S) + 1
    return copy(SVD.Vt[indstart:end,:]')
end

nullspace(A::AbstractVector; atol::Real = 0.0, rtol::Real = (min(size(A)...)*eps(real(float(one(eltype(A))))))*iszero(atol)) = nullspace(reshape(A, length(A), 1), rtol= rtol, atol= atol)

"""
    cond(M, p::Real=2)

Condition number of the matrix `M`, computed using the operator `p`-norm. Valid values for
`p` are `1`, `2` (default), or `Inf`.
"""
function cond(A::AbstractMatrix, p::Real=2)
    if p == 2
        v = svdvals(A)
        maxv = maximum(v)
        return iszero(maxv) ? oftype(real(maxv), Inf) : maxv / minimum(v)
    elseif p == 1 || p == Inf
        checksquare(A)
        try
            Ainv = inv(A)
            return opnorm(A, p)*opnorm(Ainv, p)
        catch e
            if isa(e, LAPACKException) || isa(e, SingularException)
                return convert(float(real(eltype(A))), Inf)
            else
                rethrow()
            end
        end
    end
    throw(ArgumentError("p-norm must be 1, 2 or Inf, got $p"))
end

## Lyapunov and Sylvester equation

# AX + XB + C = 0

"""
    sylvester(A, B, C)

Computes the solution `X` to the Sylvester equation `AX + XB + C = 0`, where `A`, `B` and
`C` have compatible dimensions and `A` and `-B` have no eigenvalues with equal real part.

# Examples
```jldoctest
julia> A = [3. 4.; 5. 6]
2×2 Matrix{Float64}:
 3.0  4.0
 5.0  6.0

julia> B = [1. 1.; 1. 2.]
2×2 Matrix{Float64}:
 1.0  1.0
 1.0  2.0

julia> C = [1. 2.; -2. 1]
2×2 Matrix{Float64}:
  1.0  2.0
 -2.0  1.0

julia> X = sylvester(A, B, C)
2×2 Matrix{Float64}:
 -4.46667   1.93333
  3.73333  -1.8

julia> A*X + X*B + C
2×2 Matrix{Float64}:
  2.66454e-15  1.77636e-15
 -3.77476e-15  4.44089e-16
```
"""
function sylvester(A::StridedMatrix{T},B::StridedMatrix{T},C::StridedMatrix{T}) where T<:BlasFloat
    RA, QA = schur(A)
    RB, QB = schur(B)

    D = -(adjoint(QA) * (C*QB))
    Y, scale = LAPACK.trsyl!('N','N', RA, RB, D)
    rmul!(QA*(Y * adjoint(QB)), inv(scale))
end
sylvester(A::StridedMatrix{T}, B::StridedMatrix{T}, C::StridedMatrix{T}) where {T<:Integer} = sylvester(float(A), float(B), float(C))

sylvester(a::Union{Real,Complex}, b::Union{Real,Complex}, c::Union{Real,Complex}) = -c / (a + b)

# AX + XA' + C = 0

"""
    lyap(A, C)

Computes the solution `X` to the continuous Lyapunov equation `AX + XA' + C = 0`, where no
eigenvalue of `A` has a zero real part and no two eigenvalues are negative complex
conjugates of each other.

# Examples
```jldoctest
julia> A = [3. 4.; 5. 6]
2×2 Matrix{Float64}:
 3.0  4.0
 5.0  6.0

julia> B = [1. 1.; 1. 2.]
2×2 Matrix{Float64}:
 1.0  1.0
 1.0  2.0

julia> X = lyap(A, B)
2×2 Matrix{Float64}:
  0.5  -0.5
 -0.5   0.25

julia> A*X + X*A' + B
2×2 Matrix{Float64}:
 0.0          6.66134e-16
 6.66134e-16  8.88178e-16
```
"""
function lyap(A::StridedMatrix{T}, C::StridedMatrix{T}) where {T<:BlasFloat}
    R, Q = schur(A)

    D = -(adjoint(Q) * (C*Q))
    Y, scale = LAPACK.trsyl!('N', T <: Complex ? 'C' : 'T', R, R, D)
    rmul!(Q*(Y * adjoint(Q)), inv(scale))
end
lyap(A::StridedMatrix{T}, C::StridedMatrix{T}) where {T<:Integer} = lyap(float(A), float(C))
lyap(a::T, c::T) where {T<:Number} = -c/(2a)
