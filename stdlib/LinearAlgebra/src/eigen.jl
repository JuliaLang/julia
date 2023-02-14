# This file is a part of Julia. License is MIT: https://julialang.org/license

# Eigendecomposition
"""
    Eigen <: Factorization

Matrix factorization type of the eigenvalue/spectral decomposition of a square
matrix `A`. This is the return type of [`eigen`](@ref), the corresponding matrix
factorization function.

If `F::Eigen` is the factorization object, the eigenvalues can be obtained via
`F.values`, the eigenvectors and left eigenvectors as the columns of the matrices
`F.vectors` and `F.vectorsl`, and error bounds for eigenvalues and eigenvectors
`F.eerrbd` and `F.verrbd`.

(The `k`th eigenvector can be obtained from the slice `F.vectors[:, k]`.)

Iterating the decomposition produces the components `F.values`, `F.vectors`, `F.vectorsl`,
`F.eerrbd`, `F.verrbd`.

The matrix of left eigenvectors is empty, if not opted in by `eigen( ; left=true)`.

The error bounds are empty, if not opted in by `eigen( ; eerror=true, verror=true)`.

# Examples
```jldoctest
julia> F = eigen([1.0 0.0 0.0; 0.0 3.0 0.0; 0.0 0.0 18.0]);

julia> F.values
3-element Vector{Float64}:
  1.0
  3.0
 18.0

julia> F.vectors
3×3 Matrix{Float64}:
 1.0  0.0  0.0
 0.0  1.0  0.0
 0.0  0.0  1.0

julia> vals, vecs = F; # destructuring via iteration

julia> vals == F.values && vecs == F.vectors
true
```
"""
struct Eigen{T,V,S<:AbstractMatrix,U<:AbstractVector,R<:AbstractVector} <: Factorization{T}
    values::U
    vectors::S
    vectorsl::S
    eerrbd::R
    verrbd::R
    Eigen{T,V,S,U,R}(values::U, vectors::S, vectorsl::S,
                     eerrbd::R, verrbd::R,
                    ) where {T,V,S<:AbstractMatrix{T},U<:AbstractVector{V},R} =
        new(values, vectors, vectorsl, eerrbd, verrbd)
end

function Eigen(values::U,
               vectors::S,
               vectorsl::S=vectors,
               eerrbd::R=emptyvec(T),
               verrbd::R=emptyvec(T),
              ) where {T,V,R,S<:AbstractMatrix{T},U<:AbstractVector{V}}

    Eigen{T,V,S,U,R}(values, vectors, vectorsl, eerrbd, verrbd)
end

# Generalized eigenvalue problem.
"""
    GeneralizedEigen <: Factorization

Matrix factorization type of the generalized eigenvalue/spectral decomposition of
`A` and `B`. This is the return type of [`eigen`](@ref), the corresponding
matrix factorization function, when called with two matrix arguments.

If `F::GeneralizedEigen` is the factorization object, the eigenvalues can be obtained via
`F.values`, the eigenvectors and left eigenvectors as the columns of the matrices
`F.vectors` or `F.vectorsl`.

(The `k`th eigenvector can be obtained from the slice `F.vectors[:, k]`.)

Iterating the decomposition produces the components `F.values`, `F.vectors`, `F.vectorsl`.

The matrix of left eigenvectors is empty, if not opted in by `eigen( ; left=true)`.

# Examples
```jldoctest
julia> A = [1 0; 0 -1]
2×2 Matrix{Int64}:
 1   0
 0  -1

julia> B = [0 1; 1 0]
2×2 Matrix{Int64}:
 0  1
 1  0

julia> F = eigen(A, B)
GeneralizedEigen{ComplexF64, ComplexF64, Matrix{ComplexF64}, Vector{ComplexF64}}
values:
2-element Vector{ComplexF64}:
 0.0 - 1.0im
 0.0 + 1.0im
vectors:
2×2 Matrix{ComplexF64}:
  0.0+1.0im   0.0-1.0im
 -1.0+0.0im  -1.0-0.0im

julia> F.values
2-element Vector{ComplexF64}:
 0.0 - 1.0im
 0.0 + 1.0im

julia> F.vectors
2×2 Matrix{ComplexF64}:
  0.0+1.0im   0.0-1.0im
 -1.0+0.0im  -1.0-0.0im

julia> vals, vecs = F; # destructuring via iteration

julia> vals == F.values && vecs == F.vectors
true
```
"""
struct GeneralizedEigen{T,V,S<:AbstractMatrix,U<:AbstractVector} <: Factorization{T}
    values::U
    vectors::S
    vectorsl::S
    GeneralizedEigen{T,V,S,U}(values::U, vectors::S, vectorsl::S,
                             ) where {T,V,S<:AbstractMatrix{T},U<:AbstractVector{V}} =
        new(values, vectors, vectorsl)
end
function GeneralizedEigen(values::U,
                          vectors::S,
                          vectorsl::S=vectors,
                          eerrbd=nothing,
                          verrbd=nothing,
                         ) where {T,V,S<:AbstractMatrix{T},U<:AbstractVector{V}}

    GeneralizedEigen{T,V,S,U}(values, vectors, vectorsl)
end

# iteration for destructuring into components
Base.iterate(S::Union{Eigen,GeneralizedEigen}) = (S.values, Val(:vectors))
Base.iterate(S::Union{Eigen,GeneralizedEigen}, ::Val{:vectors}) = (S.vectors, Val(:vectorsl))
Base.iterate(S::Eigen, ::Val{:vectorsl}) = (S.vectorsl, Val(:eerrbd))
Base.iterate(S::GeneralizedEigen, ::Val{:vectorsl}) = (S.vectorsl, Val(:done))
Base.iterate(S::Union{Eigen,GeneralizedEigen}, ::Val{:eerrbd}) = (S.eerrbd, Val(:verrbd))
Base.iterate(S::Union{Eigen,GeneralizedEigen}, ::Val{:verrbd}) = (S.verrbd, Val(:done))
Base.iterate(S::Union{Eigen,GeneralizedEigen}, ::Val{:done}) = nothing

isposdef(A::Union{Eigen,GeneralizedEigen}) = isreal(A.values) && all(x -> x > 0, A.values)

# pick a canonical ordering to avoid returning eigenvalues in "random" order
# as is the LAPACK default (for complex λ — LAPACK sorts by λ for the Hermitian/Symmetric case)
eigsortby(λ::Real) = λ
eigsortby(λ::Complex) = (real(λ), imag(λ))

# old API for symmetric cases
function sorteig!(λ::AbstractVector,
                  X::AbstractMatrix,
                  sortby::Union{Function,Nothing}=eigsortby,
                 )
    sorteig!(sortby, λ, X, X)
end

# new API with sorting function as first argument
function sorteig!(sortby::Union{Function,Nothing}, λ::AbstractVector)
    sortby === nothing ? λ : sort!(λ, by=sortby)
end
function sorteig!(sortby::Union{Function,Nothing},
                  λ::AbstractVector, X::AbstractMatrix, Y::AbstractMatrix,
                 )
    z = emptyvec(typeof(λ))
    sorteig!(sortby, λ, X, Y, z, z)
end
function sorteig!(sortby::Union{Function,Nothing},
                λ::AbstractVector, X::AbstractMatrix, Y::AbstractMatrix,
                eerrbd::AbstractVector, verrbd::AbstractVector)

    if sortby !== nothing && !issorted(λ, by=sortby)
        p = sortperm(λ; alg=QuickSort, by=sortby)
        permute!(λ, p)
        !isempty(eerrbd) && permute!(eerrbd, p)
        !isempty(verrbd) && permute!(verrbd, p)
        !isempty(X) && Base.permutecols!!(X, copy(p))
        !isempty(Y) && X !== Y && Base.permutecols!!(Y, p)
    end
    return λ, X, Y, eerrbd, verrbd
end

"""
    eigen!(A; permute, scale, sortby, left, eerror, verror)
    eigen!(A, B; sortby)

Same as [`eigen`](@ref), but saves space by overwriting the input `A` (and
`B`), instead of creating a copy.
"""
function eigen!(A::StridedMatrix{T};
                permute::Bool=true,
                scale::Bool=true,
                sortby::Union{Function,Nothing}=eigsortby,
                left::Bool=false,
                eerror::Bool=false,
                verror::Bool=false,
               ) where T <: Union{BlasReal,BlasComplex}

    right = true
    n = size(A, 2)
    n == 0 && return Eigen(zeros(T, 0), zeros(T, 0, 0))
    if T <: Real
        issymmetric(A) && return eigen!(Symmetric(A); sortby)
        rx = 3
    else
        ishermitian(A) && return eigen!(Hermitian(A); sortby)
        rx = 2
    end

    balance, jobvl, jobvr, sense = mapopts(permute, scale, left, true, eerror, verror)
    res = LAPACK.geevx!(balance, jobvl, jobvr, sense, A)

    A, WR, = res
    WI = rx == 3 ? res[rx] : zeros(eltype(WR), 0)
    VL, VR, _, _, scale, abnrm, rconde, rcondv = Iterators.drop(res, rx)

    !eerror && (rconde = emptyvec(T))
    !verror && (rcondv = emptyvec(T))
    !left && (VL = similar(A, 0, 0))
    !right && (VR = similar(A, 0, 0))

    if T <: Complex || iszero(WI)
        evecr = VR
        evecl = VL
        evals = WR
    else
        evecr = complexeig(WI, VR)
        evecl = complexeig(WI, VL)
        evals = complex.(WR, WI)
    end
    fn = epsmch(abnrm)
    Eigen(sorteig!(sortby, evals, evecr, evecl, fn ./ rconde, fn ./ rcondv)...)
end

# construct complex eigenvectors from output of geevx
function complexeig(WI::Vector{T}, VR::Matrix{T}) where T
    n = min(size(VR)...)
    evec = zeros(Complex{T}, n, n)
    j = 1
    while j <= n
        if WI[j] == 0
            evec[:,j] = view(VR, :, j)
        else
            for i = 1:n
                evec[i,j]   = VR[i,j] + im*VR[i,j+1]
                evec[i,j+1] = VR[i,j] - im*VR[i,j+1]
            end
            j += 1
        end
        j += 1
    end
    return evec
end

"""
    epsmch(abnorm) = xLAMCH('E') * abnrm in LAPACK
"""
epsmch(abnrm) = eps(one(abnrm)) / 2 * abnrm

# transform input options of eigen to those of geevx
function mapopts(permute, scale, left, right, eerror, verror)
    balance = permute ? (scale ? 'B' : 'P') : (scale ? 'S' : 'N')
    jobvl = left || eerror ? 'V' : 'N'
    jobvr = right || eerror ? 'V' : 'N'
    sense = eerror && verror ? 'B' : eerror ? 'E' : verror ? 'V' : 'N'
    balance, jobvl, jobvr, sense
end

"""
    sintheta(a::Vector, b::Vector)

Calculate the sine of the acute angle between numeric vectors `a` and `b`.

The calculation is precise for tiny results. If an argument is zero, `0` is returned.
"""
function sintheta(a::S, b::T) where {S<:AbstractVector,T<:AbstractVector}
    a = normalize(a)
    b = normalize(b)
    norm(b - a * dot(a, b))
end

"""
    eigen(A; permute=true, scale=true, sortby, left=false, eerror=false, verror=false) -> Eigen

Compute the eigenvalue decomposition of `A`, returning an [`Eigen`](@ref) factorization object `F`
which contains the eigenvalues in `F.values` and the eigenvectors in the columns of the
matrix `F.vectors`, the optional left eigenvectors in `F.vectorsl`, and optional error bounds
`F.eerrbd` for the eigenvalues, `F.verrbd` for the eigenvectors.
(The `k`th eigenvector can be obtained from the slice `F.vectors[:, k]`.)

Iterating the decomposition produces the components `F.values, F.vectors, F.vectorsl, F.eerrbd, F.verrbd`.
The optional outputs default to empty arrays of the appropriate types.

The following functions are available for `Eigen` objects: [`inv`](@ref), [`det`](@ref), and [`isposdef`](@ref).

For general nonsymmetric matrices it is possible to specify how the matrix is balanced
before the eigenvector calculation. The option `permute=true` permutes the matrix to become
closer to upper triangular, and `scale=true` scales the matrix by its diagonal elements to
make rows and columns more equal in norm. The default is `true` for both options.

By default, the eigenvalues and vectors are sorted lexicographically by `(real(λ),imag(λ))`.
A different comparison function `by(λ)` can be passed to `sortby`, or you can pass
`sortby=nothing` to leave the eigenvalues in an arbitrary order. Some special matrix types
(e.g. [`Diagonal`](@ref) or [`SymTridiagonal`](@ref)) may implement their own sorting convention and not
accept a `sortby` keyword.

The defining formula are `A * F.vectors == F.vecors * Diagonal(F.values)` and
`F.vectorsl' * A == F.vectorsl' * Diagonal(F.values)`.

While the right eigenvectors are normalized, we have `F.vectorsl' * F.vectors = I`.

The returned error bounds for the eigenvectors are estimating the maximal size of the
sine of the acute angles between the calculated and true vectors. For details see
[LAPACK](https://netlib.org/lapack/lug/node91.html).

# Examples
```jldoctest
julia> F = eigen([1.0 1.0+eps() 0.0; 1.0 1.0 0.0; 0.0 0.0 18.0], left=true, eerror=true, verror=true)
Eigen{Float64, Float64, Matrix{Float64}, Vector{Float64}, Vector{Float64}}
values:
3-element Vector{Float64}:
 -2.220446049250313e-16
  2.0
 18.0
right vectors:
3×3 Matrix{Float64}:
 -0.707107  0.707107  0.0
  0.707107  0.707107  0.0
  0.0       0.0       1.0
left vectors:
3×3 Matrix{Float64}:
 -0.707107  0.707107  0.0
  0.707107  0.707107  0.0
  0.0       0.0       1.0
value error bounds:
3-element Vector{Float64}:
 1.998401444325282e-15
 1.998401444325282e-15
 1.9984014443252818e-15
vector error bounds:
3-element Vector{Float64}:
 9.992007221626409e-16
 9.992007221626409e-16
 1.249000902703301e-16

julia> vals, vecs, vl, eerr, verr = F; # destructuring via iteration

julia> vals == F.values && vecs === F.vectors &&
       vl === F.vectorsl && eerr === F.eerrbd && verr === F.verrbd
true
```
"""
function eigen(A::AbstractMatrix{T}; sortby::Union{Function,Nothing}=eigsortby, kws...) where T
    isdiag(A) && return eigen(Diagonal{eigtype(T)}(diag(A)); sortby)
    E = if T <: Complex && ishermitian(A)
        eigen!(eigencopy_oftype(Hermitian(A), eigtype(T)); sortby)
    elseif T <: Real && issymmetric(A)
        eigen!(eigencopy_oftype(Symmetric(A), eigtype(T)); sortby)
    else
        eigen!(eigencopy_oftype(A, eigtype(T)); sortby, kws...)
    end
    return T <: Union{Float16,Complex{Float16}} ? Eigen_16(E) : E
end
eigen(x::Number; kws...) = Eigen([x], fill(one(x), 1, 1))

function Eigen_16(E::Eigen)
    (;values, vectors, vectorsl, eerrbd, verrbd) = E
    overest = eps(Float16) / eps(eltype(eerrbd))
    isempty(verrbd) || (verrbd .*= overest)
    isempty(eerrbd) || (eerrbd .*= overest)
    Eigen(copy_16.((values, vectors, vectorsl, eerrbd, verrbd))...)
end
function Eigen_16(E::GeneralizedEigen)
    (;values, vectors, vectorsl) = E
    GeneralizedEigen(copy_16.((values, vectors, vectorsl))...)
end

copy_16(a::AbstractArray{<:Real}) = copy_similar(a, Float16)
copy_16(a::AbstractArray{<:Complex}) = copy_similar(a, Complex{Float16})

"""
    eigvecs(A; permute::Bool=true, scale::Bool=true, `sortby`) -> Matrix

Return a matrix `M` whose columns are the eigenvectors of `A`. (The `k`th eigenvector can
be obtained from the slice `M[:, k]`.) The `permute`, `scale`, and `sortby` keywords are the same as
for [`eigen`](@ref).

# Examples
```jldoctest
julia> eigvecs([1.0 0.0 0.0; 0.0 3.0 0.0; 0.0 0.0 18.0])
3×3 Matrix{Float64}:
 1.0  0.0  0.0
 0.0  1.0  0.0
 0.0  0.0  1.0
```
"""
eigvecs(A::Union{Number, AbstractMatrix}; kws...) =
    eigvecs(eigen(A; kws...))
eigvecs(F::Union{Eigen, GeneralizedEigen}) = F.vectors

eigvals(F::Union{Eigen, GeneralizedEigen}) = F.values

"""
    eigvals!(A; permute::Bool=true, scale::Bool=true, sortby) -> values

Same as [`eigvals`](@ref), but saves space by overwriting the input `A`, instead of creating a copy.
The `permute`, `scale`, and `sortby` keywords are the same as for [`eigen`](@ref).

!!! note
    The input matrix `A` will not contain its eigenvalues after `eigvals!` is
    called on it - `A` is used as a workspace.

# Examples
```jldoctest
julia> A = [1. 2.; 3. 4.]
2×2 Matrix{Float64}:
 1.0  2.0
 3.0  4.0

julia> eigvals!(A)
2-element Vector{Float64}:
 -0.3722813232690143
  5.372281323269014

julia> A
2×2 Matrix{Float64}:
 -0.372281  -1.0
  0.0        5.37228
```
"""
function eigvals!(A::StridedMatrix{<:BlasReal}; permute::Bool=true, scale::Bool=true, sortby::Union{Function,Nothing}=eigsortby)
    issymmetric(A) && return sorteig!(sortby, eigvals!(Symmetric(A)))
    _, valsre, valsim, _ = LAPACK.geevx!(permute ? (scale ? 'B' : 'P') : (scale ? 'S' : 'N'), 'N', 'N', 'N', A)
    return sorteig!(sortby, iszero(valsim) ? valsre : complex.(valsre, valsim))
end
function eigvals!(A::StridedMatrix{<:BlasComplex}; permute::Bool=true, scale::Bool=true, sortby::Union{Function,Nothing}=eigsortby)
    ishermitian(A) && return sorteig!(sortby, eigvals(Hermitian(A)))
    return sorteig!(sortby, LAPACK.geevx!(permute ? (scale ? 'B' : 'P') : (scale ? 'S' : 'N'), 'N', 'N', 'N', A)[2])
end

# promotion type to use for eigenvalues of a Matrix{T}
eigtype(T) = promote_type(Float32, typeof(zero(T)/sqrt(abs2(one(T)))))
emptyvec(T::Type) = zeros(real(eigtype(eltype(T))), 0)

"""
    eigvals(A; permute::Bool=true, scale::Bool=true, sortby) -> values

Return the eigenvalues of `A`.

For general non-symmetric matrices it is possible to specify how the matrix is balanced
before the eigenvalue calculation. The `permute`, `scale`, and `sortby` keywords are
the same as for [`eigen`](@ref).

# Examples
```jldoctest
julia> diag_matrix = [1 0; 0 4]
2×2 Matrix{Int64}:
 1  0
 0  4

julia> eigvals(diag_matrix)
2-element Vector{Float64}:
 1.0
 4.0
```
"""
eigvals(A::AbstractMatrix{T}; kws...) where T =
    eigvals!(eigencopy_oftype(A, eigtype(T)); kws...)

"""
For a scalar input, `eigvals` will return a scalar.

# Example
```jldoctest
julia> eigvals(-2)
-2
```
"""
eigvals(x::Number; kwargs...) = imag(x) == 0 ? real(x) : x

"""
    eigmax(A; permute::Bool=true, scale::Bool=true)

Return the largest eigenvalue of `A`.
The option `permute=true` permutes the matrix to become
closer to upper triangular, and `scale=true` scales the matrix by its diagonal elements to
make rows and columns more equal in norm.
Note that if the eigenvalues of `A` are complex,
this method will fail, since complex numbers cannot
be sorted.

# Examples
```jldoctest
julia> A = [0 im; -im 0]
2×2 Matrix{Complex{Int64}}:
 0+0im  0+1im
 0-1im  0+0im

julia> eigmax(A)
1.0

julia> A = [0 im; -1 0]
2×2 Matrix{Complex{Int64}}:
  0+0im  0+1im
 -1+0im  0+0im

julia> eigmax(A)
ERROR: DomainError with Complex{Int64}[0+0im 0+1im; -1+0im 0+0im]:
`A` cannot have complex eigenvalues.
Stacktrace:
[...]
```
"""
function eigmax(A::Union{Number, AbstractMatrix}; permute::Bool=true, scale::Bool=true)
    v = eigvals(A, permute = permute, scale = scale)
    if eltype(v)<:Complex
        throw(DomainError(A, "`A` cannot have complex eigenvalues."))
    end
    maximum(v)
end

"""
    eigmin(A; permute::Bool=true, scale::Bool=true)

Return the smallest eigenvalue of `A`.
The option `permute=true` permutes the matrix to become
closer to upper triangular, and `scale=true` scales the matrix by its diagonal elements to
make rows and columns more equal in norm.
Note that if the eigenvalues of `A` are complex,
this method will fail, since complex numbers cannot
be sorted.

# Examples
```jldoctest
julia> A = [0 im; -im 0]
2×2 Matrix{Complex{Int64}}:
 0+0im  0+1im
 0-1im  0+0im

julia> eigmin(A)
-1.0

julia> A = [0 im; -1 0]
2×2 Matrix{Complex{Int64}}:
  0+0im  0+1im
 -1+0im  0+0im

julia> eigmin(A)
ERROR: DomainError with Complex{Int64}[0+0im 0+1im; -1+0im 0+0im]:
`A` cannot have complex eigenvalues.
Stacktrace:
[...]
```
"""
function eigmin(A::Union{Number, AbstractMatrix};
                permute::Bool=true, scale::Bool=true)
    v = eigvals(A, permute = permute, scale = scale)
    if eltype(v)<:Complex
        throw(DomainError(A, "`A` cannot have complex eigenvalues."))
    end
    minimum(v)
end

"""
    apply(f, F::Eigen)
Construct a matrix from an eigen-decomposition `F` by applying the function to
the spectrum (diagonal) of `F`.
"""
function apply(f, E::Eigen)
    d = Diagonal(f.(E.values))
    vr = E.vectors
    vl = E.vectorsl
    isempty(vr) && throw(ArgumentError("missing eigenvectors"))
    vd = vr * d
    if isempty(vl)
        vd / vr
    else
        D = inv.(dot.(eachcol(vr), eachcol(vl)))
        vd * (vl ./ D)'
    end
end
Base.inv(A::Eigen) = apply(inv, A)
det(A::Eigen) = prod(A.values)

# Generalized eigenproblem
function eigen!(A::StridedMatrix{T}, B::StridedMatrix{T};
                sortby::Union{Function,Nothing}=eigsortby,
                left::Bool=false,
               ) where T<:BlasReal

    issymmetric(A) && isposdef(B) && return eigen!(Symmetric(A), Symmetric(B); sortby)
    n = size(A, 1)

    alphar, alphai, beta, vl, vr = LAPACK.ggev!(left ? 'V' : 'N', 'V', A, B)
    if iszero(alphai)
        if !left
            vl = similar(vr, 0, 0)
        end
        GeneralizedEigen(sorteig!(sortby, alphar ./ beta, vr, vl)...)
    else
        vecs = complexeig(alphai, vr)
        if left
            vl = vr !== vl ? complexeig(alphai, vl) : vecs
        else
            vl = similar(vecs, 0, 0)
        end
        GeneralizedEigen(sorteig!(sortby, complex.(alphar, alphai)./beta, vecs, vl)...)
    end
end

function eigen!(A::StridedMatrix{T}, B::StridedMatrix{T};
                sortby::Union{Function,Nothing}=eigsortby,
                left::Bool=false,
               ) where T<:BlasComplex

    ishermitian(A) && isposdef(B) && return eigen!(Hermitian(A), Hermitian(B); sortby)
    alpha, beta, vl, vr = LAPACK.ggev!(left ? 'V' : 'N', 'V', A, B)
    !left && (vl = similar(vr, 0, 0))
    return GeneralizedEigen(sorteig!(sortby, alpha./beta, vr, vl)...)
end

"""
    eigen(A, B; sortby, left) -> GeneralizedEigen

Compute the generalized eigenvalue decomposition of `A` and `B`, returning a
[`GeneralizedEigen`](@ref) factorization object `F` which contains the generalized eigenvalues in
`F.values` and the generalized eigenvectors in the columns of the matrix `F.vectors`.
If `left == true`, the optional left eigenvectors are stored in `F.vectorsl`.
(The `k`th generalized eigenvector can be obtained from the slice `F.vectors[:, k]`.)

Iterating the decomposition produces the components `F.values` and `F.vectors`.

By default, the eigenvalues and vectors are sorted lexicographically by `(real(λ),imag(λ))`.
A different comparison function `by(λ)` can be passed to `sortby`, or you can pass
`sortby=nothing` to leave the eigenvalues in an arbitrary order.

The defining formula are `A * F.vectors == B * F.vecors * Diagonal(F.values)` and
`F.vectorsl' * A == F.vectorsl' * B * Diagonal(F.values)`.

While the right eigenvectors are normalized, we have `F.vectorsl' * F.vectors = I`.

# Examples
```jldoctest
julia> A = [1 0; 0 -1]
2×2 Matrix{Int64}:
 1   0
 0  -1

julia> B = [0 1; 1 0]
2×2 Matrix{Int64}:
 0  1
 1  0

julia> F = eigen(A, B);

julia> F.values
2-element Vector{ComplexF64}:
 0.0 - 1.0im
 0.0 + 1.0im

julia> F.vectors
2×2 Matrix{ComplexF64}:
  0.0+1.0im   0.0-1.0im
 -1.0+0.0im  -1.0-0.0im

julia> vals, vecs = F; # destructuring via iteration

julia> vals == F.values && vecs == F.vectors
true
```
"""
function eigen(A::AbstractMatrix{TA}, B::AbstractMatrix{TB}; kws...) where {TA,TB}
    T = promote_type(TA, TB)
    S = promote_type(eigtype(TA), TB)
    E = eigen!(eigencopy_oftype(A, S), eigencopy_oftype(B, S); kws...)
    return T <: Union{Float16,Complex{Float16}} ? Eigen_16(E) : E
end
eigen(A::Number, B::Number) = eigen(fill(A,1,1), fill(B,1,1))

"""
    LinearAlgebra.eigencopy_oftype(A::AbstractMatrix, ::Type{S})

Creates a dense copy of `A` with eltype `S` by calling `copy_similar(A, S)`.
In the case of `Hermitian` or `Symmetric` matrices additionally retains the wrapper,
together with the `uplo` field.
"""
eigencopy_oftype(A, S) = copy_similar(A, S)

"""
    eigvals!(A, B; sortby) -> values

Same as [`eigvals`](@ref), but saves space by overwriting the input `A` (and `B`),
instead of creating copies.

!!! note
    The input matrices `A` and `B` will not contain their eigenvalues after
    `eigvals!` is called. They are used as workspaces.

# Examples
```jldoctest
julia> A = [1. 0.; 0. -1.]
2×2 Matrix{Float64}:
 1.0   0.0
 0.0  -1.0

julia> B = [0. 1.; 1. 0.]
2×2 Matrix{Float64}:
 0.0  1.0
 1.0  0.0

julia> eigvals!(A, B)
2-element Vector{ComplexF64}:
 0.0 - 1.0im
 0.0 + 1.0im

julia> A
2×2 Matrix{Float64}:
 -0.0  -1.0
  1.0  -0.0

julia> B
2×2 Matrix{Float64}:
 1.0  0.0
 0.0  1.0
```
"""
function eigvals!(A::StridedMatrix{T}, B::StridedMatrix{T}; sortby::Union{Function,Nothing}=eigsortby) where T<:BlasReal
    issymmetric(A) && isposdef(B) && return sorteig!(sortby, eigvals!(Symmetric(A), Symmetric(B)))
    alphar, alphai, beta = LAPACK.ggev!('N', 'N', A, B)
    return sorteig!(sortby, (iszero(alphai) ? alphar : complex.(alphar, alphai))./beta)
end
function eigvals!(A::StridedMatrix{T}, B::StridedMatrix{T}; sortby::Union{Function,Nothing}=eigsortby) where T<:BlasComplex
    ishermitian(A) && isposdef(B) && return sorteig!(sortby, eigvals!(Hermitian(A), Hermitian(B)))
    alpha, beta = LAPACK.ggev!('N', 'N', A, B)
    return sorteig!(sortby, alpha./beta)
end

"""
    eigvals(A, B) -> values

Compute the generalized eigenvalues of `A` and `B`.

# Examples
```jldoctest
julia> A = [1 0; 0 -1]
2×2 Matrix{Int64}:
 1   0
 0  -1

julia> B = [0 1; 1 0]
2×2 Matrix{Int64}:
 0  1
 1  0

julia> eigvals(A,B)
2-element Vector{ComplexF64}:
 0.0 - 1.0im
 0.0 + 1.0im
```
"""
function eigvals(A::AbstractMatrix{TA}, B::AbstractMatrix{TB}; kws...) where {TA,TB}
    S = promote_type(eigtype(TA), TB)
    return eigvals!(eigencopy_oftype(A, S), eigencopy_oftype(B, S); kws...)
end

"""
    eigvecs(A, B) -> Matrix

Return a matrix `M` whose columns are the generalized eigenvectors of `A` and `B`. (The `k`th eigenvector can
be obtained from the slice `M[:, k]`.)

# Examples
```jldoctest
julia> A = [1 0; 0 -1]
2×2 Matrix{Int64}:
 1   0
 0  -1

julia> B = [0 1; 1 0]
2×2 Matrix{Int64}:
 0  1
 1  0

julia> eigvecs(A, B)
2×2 Matrix{ComplexF64}:
  0.0+1.0im   0.0-1.0im
 -1.0+0.0im  -1.0-0.0im
```
"""
eigvecs(A::AbstractMatrix, B::AbstractMatrix; kws...) = eigvecs(eigen(A, B; kws...))

function Base.show(io::IO, mime::MIME{Symbol("text/plain")}, F::Union{Eigen,GeneralizedEigen})
    summary(io, F); println(io)
    println(io, "values:")
    show(io, mime, F.values)
    if !isdefined(F, :vectorsl) || (!isempty(F.vectors) && (F.vectors === F.vectorsl || isempty(F.vectorsl)))
        println(io, "\nvectors:")
        show(io, mime, F.vectors)
    else
        if !isempty(F.vectors)
            println(io, "\nright vectors:")
            show(io, mime, F.vectors)
        end
        if !isempty(F.vectorsl)
            println(io, "\nleft vectors:")
            show(io, mime, F.vectorsl)
        end
    end
    if isdefined(F, :eerrbd) && !isempty(F.eerrbd)
        println(io, "\nvalue error bounds:")
        show(io, mime, F.eerrbd)
    end
    if isdefined(F, :verrbd) && !isempty(F.verrbd)
        println(io, "\nvector error bounds:")
        show(io, mime, F.verrbd)
    end
nothing
end

function Base.hash(F::Eigen, h::UInt)
    return hash(F.values, hash(F.vectors, hash(F.vectorsl, hash(Eigen, h))))
end
function Base.:(==)(A::Eigen, B::Eigen)
    return A.values == B.values && A.vectors == B.vectors && A.vectorsl == B.vectorsl
end
function Base.isequal(A::Eigen, B::Eigen)
    return isequal(A.values, B.values) &&
           isequal(A.vectors, B.vectors) &&
           isequal(A.vectorsl, B.vectorsl)
end

# Conversion methods

## Can we determine the source/result is Real?  This is not stored in the type Eigen
AbstractMatrix(F::Eigen) = apply(identity, F)
AbstractArray(F::Eigen) = AbstractMatrix(F)
Matrix(F::Eigen) = Array(AbstractArray(F))
Array(F::Eigen) = Matrix(F)
