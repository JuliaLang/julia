# This file is a part of Julia. License is MIT: https://julialang.org/license

# Eigendecomposition
"""
    Eigen <: Factorization

Matrix factorization type of the eigenvalue/spectral decomposition of a square
matrix `A`. This is the return type of [`eigen`](@ref), the corresponding matrix
factorization function.

If `F::Eigen` is the factorization object, the eigenvalues can be obtained via
`F.values` and the eigenvectors as the columns of the matrix `F.vectors`.
(The `k`th eigenvector can be obtained from the slice `F.vectors[:, k]`.)

Iterating the decomposition produces the components `F.values` and `F.vectors`.

# Examples
```jldoctest
julia> F = eigen([1.0 0.0 0.0; 0.0 3.0 0.0; 0.0 0.0 18.0])
Eigen{Float64,Float64,Array{Float64,2},Array{Float64,1}}
values:
3-element Array{Float64,1}:
  1.0
  3.0
 18.0
vectors:
3×3 Array{Float64,2}:
 1.0  0.0  0.0
 0.0  1.0  0.0
 0.0  0.0  1.0

julia> F.values
3-element Array{Float64,1}:
  1.0
  3.0
 18.0

julia> F.vectors
3×3 Array{Float64,2}:
 1.0  0.0  0.0
 0.0  1.0  0.0
 0.0  0.0  1.0

julia> vals, vecs = F; # destructuring via iteration

julia> vals == F.values && vecs == F.vectors
true
```
"""
struct Eigen{T,V,S<:AbstractMatrix,U<:AbstractVector} <: Factorization{T}
    values::U
    vectors::S
    Eigen{T,V,S,U}(values::AbstractVector{V}, vectors::AbstractMatrix{T}) where {T,V,S,U} =
        new(values, vectors)
end
Eigen(values::AbstractVector{V}, vectors::AbstractMatrix{T}) where {T,V} =
    Eigen{T,V,typeof(vectors),typeof(values)}(values, vectors)

# Generalized eigenvalue problem.
"""
    GeneralizedEigen <: Factorization

Matrix factorization type of the generalized eigenvalue/spectral decomposition of
`A` and `B`. This is the return type of [`eigen`](@ref), the corresponding
matrix factorization function, when called with two matrix arguments.

If `F::GeneralizedEigen` is the factorization object, the eigenvalues can be obtained via
`F.values` and the eigenvectors as the columns of the matrix `F.vectors`.
(The `k`th eigenvector can be obtained from the slice `F.vectors[:, k]`.)

Iterating the decomposition produces the components `F.values` and `F.vectors`.

# Examples
```jldoctest
julia> A = [1 0; 0 -1]
2×2 Array{Int64,2}:
 1   0
 0  -1

julia> B = [0 1; 1 0]
2×2 Array{Int64,2}:
 0  1
 1  0

julia> F = eigen(A, B)
GeneralizedEigen{Complex{Float64},Complex{Float64},Array{Complex{Float64},2},Array{Complex{Float64},1}}
values:
2-element Array{Complex{Float64},1}:
 0.0 - 1.0im
 0.0 + 1.0im
vectors:
2×2 Array{Complex{Float64},2}:
  0.0+1.0im   0.0-1.0im
 -1.0+0.0im  -1.0-0.0im

julia> F.values
2-element Array{Complex{Float64},1}:
 0.0 - 1.0im
 0.0 + 1.0im

julia> F.vectors
2×2 Array{Complex{Float64},2}:
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
    GeneralizedEigen{T,V,S,U}(values::AbstractVector{V}, vectors::AbstractMatrix{T}) where {T,V,S,U} =
        new(values, vectors)
end
GeneralizedEigen(values::AbstractVector{V}, vectors::AbstractMatrix{T}) where {T,V} =
    GeneralizedEigen{T,V,typeof(vectors),typeof(values)}(values, vectors)

# iteration for destructuring into components
Base.iterate(S::Union{Eigen,GeneralizedEigen}) = (S.values, Val(:vectors))
Base.iterate(S::Union{Eigen,GeneralizedEigen}, ::Val{:vectors}) = (S.vectors, Val(:done))
Base.iterate(S::Union{Eigen,GeneralizedEigen}, ::Val{:done}) = nothing

isposdef(A::Union{Eigen,GeneralizedEigen}) = isreal(A.values) && all(x -> x > 0, A.values)

# pick a canonical ordering to avoid returning eigenvalues in "random" order
# as is the LAPACK default (for complex λ — LAPACK sorts by λ for the Hermitian/Symmetric case)
eigsortby(λ::Real) = λ
eigsortby(λ::Complex) = (real(λ),imag(λ))
function sorteig!(λ::AbstractVector, X::AbstractMatrix, sortby::Union{Function,Nothing}=eigsortby)
    if sortby !== nothing && !issorted(λ, by=sortby)
        p = sortperm(λ; alg=QuickSort, by=sortby)
        permute!(λ, p)
        Base.permutecols!!(X, p)
    end
    return λ, X
end
sorteig!(λ::AbstractVector, sortby::Union{Function,Nothing}=eigsortby) = sortby === nothing ? λ : sort!(λ, by=sortby)

"""
    eigen!(A, [B]; permute, scale, sortby)

Same as [`eigen`](@ref), but saves space by overwriting the input `A` (and
`B`), instead of creating a copy.
"""
function eigen!(A::StridedMatrix{T}; permute::Bool=true, scale::Bool=true, sortby::Union{Function,Nothing}=eigsortby) where T<:BlasReal
    n = size(A, 2)
    n == 0 && return Eigen(zeros(T, 0), zeros(T, 0, 0))
    issymmetric(A) && return eigen!(Symmetric(A), sortby=sortby)
    A, WR, WI, VL, VR, _ = LAPACK.geevx!(permute ? (scale ? 'B' : 'P') : (scale ? 'S' : 'N'), 'N', 'V', 'N', A)
    iszero(WI) && return Eigen(sorteig!(WR, VR, sortby)...)
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
    return Eigen(sorteig!(complex.(WR, WI), evec, sortby)...)
end

function eigen!(A::StridedMatrix{T}; permute::Bool=true, scale::Bool=true, sortby::Union{Function,Nothing}=eigsortby) where T<:BlasComplex
    n = size(A, 2)
    n == 0 && return Eigen(zeros(T, 0), zeros(T, 0, 0))
    ishermitian(A) && return eigen!(Hermitian(A), sortby=sortby)
    eval, evec = LAPACK.geevx!(permute ? (scale ? 'B' : 'P') : (scale ? 'S' : 'N'), 'N', 'V', 'N', A)[[2,4]]
    return Eigen(sorteig!(eval, evec, sortby)...)
end

"""
    eigen(A; permute::Bool=true, scale::Bool=true, sortby) -> Eigen

Computes the eigenvalue decomposition of `A`, returning an [`Eigen`](@ref) factorization object `F`
which contains the eigenvalues in `F.values` and the eigenvectors in the columns of the
matrix `F.vectors`. (The `k`th eigenvector can be obtained from the slice `F.vectors[:, k]`.)

Iterating the decomposition produces the components `F.values` and `F.vectors`.

The following functions are available for `Eigen` objects: [`inv`](@ref), [`det`](@ref), and [`isposdef`](@ref).

For general nonsymmetric matrices it is possible to specify how the matrix is balanced
before the eigenvector calculation. The option `permute=true` permutes the matrix to become
closer to upper triangular, and `scale=true` scales the matrix by its diagonal elements to
make rows and columns more equal in norm. The default is `true` for both options.

By default, the eigenvalues and vectors are sorted lexicographically by `(real(λ),imag(λ))`.
A different comparison function `by(λ)` can be passed to `sortby`, or you can pass
`sortby=nothing` to leave the eigenvalues in an arbitrary order.   Some special matrix types
(e.g. [`Diagonal`](@ref) or [`SymTridiagonal`](@ref)) may implement their own sorting convention and not
accept a `sortby` keyword.

# Examples
```jldoctest
julia> F = eigen([1.0 0.0 0.0; 0.0 3.0 0.0; 0.0 0.0 18.0])
Eigen{Float64,Float64,Array{Float64,2},Array{Float64,1}}
values:
3-element Array{Float64,1}:
  1.0
  3.0
 18.0
vectors:
3×3 Array{Float64,2}:
 1.0  0.0  0.0
 0.0  1.0  0.0
 0.0  0.0  1.0

julia> F.values
3-element Array{Float64,1}:
  1.0
  3.0
 18.0

julia> F.vectors
3×3 Array{Float64,2}:
 1.0  0.0  0.0
 0.0  1.0  0.0
 0.0  0.0  1.0

julia> vals, vecs = F; # destructuring via iteration

julia> vals == F.values && vecs == F.vectors
true
```
"""
function eigen(A::AbstractMatrix{T}; permute::Bool=true, scale::Bool=true, sortby::Union{Function,Nothing}=eigsortby) where T
    AA = copy_oftype(A, eigtype(T))
    isdiag(AA) && return eigen(Diagonal(AA); permute=permute, scale=scale, sortby=sortby)
    return eigen!(AA; permute=permute, scale=scale, sortby=sortby)
end
eigen(x::Number) = Eigen([x], fill(one(x), 1, 1))

"""
    eigvecs(A; permute::Bool=true, scale::Bool=true, `sortby`) -> Matrix

Return a matrix `M` whose columns are the eigenvectors of `A`. (The `k`th eigenvector can
be obtained from the slice `M[:, k]`.) The `permute`, `scale`, and `sortby` keywords are the same as
for [`eigen`](@ref).

# Examples
```jldoctest
julia> eigvecs([1.0 0.0 0.0; 0.0 3.0 0.0; 0.0 0.0 18.0])
3×3 Array{Float64,2}:
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
2×2 Array{Float64,2}:
 1.0  2.0
 3.0  4.0

julia> eigvals!(A)
2-element Array{Float64,1}:
 -0.3722813232690143
  5.372281323269014

julia> A
2×2 Array{Float64,2}:
 -0.372281  -1.0
  0.0        5.37228
```
"""
function eigvals!(A::StridedMatrix{<:BlasReal}; permute::Bool=true, scale::Bool=true, sortby::Union{Function,Nothing}=eigsortby)
    issymmetric(A) && return sorteig!(eigvals!(Symmetric(A)), sortby)
    _, valsre, valsim, _ = LAPACK.geevx!(permute ? (scale ? 'B' : 'P') : (scale ? 'S' : 'N'), 'N', 'N', 'N', A)
    return sorteig!(iszero(valsim) ? valsre : complex.(valsre, valsim), sortby)
end
function eigvals!(A::StridedMatrix{<:BlasComplex}; permute::Bool=true, scale::Bool=true, sortby::Union{Function,Nothing}=eigsortby)
    ishermitian(A) && return sorteig!(eigvals(Hermitian(A)), sortby)
    return sorteig!(LAPACK.geevx!(permute ? (scale ? 'B' : 'P') : (scale ? 'S' : 'N'), 'N', 'N', 'N', A)[2], sortby)
end

# promotion type to use for eigenvalues of a Matrix{T}
eigtype(T) = promote_type(Float32, typeof(zero(T)/sqrt(abs2(one(T)))))

"""
    eigvals(A; permute::Bool=true, scale::Bool=true, sortby) -> values

Return the eigenvalues of `A`.

For general non-symmetric matrices it is possible to specify how the matrix is balanced
before the eigenvalue calculation. The `permute`, `scale`, and `sortby` keywords are
the same as for [`eigen!`](@ref).

# Examples
```jldoctest
julia> diag_matrix = [1 0; 0 4]
2×2 Array{Int64,2}:
 1  0
 0  4

julia> eigvals(diag_matrix)
2-element Array{Float64,1}:
 1.0
 4.0
```
"""
eigvals(A::AbstractMatrix{T}; kws...) where T =
    eigvals!(copy_oftype(A, eigtype(T)); kws...)

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
2×2 Array{Complex{Int64},2}:
 0+0im  0+1im
 0-1im  0+0im

julia> eigmax(A)
1.0

julia> A = [0 im; -1 0]
2×2 Array{Complex{Int64},2}:
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
2×2 Array{Complex{Int64},2}:
 0+0im  0+1im
 0-1im  0+0im

julia> eigmin(A)
-1.0

julia> A = [0 im; -1 0]
2×2 Array{Complex{Int64},2}:
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

inv(A::Eigen) = A.vectors * inv(Diagonal(A.values)) / A.vectors
det(A::Eigen) = prod(A.values)

# Generalized eigenproblem
function eigen!(A::StridedMatrix{T}, B::StridedMatrix{T}; sortby::Union{Function,Nothing}=eigsortby) where T<:BlasReal
    issymmetric(A) && isposdef(B) && return eigen!(Symmetric(A), Symmetric(B), sortby=sortby)
    n = size(A, 1)
    alphar, alphai, beta, _, vr = LAPACK.ggev!('N', 'V', A, B)
    iszero(alphai) && return GeneralizedEigen(sorteig!(alphar ./ beta, vr, sortby)...)

    vecs = zeros(Complex{T}, n, n)
    j = 1
    while j <= n
        if alphai[j] == 0
            vecs[:,j] = view(vr, :, j)
        else
            for i = 1:n
                vecs[i,j  ] = vr[i,j] + im*vr[i,j+1]
                vecs[i,j+1] = vr[i,j] - im*vr[i,j+1]
            end
            j += 1
        end
        j += 1
    end
    return GeneralizedEigen(sorteig!(complex.(alphar, alphai)./beta, vecs, sortby)...)
end

function eigen!(A::StridedMatrix{T}, B::StridedMatrix{T}; sortby::Union{Function,Nothing}=eigsortby) where T<:BlasComplex
    ishermitian(A) && isposdef(B) && return eigen!(Hermitian(A), Hermitian(B), sortby=sortby)
    alpha, beta, _, vr = LAPACK.ggev!('N', 'V', A, B)
    return GeneralizedEigen(sorteig!(alpha./beta, vr, sortby)...)
end

"""
    eigen(A, B) -> GeneralizedEigen

Computes the generalized eigenvalue decomposition of `A` and `B`, returning a
[`GeneralizedEigen`](@ref) factorization object `F` which contains the generalized eigenvalues in
`F.values` and the generalized eigenvectors in the columns of the matrix `F.vectors`.
(The `k`th generalized eigenvector can be obtained from the slice `F.vectors[:, k]`.)

Iterating the decomposition produces the components `F.values` and `F.vectors`.

Any keyword arguments passed to `eigen` are passed through to the lower-level
[`eigen!`](@ref) function.

# Examples
```jldoctest
julia> A = [1 0; 0 -1]
2×2 Array{Int64,2}:
 1   0
 0  -1

julia> B = [0 1; 1 0]
2×2 Array{Int64,2}:
 0  1
 1  0

julia> F = eigen(A, B);

julia> F.values
2-element Array{Complex{Float64},1}:
 0.0 - 1.0im
 0.0 + 1.0im

julia> F.vectors
2×2 Array{Complex{Float64},2}:
  0.0+1.0im   0.0-1.0im
 -1.0+0.0im  -1.0-0.0im

julia> vals, vecs = F; # destructuring via iteration

julia> vals == F.values && vecs == F.vectors
true
```
"""
function eigen(A::AbstractMatrix{TA}, B::AbstractMatrix{TB}; kws...) where {TA,TB}
    S = promote_type(eigtype(TA),TB)
    eigen!(copy_oftype(A, S), copy_oftype(B, S); kws...)
end

eigen(A::Number, B::Number) = eigen(fill(A,1,1), fill(B,1,1))

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
2×2 Array{Float64,2}:
 1.0   0.0
 0.0  -1.0

julia> B = [0. 1.; 1. 0.]
2×2 Array{Float64,2}:
 0.0  1.0
 1.0  0.0

julia> eigvals!(A, B)
2-element Array{Complex{Float64},1}:
 0.0 - 1.0im
 0.0 + 1.0im

julia> A
2×2 Array{Float64,2}:
 -0.0  -1.0
  1.0  -0.0

julia> B
2×2 Array{Float64,2}:
 1.0  0.0
 0.0  1.0
```
"""
function eigvals!(A::StridedMatrix{T}, B::StridedMatrix{T}; sortby::Union{Function,Nothing}=eigsortby) where T<:BlasReal
    issymmetric(A) && isposdef(B) && return sorteig!(eigvals!(Symmetric(A), Symmetric(B)), sortby)
    alphar, alphai, beta, vl, vr = LAPACK.ggev!('N', 'N', A, B)
    return sorteig!((iszero(alphai) ? alphar : complex.(alphar, alphai))./beta, sortby)
end
function eigvals!(A::StridedMatrix{T}, B::StridedMatrix{T}; sortby::Union{Function,Nothing}=eigsortby) where T<:BlasComplex
    ishermitian(A) && isposdef(B) && return sorteig!(eigvals!(Hermitian(A), Hermitian(B)), sortby)
    alpha, beta, vl, vr = LAPACK.ggev!('N', 'N', A, B)
    return sorteig!(alpha./beta, sortby)
end

"""
    eigvals(A, B) -> values

Computes the generalized eigenvalues of `A` and `B`.

# Examples
```jldoctest
julia> A = [1 0; 0 -1]
2×2 Array{Int64,2}:
 1   0
 0  -1

julia> B = [0 1; 1 0]
2×2 Array{Int64,2}:
 0  1
 1  0

julia> eigvals(A,B)
2-element Array{Complex{Float64},1}:
 0.0 - 1.0im
 0.0 + 1.0im
```
"""
function eigvals(A::AbstractMatrix{TA}, B::AbstractMatrix{TB}; kws...) where {TA,TB}
    S = promote_type(eigtype(TA),TB)
    return eigvals!(copy_oftype(A, S), copy_oftype(B, S); kws...)
end

"""
    eigvecs(A, B) -> Matrix

Return a matrix `M` whose columns are the generalized eigenvectors of `A` and `B`. (The `k`th eigenvector can
be obtained from the slice `M[:, k]`.)

# Examples
```jldoctest
julia> A = [1 0; 0 -1]
2×2 Array{Int64,2}:
 1   0
 0  -1

julia> B = [0 1; 1 0]
2×2 Array{Int64,2}:
 0  1
 1  0

julia> eigvecs(A, B)
2×2 Array{Complex{Float64},2}:
  0.0+1.0im   0.0-1.0im
 -1.0+0.0im  -1.0-0.0im
```
"""
eigvecs(A::AbstractMatrix, B::AbstractMatrix; kws...) = eigvecs(eigen(A, B; kws...))

function show(io::IO, mime::MIME{Symbol("text/plain")}, F::Union{Eigen,GeneralizedEigen})
    summary(io, F); println(io)
    println(io, "values:")
    show(io, mime, F.values)
    println(io, "\nvectors:")
    show(io, mime, F.vectors)
end

# Conversion methods

## Can we determine the source/result is Real?  This is not stored in the type Eigen
AbstractMatrix(F::Eigen) = F.vectors * Diagonal(F.values) / F.vectors
AbstractArray(F::Eigen) = AbstractMatrix(F)
Matrix(F::Eigen) = Array(AbstractArray(F))
Array(F::Eigen) = Matrix(F)
