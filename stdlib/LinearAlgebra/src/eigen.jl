# This file is a part of Julia. License is MIT: https://julialang.org/license

# Eigendecomposition
struct Eigen{T,V,S<:AbstractMatrix,U<:AbstractVector} <: Factorization{T}
    values::U
    vectors::S
    Eigen{T,V,S,U}(values::AbstractVector{V}, vectors::AbstractMatrix{T}) where {T,V,S,U} =
        new(values, vectors)
end
Eigen(values::AbstractVector{V}, vectors::AbstractMatrix{T}) where {T,V} =
    Eigen{T,V,typeof(vectors),typeof(values)}(values, vectors)

# Generalized eigenvalue problem.
struct GeneralizedEigen{T,V,S<:AbstractMatrix,U<:AbstractVector} <: Factorization{T}
    values::U
    vectors::S
    GeneralizedEigen{T,V,S,U}(values::AbstractVector{V}, vectors::AbstractMatrix{T}) where {T,V,S,U} =
        new(values, vectors)
end
GeneralizedEigen(values::AbstractVector{V}, vectors::AbstractMatrix{T}) where {T,V} =
    GeneralizedEigen{T,V,typeof(vectors),typeof(values)}(values, vectors)

isposdef(A::Union{Eigen,GeneralizedEigen}) = isreal(A.values) && all(x -> x > 0, A.values)

"""
    eigfact!(A, [B])

Same as [`eigfact`](@ref), but saves space by overwriting the input `A` (and
`B`), instead of creating a copy.
"""
function eigfact!(A::StridedMatrix{T}; permute::Bool=true, scale::Bool=true) where T<:BlasReal
    n = size(A, 2)
    n == 0 && return Eigen(zeros(T, 0), zeros(T, 0, 0))
    issymmetric(A) && return eigfact!(Symmetric(A))
    A, WR, WI, VL, VR, _ = LAPACK.geevx!(permute ? (scale ? 'B' : 'P') : (scale ? 'S' : 'N'), 'N', 'V', 'N', A)
    iszero(WI) && return Eigen(WR, VR)
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
    return Eigen(complex.(WR, WI), evec)
end

function eigfact!(A::StridedMatrix{T}; permute::Bool=true, scale::Bool=true) where T<:BlasComplex
    n = size(A, 2)
    n == 0 && return Eigen(zeros(T, 0), zeros(T, 0, 0))
    ishermitian(A) && return eigfact!(Hermitian(A))
    return Eigen(LAPACK.geevx!(permute ? (scale ? 'B' : 'P') : (scale ? 'S' : 'N'), 'N', 'V', 'N', A)[[2,4]]...)
end

"""
    eigfact(A; permute::Bool=true, scale::Bool=true) -> Eigen

Computes the eigenvalue decomposition of `A`, returning an `Eigen` factorization object `F`
which contains the eigenvalues in `F.values` and the eigenvectors in the columns of the
matrix `F.vectors`. (The `k`th eigenvector can be obtained from the slice `F.vectors[:, k]`.)

The following functions are available for `Eigen` objects: [`inv`](@ref), [`det`](@ref), and [`isposdef`](@ref).

For general nonsymmetric matrices it is possible to specify how the matrix is balanced
before the eigenvector calculation. The option `permute=true` permutes the matrix to become
closer to upper triangular, and `scale=true` scales the matrix by its diagonal elements to
make rows and columns more equal in norm. The default is `true` for both options.

# Examples
```jldoctest
julia> F = eigfact([1.0 0.0 0.0; 0.0 3.0 0.0; 0.0 0.0 18.0])
Eigen{Float64,Float64,Array{Float64,2},Array{Float64,1}}
eigenvalues:
3-element Array{Float64,1}:
  1.0
  3.0
 18.0
eigenvectors:
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
```
"""
function eigfact(A::StridedMatrix{T}; permute::Bool=true, scale::Bool=true) where T
    AA = copy_oftype(A, eigtype(T))
    isdiag(AA) && return eigfact(Diagonal(AA), permute = permute, scale = scale)
    return eigfact!(AA, permute = permute, scale = scale)
end
eigfact(x::Number) = Eigen([x], fill(one(x), 1, 1))

function eig(A::Union{Number, StridedMatrix}; permute::Bool=true, scale::Bool=true)
    F = eigfact(A, permute=permute, scale=scale)
    F.values, F.vectors
end

"""
    eig(A::Union{SymTridiagonal, Hermitian, Symmetric}, irange::UnitRange) -> D, V
    eig(A::Union{SymTridiagonal, Hermitian, Symmetric}, vl::Real, vu::Real) -> D, V
    eig(A, permute::Bool=true, scale::Bool=true) -> D, V

Computes eigenvalues (`D`) and eigenvectors (`V`) of `A`.
See [`eigfact`](@ref) for details on the
`irange`, `vl`, and `vu` arguments
(for [`SymTridiagonal`](@ref), [`Hermitian`](@ref), and
[`Symmetric`](@ref) matrices)
and the `permute` and `scale` keyword arguments.
The eigenvectors are returned columnwise.

# Examples
```jldoctest
julia> eig([1.0 0.0 0.0; 0.0 3.0 0.0; 0.0 0.0 18.0])
([1.0, 3.0, 18.0], [1.0 0.0 0.0; 0.0 1.0 0.0; 0.0 0.0 1.0])
```

`eig` is a wrapper around [`eigfact`](@ref), extracting all parts of the
factorization to a tuple; where possible, using [`eigfact`](@ref) is recommended.
"""
function eig(A::AbstractMatrix, args...)
    F = eigfact(A, args...)
    F.values, F.vectors
end

"""
    eigvecs(A; permute::Bool=true, scale::Bool=true) -> Matrix

Return a matrix `M` whose columns are the eigenvectors of `A`. (The `k`th eigenvector can
be obtained from the slice `M[:, k]`.) The `permute` and `scale` keywords are the same as
for [`eigfact`](@ref).

# Examples
```jldoctest
julia> eigvecs([1.0 0.0 0.0; 0.0 3.0 0.0; 0.0 0.0 18.0])
3×3 Array{Float64,2}:
 1.0  0.0  0.0
 0.0  1.0  0.0
 0.0  0.0  1.0
```
"""
eigvecs(A::Union{Number, AbstractMatrix}; permute::Bool=true, scale::Bool=true) =
    eigvecs(eigfact(A, permute=permute, scale=scale))
eigvecs(F::Union{Eigen, GeneralizedEigen}) = F.vectors

eigvals(F::Union{Eigen, GeneralizedEigen}) = F.values

"""
    eigvals!(A; permute::Bool=true, scale::Bool=true) -> values

Same as [`eigvals`](@ref), but saves space by overwriting the input `A`, instead of creating a copy.
The option `permute=true` permutes the matrix to become
closer to upper triangular, and `scale=true` scales the matrix by its diagonal elements to
make rows and columns more equal in norm.

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
function eigvals!(A::StridedMatrix{<:BlasReal}; permute::Bool=true, scale::Bool=true)
    issymmetric(A) && return eigvals!(Symmetric(A))
    _, valsre, valsim, _ = LAPACK.geevx!(permute ? (scale ? 'B' : 'P') : (scale ? 'S' : 'N'), 'N', 'N', 'N', A)
    return iszero(valsim) ? valsre : complex.(valsre, valsim)
end
function eigvals!(A::StridedMatrix{<:BlasComplex}; permute::Bool=true, scale::Bool=true)
    ishermitian(A) && return eigvals(Hermitian(A))
    return LAPACK.geevx!(permute ? (scale ? 'B' : 'P') : (scale ? 'S' : 'N'), 'N', 'N', 'N', A)[2]
end

# promotion type to use for eigenvalues of a Matrix{T}
eigtype(T) = promote_type(Float32, typeof(zero(T)/sqrt(abs2(one(T)))))

"""
    eigvals(A; permute::Bool=true, scale::Bool=true) -> values

Return the eigenvalues of `A`.

For general non-symmetric matrices it is possible to specify how the matrix is balanced
before the eigenvalue calculation. The option `permute=true` permutes the matrix to
become closer to upper triangular, and `scale=true` scales the matrix by its diagonal
elements to make rows and columns more equal in norm. The default is `true` for both
options.

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
eigvals(A::StridedMatrix{T}; permute::Bool=true, scale::Bool=true) where T =
    eigvals!(copy_oftype(A, eigtype(T)), permute = permute, scale = scale)
function eigvals(x::T; kwargs...) where T<:Number
    val = convert(eigtype(T), x)
    return imag(val) == 0 ? [real(val)] : [val]
end

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
function eigmax(A::Union{Number, StridedMatrix}; permute::Bool=true, scale::Bool=true)
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
function eigmin(A::Union{Number, StridedMatrix}; permute::Bool=true, scale::Bool=true)
    v = eigvals(A, permute = permute, scale = scale)
    if eltype(v)<:Complex
        throw(DomainError(A, "`A` cannot have complex eigenvalues."))
    end
    minimum(v)
end

inv(A::Eigen) = A.vectors * inv(Diagonal(A.values)) / A.vectors
det(A::Eigen) = prod(A.values)

# Generalized eigenproblem
function eigfact!(A::StridedMatrix{T}, B::StridedMatrix{T}) where T<:BlasReal
    issymmetric(A) && isposdef(B) && return eigfact!(Symmetric(A), Symmetric(B))
    n = size(A, 1)
    alphar, alphai, beta, _, vr = LAPACK.ggev!('N', 'V', A, B)
    iszero(alphai) && return GeneralizedEigen(alphar ./ beta, vr)

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
    return GeneralizedEigen(complex.(alphar, alphai)./beta, vecs)
end

function eigfact!(A::StridedMatrix{T}, B::StridedMatrix{T}) where T<:BlasComplex
    ishermitian(A) && isposdef(B) && return eigfact!(Hermitian(A), Hermitian(B))
    alpha, beta, _, vr = LAPACK.ggev!('N', 'V', A, B)
    return GeneralizedEigen(alpha./beta, vr)
end

"""
    eigfact(A, B) -> GeneralizedEigen

Computes the generalized eigenvalue decomposition of `A` and `B`, returning a
`GeneralizedEigen` factorization object `F` which contains the generalized eigenvalues in
`F.values` and the generalized eigenvectors in the columns of the matrix `F.vectors`.
(The `k`th generalized eigenvector can be obtained from the slice `F.vectors[:, k]`.)

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

julia> F = eigfact(A, B);

julia> F.values
2-element Array{Complex{Float64},1}:
 0.0 + 1.0im
 0.0 - 1.0im

julia> F.vectors
2×2 Array{Complex{Float64},2}:
  0.0-1.0im   0.0+1.0im
 -1.0-0.0im  -1.0+0.0im
```
"""
function eigfact(A::AbstractMatrix{TA}, B::AbstractMatrix{TB}) where {TA,TB}
    S = promote_type(eigtype(TA),TB)
    return eigfact!(copy_oftype(A, S), copy_oftype(B, S))
end

eigfact(A::Number, B::Number) = eigfact(fill(A,1,1), fill(B,1,1))

"""
    eig(A, B) -> D, V

Computes generalized eigenvalues (`D`) and vectors (`V`) of `A` with respect to `B`.

`eig` is a wrapper around [`eigfact`](@ref), extracting all parts of the
factorization to a tuple; where possible, using [`eigfact`](@ref) is recommended.

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

julia> eig(A, B)
(Complex{Float64}[0.0+1.0im, 0.0-1.0im], Complex{Float64}[0.0-1.0im 0.0+1.0im; -1.0-0.0im -1.0+0.0im])
```
"""
function eig(A::AbstractMatrix, B::AbstractMatrix)
    F = eigfact(A,B)
    F.values, F.vectors
end
function eig(A::Number, B::Number)
    F = eigfact(A,B)
    F.values, F.vectors
end

"""
    eigvals!(A, B) -> values

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
 0.0 + 1.0im
 0.0 - 1.0im

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
function eigvals!(A::StridedMatrix{T}, B::StridedMatrix{T}) where T<:BlasReal
    issymmetric(A) && isposdef(B) && return eigvals!(Symmetric(A), Symmetric(B))
    alphar, alphai, beta, vl, vr = LAPACK.ggev!('N', 'N', A, B)
    return (iszero(alphai) ? alphar : complex.(alphar, alphai))./beta
end
function eigvals!(A::StridedMatrix{T}, B::StridedMatrix{T}) where T<:BlasComplex
    ishermitian(A) && isposdef(B) && return eigvals!(Hermitian(A), Hermitian(B))
    alpha, beta, vl, vr = LAPACK.ggev!('N', 'N', A, B)
    alpha./beta
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
 0.0 + 1.0im
 0.0 - 1.0im
```
"""
function eigvals(A::AbstractMatrix{TA}, B::AbstractMatrix{TB}) where {TA,TB}
    S = promote_type(eigtype(TA),TB)
    return eigvals!(copy_oftype(A, S), copy_oftype(B, S))
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
  0.0-1.0im   0.0+1.0im
 -1.0-0.0im  -1.0+0.0im
```
"""
eigvecs(A::AbstractMatrix, B::AbstractMatrix) = eigvecs(eigfact(A, B))

function show(io::IO, mime::MIME{Symbol("text/plain")}, F::Union{Eigen,GeneralizedEigen})
    println(io, summary(F))
    println(io, "eigenvalues:")
    show(io, mime, F.values)
    println(io, "\neigenvectors:")
    show(io, mime, F.vectors)
end

# Conversion methods

## Can we determine the source/result is Real?  This is not stored in the type Eigen
AbstractMatrix(F::Eigen) = F.vectors * Diagonal(F.values) / F.vectors
AbstractArray(F::Eigen) = AbstractMatrix(F)
Matrix(F::Eigen) = Array(AbstractArray(F))
Array(F::Eigen) = Matrix(F)
