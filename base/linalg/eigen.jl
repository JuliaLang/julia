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


function getindex(A::Union{Eigen,GeneralizedEigen}, d::Symbol)
    d == :values && return A.values
    d == :vectors && return A.vectors
    throw(KeyError(d))
end

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
which contains the eigenvalues in `F[:values]` and the eigenvectors in the columns of the
matrix `F[:vectors]`. (The `k`th eigenvector can be obtained from the slice `F[:vectors][:, k]`.)

The following functions are available for `Eigen` objects: [`inv`](@ref), [`det`](@ref), and [`isposdef`](@ref).

For general nonsymmetric matrices it is possible to specify how the matrix is balanced
before the eigenvector calculation. The option `permute=true` permutes the matrix to become
closer to upper triangular, and `scale=true` scales the matrix by its diagonal elements to
make rows and columns more equal in norm. The default is `true` for both options.

# Example

```jldoctest
julia> F = eigfact([1.0 0.0 0.0; 0.0 3.0 0.0; 0.0 0.0 18.0])
Base.LinAlg.Eigen{Float64,Float64,Array{Float64,2},Array{Float64,1}}([1.0, 3.0, 18.0], [1.0 0.0 0.0; 0.0 1.0 0.0; 0.0 0.0 1.0])

julia> F[:values]
3-element Array{Float64,1}:
  1.0
  3.0
 18.0

julia> F[:vectors]
3×3 Array{Float64,2}:
 1.0  0.0  0.0
 0.0  1.0  0.0
 0.0  0.0  1.0
```
"""
function eigfact(A::StridedMatrix{T}; permute::Bool=true, scale::Bool=true) where T
    S = promote_type(Float32, typeof(one(T)/norm(one(T))))
    eigfact!(copy_oftype(A, S), permute = permute, scale = scale)
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
(for [`SymTridiagonal`](@ref), `Hermitian`, and
`Symmetric` matrices)
and the `permute` and `scale` keyword arguments.
The eigenvectors are returned columnwise.

# Example

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

Returns a matrix `M` whose columns are the eigenvectors of `A`. (The `k`th eigenvector can
be obtained from the slice `M[:, k]`.) The `permute` and `scale` keywords are the same as
for [`eigfact`](@ref).

# Example

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
eigvecs(F::Union{Eigen{T,V,S,U}, GeneralizedEigen{T,V,S,U}}) where {T,V,S,U} = F[:vectors]::S

eigvals(F::Union{Eigen{T,V,S,U}, GeneralizedEigen{T,V,S,U}}) where {T,V,S,U} = F[:values]::U

"""
    eigvals!(A; permute::Bool=true, scale::Bool=true) -> values

Same as [`eigvals`](@ref), but saves space by overwriting the input `A`, instead of creating a copy.
The option `permute=true` permutes the matrix to become
closer to upper triangular, and `scale=true` scales the matrix by its diagonal elements to
make rows and columns more equal in norm.
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

"""
    eigvals(A; permute::Bool=true, scale::Bool=true) -> values

Returns the eigenvalues of `A`.

For general non-symmetric matrices it is possible to specify how the matrix is balanced
before the eigenvalue calculation. The option `permute=true` permutes the matrix to
become closer to upper triangular, and `scale=true` scales the matrix by its diagonal
elements to make rows and columns more equal in norm. The default is `true` for both
options.
"""
function eigvals(A::StridedMatrix{T}; permute::Bool=true, scale::Bool=true) where T
    S = promote_type(Float32, typeof(one(T)/norm(one(T))))
    return eigvals!(copy_oftype(A, S), permute = permute, scale = scale)
end
function eigvals(x::T; kwargs...) where T<:Number
    val = convert(promote_type(Float32, typeof(one(T)/norm(one(T)))), x)
    return imag(val) == 0 ? [real(val)] : [val]
end

"""
    eigmax(A; permute::Bool=true, scale::Bool=true)

Returns the largest eigenvalue of `A`.
The option `permute=true` permutes the matrix to become
closer to upper triangular, and `scale=true` scales the matrix by its diagonal elements to
make rows and columns more equal in norm.
Note that if the eigenvalues of `A` are complex,
this method will fail, since complex numbers cannot
be sorted.

# Example

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
ERROR: DomainError:
Stacktrace:
 [1] #eigmax#46(::Bool, ::Bool, ::Function, ::Array{Complex{Int64},2}) at ./linalg/eigen.jl:238
 [2] eigmax(::Array{Complex{Int64},2}) at ./linalg/eigen.jl:236
```
"""
function eigmax(A::Union{Number, StridedMatrix}; permute::Bool=true, scale::Bool=true)
    v = eigvals(A, permute = permute, scale = scale)
    if eltype(v)<:Complex
        throw(DomainError())
    end
    maximum(v)
end

"""
    eigmin(A; permute::Bool=true, scale::Bool=true)

Returns the smallest eigenvalue of `A`.
The option `permute=true` permutes the matrix to become
closer to upper triangular, and `scale=true` scales the matrix by its diagonal elements to
make rows and columns more equal in norm.
Note that if the eigenvalues of `A` are complex,
this method will fail, since complex numbers cannot
be sorted.

# Example

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
ERROR: DomainError:
Stacktrace:
 [1] #eigmin#47(::Bool, ::Bool, ::Function, ::Array{Complex{Int64},2}) at ./linalg/eigen.jl:280
 [2] eigmin(::Array{Complex{Int64},2}) at ./linalg/eigen.jl:278
```
"""
function eigmin(A::Union{Number, StridedMatrix}; permute::Bool=true, scale::Bool=true)
    v = eigvals(A, permute = permute, scale = scale)
    if eltype(v)<:Complex
        throw(DomainError())
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
`F[:values]` and the generalized eigenvectors in the columns of the matrix `F[:vectors]`.
(The `k`th generalized eigenvector can be obtained from the slice `F[:vectors][:, k]`.)
"""
function eigfact(A::AbstractMatrix{TA}, B::AbstractMatrix{TB}) where {TA,TB}
    S = promote_type(Float32, typeof(one(TA)/norm(one(TA))),TB)
    return eigfact!(copy_oftype(A, S), copy_oftype(B, S))
end

eigfact(A::Number, B::Number) = eigfact(fill(A,1,1), fill(B,1,1))

"""
    eig(A, B) -> D, V

Computes generalized eigenvalues (`D`) and vectors (`V`) of `A` with respect to `B`.

`eig` is a wrapper around [`eigfact`](@ref), extracting all parts of the
factorization to a tuple; where possible, using [`eigfact`](@ref) is recommended.

# Example

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

Same as [`eigvals`](@ref), but saves space by overwriting the input `A` (and `B`), instead of creating copies.
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

# Example

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
 0.0+1.0im
 0.0-1.0im
```
"""
function eigvals(A::AbstractMatrix{TA}, B::AbstractMatrix{TB}) where {TA,TB}
    S = promote_type(Float32, typeof(one(TA)/norm(one(TA))),TB)
    return eigvals!(copy_oftype(A, S), copy_oftype(B, S))
end

"""
    eigvecs(A, B) -> Matrix

Returns a matrix `M` whose columns are the generalized eigenvectors of `A` and `B`. (The `k`th eigenvector can
be obtained from the slice `M[:, k]`.)

# Example

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

# Conversion methods

## Can we determine the source/result is Real?  This is not stored in the type Eigen
convert(::Type{AbstractMatrix}, F::Eigen) = F.vectors * Diagonal(F.values) / F.vectors
convert(::Type{AbstractArray}, F::Eigen) = convert(AbstractMatrix, F)
convert(::Type{Matrix}, F::Eigen) = convert(Array, convert(AbstractArray, F))
convert(::Type{Array}, F::Eigen) = convert(Matrix, F)
full(F::Eigen) = convert(AbstractArray, F)
