# This file is a part of Julia. License is MIT: https://julialang.org/license

#### Specialized matrix types ####

## (complex) symmetric tridiagonal matrices
struct SymTridiagonal{T,V<:AbstractVector{T}} <: AbstractMatrix{T}
    dv::V                        # diagonal
    ev::V                        # subdiagonal
    function SymTridiagonal{T}(dv::V, ev::V) where {T,V<:AbstractVector{T}}
        if !(length(dv) - 1 <= length(ev) <= length(dv))
            throw(DimensionMismatch("subdiagonal has wrong length. Has length $(length(ev)), but should be either $(length(dv) - 1) or $(length(dv))."))
        end
        new{T,V}(dv,ev)
    end
end

"""
    SymTridiagonal(dv::V, ev::V) where V <: AbstractVector

Construct a symmetric tridiagonal matrix from the diagonal (`dv`) and first
sub/super-diagonal (`ev`), respectively. The result is of type `SymTridiagonal`
and provides efficient specialized eigensolvers, but may be converted into a
regular matrix with [`convert(Array, _)`](@ref) (or `Array(_)` for short).

# Examples
```jldoctest
julia> dv = [1, 2, 3, 4]
4-element Array{Int64,1}:
 1
 2
 3
 4

julia> ev = [7, 8, 9]
3-element Array{Int64,1}:
 7
 8
 9

julia> SymTridiagonal(dv, ev)
4×4 SymTridiagonal{Int64,Array{Int64,1}}:
 1  7  ⋅  ⋅
 7  2  8  ⋅
 ⋅  8  3  9
 ⋅  ⋅  9  4
```
"""
SymTridiagonal(dv::V, ev::V) where {T,V<:AbstractVector{T}} = SymTridiagonal{T}(dv, ev)

"""
    SymTridiagonal(A::AbstractMatrix)

Construct a symmetric tridiagonal matrix from the diagonal and
first sub/super-diagonal, of the symmetric matrix `A`.

# Examples
```jldoctest
julia> A = [1 2 3; 2 4 5; 3 5 6]
3×3 Array{Int64,2}:
 1  2  3
 2  4  5
 3  5  6

julia> SymTridiagonal(A)
3×3 SymTridiagonal{Int64,Array{Int64,1}}:
 1  2  ⋅
 2  4  5
 ⋅  5  6
```
"""
function SymTridiagonal(A::AbstractMatrix)
    if diag(A,1) == diag(A,-1)
        SymTridiagonal(diag(A,0), diag(A,1))
    else
        throw(ArgumentError("matrix is not symmetric; cannot convert to SymTridiagonal"))
    end
end

SymTridiagonal{T}(S::SymTridiagonal) where {T} =
    SymTridiagonal(convert(AbstractVector{T}, S.dv), convert(AbstractVector{T}, S.ev))
AbstractMatrix{T}(S::SymTridiagonal) where {T} =
    SymTridiagonal(convert(AbstractVector{T}, S.dv), convert(AbstractVector{T}, S.ev))
function Matrix{T}(M::SymTridiagonal) where T
    n = size(M, 1)
    Mf = zeros(T, n, n)
    @inbounds begin
        @simd for i = 1:n-1
            Mf[i,i] = M.dv[i]
            Mf[i+1,i] = M.ev[i]
            Mf[i,i+1] = M.ev[i]
        end
        Mf[n,n] = M.dv[n]
    end
    return Mf
end
Matrix(M::SymTridiagonal{T}) where {T} = Matrix{T}(M)
Array(M::SymTridiagonal) = Matrix(M)

size(A::SymTridiagonal) = (length(A.dv), length(A.dv))
function size(A::SymTridiagonal, d::Integer)
    if d < 1
        throw(ArgumentError("dimension must be ≥ 1, got $d"))
    elseif d<=2
        return length(A.dv)
    else
        return 1
    end
end

# For S<:SymTridiagonal, similar(S[, neweltype]) should yield a SymTridiagonal matrix.
# On the other hand, similar(S, [neweltype,] shape...) should yield a sparse matrix.
# The first method below effects the former, and the second the latter.
similar(S::SymTridiagonal, ::Type{T}) where {T} = SymTridiagonal(similar(S.dv, T), similar(S.ev, T))
# The method below is moved to SparseArrays for now
# similar(S::SymTridiagonal, ::Type{T}, dims::Union{Dims{1},Dims{2}}) where {T} = spzeros(T, dims...)

#Elementary operations
broadcast(::typeof(abs), M::SymTridiagonal) = SymTridiagonal(abs.(M.dv), abs.(M.ev))
broadcast(::typeof(round), M::SymTridiagonal) = SymTridiagonal(round.(M.dv), round.(M.ev))
broadcast(::typeof(trunc), M::SymTridiagonal) = SymTridiagonal(trunc.(M.dv), trunc.(M.ev))
broadcast(::typeof(floor), M::SymTridiagonal) = SymTridiagonal(floor.(M.dv), floor.(M.ev))
broadcast(::typeof(ceil), M::SymTridiagonal) = SymTridiagonal(ceil.(M.dv), ceil.(M.ev))
for func in (:conj, :copy, :real, :imag)
    @eval ($func)(M::SymTridiagonal) = SymTridiagonal(($func)(M.dv), ($func)(M.ev))
end
broadcast(::typeof(round), ::Type{T}, M::SymTridiagonal) where {T<:Integer} = SymTridiagonal(round.(T, M.dv), round.(T, M.ev))
broadcast(::typeof(trunc), ::Type{T}, M::SymTridiagonal) where {T<:Integer} = SymTridiagonal(trunc.(T, M.dv), trunc.(T, M.ev))
broadcast(::typeof(floor), ::Type{T}, M::SymTridiagonal) where {T<:Integer} = SymTridiagonal(floor.(T, M.dv), floor.(T, M.ev))
broadcast(::typeof(ceil), ::Type{T}, M::SymTridiagonal) where {T<:Integer} = SymTridiagonal(ceil.(T, M.dv), ceil.(T, M.ev))

transpose(S::SymTridiagonal) = S
adjoint(S::SymTridiagonal{<:Real}) = S
adjoint(S::SymTridiagonal) = Adjoint(S)
Base.copy(S::Adjoint{<:Any,<:SymTridiagonal}) = SymTridiagonal(map(x -> copy.(adjoint.(x)), (S.parent.dv, S.parent.ev))...)
Base.copy(S::Transpose{<:Any,<:SymTridiagonal}) = SymTridiagonal(map(x -> copy.(transpose.(x)), (S.parent.dv, S.parent.ev))...)

function diag(M::SymTridiagonal, n::Integer=0)
    # every branch call similar(..., ::Int) to make sure the
    # same vector type is returned independent of n
    absn = abs(n)
    if absn == 0
        return copyto!(similar(M.dv, length(M.dv)), M.dv)
    elseif absn==1
        return copyto!(similar(M.ev, length(M.ev)), M.ev)
    elseif absn <= size(M,1)
        return fill!(similar(M.dv, size(M,1)-absn), 0)
    else
        throw(ArgumentError(string("requested diagonal, $n, must be at least $(-size(M, 1)) ",
            "and at most $(size(M, 2)) for an $(size(M, 1))-by-$(size(M, 2)) matrix")))
    end
end

+(A::SymTridiagonal, B::SymTridiagonal) = SymTridiagonal(A.dv+B.dv, A.ev+B.ev)
-(A::SymTridiagonal, B::SymTridiagonal) = SymTridiagonal(A.dv-B.dv, A.ev-B.ev)
*(A::SymTridiagonal, B::Number) = SymTridiagonal(A.dv*B, A.ev*B)
*(B::Number, A::SymTridiagonal) = A*B
/(A::SymTridiagonal, B::Number) = SymTridiagonal(A.dv/B, A.ev/B)
==(A::SymTridiagonal, B::SymTridiagonal) = (A.dv==B.dv) && (A.ev==B.ev)

function mul!(C::StridedVecOrMat, S::SymTridiagonal, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if !(m == size(S, 1) == size(C, 1))
        throw(DimensionMismatch("A has first dimension $(size(S,1)), B has $(size(B,1)), C has $(size(C,1)) but all must match"))
    end
    if n != size(C, 2)
        throw(DimensionMismatch("second dimension of B, $n, doesn't match second dimension of C, $(size(C,2))"))
    end

    α = S.dv
    β = S.ev
    @inbounds begin
        for j = 1:n
            x₀, x₊ = B[1, j], B[2, j]
            β₀ = β[1]
            C[1, j] = α[1]*x₀ + x₊*β₀
            for i = 2:m - 1
                x₋, x₀, x₊ = x₀, x₊, B[i + 1, j]
                β₋, β₀ = β₀, β[i]
                C[i, j] = β₋*x₋ + α[i]*x₀ + β₀*x₊
            end
            C[m, j] = β₀*x₀ + α[m]*x₊
        end
    end

    return C
end

(\)(T::SymTridiagonal, B::StridedVecOrMat) = ldltfact(T)\B

eigfact!(A::SymTridiagonal{<:BlasReal}) = Eigen(LAPACK.stegr!('V', A.dv, A.ev)...)
eigfact(A::SymTridiagonal{T}) where T = eigfact!(copy_oftype(A, eigtype(T)))

eigfact!(A::SymTridiagonal{<:BlasReal}, irange::UnitRange) =
    Eigen(LAPACK.stegr!('V', 'I', A.dv, A.ev, 0.0, 0.0, irange.start, irange.stop)...)
eigfact(A::SymTridiagonal{T}, irange::UnitRange) where T =
    eigfact!(copy_oftype(A, eigtype(T)), irange)

eigfact!(A::SymTridiagonal{<:BlasReal}, vl::Real, vu::Real) =
    Eigen(LAPACK.stegr!('V', 'V', A.dv, A.ev, vl, vu, 0, 0)...)
eigfact(A::SymTridiagonal{T}, vl::Real, vu::Real) where T =
    eigfact!(copy_oftype(A, eigtype(T)), vl, vu)

eigvals!(A::SymTridiagonal{<:BlasReal}) = LAPACK.stev!('N', A.dv, A.ev)[1]
eigvals(A::SymTridiagonal{T}) where T = eigvals!(copy_oftype(A, eigtype(T)))

eigvals!(A::SymTridiagonal{<:BlasReal}, irange::UnitRange) =
    LAPACK.stegr!('N', 'I', A.dv, A.ev, 0.0, 0.0, irange.start, irange.stop)[1]
eigvals(A::SymTridiagonal{T}, irange::UnitRange) where T =
    eigvals!(copy_oftype(A, eigtype(T)), irange)

eigvals!(A::SymTridiagonal{<:BlasReal}, vl::Real, vu::Real) =
    LAPACK.stegr!('N', 'V', A.dv, A.ev, vl, vu, 0, 0)[1]
eigvals(A::SymTridiagonal{T}, vl::Real, vu::Real) where T =
    eigvals!(copy_oftype(A, eigtype(T)), vl, vu)

#Computes largest and smallest eigenvalue
eigmax(A::SymTridiagonal) = eigvals(A, size(A, 1):size(A, 1))[1]
eigmin(A::SymTridiagonal) = eigvals(A, 1:1)[1]

#Compute selected eigenvectors only corresponding to particular eigenvalues
eigvecs(A::SymTridiagonal) = eigfact(A).vectors

"""
    eigvecs(A::SymTridiagonal[, eigvals]) -> Matrix

Return a matrix `M` whose columns are the eigenvectors of `A`. (The `k`th eigenvector can
be obtained from the slice `M[:, k]`.)

If the optional vector of eigenvalues `eigvals` is specified, `eigvecs`
returns the specific corresponding eigenvectors.

# Examples
```jldoctest
julia> A = SymTridiagonal([1.; 2.; 1.], [2.; 3.])
3×3 SymTridiagonal{Float64,Array{Float64,1}}:
 1.0  2.0   ⋅
 2.0  2.0  3.0
  ⋅   3.0  1.0

julia> eigvals(A)
3-element Array{Float64,1}:
 -2.1400549446402604
  1.0000000000000002
  5.140054944640259

julia> eigvecs(A)
3×3 Array{Float64,2}:
  0.418304  -0.83205      0.364299
 -0.656749  -7.39009e-16  0.754109
  0.627457   0.5547       0.546448

julia> eigvecs(A, [1.])
3×1 Array{Float64,2}:
  0.8320502943378438
  4.263514128092366e-17
 -0.5547001962252291
```
"""
eigvecs(A::SymTridiagonal{<:BlasFloat}, eigvals::Vector{<:Real}) = LAPACK.stein!(A.dv, A.ev, eigvals)

#tril and triu

istriu(M::SymTridiagonal) = iszero(M.ev)
istril(M::SymTridiagonal) = iszero(M.ev)

function tril!(M::SymTridiagonal, k::Integer=0)
    n = length(M.dv)
    if !(-n - 1 <= k <= n - 1)
        throw(ArgumentError(string("the requested diagonal, $k, must be at least ",
            "$(-n - 1) and at most $(n - 1) in an $n-by-$n matrix")))
    elseif k < -1
        fill!(M.ev,0)
        fill!(M.dv,0)
        return Tridiagonal(M.ev,M.dv,copy(M.ev))
    elseif k == -1
        fill!(M.dv,0)
        return Tridiagonal(M.ev,M.dv,zero(M.ev))
    elseif k == 0
        return Tridiagonal(M.ev,M.dv,zero(M.ev))
    elseif k >= 1
        return Tridiagonal(M.ev,M.dv,copy(M.ev))
    end
end

function triu!(M::SymTridiagonal, k::Integer=0)
    n = length(M.dv)
    if !(-n + 1 <= k <= n + 1)
        throw(ArgumentError(string("the requested diagonal, $k, must be at least ",
            "$(-n + 1) and at most $(n + 1) in an $n-by-$n matrix")))
    elseif k > 1
        fill!(M.ev,0)
        fill!(M.dv,0)
        return Tridiagonal(M.ev,M.dv,copy(M.ev))
    elseif k == 1
        fill!(M.dv,0)
        return Tridiagonal(zero(M.ev),M.dv,M.ev)
    elseif k == 0
        return Tridiagonal(zero(M.ev),M.dv,M.ev)
    elseif k <= -1
        return Tridiagonal(M.ev,M.dv,copy(M.ev))
    end
end

###################
# Generic methods #
###################

## structured matrix methods ##
function Base.replace_in_print_matrix(A::SymTridiagonal, i::Integer, j::Integer, s::AbstractString)
    i==j-1||i==j||i==j+1 ? s : Base.replace_with_centered_mark(s)
end

#Implements the inverse using the recurrence relation between principal minors
# a, b, c are assumed to be the subdiagonal, diagonal, and superdiagonal of
# a tridiagonal matrix.
#Reference:
#    R. Usmani, "Inversion of a tridiagonal Jacobi matrix",
#    Linear Algebra and its Applications 212-213 (1994), pp.413-414
#    doi:10.1016/0024-3795(94)90414-6
function inv_usmani(a::V, b::V, c::V) where {T,V<:AbstractVector{T}}
    n = length(b)
    θ = zeros(T, n+1) #principal minors of A
    θ[1] = 1
    n>=1 && (θ[2] = b[1])
    for i=2:n
        θ[i+1] = b[i]*θ[i]-a[i-1]*c[i-1]*θ[i-1]
    end
    φ = zeros(T, n+1)
    φ[n+1] = 1
    n>=1 && (φ[n] = b[n])
    for i=n-1:-1:1
        φ[i] = b[i]*φ[i+1]-a[i]*c[i]*φ[i+2]
    end
    α = Matrix{T}(undef, n, n)
    for i=1:n, j=1:n
        sign = (i+j)%2==0 ? (+) : (-)
        if i<j
            α[i,j]=(sign)(prod(c[i:j-1]))*θ[i]*φ[j+1]/θ[n+1]
        elseif i==j
            α[i,i]=                       θ[i]*φ[i+1]/θ[n+1]
        else #i>j
            α[i,j]=(sign)(prod(a[j:i-1]))*θ[j]*φ[i+1]/θ[n+1]
        end
    end
    α
end

#Implements the determinant using principal minors
#Inputs and reference are as above for inv_usmani()
function det_usmani(a::V, b::V, c::V) where {T,V<:AbstractVector{T}}
    n = length(b)
    θa = one(T)
    if n == 0
        return θa
    end
    θb = b[1]
    for i=2:n
        θb, θa = b[i]*θb-a[i-1]*c[i-1]*θa, θb
    end
    return θb
end

inv(A::SymTridiagonal) = inv_usmani(A.ev, A.dv, A.ev)
det(A::SymTridiagonal) = det_usmani(A.ev, A.dv, A.ev)

function getindex(A::SymTridiagonal{T}, i::Integer, j::Integer) where T
    if !(1 <= i <= size(A,2) && 1 <= j <= size(A,2))
        throw(BoundsError(A, (i,j)))
    end
    if i == j
        return A.dv[i]
    elseif i == j + 1
        return A.ev[j]
    elseif i + 1 == j
        return A.ev[i]
    else
        return zero(T)
    end
end

function setindex!(A::SymTridiagonal, x, i::Integer, j::Integer)
    @boundscheck checkbounds(A, i, j)
    if i == j
        @inbounds A.dv[i] = x
    else
        throw(ArgumentError("cannot set off-diagonal entry ($i, $j)"))
    end
    return x
end

## Tridiagonal matrices ##
struct Tridiagonal{T,V<:AbstractVector{T}} <: AbstractMatrix{T}
    dl::V    # sub-diagonal
    d::V     # diagonal
    du::V    # sup-diagonal
    du2::V   # supsup-diagonal for pivoting in LU
    function Tridiagonal{T}(dl::V, d::V, du::V) where {T,V<:AbstractVector{T}}
        n = length(d)
        if (length(dl) != n-1 || length(du) != n-1)
            throw(ArgumentError(string("cannot construct Tridiagonal from incompatible ",
                "lengths of subdiagonal, diagonal and superdiagonal: ",
                "($(length(dl)), $(length(d)), $(length(du)))")))
        end
        new{T,V}(dl, d, du)
    end
    # constructor used in lufact!
    function Tridiagonal{T,V}(dl::V, d::V, du::V, du2::V) where {T,V<:AbstractVector{T}}
        new{T,V}(dl, d, du, du2)
    end
end

"""
    Tridiagonal(dl::V, d::V, du::V) where V <: AbstractVector

Construct a tridiagonal matrix from the first subdiagonal, diagonal, and first superdiagonal,
respectively. The result is of type `Tridiagonal` and provides efficient specialized linear
solvers, but may be converted into a regular matrix with
[`convert(Array, _)`](@ref) (or `Array(_)` for short).
The lengths of `dl` and `du` must be one less than the length of `d`.

# Examples
```jldoctest
julia> dl = [1, 2, 3];

julia> du = [4, 5, 6];

julia> d = [7, 8, 9, 0];

julia> Tridiagonal(dl, d, du)
4×4 Tridiagonal{Int64,Array{Int64,1}}:
 7  4  ⋅  ⋅
 1  8  5  ⋅
 ⋅  2  9  6
 ⋅  ⋅  3  0
```
"""
Tridiagonal(dl::V, d::V, du::V) where {T,V<:AbstractVector{T}} = Tridiagonal{T}(dl, d, du)

"""
    Tridiagonal(A)

Construct a tridiagonal matrix from the first sub-diagonal,
diagonal and first super-diagonal of the matrix `A`.

# Examples
```jldoctest
julia> A = [1 2 3 4; 1 2 3 4; 1 2 3 4; 1 2 3 4]
4×4 Array{Int64,2}:
 1  2  3  4
 1  2  3  4
 1  2  3  4
 1  2  3  4

julia> Tridiagonal(A)
4×4 Tridiagonal{Int64,Array{Int64,1}}:
 1  2  ⋅  ⋅
 1  2  3  ⋅
 ⋅  2  3  4
 ⋅  ⋅  3  4
```
"""
Tridiagonal(A::AbstractMatrix) = Tridiagonal(diag(A,-1), diag(A,0), diag(A,1))

size(M::Tridiagonal) = (length(M.d), length(M.d))
function size(M::Tridiagonal, d::Integer)
    if d < 1
        throw(ArgumentError("dimension d must be ≥ 1, got $d"))
    elseif d <= 2
        return length(M.d)
    else
        return 1
    end
end

function Matrix{T}(M::Tridiagonal{T}) where T
    A = zeros(T, size(M))
    for i = 1:length(M.d)
        A[i,i] = M.d[i]
    end
    for i = 1:length(M.d)-1
        A[i+1,i] = M.dl[i]
        A[i,i+1] = M.du[i]
    end
    A
end
Matrix(M::Tridiagonal{T}) where {T} = Matrix{T}(M)
Array(M::Tridiagonal) = Matrix(M)

# For M<:Tridiagonal, similar(M[, neweltype]) should yield a Tridiagonal matrix.
# On the other hand, similar(M, [neweltype,] shape...) should yield a sparse matrix.
# The first method below effects the former, and the second the latter.
similar(M::Tridiagonal, ::Type{T}) where {T} = Tridiagonal(similar(M.dl, T), similar(M.d, T), similar(M.du, T))
# The method below is moved to SparseArrays for now
# similar(M::Tridiagonal, ::Type{T}, dims::Union{Dims{1},Dims{2}}) where {T} = spzeros(T, dims...)

# Operations on Tridiagonal matrices
copyto!(dest::Tridiagonal, src::Tridiagonal) = (copyto!(dest.dl, src.dl); copyto!(dest.d, src.d); copyto!(dest.du, src.du); dest)

#Elementary operations
broadcast(::typeof(abs), M::Tridiagonal) = Tridiagonal(abs.(M.dl), abs.(M.d), abs.(M.du))
broadcast(::typeof(round), M::Tridiagonal) = Tridiagonal(round.(M.dl), round.(M.d), round.(M.du))
broadcast(::typeof(trunc), M::Tridiagonal) = Tridiagonal(trunc.(M.dl), trunc.(M.d), trunc.(M.du))
broadcast(::typeof(floor), M::Tridiagonal) = Tridiagonal(floor.(M.dl), floor.(M.d), floor.(M.du))
broadcast(::typeof(ceil), M::Tridiagonal) = Tridiagonal(ceil.(M.dl), ceil.(M.d), ceil.(M.du))
for func in (:conj, :copy, :real, :imag)
    @eval function ($func)(M::Tridiagonal)
        Tridiagonal(($func)(M.dl), ($func)(M.d), ($func)(M.du))
    end
end
broadcast(::typeof(round), ::Type{T}, M::Tridiagonal) where {T<:Integer} =
    Tridiagonal(round.(T, M.dl), round.(T, M.d), round.(T, M.du))
broadcast(::typeof(trunc), ::Type{T}, M::Tridiagonal) where {T<:Integer} =
    Tridiagonal(trunc.(T, M.dl), trunc.(T, M.d), trunc.(T, M.du))
broadcast(::typeof(floor), ::Type{T}, M::Tridiagonal) where {T<:Integer} =
    Tridiagonal(floor.(T, M.dl), floor.(T, M.d), floor.(T, M.du))
broadcast(::typeof(ceil), ::Type{T}, M::Tridiagonal) where {T<:Integer} =
    Tridiagonal(ceil.(T, M.dl), ceil.(T, M.d), ceil.(T, M.du))

adjoint(S::Tridiagonal) = Adjoint(S)
transpose(S::Tridiagonal) = Transpose(S)
adjoint(S::Tridiagonal{<:Real}) = Tridiagonal(S.du, S.d, S.dl)
transpose(S::Tridiagonal{<:Number}) = Tridiagonal(S.du, S.d, S.dl)
Base.copy(aS::Adjoint{<:Any,<:Tridiagonal}) = (S = aS.parent; Tridiagonal(map(x -> copy.(adjoint.(x)), (S.du, S.d, S.dl))...))
Base.copy(tS::Transpose{<:Any,<:Tridiagonal}) = (S = tS.parent; Tridiagonal(map(x -> copy.(transpose.(x)), (S.du, S.d, S.dl))...))

\(A::Adjoint{<:Any,<:Tridiagonal}, B::Adjoint{<:Any,<:StridedVecOrMat}) = copy(A) \ copy(B)

function diag(M::Tridiagonal{T}, n::Integer=0) where T
    # every branch call similar(..., ::Int) to make sure the
    # same vector type is returned independent of n
    if n == 0
        return copyto!(similar(M.d, length(M.d)), M.d)
    elseif n == -1
        return copyto!(similar(M.dl, length(M.dl)), M.dl)
    elseif n == 1
        return copyto!(similar(M.du, length(M.du)), M.du)
    elseif abs(n) <= size(M,1)
        return fill!(similar(M.d, size(M,1)-abs(n)), 0)
    else
        throw(ArgumentError(string("requested diagonal, $n, must be at least $(-size(M, 1)) ",
            "and at most $(size(M, 2)) for an $(size(M, 1))-by-$(size(M, 2)) matrix")))
    end
end

function getindex(A::Tridiagonal{T}, i::Integer, j::Integer) where T
    if !(1 <= i <= size(A,2) && 1 <= j <= size(A,2))
        throw(BoundsError(A, (i,j)))
    end
    if i == j
        return A.d[i]
    elseif i == j + 1
        return A.dl[j]
    elseif i + 1 == j
        return A.du[i]
    else
        return zero(T)
    end
end

function setindex!(A::Tridiagonal, x, i::Integer, j::Integer)
    @boundscheck checkbounds(A, i, j)
    if i == j
        @inbounds A.d[i] = x
    elseif i - j == 1
        @inbounds A.dl[j] = x
    elseif j - i == 1
        @inbounds A.du[i] = x
    elseif !iszero(x)
        throw(ArgumentError(string("cannot set entry ($i, $j) off ",
            "the tridiagonal band to a nonzero value ($x)")))
    end
    return x
end

## structured matrix methods ##
function Base.replace_in_print_matrix(A::Tridiagonal,i::Integer,j::Integer,s::AbstractString)
    i==j-1||i==j||i==j+1 ? s : Base.replace_with_centered_mark(s)
end

#tril and triu

istriu(M::Tridiagonal) = iszero(M.dl)
istril(M::Tridiagonal) = iszero(M.du)

function tril!(M::Tridiagonal, k::Integer=0)
    n = length(M.d)
    if !(-n - 1 <= k <= n - 1)
        throw(ArgumentError(string("the requested diagonal, $k, must be at least ",
            "$(-n - 1) and at most $(n - 1) in an $n-by-$n matrix")))
    elseif k < -1
        fill!(M.dl,0)
        fill!(M.d,0)
        fill!(M.du,0)
    elseif k == -1
        fill!(M.d,0)
        fill!(M.du,0)
    elseif k == 0
        fill!(M.du,0)
    end
    return M
end

function triu!(M::Tridiagonal, k::Integer=0)
    n = length(M.d)
    if !(-n + 1 <= k <= n + 1)
        throw(ArgumentError(string("the requested diagonal, $k, must be at least ",
            "$(-n + 1) and at most $(n + 1) in an $n-by-$n matrix")))
    elseif k > 1
        fill!(M.dl,0)
        fill!(M.d,0)
        fill!(M.du,0)
    elseif k == 1
        fill!(M.dl,0)
        fill!(M.d,0)
    elseif k == 0
        fill!(M.dl,0)
    end
    return M
end

###################
# Generic methods #
###################

+(A::Tridiagonal, B::Tridiagonal) = Tridiagonal(A.dl+B.dl, A.d+B.d, A.du+B.du)
-(A::Tridiagonal, B::Tridiagonal) = Tridiagonal(A.dl-B.dl, A.d-B.d, A.du-B.du)
*(A::Tridiagonal, B::Number) = Tridiagonal(A.dl*B, A.d*B, A.du*B)
*(B::Number, A::Tridiagonal) = A*B
/(A::Tridiagonal, B::Number) = Tridiagonal(A.dl/B, A.d/B, A.du/B)

==(A::Tridiagonal, B::Tridiagonal) = (A.dl==B.dl) && (A.d==B.d) && (A.du==B.du)
==(A::Tridiagonal, B::SymTridiagonal) = (A.dl==A.du==B.ev) && (A.d==B.dv)
==(A::SymTridiagonal, B::Tridiagonal) = (B.dl==B.du==A.ev) && (B.d==A.dv)

inv(A::Tridiagonal) = inv_usmani(A.dl, A.d, A.du)
det(A::Tridiagonal) = det_usmani(A.dl, A.d, A.du)

Tridiagonal{T}(M::Tridiagonal) where {T} =
    Tridiagonal(convert(AbstractVector{T}, M.dl), convert(AbstractVector{T}, M.d), convert(AbstractVector{T}, M.du))
AbstractMatrix{T}(M::Tridiagonal) where {T} = Tridiagonal{T}(M)
Tridiagonal{T}(M::SymTridiagonal{T}) where {T} = Tridiagonal(M)
function SymTridiagonal{T}(M::Tridiagonal) where T
    if M.dl == M.du
        return SymTridiagonal{T}(convert(AbstractVector{T},M.d), convert(AbstractVector{T},M.dl))
    else
        throw(ArgumentError("Tridiagonal is not symmetric, cannot convert to SymTridiagonal"))
    end
end
