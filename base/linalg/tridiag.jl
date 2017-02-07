# This file is a part of Julia. License is MIT: http://julialang.org/license

#### Specialized matrix types ####

## (complex) symmetric tridiagonal matrices
immutable SymTridiagonal{T} <: AbstractMatrix{T}
    dv::Vector{T}                        # diagonal
    ev::Vector{T}                        # subdiagonal
    function SymTridiagonal{T}(dv::Vector{T}, ev::Vector{T}) where T
        if !(length(dv) - 1 <= length(ev) <= length(dv))
            throw(DimensionMismatch("subdiagonal has wrong length. Has length $(length(ev)), but should be either $(length(dv) - 1) or $(length(dv))."))
        end
        new(dv,ev)
    end
end

"""
    SymTridiagonal(dv, ev)

Construct a symmetric tridiagonal matrix from the diagonal and first sub/super-diagonal,
respectively. The result is of type `SymTridiagonal` and provides efficient specialized
eigensolvers, but may be converted into a regular matrix with
[`convert(Array, _)`](@ref) (or `Array(_)` for short).

# Example

```jldoctest
julia> dv = [1; 2; 3; 4]
4-element Array{Int64,1}:
 1
 2
 3
 4

julia> ev = [7; 8; 9]
3-element Array{Int64,1}:
 7
 8
 9

julia> SymTridiagonal(dv, ev)
4×4 SymTridiagonal{Int64}:
 1  7  ⋅  ⋅
 7  2  8  ⋅
 ⋅  8  3  9
 ⋅  ⋅  9  4
```
"""
SymTridiagonal(dv::Vector{T}, ev::Vector{T}) where T = SymTridiagonal{T}(dv, ev)

function SymTridiagonal(dv::AbstractVector{Td}, ev::AbstractVector{Te}) where {Td,Te}
    T = promote_type(Td,Te)
    SymTridiagonal(convert(Vector{T}, dv), convert(Vector{T}, ev))
end

function SymTridiagonal(A::AbstractMatrix)
    if diag(A,1) == diag(A,-1)
        SymTridiagonal(diag(A), diag(A,1))
    else
        throw(ArgumentError("matrix is not symmetric; cannot convert to SymTridiagonal"))
    end
end

convert{T}(::Type{SymTridiagonal{T}}, S::SymTridiagonal) =
    SymTridiagonal(convert(Vector{T}, S.dv), convert(Vector{T}, S.ev))
convert{T}(::Type{AbstractMatrix{T}}, S::SymTridiagonal) =
    SymTridiagonal(convert(Vector{T}, S.dv), convert(Vector{T}, S.ev))
function convert{T}(::Type{Matrix{T}}, M::SymTridiagonal{T})
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
convert{T}(::Type{Matrix}, M::SymTridiagonal{T}) = convert(Matrix{T}, M)
convert(::Type{Array}, M::SymTridiagonal) = convert(Matrix, M)
full(M::SymTridiagonal) = convert(Array, M)

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

similar{T}(S::SymTridiagonal, ::Type{T}) = SymTridiagonal{T}(similar(S.dv, T), similar(S.ev, T))

#Elementary operations
broadcast(::typeof(abs), M::SymTridiagonal) = SymTridiagonal(abs.(M.dv), abs.(M.ev))
broadcast(::typeof(round), M::SymTridiagonal) = SymTridiagonal(round.(M.dv), round.(M.ev))
broadcast(::typeof(trunc), M::SymTridiagonal) = SymTridiagonal(trunc.(M.dv), trunc.(M.ev))
broadcast(::typeof(floor), M::SymTridiagonal) = SymTridiagonal(floor.(M.dv), floor.(M.ev))
broadcast(::typeof(ceil), M::SymTridiagonal) = SymTridiagonal(ceil.(M.dv), ceil.(M.ev))
for func in (:conj, :copy, :real, :imag)
    @eval ($func)(M::SymTridiagonal) = SymTridiagonal(($func)(M.dv), ($func)(M.ev))
end
broadcast{T<:Integer}(::typeof(round), ::Type{T}, M::SymTridiagonal) = SymTridiagonal(round.(T, M.dv), round.(T, M.ev))
broadcast{T<:Integer}(::typeof(trunc), ::Type{T}, M::SymTridiagonal) = SymTridiagonal(trunc.(T, M.dv), trunc.(T, M.ev))
broadcast{T<:Integer}(::typeof(floor), ::Type{T}, M::SymTridiagonal) = SymTridiagonal(floor.(T, M.dv), floor.(T, M.ev))
broadcast{T<:Integer}(::typeof(ceil), ::Type{T}, M::SymTridiagonal) = SymTridiagonal(ceil.(T, M.dv), ceil.(T, M.ev))

transpose(M::SymTridiagonal) = M #Identity operation
ctranspose(M::SymTridiagonal) = conj(M)

function diag{T}(M::SymTridiagonal{T}, n::Integer=0)
    absn = abs(n)
    if absn == 0
        return M.dv
    elseif absn==1
        return M.ev
    elseif absn<size(M,1)
        return zeros(T,size(M,1)-absn)
    else
        throw(ArgumentError("$n-th diagonal of a $(size(M)) matrix doesn't exist!"))
    end
end

+(A::SymTridiagonal, B::SymTridiagonal) = SymTridiagonal(A.dv+B.dv, A.ev+B.ev)
-(A::SymTridiagonal, B::SymTridiagonal) = SymTridiagonal(A.dv-B.dv, A.ev-B.ev)
*(A::SymTridiagonal, B::Number) = SymTridiagonal(A.dv*B, A.ev*B)
*(B::Number, A::SymTridiagonal) = A*B
/(A::SymTridiagonal, B::Number) = SymTridiagonal(A.dv/B, A.ev/B)
==(A::SymTridiagonal, B::SymTridiagonal) = (A.dv==B.dv) && (A.ev==B.ev)

function A_mul_B!(C::StridedVecOrMat, S::SymTridiagonal, B::StridedVecOrMat)
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

eigfact!{T<:BlasReal}(A::SymTridiagonal{T}) = Eigen(LAPACK.stegr!('V', A.dv, A.ev)...)
function eigfact{T}(A::SymTridiagonal{T})
    S = promote_type(Float32, typeof(zero(T)/norm(one(T))))
    eigfact!(copy_oftype(A, S))
end

eigfact!{T<:BlasReal}(A::SymTridiagonal{T}, irange::UnitRange) =
    Eigen(LAPACK.stegr!('V', 'I', A.dv, A.ev, 0.0, 0.0, irange.start, irange.stop)...)
function eigfact{T}(A::SymTridiagonal{T}, irange::UnitRange)
    S = promote_type(Float32, typeof(zero(T)/norm(one(T))))
    return eigfact!(copy_oftype(A, S), irange)
end

eigfact!{T<:BlasReal}(A::SymTridiagonal{T}, vl::Real, vu::Real) =
    Eigen(LAPACK.stegr!('V', 'V', A.dv, A.ev, vl, vu, 0, 0)...)
function eigfact{T}(A::SymTridiagonal{T}, vl::Real, vu::Real)
    S = promote_type(Float32, typeof(zero(T)/norm(one(T))))
    return eigfact!(copy_oftype(A, S), vl, vu)
end

eigvals!{T<:BlasReal}(A::SymTridiagonal{T}) = LAPACK.stev!('N', A.dv, A.ev)[1]
function eigvals{T}(A::SymTridiagonal{T})
    S = promote_type(Float32, typeof(zero(T)/norm(one(T))))
    return eigvals!(copy_oftype(A, S))
end

eigvals!{T<:BlasReal}(A::SymTridiagonal{T}, irange::UnitRange) =
    LAPACK.stegr!('N', 'I', A.dv, A.ev, 0.0, 0.0, irange.start, irange.stop)[1]
function eigvals{T}(A::SymTridiagonal{T}, irange::UnitRange)
    S = promote_type(Float32, typeof(zero(T)/norm(one(T))))
    return eigvals!(copy_oftype(A, S), irange)
end

eigvals!{T<:BlasReal}(A::SymTridiagonal{T}, vl::Real, vu::Real) =
    LAPACK.stegr!('N', 'V', A.dv, A.ev, vl, vu, 0, 0)[1]
function eigvals{T}(A::SymTridiagonal{T}, vl::Real, vu::Real)
    S = promote_type(Float32, typeof(zero(T)/norm(one(T))))
    return eigvals!(copy_oftype(A, S), vl, vu)
end

#Computes largest and smallest eigenvalue
eigmax(A::SymTridiagonal) = eigvals(A, size(A, 1):size(A, 1))[1]
eigmin(A::SymTridiagonal) = eigvals(A, 1:1)[1]

#Compute selected eigenvectors only corresponding to particular eigenvalues
eigvecs(A::SymTridiagonal) = eigfact(A)[:vectors]

"""
    eigvecs(A::SymTridiagonal[, eigvals]) -> Matrix

Returns a matrix `M` whose columns are the eigenvectors of `A`. (The `k`th eigenvector can
be obtained from the slice `M[:, k]`.)

If the optional vector of eigenvalues `eigvals` is specified, `eigvecs`
returns the specific corresponding eigenvectors.

# Example

```jldoctest
julia> A = SymTridiagonal([1.; 2.; 1.], [2.; 3.])
3×3 SymTridiagonal{Float64}:
 1.0  2.0   ⋅
 2.0  2.0  3.0
  ⋅   3.0  1.0

julia> eigvals(A)
3-element Array{Float64,1}:
 -2.14005
  1.0
  5.14005

julia> eigvecs(A)
3×3 Array{Float64,2}:
  0.418304  -0.83205      0.364299
 -0.656749  -7.39009e-16  0.754109
  0.627457   0.5547       0.546448

julia> eigvecs(A, [1.])
3×1 Array{Float64,2}:
  0.83205
  4.26351e-17
 -0.5547
```
"""
eigvecs{T<:BlasFloat,Eigenvalue<:Real}(A::SymTridiagonal{T}, eigvals::Vector{Eigenvalue}) = LAPACK.stein!(A.dv, A.ev, eigvals)

#tril and triu

istriu(M::SymTridiagonal) = iszero(M.ev)
istril(M::SymTridiagonal) = iszero(M.ev)

function tril!(M::SymTridiagonal, k::Integer=0)
    n = length(M.dv)
    if abs(k) > n
        throw(ArgumentError("requested diagonal, $k, out of bounds in matrix of size ($n,$n)"))
    elseif k < -1
        fill!(M.ev,0)
        fill!(M.dv,0)
        return Tridiagonal(M.ev,M.dv,copy(M.ev))
    elseif k == -1
        fill!(M.dv,0)
        return Tridiagonal(M.ev,M.dv,zeros(M.ev))
    elseif k == 0
        return Tridiagonal(M.ev,M.dv,zeros(M.ev))
    elseif k >= 1
        return Tridiagonal(M.ev,M.dv,copy(M.ev))
    end
end

function triu!(M::SymTridiagonal, k::Integer=0)
    n = length(M.dv)
    if abs(k) > n
        throw(ArgumentError("requested diagonal, $k, out of bounds in matrix of size ($n,$n)"))
    elseif k > 1
        fill!(M.ev,0)
        fill!(M.dv,0)
        return Tridiagonal(M.ev,M.dv,copy(M.ev))
    elseif k == 1
        fill!(M.dv,0)
        return Tridiagonal(zeros(M.ev),M.dv,M.ev)
    elseif k == 0
        return Tridiagonal(zeros(M.ev),M.dv,M.ev)
    elseif k <= -1
        return Tridiagonal(M.ev,M.dv,copy(M.ev))
    end
end

###################
# Generic methods #
###################

#Needed for inv_usmani()
type ZeroOffsetVector
    data::Vector
end
getindex( a::ZeroOffsetVector, i) = a.data[i+1]
setindex!(a::ZeroOffsetVector, x, i) = a.data[i+1]=x


## structured matrix methods ##
function Base.replace_in_print_matrix(A::SymTridiagonal,i::Integer,j::Integer,s::AbstractString)
    i==j-1||i==j||i==j+1 ? s : Base.replace_with_centered_mark(s)
end

#Implements the inverse using the recurrence relation between principal minors
# a, b, c are assumed to be the subdiagonal, diagonal, and superdiagonal of
# a tridiagonal matrix.
#Reference:
#    R. Usmani, "Inversion of a tridiagonal Jacobi matrix",
#    Linear Algebra and its Applications 212-213 (1994), pp.413-414
#    doi:10.1016/0024-3795(94)90414-6
function inv_usmani{T}(a::Vector{T}, b::Vector{T}, c::Vector{T})
    n = length(b)
    θ = ZeroOffsetVector(zeros(T, n+1)) #principal minors of A
    θ[0] = 1
    n>=1 && (θ[1] = b[1])
    for i=2:n
        θ[i] = b[i]*θ[i-1]-a[i-1]*c[i-1]*θ[i-2]
    end
    φ = zeros(T, n+1)
    φ[n+1] = 1
    n>=1 && (φ[n] = b[n])
    for i=n-1:-1:1
        φ[i] = b[i]*φ[i+1]-a[i]*c[i]*φ[i+2]
    end
    α = Array{T}(n, n)
    for i=1:n, j=1:n
        sign = (i+j)%2==0 ? (+) : (-)
        if i<j
            α[i,j]=(sign)(prod(c[i:j-1]))*θ[i-1]*φ[j+1]/θ[n]
        elseif i==j
            α[i,i]=                       θ[i-1]*φ[i+1]/θ[n]
        else #i>j
            α[i,j]=(sign)(prod(a[j:i-1]))*θ[j-1]*φ[i+1]/θ[n]
        end
    end
    α
end

#Implements the determinant using principal minors
#Inputs and reference are as above for inv_usmani()
function det_usmani{T}(a::Vector{T}, b::Vector{T}, c::Vector{T})
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

function getindex{T}(A::SymTridiagonal{T}, i::Integer, j::Integer)
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
    if i == j
        A.dv[i] = x
    elseif abs(i - j) == 1
        A.ev[min(i,j)] = x
    else
        throw(ArgumentError("cannot set elements outside the sub, main, or super diagonals"))
    end
end

## Tridiagonal matrices ##
immutable Tridiagonal{T} <: AbstractMatrix{T}
    dl::Vector{T}    # sub-diagonal
    d::Vector{T}     # diagonal
    du::Vector{T}    # sup-diagonal
    du2::Vector{T}   # supsup-diagonal for pivoting
end

"""
    Tridiagonal(dl, d, du)

Construct a tridiagonal matrix from the first subdiagonal, diagonal, and first superdiagonal,
respectively.  The result is of type `Tridiagonal` and provides efficient specialized linear
solvers, but may be converted into a regular matrix with
[`convert(Array, _)`](@ref) (or `Array(_)` for short).
The lengths of `dl` and `du` must be one less than the length of `d`.

# Example

```jldoctest
julia> dl = [1; 2; 3]
3-element Array{Int64,1}:
 1
 2
 3

julia> du = [4; 5; 6]
3-element Array{Int64,1}:
 4
 5
 6

julia> d = [7; 8; 9; 0]
4-element Array{Int64,1}:
 7
 8
 9
 0

julia> Tridiagonal(dl, d, du)
4×4 Tridiagonal{Int64}:
 7  4  ⋅  ⋅
 1  8  5  ⋅
 ⋅  2  9  6
 ⋅  ⋅  3  0
```
"""
# Basic constructor takes in three dense vectors of same type
function Tridiagonal{T}(dl::Vector{T}, d::Vector{T}, du::Vector{T})
    n = length(d)
    if (length(dl) != n-1 || length(du) != n-1)
        throw(ArgumentError("cannot make Tridiagonal from incompatible lengths of subdiagonal, diagonal and superdiagonal: ($(length(dl)), $(length(d)), $(length(du))"))
    end
    Tridiagonal(dl, d, du, zeros(T,n-2))
end

# Construct from diagonals of any abstract vector, any eltype
function Tridiagonal{Tl, Td, Tu}(dl::AbstractVector{Tl}, d::AbstractVector{Td}, du::AbstractVector{Tu})
    Tridiagonal(map(v->convert(Vector{promote_type(Tl,Td,Tu)}, v), (dl, d, du))...)
end

# Provide a constructor Tridiagonal(A) similar to the triangulars, diagonal, symmetric
"""
    Tridiagonal(A)

returns a `Tridiagonal` array based on (abstract) matrix `A`, using its first lower diagonal,
main diagonal, and first upper diagonal.

# Example

```jldoctest
julia> A = [1 2 3 4; 1 2 3 4; 1 2 3 4; 1 2 3 4]
4×4 Array{Int64,2}:
 1  2  3  4
 1  2  3  4
 1  2  3  4
 1  2  3  4

julia> Tridiagonal(A)
4×4 Tridiagonal{Int64}:
 1  2  ⋅  ⋅
 1  2  3  ⋅
 ⋅  2  3  4
 ⋅  ⋅  3  4
```
"""
function Tridiagonal(A::AbstractMatrix)
    return Tridiagonal(diag(A,-1), diag(A), diag(A,+1))
end

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

function convert{T}(::Type{Matrix{T}}, M::Tridiagonal{T})
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
convert{T}(::Type{Matrix}, M::Tridiagonal{T}) = convert(Matrix{T}, M)
convert(::Type{Array}, M::Tridiagonal) = convert(Matrix, M)
full(M::Tridiagonal) = convert(Array, M)
function similar{T}(M::Tridiagonal, ::Type{T})
    Tridiagonal{T}(similar(M.dl, T), similar(M.d, T), similar(M.du, T), similar(M.du2, T))
end

# Operations on Tridiagonal matrices
copy!(dest::Tridiagonal, src::Tridiagonal) = Tridiagonal(copy!(dest.dl, src.dl), copy!(dest.d, src.d), copy!(dest.du, src.du), copy!(dest.du2, src.du2))

#Elementary operations
broadcast(::typeof(abs), M::Tridiagonal) = Tridiagonal(abs.(M.dl), abs.(M.d), abs.(M.du), abs.(M.du2))
broadcast(::typeof(round), M::Tridiagonal) = Tridiagonal(round.(M.dl), round.(M.d), round.(M.du), round.(M.du2))
broadcast(::typeof(trunc), M::Tridiagonal) = Tridiagonal(trunc.(M.dl), trunc.(M.d), trunc.(M.du), trunc.(M.du2))
broadcast(::typeof(floor), M::Tridiagonal) = Tridiagonal(floor.(M.dl), floor.(M.d), floor.(M.du), floor.(M.du2))
broadcast(::typeof(ceil), M::Tridiagonal) = Tridiagonal(ceil.(M.dl), ceil.(M.d), ceil.(M.du), ceil.(M.du2))
for func in (:conj, :copy, :real, :imag)
    @eval function ($func)(M::Tridiagonal)
        Tridiagonal(($func)(M.dl), ($func)(M.d), ($func)(M.du), ($func)(M.du2))
    end
end
broadcast{T<:Integer}(::typeof(round), ::Type{T}, M::Tridiagonal) =
    Tridiagonal(round.(T, M.dl), round.(T, M.d), round.(T, M.du), round.(T, M.du2))
broadcast{T<:Integer}(::typeof(trunc), ::Type{T}, M::Tridiagonal) =
    Tridiagonal(trunc.(T, M.dl), trunc.(T, M.d), trunc.(T, M.du), trunc.(T, M.du2))
broadcast{T<:Integer}(::typeof(floor), ::Type{T}, M::Tridiagonal) =
    Tridiagonal(floor.(T, M.dl), floor.(T, M.d), floor.(T, M.du), floor.(T, M.du2))
broadcast{T<:Integer}(::typeof(ceil), ::Type{T}, M::Tridiagonal) =
    Tridiagonal(ceil.(T, M.dl), ceil.(T, M.d), ceil.(T, M.du), ceil.(T, M.du2))

transpose(M::Tridiagonal) = Tridiagonal(M.du, M.d, M.dl)
ctranspose(M::Tridiagonal) = conj(transpose(M))

function diag{T}(M::Tridiagonal{T}, n::Integer=0)
    if n == 0
        return M.d
    elseif n == -1
        return M.dl
    elseif n == 1
        return M.du
    elseif abs(n) < size(M,1)
        return zeros(T,size(M,1)-abs(n))
    else
        throw(ArgumentError("$n-th diagonal of a $(size(M)) matrix doesn't exist!"))
    end
end

function getindex{T}(A::Tridiagonal{T}, i::Integer, j::Integer)
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
    if i == j
        A.d[i] = x
    elseif i - j == 1
        A.dl[j] = x
    elseif j - i == 1
        A.du[i] = x
    else
        throw(ArgumentError("cannot set elements outside the sub, main, or super diagonals"))
    end
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
    if abs(k) > n
        throw(ArgumentError("requested diagonal, $k, out of bounds in matrix of size ($n,$n)"))
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
    if abs(k) > n
        throw(ArgumentError("requested diagonal, $k, out of bounds in matrix of size ($n,$n)"))
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

convert{T}(::Type{Tridiagonal{T}},M::Tridiagonal) = Tridiagonal(convert(Vector{T}, M.dl), convert(Vector{T}, M.d), convert(Vector{T}, M.du), convert(Vector{T}, M.du2))
convert{T}(::Type{AbstractMatrix{T}},M::Tridiagonal) = convert(Tridiagonal{T}, M)
convert{T}(::Type{Tridiagonal{T}}, M::SymTridiagonal{T}) = Tridiagonal(M)
function convert{T}(::Type{SymTridiagonal{T}}, M::Tridiagonal)
    if M.dl == M.du
        return SymTridiagonal(convert(Vector{T},M.d), convert(Vector{T},M.dl))
    else
        throw(ArgumentError("Tridiagonal is not symmetric, cannot convert to SymTridiagonal"))
    end
end
