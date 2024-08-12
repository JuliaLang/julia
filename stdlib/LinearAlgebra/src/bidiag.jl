# This file is a part of Julia. License is MIT: https://julialang.org/license

# Bidiagonal matrices
struct Bidiagonal{T,V<:AbstractVector{T}} <: AbstractMatrix{T}
    dv::V      # diagonal
    ev::V      # sub/super diagonal
    uplo::Char # upper bidiagonal ('U') or lower ('L')
    function Bidiagonal{T,V}(dv, ev, uplo::AbstractChar) where {T,V<:AbstractVector{T}}
        require_one_based_indexing(dv, ev)
        if length(ev) != max(length(dv)-1, 0)
            throw(DimensionMismatch(lazy"length of diagonal vector is $(length(dv)), length of off-diagonal vector is $(length(ev))"))
        end
        (uplo != 'U' && uplo != 'L') && throw_uplo()
        new{T,V}(dv, ev, uplo)
    end
end
function Bidiagonal{T,V}(dv, ev, uplo::Symbol) where {T,V<:AbstractVector{T}}
    Bidiagonal{T,V}(dv, ev, char_uplo(uplo))
end
function Bidiagonal{T}(dv::AbstractVector, ev::AbstractVector, uplo::Union{Symbol,AbstractChar}) where {T}
    Bidiagonal(convert(AbstractVector{T}, dv)::AbstractVector{T},
               convert(AbstractVector{T}, ev)::AbstractVector{T},
               uplo)
end
function Bidiagonal{T,V}(A::Bidiagonal) where {T,V<:AbstractVector{T}}
    Bidiagonal{T,V}(A.dv, A.ev, A.uplo)
end

"""
    Bidiagonal(dv::V, ev::V, uplo::Symbol) where V <: AbstractVector

Constructs an upper (`uplo=:U`) or lower (`uplo=:L`) bidiagonal matrix using the
given diagonal (`dv`) and off-diagonal (`ev`) vectors. The result is of type `Bidiagonal`
and provides efficient specialized linear solvers, but may be converted into a regular
matrix with [`convert(Array, _)`](@ref) (or `Array(_)` for short). The length of `ev`
must be one less than the length of `dv`.

# Examples
```jldoctest
julia> dv = [1, 2, 3, 4]
4-element Vector{Int64}:
 1
 2
 3
 4

julia> ev = [7, 8, 9]
3-element Vector{Int64}:
 7
 8
 9

julia> Bu = Bidiagonal(dv, ev, :U) # ev is on the first superdiagonal
4×4 Bidiagonal{Int64, Vector{Int64}}:
 1  7  ⋅  ⋅
 ⋅  2  8  ⋅
 ⋅  ⋅  3  9
 ⋅  ⋅  ⋅  4

julia> Bl = Bidiagonal(dv, ev, :L) # ev is on the first subdiagonal
4×4 Bidiagonal{Int64, Vector{Int64}}:
 1  ⋅  ⋅  ⋅
 7  2  ⋅  ⋅
 ⋅  8  3  ⋅
 ⋅  ⋅  9  4
```
"""
function Bidiagonal(dv::V, ev::V, uplo::Symbol) where {T,V<:AbstractVector{T}}
    Bidiagonal{T,V}(dv, ev, uplo)
end
function Bidiagonal(dv::V, ev::V, uplo::AbstractChar) where {T,V<:AbstractVector{T}}
    Bidiagonal{T,V}(dv, ev, uplo)
end

#To allow Bidiagonal's where the "dv" is Vector{T} and "ev" Vector{S},
#where T and S can be promoted
function Bidiagonal(dv::Vector{T}, ev::Vector{S}, uplo::Symbol) where {T,S}
    TS = promote_type(T,S)
    return Bidiagonal{TS,Vector{TS}}(dv, ev, uplo)
end

"""
    Bidiagonal(A, uplo::Symbol)

Construct a `Bidiagonal` matrix from the main diagonal of `A` and
its first super- (if `uplo=:U`) or sub-diagonal (if `uplo=:L`).

# Examples
```jldoctest
julia> A = [1 1 1 1; 2 2 2 2; 3 3 3 3; 4 4 4 4]
4×4 Matrix{Int64}:
 1  1  1  1
 2  2  2  2
 3  3  3  3
 4  4  4  4

julia> Bidiagonal(A, :U) # contains the main diagonal and first superdiagonal of A
4×4 Bidiagonal{Int64, Vector{Int64}}:
 1  1  ⋅  ⋅
 ⋅  2  2  ⋅
 ⋅  ⋅  3  3
 ⋅  ⋅  ⋅  4

julia> Bidiagonal(A, :L) # contains the main diagonal and first subdiagonal of A
4×4 Bidiagonal{Int64, Vector{Int64}}:
 1  ⋅  ⋅  ⋅
 2  2  ⋅  ⋅
 ⋅  3  3  ⋅
 ⋅  ⋅  4  4
```
"""
function Bidiagonal(A::AbstractMatrix, uplo::Symbol)
    Bidiagonal(diag(A, 0), diag(A, uplo === :U ? 1 : -1), uplo)
end


Bidiagonal(A::Bidiagonal) = A
Bidiagonal{T}(A::Bidiagonal{T}) where {T} = A
Bidiagonal{T}(A::Bidiagonal) where {T} = Bidiagonal{T}(A.dv, A.ev, A.uplo)

bidiagzero(::Bidiagonal{T}, i, j) where {T} = zero(T)
function bidiagzero(A::Bidiagonal{<:AbstractMatrix}, i, j)
    Tel = eltype(eltype(A.dv))
    if i < j && A.uplo == 'U' #= top right zeros =#
        return zeros(Tel, size(A.ev[i], 1), size(A.ev[j-1], 2))
    elseif j < i && A.uplo == 'L' #= bottom left zeros =#
        return zeros(Tel, size(A.ev[i-1], 1), size(A.ev[j], 2))
    else
        return zeros(Tel, size(A.dv[i], 1), size(A.dv[j], 2))
    end
end

_offdiagind(uplo) = uplo == 'U' ? 1 : -1

@inline function Base.isassigned(A::Bidiagonal, i::Int, j::Int)
    @boundscheck checkbounds(Bool, A, i, j) || return false
    if i == j
        return @inbounds isassigned(A.dv, i)
    elseif i == j - _offdiagind(A.uplo)
        return @inbounds isassigned(A.ev, A.uplo == 'U' ? i : j)
    else
        return true
    end
end

@inline function Base.isstored(A::Bidiagonal, i::Int, j::Int)
    @boundscheck checkbounds(A, i, j)
    if i == j
        return @inbounds Base.isstored(A.dv, i)
    elseif i == j - _offdiagind(A.uplo)
        return @inbounds Base.isstored(A.ev, A.uplo == 'U' ? i : j)
    else
        return false
    end
end

@inline function getindex(A::Bidiagonal{T}, i::Int, j::Int) where T
    @boundscheck checkbounds(A, i, j)
    if i == j
        return @inbounds A.dv[i]
    elseif i == j - _offdiagind(A.uplo)
        return @inbounds A.ev[A.uplo == 'U' ? i : j]
    else
        return bidiagzero(A, i, j)
    end
end

@inline function getindex(A::Bidiagonal{T}, b::BandIndex) where T
    @boundscheck checkbounds(A, _cartinds(b))
    if b.band == 0
        return @inbounds A.dv[b.index]
    elseif b.band == _offdiagind(A.uplo)
        return @inbounds A.ev[b.index]
    else
        return bidiagzero(A, Tuple(_cartinds(b))...)
    end
end

@inline function setindex!(A::Bidiagonal, x, i::Integer, j::Integer)
    @boundscheck checkbounds(A, i, j)
    if i == j
        @inbounds A.dv[i] = x
    elseif i == j - _offdiagind(A.uplo)
        @inbounds A.ev[A.uplo == 'U' ? i : j] = x
    elseif !iszero(x)
        throw(ArgumentError(LazyString(lazy"cannot set entry ($i, $j) off the ",
            A.uplo == 'U' ? "upper" : "lower", " bidiagonal band to a nonzero value ", x)))
    end
    return x
end

Base._reverse(A::Bidiagonal, dims) = reverse!(Matrix(A); dims)
Base._reverse(A::Bidiagonal, ::Colon) = Bidiagonal(reverse(A.dv), reverse(A.ev), A.uplo == 'U' ? :L : :U)

## structured matrix methods ##
function Base.replace_in_print_matrix(A::Bidiagonal,i::Integer,j::Integer,s::AbstractString)
    i==j || i==j-_offdiagind(A.uplo) ? s : Base.replace_with_centered_mark(s)
end

#Converting from Bidiagonal to dense Matrix
function Matrix{T}(A::Bidiagonal) where T
    B = Matrix{T}(undef, size(A))
    if haszero(T) # optimized path for types with zero(T) defined
        size(B,1) > 1 && fill!(B, zero(T))
        copyto!(view(B, diagind(B)), A.dv)
        copyto!(view(B, diagind(B, _offdiagind(A.uplo))), A.ev)
    else
        copyto!(B, A)
    end
    return B
end
Matrix(A::Bidiagonal{T}) where {T} = Matrix{promote_type(T, typeof(zero(T)))}(A)
Array(A::Bidiagonal) = Matrix(A)
promote_rule(::Type{Matrix{T}}, ::Type{<:Bidiagonal{S}}) where {T,S} =
    @isdefined(T) && @isdefined(S) ? Matrix{promote_type(T,S)} : Matrix
promote_rule(::Type{<:Matrix}, ::Type{<:Bidiagonal}) = Matrix

#Converting from Bidiagonal to Tridiagonal
function Tridiagonal{T}(A::Bidiagonal) where T
    dv = convert(AbstractVector{T}, A.dv)
    ev = convert(AbstractVector{T}, A.ev)
    # ensure that the types are identical, even if zero returns a different type
    z = oftype(ev, zero(ev))
    A.uplo == 'U' ? Tridiagonal(z, dv, ev) : Tridiagonal(ev, dv, z)
end
promote_rule(::Type{<:Tridiagonal{T}}, ::Type{<:Bidiagonal{S}}) where {T,S} =
    @isdefined(T) && @isdefined(S) ? Tridiagonal{promote_type(T,S)} : Tridiagonal
promote_rule(::Type{<:Tridiagonal}, ::Type{<:Bidiagonal}) = Tridiagonal

# When asked to convert Bidiagonal to AbstractMatrix{T}, preserve structure by converting to Bidiagonal{T} <: AbstractMatrix{T}
AbstractMatrix{T}(A::Bidiagonal) where {T} = Bidiagonal{T}(A)
AbstractMatrix{T}(A::Bidiagonal{T}) where {T} = copy(A)

convert(::Type{T}, m::AbstractMatrix) where {T<:Bidiagonal} = m isa T ? m : T(m)::T

similar(B::Bidiagonal, ::Type{T}) where {T} = Bidiagonal(similar(B.dv, T), similar(B.ev, T), B.uplo)
similar(B::Bidiagonal, ::Type{T}, dims::Union{Dims{1},Dims{2}}) where {T} = similar(B.dv, T, dims)

tr(B::Bidiagonal) = sum(B.dv)

function kron(A::Diagonal, B::Bidiagonal)
    # `_droplast!` is only guaranteed to work with `Vector`
    kdv = convert(Vector, kron(diag(A), B.dv))
    kev = _droplast!(convert(Vector, kron(diag(A), _pushzero(B.ev))))
    Bidiagonal(kdv, kev, B.uplo)
end

###################
# LAPACK routines #
###################

#Singular values
svdvals!(M::Bidiagonal{<:BlasReal}) = LAPACK.bdsdc!(M.uplo, 'N', M.dv, M.ev)[1]
function svd!(M::Bidiagonal{<:BlasReal}; full::Bool = false)
    d, e, U, Vt, Q, iQ = LAPACK.bdsdc!(M.uplo, 'I', M.dv, M.ev)
    SVD(U, d, Vt)
end
function svd(M::Bidiagonal; kw...)
    svd!(copy(M), kw...)
end

####################
# Generic routines #
####################

function show(io::IO, M::Bidiagonal)
    print(io, "Bidiagonal(")
    show(io, M.dv)
    print(io, ", ")
    show(io, M.ev)
    print(io, ", ")
    show(io, sym_uplo(M.uplo))
    print(io, ")")
end

size(M::Bidiagonal) = (n = length(M.dv); (n, n))
axes(M::Bidiagonal) = (ax = axes(M.dv, 1); (ax, ax))

#Elementary operations
for func in (:conj, :copy, :real, :imag)
    @eval ($func)(M::Bidiagonal) = Bidiagonal(($func)(M.dv), ($func)(M.ev), M.uplo)
end

adjoint(B::Bidiagonal{<:Number}) = Bidiagonal(vec(adjoint(B.dv)), vec(adjoint(B.ev)), B.uplo == 'U' ? :L : :U)
adjoint(B::Bidiagonal{<:Number, <:Base.ReshapedArray{<:Number,1,<:Adjoint}}) =
    Bidiagonal(adjoint(parent(B.dv)), adjoint(parent(B.ev)), B.uplo == 'U' ? :L : :U)
transpose(B::Bidiagonal{<:Number}) = Bidiagonal(B.dv, B.ev, B.uplo == 'U' ? :L : :U)
permutedims(B::Bidiagonal) = Bidiagonal(B.dv, B.ev, B.uplo == 'U' ? 'L' : 'U')
function permutedims(B::Bidiagonal, perm)
    Base.checkdims_perm(axes(B), axes(B), perm)
    NTuple{2}(perm) == (2, 1) ? permutedims(B) : B
end
function Base.copy(aB::Adjoint{<:Any,<:Bidiagonal})
    B = aB.parent
    return Bidiagonal(map(x -> copy.(adjoint.(x)), (B.dv, B.ev))..., B.uplo == 'U' ? :L : :U)
end
function Base.copy(tB::Transpose{<:Any,<:Bidiagonal})
    B = tB.parent
    return Bidiagonal(map(x -> copy.(transpose.(x)), (B.dv, B.ev))..., B.uplo == 'U' ? :L : :U)
end

@noinline function throw_zeroband_error(A)
    uplo = A.uplo
    zeroband = uplo == 'U' ? "lower" : "upper"
    throw(ArgumentError(LazyString("cannot set the ",
        zeroband, " bidiagonal band to a nonzero value for uplo=:", uplo)))
end

# copyto! for matching axes
function _copyto_banded!(A::Bidiagonal, B::Bidiagonal)
    A.dv .= B.dv
    if A.uplo == B.uplo
        A.ev .= B.ev
    elseif iszero(B.ev) # diagonal source
        A.ev .= B.ev
    else
        throw_zeroband_error(A)
    end
    return A
end

iszero(M::Bidiagonal) = iszero(M.dv) && iszero(M.ev)
isone(M::Bidiagonal) = all(isone, M.dv) && iszero(M.ev)
Base.@constprop :aggressive function istriu(M::Bidiagonal, k::Integer=0)
    if M.uplo == 'U'
        if k <= 0
            return true
        elseif k == 1
            return iszero(M.dv)
        else # k >= 2
            return iszero(M.dv) && iszero(M.ev)
        end
    else # M.uplo == 'L'
        if k <= -1
            return true
        elseif k == 0
            return iszero(M.ev)
        else # k >= 1
            return iszero(M.ev) && iszero(M.dv)
        end
    end
end
Base.@constprop :aggressive function istril(M::Bidiagonal, k::Integer=0)
    if M.uplo == 'U'
        if k >= 1
            return true
        elseif k == 0
            return iszero(M.ev)
        else # k <= -1
            return iszero(M.ev) && iszero(M.dv)
        end
    else # M.uplo == 'L'
        if k >= 0
            return true
        elseif k == -1
            return iszero(M.dv)
        else # k <= -2
            return iszero(M.dv) && iszero(M.ev)
        end
    end
end
isdiag(M::Bidiagonal) = iszero(M.ev)
issymmetric(M::Bidiagonal) = isdiag(M) && all(issymmetric, M.dv)
ishermitian(M::Bidiagonal) = isdiag(M) && all(ishermitian, M.dv)

function tril!(M::Bidiagonal{T}, k::Integer=0) where T
    n = length(M.dv)
    if !(-n - 1 <= k <= n - 1)
        throw(ArgumentError(LazyString(lazy"the requested diagonal, $k, must be at least ",
            lazy"$(-n - 1) and at most $(n - 1) in an $n-by-$n matrix")))
    elseif M.uplo == 'U' && k < 0
        fill!(M.dv, zero(T))
        fill!(M.ev, zero(T))
    elseif k < -1
        fill!(M.dv, zero(T))
        fill!(M.ev, zero(T))
    elseif M.uplo == 'U' && k == 0
        fill!(M.ev, zero(T))
    elseif M.uplo == 'L' && k == -1
        fill!(M.dv, zero(T))
    end
    return M
end

function triu!(M::Bidiagonal{T}, k::Integer=0) where T
    n = length(M.dv)
    if !(-n + 1 <= k <= n + 1)
        throw(ArgumentError(LazyString(lazy"the requested diagonal, $k, must be at least",
            lazy"$(-n + 1) and at most $(n + 1) in an $n-by-$n matrix")))
    elseif M.uplo == 'L' && k > 0
        fill!(M.dv, zero(T))
        fill!(M.ev, zero(T))
    elseif k > 1
        fill!(M.dv, zero(T))
        fill!(M.ev, zero(T))
    elseif M.uplo == 'L' && k == 0
        fill!(M.ev, zero(T))
    elseif M.uplo == 'U' && k == 1
        fill!(M.dv, zero(T))
    end
    return M
end

function diag(M::Bidiagonal{T}, n::Integer=0) where T
    # every branch call similar(..., ::Int) to make sure the
    # same vector type is returned independent of n
    if n == 0
        return copyto!(similar(M.dv, length(M.dv)), M.dv)
    elseif (n == 1 && M.uplo == 'U') ||  (n == -1 && M.uplo == 'L')
        return copyto!(similar(M.ev, length(M.ev)), M.ev)
    elseif -size(M,1) <= n <= size(M,1)
        return fill!(similar(M.dv, size(M,1)-abs(n)), zero(T))
    else
        throw(ArgumentError(LazyString(lazy"requested diagonal, $n, must be at least $(-size(M, 1)) ",
            lazy"and at most $(size(M, 2)) for an $(size(M, 1))-by-$(size(M, 2)) matrix")))
    end
end

function +(A::Bidiagonal, B::Bidiagonal)
    if A.uplo == B.uplo || length(A.dv) == 0
        Bidiagonal(A.dv+B.dv, A.ev+B.ev, A.uplo)
    else
        newdv = A.dv+B.dv
        Tridiagonal((A.uplo == 'U' ? (typeof(newdv)(B.ev), newdv, typeof(newdv)(A.ev)) : (typeof(newdv)(A.ev), newdv, typeof(newdv)(B.ev)))...)
    end
end

function -(A::Bidiagonal, B::Bidiagonal)
    if A.uplo == B.uplo || length(A.dv) == 0
        Bidiagonal(A.dv-B.dv, A.ev-B.ev, A.uplo)
    else
        newdv = A.dv-B.dv
        Tridiagonal((A.uplo == 'U' ? (typeof(newdv)(-B.ev), newdv, typeof(newdv)(A.ev)) : (typeof(newdv)(A.ev), newdv, typeof(newdv)(-B.ev)))...)
    end
end

-(A::Bidiagonal)=Bidiagonal(-A.dv,-A.ev,A.uplo)
*(A::Bidiagonal, B::Number) = Bidiagonal(A.dv*B, A.ev*B, A.uplo)
*(B::Number, A::Bidiagonal) = Bidiagonal(B*A.dv, B*A.ev, A.uplo)
/(A::Bidiagonal, B::Number) = Bidiagonal(A.dv/B, A.ev/B, A.uplo)
\(B::Number, A::Bidiagonal) = Bidiagonal(B\A.dv, B\A.ev, A.uplo)

function ==(A::Bidiagonal, B::Bidiagonal)
    if A.uplo == B.uplo
        return A.dv == B.dv && A.ev == B.ev
    else
        return iszero(A.ev) && iszero(B.ev) && A.dv == B.dv
    end
end

const BandedMatrix = Union{Bidiagonal,Diagonal,Tridiagonal,SymTridiagonal} # or BiDiTriSym
const BiTriSym = Union{Bidiagonal,Tridiagonal,SymTridiagonal}
const TriSym = Union{Tridiagonal,SymTridiagonal}
const BiTri = Union{Bidiagonal,Tridiagonal}
@inline _mul!(C::AbstractVector, A::BandedMatrix, B::AbstractVector, alpha::Number, beta::Number) =
    @stable_muladdmul _mul!(C, A, B, MulAddMul(alpha, beta))
@inline _mul!(C::AbstractMatrix, A::BandedMatrix, B::AbstractVector, alpha::Number, beta::Number) =
    @stable_muladdmul _mul!(C, A, B, MulAddMul(alpha, beta))
@inline _mul!(C::AbstractMatrix, A::BandedMatrix, B::AbstractMatrix, alpha::Number, beta::Number) =
    @stable_muladdmul _mul!(C, A, B, MulAddMul(alpha, beta))
@inline _mul!(C::AbstractMatrix, A::AbstractMatrix, B::BandedMatrix, alpha::Number, beta::Number) =
    @stable_muladdmul _mul!(C, A, B, MulAddMul(alpha, beta))
@inline _mul!(C::AbstractMatrix, A::BandedMatrix, B::BandedMatrix, alpha::Number, beta::Number) =
    @stable_muladdmul _mul!(C, A, B, MulAddMul(alpha, beta))

# B .= A * B
function lmul!(A::Bidiagonal, B::AbstractVecOrMat)
    _muldiag_size_check(size(A), size(B))
    (; dv, ev) = A
    if A.uplo == 'U'
        for k in axes(B,2)
            for i in axes(ev,1)
                B[i,k] = dv[i] * B[i,k] + ev[i] * B[i+1,k]
            end
            B[end,k] = dv[end] * B[end,k]
        end
    else
        for k in axes(B,2)
            for i in reverse(axes(dv,1)[2:end])
                B[i,k] = dv[i] * B[i,k] + ev[i-1] * B[i-1,k]
            end
            B[1,k] = dv[1] * B[1,k]
        end
    end
    return B
end
# B .= D * B
function lmul!(D::Diagonal, B::Bidiagonal)
    _muldiag_size_check(size(D), size(B))
    (; dv, ev) = B
    isL = B.uplo == 'L'
    dv[1] = D.diag[1] * dv[1]
    for i in axes(ev,1)
        ev[i] = D.diag[i + isL] * ev[i]
        dv[i+1] = D.diag[i+1] * dv[i+1]
    end
    return B
end
# B .= B * A
function rmul!(B::AbstractMatrix, A::Bidiagonal)
    _muldiag_size_check(size(A), size(B))
    (; dv, ev) = A
    if A.uplo == 'U'
        for k in reverse(axes(dv,1)[2:end])
            for i in axes(B,1)
                B[i,k] = B[i,k] * dv[k] + B[i,k-1] * ev[k-1]
            end
        end
        for i in axes(B,1)
            B[i,1] *= dv[1]
        end
    else
        for k in axes(ev,1)
            for i in axes(B,1)
                B[i,k] = B[i,k] * dv[k] + B[i,k+1] * ev[k]
            end
        end
        for i in axes(B,1)
            B[i,end] *= dv[end]
        end
    end
    return B
end
# B .= B * D
function rmul!(B::Bidiagonal, D::Diagonal)
    _muldiag_size_check(size(B), size(D))
    (; dv, ev) = B
    isU = B.uplo == 'U'
    dv[1] *= D.diag[1]
    for i in axes(ev,1)
        ev[i] *= D.diag[i + isU]
        dv[i+1] *= D.diag[i+1]
    end
    return B
end

@noinline function check_A_mul_B!_sizes((mC, nC)::NTuple{2,Integer}, (mA, nA)::NTuple{2,Integer}, (mB, nB)::NTuple{2,Integer})
    if mA != mC
        throw(DimensionMismatch(lazy"first dimension of A, $mA, and first dimension of output C, $mC, must match"))
    elseif nA != mB
        throw(DimensionMismatch(lazy"second dimension of A, $nA, and first dimension of B, $mB, must match"))
    elseif nB != nC
        throw(DimensionMismatch(lazy"second dimension of output C, $nC, and second dimension of B, $nB, must match"))
    end
end

# function to get the internally stored vectors for Bidiagonal and [Sym]Tridiagonal
# to avoid allocations in _mul! below (#24324, #24578)
_diag(A::Tridiagonal, k) = k == -1 ? A.dl : k == 0 ? A.d : A.du
_diag(A::SymTridiagonal, k) = k == 0 ? A.dv : A.ev
function _diag(A::Bidiagonal, k)
    if k == 0
        return A.dv
    elseif k == _offdiagind(A.uplo)
        return A.ev
    else
        return diag(A, k)
    end
end

_mul!(C::AbstractMatrix, A::BiTriSym, B::TriSym, _add::MulAddMul) =
    _bibimul!(C, A, B, _add)
_mul!(C::AbstractMatrix, A::BiTriSym, B::Bidiagonal, _add::MulAddMul) =
    _bibimul!(C, A, B, _add)
function _bibimul!(C, A, B, _add)
    check_A_mul_B!_sizes(size(C), size(A), size(B))
    n = size(A,1)
    n <= 3 && return mul!(C, Array(A), Array(B), _add.alpha, _add.beta)
    # We use `_rmul_or_fill!` instead of `_modify!` here since using
    # `_modify!` in the following loop will not update the
    # off-diagonal elements for non-zero beta.
    _rmul_or_fill!(C, _add.beta)
    iszero(_add.alpha) && return C
    Al = _diag(A, -1)
    Ad = _diag(A, 0)
    Au = _diag(A, 1)
    Bl = _diag(B, -1)
    Bd = _diag(B, 0)
    Bu = _diag(B, 1)
    @inbounds begin
        # first row of C
        C[1,1] += _add(A[1,1]*B[1,1] + A[1, 2]*B[2, 1])
        C[1,2] += _add(A[1,1]*B[1,2] + A[1,2]*B[2,2])
        C[1,3] += _add(A[1,2]*B[2,3])
        # second row of C
        C[2,1] += _add(A[2,1]*B[1,1] + A[2,2]*B[2,1])
        C[2,2] += _add(A[2,1]*B[1,2] + A[2,2]*B[2,2] + A[2,3]*B[3,2])
        C[2,3] += _add(A[2,2]*B[2,3] + A[2,3]*B[3,3])
        C[2,4] += _add(A[2,3]*B[3,4])
        for j in 3:n-2
            Ajj₋1   = Al[j-1]
            Ajj     = Ad[j]
            Ajj₊1   = Au[j]
            Bj₋1j₋2 = Bl[j-2]
            Bj₋1j₋1 = Bd[j-1]
            Bj₋1j   = Bu[j-1]
            Bjj₋1   = Bl[j-1]
            Bjj     = Bd[j]
            Bjj₊1   = Bu[j]
            Bj₊1j   = Bl[j]
            Bj₊1j₊1 = Bd[j+1]
            Bj₊1j₊2 = Bu[j+1]
            C[j,j-2]  += _add( Ajj₋1*Bj₋1j₋2)
            C[j, j-1] += _add(Ajj₋1*Bj₋1j₋1 + Ajj*Bjj₋1)
            C[j, j  ] += _add(Ajj₋1*Bj₋1j   + Ajj*Bjj       + Ajj₊1*Bj₊1j)
            C[j, j+1] += _add(Ajj  *Bjj₊1   + Ajj₊1*Bj₊1j₊1)
            C[j, j+2] += _add(Ajj₊1*Bj₊1j₊2)
        end
        # row before last of C
        C[n-1,n-3] += _add(A[n-1,n-2]*B[n-2,n-3])
        C[n-1,n-2] += _add(A[n-1,n-1]*B[n-1,n-2] + A[n-1,n-2]*B[n-2,n-2])
        C[n-1,n-1] += _add(A[n-1,n-2]*B[n-2,n-1] + A[n-1,n-1]*B[n-1,n-1] + A[n-1,n]*B[n,n-1])
        C[n-1,n  ] += _add(A[n-1,n-1]*B[n-1,n  ] + A[n-1,  n]*B[n  ,n  ])
        # last row of C
        C[n,n-2] += _add(A[n,n-1]*B[n-1,n-2])
        C[n,n-1] += _add(A[n,n-1]*B[n-1,n-1] + A[n,n]*B[n,n-1])
        C[n,n  ] += _add(A[n,n-1]*B[n-1,n  ] + A[n,n]*B[n,n  ])
    end # inbounds
    C
end

function _mul!(C::AbstractMatrix, A::BiTriSym, B::Diagonal, _add::MulAddMul)
    require_one_based_indexing(C)
    check_A_mul_B!_sizes(size(C), size(A), size(B))
    n = size(A,1)
    iszero(n) && return C
    _rmul_or_fill!(C, _add.beta)  # see the same use above
    iszero(_add.alpha) && return C
    Al = _diag(A, -1)
    Ad = _diag(A, 0)
    Au = _diag(A, 1)
    Bd = B.diag
    @inbounds begin
        # first row of C
        for j in 1:min(2, n)
            C[1,j] += _add(A[1,j]*B[j,j])
        end
        # second row of C
        if n > 1
            for j in 1:min(3, n)
                C[2,j] += _add(A[2,j]*B[j,j])
            end
        end
        for j in 3:n-2
            C[j, j-1] += _add(Al[j-1]*Bd[j-1])
            C[j, j  ] += _add(Ad[j  ]*Bd[j  ])
            C[j, j+1] += _add(Au[j  ]*Bd[j+1])
        end
        if n > 3
            # row before last of C
            for j in n-2:n
                C[n-1,j] += _add(A[n-1,j]*B[j,j])
            end
        end
        # last row of C
        if n > 2
            for j in n-1:n
                C[n,j] += _add(A[n,j]*B[j,j])
            end
        end
    end # inbounds
    C
end

function _mul!(C::AbstractMatrix, A::Bidiagonal, B::Diagonal, _add::MulAddMul)
    require_one_based_indexing(C)
    check_A_mul_B!_sizes(size(C), size(A), size(B))
    n = size(A,1)
    iszero(n) && return C
    _rmul_or_fill!(C, _add.beta)  # see the same use above
    iszero(_add.alpha) && return C
    (; dv, ev) = A
    Bd = B.diag
    rowshift = A.uplo == 'U' ? -1 : 1
    evshift = Int(A.uplo == 'U')
    @inbounds begin
        # first row of C
        C[1,1] += _add(dv[1]*Bd[1])
        if n > 1
            if A.uplo == 'L'
                C[2,1] += _add(ev[1]*Bd[1])
            end
            for col in 2:n-1
                C[col+rowshift, col] += _add(ev[col - evshift]*Bd[col])
                C[col, col] += _add(dv[col]*Bd[col])
            end
            if A.uplo == 'U'
                C[n-1,n] += _add(ev[n-1]*Bd[n])
            end
            C[n, n] += _add(dv[n]*Bd[n])
        end
    end # inbounds
    C
end

function _mul!(C::Bidiagonal, A::Bidiagonal, B::Diagonal, _add::MulAddMul)
    check_A_mul_B!_sizes(size(C), size(A), size(B))
    n = size(A,1)
    iszero(n) && return C
    iszero(_add.alpha) && return _rmul_or_fill!(C, _add.beta)
    Adv, Aev = A.dv, A.ev
    Cdv, Cev = C.dv, C.ev
    Bd = B.diag
    shift = Int(A.uplo == 'U')
    if C.uplo == A.uplo
        @inbounds begin
            _modify!(_add, Adv[1]*Bd[1], Cdv, 1)
            for j in eachindex(IndexLinear(), Aev, Cev)
                _modify!(_add, Aev[j]*Bd[j+shift], Cev, j)
                _modify!(_add, Adv[j+1]*Bd[j+1], Cdv, j+1)
            end
        end # inbounds
    else
        @inbounds begin
            _modify!(_add, Adv[1]*Bd[1], Cdv, 1)
            for j in eachindex(IndexLinear(), Aev, Cev)
                _modify!(_add, Adv[j+1]*Bd[j+1], Cdv, j+1)
                # this branch will error unless the value is zero
                _modify!(_add, Aev[j]*Bd[j+shift], C, (j+1-shift, j+shift))
                # zeros of the correct type
                _modify!(_add, A[j+shift, j+1-shift]*Bd[j+1-shift], Cev, j)
            end
        end
    end
    C
end

function _mul!(C::AbstractVecOrMat, A::BiTriSym, B::AbstractVecOrMat, _add::MulAddMul)
    require_one_based_indexing(C, B)
    nA = size(A,1)
    nB = size(B,2)
    if !(size(C,1) == size(B,1) == nA)
        throw(DimensionMismatch(lazy"A has first dimension $nA, B has $(size(B,1)), C has $(size(C,1)) but all must match"))
    end
    if size(C,2) != nB
        throw(DimensionMismatch(lazy"A has second dimension $nA, B has $(size(B,2)), C has $(size(C,2)) but all must match"))
    end
    iszero(nA) && return C
    iszero(_add.alpha) && return _rmul_or_fill!(C, _add.beta)
    nA <= 3 && return mul!(C, Array(A), Array(B), _add.alpha, _add.beta)
    l = _diag(A, -1)
    d = _diag(A, 0)
    u = _diag(A, 1)
    @inbounds begin
        for j = 1:nB
            b₀, b₊ = B[1, j], B[2, j]
            _modify!(_add, d[1]*b₀ + u[1]*b₊, C, (1, j))
            for i = 2:nA - 1
                b₋, b₀, b₊ = b₀, b₊, B[i + 1, j]
                _modify!(_add, l[i - 1]*b₋ + d[i]*b₀ + u[i]*b₊, C, (i, j))
            end
            _modify!(_add, l[nA - 1]*b₀ + d[nA]*b₊, C, (nA, j))
        end
    end
    C
end

function _mul!(C::AbstractMatrix, A::AbstractMatrix, B::TriSym, _add::MulAddMul)
    require_one_based_indexing(C, A)
    check_A_mul_B!_sizes(size(C), size(A), size(B))
    iszero(_add.alpha) && return _rmul_or_fill!(C, _add.beta)
    n = size(A,1)
    m = size(B,2)
    if n <= 3 || m <= 1
        return mul!(C, Array(A), Array(B), _add.alpha, _add.beta)
    end
    Bl = _diag(B, -1)
    Bd = _diag(B, 0)
    Bu = _diag(B, 1)
    @inbounds begin
        # first and last column of C
        B11 = Bd[1]
        B21 = Bl[1]
        Bmm = Bd[m]
        Bm₋1m = Bu[m-1]
        for i in 1:n
            _modify!(_add, A[i,1] * B11 + A[i, 2] * B21, C, (i, 1))
            _modify!(_add, A[i, m-1] * Bm₋1m + A[i, m] * Bmm, C, (i, m))
        end
        # middle columns of C
        for j = 2:m-1
            Bj₋1j = Bu[j-1]
            Bjj = Bd[j]
            Bj₊1j = Bl[j]
            for i = 1:n
                _modify!(_add, A[i, j-1] * Bj₋1j + A[i, j]*Bjj + A[i, j+1] * Bj₊1j, C, (i, j))
            end
        end
    end # inbounds
    C
end

function _mul!(C::AbstractMatrix, A::AbstractMatrix, B::Bidiagonal, _add::MulAddMul)
    require_one_based_indexing(C, A)
    check_A_mul_B!_sizes(size(C), size(A), size(B))
    iszero(_add.alpha) && return _rmul_or_fill!(C, _add.beta)
    if size(A, 1) <= 3 || size(B, 2) <= 1
        return mul!(C, Array(A), Array(B), _add.alpha, _add.beta)
    end
    m, n = size(A)
    @inbounds if B.uplo == 'U'
        for i in 1:m
            for j in n:-1:2
                _modify!(_add, A[i,j] * B.dv[j] + A[i,j-1] * B.ev[j-1], C, (i, j))
            end
            _modify!(_add, A[i,1] * B.dv[1], C, (i, 1))
        end
    else # uplo == 'L'
        for i in 1:m
            for j in 1:n-1
                _modify!(_add, A[i,j] * B.dv[j] + A[i,j+1] * B.ev[j], C, (i, j))
            end
            _modify!(_add, A[i,n] * B.dv[n], C, (i, n))
        end
    end
    C
end

_mul!(C::AbstractMatrix, A::Diagonal, B::Bidiagonal, _add::MulAddMul) =
    _dibimul!(C, A, B, _add)
_mul!(C::AbstractMatrix, A::Diagonal, B::TriSym, _add::MulAddMul) =
    _dibimul!(C, A, B, _add)
function _dibimul!(C, A, B, _add)
    require_one_based_indexing(C)
    check_A_mul_B!_sizes(size(C), size(A), size(B))
    n = size(A,1)
    n <= 3 && return mul!(C, Array(A), Array(B), _add.alpha, _add.beta)
    _rmul_or_fill!(C, _add.beta)  # see the same use above
    iszero(_add.alpha) && return C
    Ad = A.diag
    Bl = _diag(B, -1)
    Bd = _diag(B, 0)
    Bu = _diag(B, 1)
    @inbounds begin
        # first row of C
        C[1,1] += _add(A[1,1]*B[1,1])
        C[1,2] += _add(A[1,1]*B[1,2])
        # second row of C
        C[2,1] += _add(A[2,2]*B[2,1])
        C[2,2] += _add(A[2,2]*B[2,2])
        C[2,3] += _add(A[2,2]*B[2,3])
        for j in 3:n-2
            Ajj       = Ad[j]
            C[j, j-1] += _add(Ajj*Bl[j-1])
            C[j, j  ] += _add(Ajj*Bd[j])
            C[j, j+1] += _add(Ajj*Bu[j])
        end
        # row before last of C
        C[n-1,n-2] += _add(A[n-1,n-1]*B[n-1,n-2])
        C[n-1,n-1] += _add(A[n-1,n-1]*B[n-1,n-1])
        C[n-1,n  ] += _add(A[n-1,n-1]*B[n-1,n  ])
        # last row of C
        C[n,n-1] += _add(A[n,n]*B[n,n-1])
        C[n,n  ] += _add(A[n,n]*B[n,n  ])
    end # inbounds
    C
end
function _dibimul!(C::AbstractMatrix, A::Diagonal, B::Bidiagonal, _add)
    require_one_based_indexing(C)
    check_A_mul_B!_sizes(size(C), size(A), size(B))
    n = size(A,1)
    iszero(n) && return C
    _rmul_or_fill!(C, _add.beta)  # see the same use above
    iszero(_add.alpha) && return C
    Ad = A.diag
    Bdv, Bev = B.dv, B.ev
    rowshift = B.uplo == 'U' ? -1 : 1
    evshift = Int(B.uplo == 'U')
    @inbounds begin
        # first row of C
        C[1,1] += _add(Ad[1]*Bdv[1])
        if n > 1
            if B.uplo == 'L'
                C[2,1] += _add(Ad[2]*Bev[1])
            end
            for col in 2:n-1
                evrow = col+rowshift
                C[evrow, col] += _add(Ad[evrow]*Bev[col - evshift])
                C[col, col] += _add(Ad[col]*Bdv[col])
            end
            if B.uplo == 'U'
                C[n-1,n] += _add(Ad[n-1]*Bev[n-1])
            end
            C[n, n] += _add(Ad[n]*Bdv[n])
        end
    end # inbounds
    C
end
function _dibimul!(C::Bidiagonal, A::Diagonal, B::Bidiagonal, _add)
    check_A_mul_B!_sizes(size(C), size(A), size(B))
    n = size(A,1)
    n == 0 && return C
    iszero(_add.alpha) && return _rmul_or_fill!(C, _add.beta)
    Ad = A.diag
    Bdv, Bev = B.dv, B.ev
    Cdv, Cev = C.dv, C.ev
    shift = Int(B.uplo == 'L')
    if C.uplo == B.uplo
        @inbounds begin
            _modify!(_add, Ad[1]*Bdv[1], Cdv, 1)
            for j in eachindex(IndexLinear(), Bev, Cev)
                _modify!(_add, Ad[j+shift]*Bev[j], Cev, j)
                _modify!(_add, Ad[j+1]*Bdv[j+1], Cdv, j+1)
            end
        end # inbounds
    else
        @inbounds begin
            _modify!(_add, Ad[1]*Bdv[1], Cdv, 1)
            for j in eachindex(IndexLinear(), Bev, Cev)
                _modify!(_add, Ad[j+1]*Bdv[j+1], Cdv, j+1)
                # this branch will error unless the value is zero
                _modify!(_add, Ad[j+shift]*Bev[j], C, (j+shift, j+1-shift))
                # zeros of the correct type
                _modify!(_add, Ad[j+1-shift]*B[j+1-shift,j+shift], Cev, j)
            end
        end
    end
    C
end

function *(A::UpperOrUnitUpperTriangular, B::Bidiagonal)
    TS = promote_op(matprod, eltype(A), eltype(B))
    C = mul!(similar(A, TS, size(A)), A, B)
    return B.uplo == 'U' ? UpperTriangular(C) : C
end

function *(A::LowerOrUnitLowerTriangular, B::Bidiagonal)
    TS = promote_op(matprod, eltype(A), eltype(B))
    C = mul!(similar(A, TS, size(A)), A, B)
    return B.uplo == 'L' ? LowerTriangular(C) : C
end

function *(A::Bidiagonal, B::UpperOrUnitUpperTriangular)
    TS = promote_op(matprod, eltype(A), eltype(B))
    C = mul!(similar(B, TS, size(B)), A, B)
    return A.uplo == 'U' ? UpperTriangular(C) : C
end

function *(A::Bidiagonal, B::LowerOrUnitLowerTriangular)
    TS = promote_op(matprod, eltype(A), eltype(B))
    C = mul!(similar(B, TS, size(B)), A, B)
    return A.uplo == 'L' ? LowerTriangular(C) : C
end

function dot(x::AbstractVector, B::Bidiagonal, y::AbstractVector)
    require_one_based_indexing(x, y)
    nx, ny = length(x), length(y)
    (nx == size(B, 1) == ny) || throw(DimensionMismatch())
    if nx ≤ 1
        nx == 0 && return dot(zero(eltype(x)), zero(eltype(B)), zero(eltype(y)))
        return dot(x[1], B.dv[1], y[1])
    end
    ev, dv = B.ev, B.dv
    @inbounds if B.uplo == 'U'
        x₀ = x[1]
        r = dot(x[1], dv[1], y[1])
        for j in 2:nx-1
            x₋, x₀ = x₀, x[j]
            r += dot(adjoint(ev[j-1])*x₋ + adjoint(dv[j])*x₀, y[j])
        end
        r += dot(adjoint(ev[nx-1])*x₀ + adjoint(dv[nx])*x[nx], y[nx])
        return r
    else # B.uplo == 'L'
        x₀ = x[1]
        x₊ = x[2]
        r = dot(adjoint(dv[1])*x₀ + adjoint(ev[1])*x₊, y[1])
        for j in 2:nx-1
            x₀, x₊ = x₊, x[j+1]
            r += dot(adjoint(dv[j])*x₀ + adjoint(ev[j])*x₊, y[j])
        end
        r += dot(x₊, dv[nx], y[nx])
        return r
    end
end

#Linear solvers
#Generic solver using naive substitution
ldiv!(A::Bidiagonal, b::AbstractVecOrMat) = @inline ldiv!(b, A, b)
function ldiv!(c::AbstractVecOrMat, A::Bidiagonal, b::AbstractVecOrMat)
    require_one_based_indexing(c, A, b)
    N = size(A, 2)
    mb, nb = size(b, 1), size(b, 2)
    if N != mb
        throw(DimensionMismatch(lazy"second dimension of A, $N, does not match first dimension of b, $mb"))
    end
    mc, nc = size(c, 1), size(c, 2)
    if mc != mb || nc != nb
        throw(DimensionMismatch(lazy"size of result, ($mc, $nc), does not match the size of b, ($mb, $nb)"))
    end

    if N == 0
        return copyto!(c, b)
    end

    zi = findfirst(iszero, A.dv)
    isnothing(zi) || throw(SingularException(zi))

    @inbounds for j in 1:nb
        if A.uplo == 'L' #do colwise forward substitution
            c[1,j] = bi1 = A.dv[1] \ b[1,j]
            for i in 2:N
                c[i,j] = bi1 = A.dv[i] \ (b[i,j] - A.ev[i - 1] * bi1)
            end
        else #do colwise backward substitution
            c[N,j] = bi1 = A.dv[N] \ b[N,j]
            for i in (N - 1):-1:1
                c[i,j] = bi1 = A.dv[i] \ (b[i,j] - A.ev[i] * bi1)
            end
        end
    end
    return c
end
ldiv!(A::AdjOrTrans{<:Any,<:Bidiagonal}, b::AbstractVecOrMat) = @inline ldiv!(b, A, b)
ldiv!(c::AbstractVecOrMat, A::AdjOrTrans{<:Any,<:Bidiagonal}, b::AbstractVecOrMat) =
    (t = wrapperop(A); _rdiv!(t(c), t(b), t(A)); return c)

### Generic promotion methods and fallbacks
\(A::Bidiagonal, B::AbstractVecOrMat) =
    ldiv!(matprod_dest(A, B, promote_op(\, eltype(A), eltype(B))), A, B)
\(xA::AdjOrTrans{<:Any,<:Bidiagonal}, B::AbstractVecOrMat) = copy(xA) \ B

### Triangular specializations
for tri in (:UpperTriangular, :UnitUpperTriangular)
    @eval function \(B::Bidiagonal, U::$tri)
        A = ldiv!(matprod_dest(B, U, promote_op(\, eltype(B), eltype(U))), B, U)
        return B.uplo == 'U' ? UpperTriangular(A) : A
    end
    @eval function \(U::$tri, B::Bidiagonal)
        A = ldiv!(matprod_dest(U, B, promote_op(\, eltype(U), eltype(B))), U, B)
        return B.uplo == 'U' ? UpperTriangular(A) : A
    end
end
for tri in (:LowerTriangular, :UnitLowerTriangular)
    @eval function \(B::Bidiagonal, L::$tri)
        A = ldiv!(matprod_dest(B, L, promote_op(\, eltype(B), eltype(L))), B, L)
        return B.uplo == 'L' ? LowerTriangular(A) : A
    end
    @eval function \(L::$tri, B::Bidiagonal)
        A = ldiv!(matprod_dest(L, B, promote_op(\, eltype(L), eltype(B))), L, B)
        return B.uplo == 'L' ? LowerTriangular(A) : A
    end
end

### Diagonal specialization
function \(B::Bidiagonal, D::Diagonal)
    A = ldiv!(similar(D, promote_op(\, eltype(B), eltype(D)), size(D)), B, D)
    return B.uplo == 'U' ? UpperTriangular(A) : LowerTriangular(A)
end

function _rdiv!(C::AbstractMatrix, A::AbstractMatrix, B::Bidiagonal)
    require_one_based_indexing(C, A, B)
    m, n = size(A)
    if size(B, 1) != n
        throw(DimensionMismatch(lazy"right hand side B needs first dimension of size $n, has size $(size(B,1))"))
    end
    mc, nc = size(C)
    if mc != m || nc != n
        throw(DimensionMismatch(lazy"expect output to have size ($m, $n), but got ($mc, $nc)"))
    end

    zi = findfirst(iszero, B.dv)
    isnothing(zi) || throw(SingularException(zi))

    if B.uplo == 'L'
        diagB = B.dv[n]
        for i in 1:m
            C[i,n] = A[i,n] / diagB
        end
        for j in n-1:-1:1
            diagB = B.dv[j]
            offdiagB = B.ev[j]
            for i in 1:m
                C[i,j] = (A[i,j] - C[i,j+1]*offdiagB)/diagB
            end
        end
    else
        diagB = B.dv[1]
        for i in 1:m
            C[i,1] = A[i,1] / diagB
        end
        for j in 2:n
            diagB = B.dv[j]
            offdiagB = B.ev[j-1]
            for i = 1:m
                C[i,j] = (A[i,j] - C[i,j-1]*offdiagB)/diagB
            end
        end
    end
    C
end
rdiv!(A::AbstractMatrix, B::Bidiagonal) = @inline _rdiv!(A, A, B)
rdiv!(A::AbstractMatrix, B::AdjOrTrans{<:Any,<:Bidiagonal}) = @inline _rdiv!(A, A, B)
_rdiv!(C::AbstractMatrix, A::AbstractMatrix, B::AdjOrTrans{<:Any,<:Bidiagonal}) =
    (t = wrapperop(B); ldiv!(t(C), t(B), t(A)); return C)

/(A::AbstractMatrix, B::Bidiagonal) =
    _rdiv!(similar(A, promote_op(/, eltype(A), eltype(B)), size(A)), A, B)

### Triangular specializations
for tri in (:UpperTriangular, :UnitUpperTriangular)
    @eval function /(U::$tri, B::Bidiagonal)
        A = _rdiv!(matprod_dest(U, B, promote_op(/, eltype(U), eltype(B))), U, B)
        return B.uplo == 'U' ? UpperTriangular(A) : A
    end
    @eval function /(B::Bidiagonal, U::$tri)
        A = _rdiv!(matprod_dest(B, U, promote_op(/, eltype(B), eltype(U))), B, U)
        return B.uplo == 'U' ? UpperTriangular(A) : A
    end
end
for tri in (:LowerTriangular, :UnitLowerTriangular)
    @eval function /(L::$tri, B::Bidiagonal)
        A = _rdiv!(matprod_dest(L, B, promote_op(/, eltype(L), eltype(B))), L, B)
        return B.uplo == 'L' ? LowerTriangular(A) : A
    end
    @eval function /(B::Bidiagonal, L::$tri)
        A = _rdiv!(matprod_dest(B, L, promote_op(/, eltype(B), eltype(L))), B, L)
        return B.uplo == 'L' ? LowerTriangular(A) : A
    end
end

### Diagonal specialization
function /(D::Diagonal, B::Bidiagonal)
    A = _rdiv!(similar(D, promote_op(/, eltype(D), eltype(B)), size(D)), D, B)
    return B.uplo == 'U' ? UpperTriangular(A) : LowerTriangular(A)
end

/(A::AbstractMatrix, B::Transpose{<:Any,<:Bidiagonal}) = A / copy(B)
/(A::AbstractMatrix, B::Adjoint{<:Any,<:Bidiagonal}) = A / copy(B)
# disambiguation
/(A::AdjointAbsVec, B::Bidiagonal) = adjoint(adjoint(B) \ parent(A))
/(A::TransposeAbsVec, B::Bidiagonal) = transpose(transpose(B) \ parent(A))
/(A::AdjointAbsVec, B::Transpose{<:Any,<:Bidiagonal}) = adjoint(adjoint(B) \ parent(A))
/(A::TransposeAbsVec, B::Transpose{<:Any,<:Bidiagonal}) = transpose(transpose(B) \ parent(A))
/(A::AdjointAbsVec, B::Adjoint{<:Any,<:Bidiagonal}) = adjoint(adjoint(B) \ parent(A))
/(A::TransposeAbsVec, B::Adjoint{<:Any,<:Bidiagonal}) = transpose(transpose(B) \ parent(A))

factorize(A::Bidiagonal) = A
function inv(B::Bidiagonal{T}) where T
    n = size(B, 1)
    dest = zeros(typeof(inv(oneunit(T))), (n, n))
    ldiv!(dest, B, Diagonal{typeof(one(T)/one(T))}(I, n))
    return B.uplo == 'U' ? UpperTriangular(dest) : LowerTriangular(dest)
end

# Eigensystems
eigvals(M::Bidiagonal) = copy(M.dv)
function eigvecs(M::Bidiagonal{T}) where T
    n = length(M.dv)
    Q = Matrix{T}(undef, n,n)
    blks = [0; findall(iszero, M.ev); n]
    v = zeros(T, n)
    if M.uplo == 'U'
        for idx_block = 1:length(blks) - 1, i = blks[idx_block] + 1:blks[idx_block + 1] #index of eigenvector
            fill!(v, zero(T))
            v[blks[idx_block] + 1] = one(T)
            for j = blks[idx_block] + 1:i - 1 #Starting from j=i, eigenvector elements will be 0
                v[j+1] = (M.dv[i] - M.dv[j])/M.ev[j] * v[j]
            end
            c = norm(v)
            for j = 1:n
                Q[j, i] = v[j] / c
            end
        end
    else
        for idx_block = 1:length(blks) - 1, i = blks[idx_block + 1]:-1:blks[idx_block] + 1 #index of eigenvector
            fill!(v, zero(T))
            v[blks[idx_block+1]] = one(T)
            for j = (blks[idx_block+1] - 1):-1:max(1, (i - 1)) #Starting from j=i, eigenvector elements will be 0
                v[j] = (M.dv[i] - M.dv[j+1])/M.ev[j] * v[j+1]
            end
            c = norm(v)
            for j = 1:n
                Q[j, i] = v[j] / c
            end
        end
    end
    Q #Actually Triangular
end
eigen(M::Bidiagonal) = Eigen(eigvals(M), eigvecs(M))

Base._sum(A::Bidiagonal, ::Colon) = sum(A.dv) + sum(A.ev)
function Base._sum(A::Bidiagonal, dims::Integer)
    res = Base.reducedim_initarray(A, dims, zero(eltype(A)))
    n = length(A.dv)
    if n == 0
        # Just to be sure. This shouldn't happen since there is a check whether
        # length(A.dv) == length(A.ev) + 1 in the constructor.
        return res
    elseif n == 1
        res[1] = A.dv[1]
        return res
    end
    @inbounds begin
        if (dims == 1 && A.uplo == 'U') || (dims == 2 && A.uplo == 'L')
            res[1] = A.dv[1]
            for i = 2:length(A.dv)
                res[i] = A.ev[i-1] + A.dv[i]
            end
        elseif (dims == 1 && A.uplo == 'L') || (dims == 2 && A.uplo == 'U')
            for i = 1:length(A.dv)-1
                res[i] = A.ev[i] + A.dv[i]
            end
            res[end] = A.dv[end]
        elseif dims >= 3
            if A.uplo == 'U'
                for i = 1:length(A.dv)-1
                    res[i,i]   = A.dv[i]
                    res[i,i+1] = A.ev[i]
                end
            else
                for i = 1:length(A.dv)-1
                    res[i,i]   = A.dv[i]
                    res[i+1,i] = A.ev[i]
                end
            end
            res[end,end] = A.dv[end]
        end
    end
    res
end
