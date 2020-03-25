# This file is a part of Julia. License is MIT: https://julialang.org/license

# Bidiagonal matrices
struct Bidiagonal{T,V<:AbstractVector{T}} <: AbstractMatrix{T}
    dv::V      # diagonal
    ev::V      # sub/super diagonal
    uplo::Char # upper bidiagonal ('U') or lower ('L')
    function Bidiagonal{T,V}(dv, ev, uplo::AbstractChar) where {T,V<:AbstractVector{T}}
        require_one_based_indexing(dv, ev)
        if length(ev) != length(dv)-1
            throw(DimensionMismatch("length of diagonal vector is $(length(dv)), length of off-diagonal vector is $(length(ev))"))
        end
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

julia> Bu = Bidiagonal(dv, ev, :U) # ev is on the first superdiagonal
4×4 Bidiagonal{Int64,Array{Int64,1}}:
 1  7  ⋅  ⋅
 ⋅  2  8  ⋅
 ⋅  ⋅  3  9
 ⋅  ⋅  ⋅  4

julia> Bl = Bidiagonal(dv, ev, :L) # ev is on the first subdiagonal
4×4 Bidiagonal{Int64,Array{Int64,1}}:
 1  ⋅  ⋅  ⋅
 7  2  ⋅  ⋅
 ⋅  8  3  ⋅
 ⋅  ⋅  9  4
```
"""
function Bidiagonal(dv::V, ev::V, uplo::Symbol) where {T,V<:AbstractVector{T}}
    Bidiagonal{T,V}(dv, ev, char_uplo(uplo))
end
function Bidiagonal(dv::V, ev::V, uplo::AbstractChar) where {T,V<:AbstractVector{T}}
    Bidiagonal{T,V}(dv, ev, uplo)
end

"""
    Bidiagonal(A, uplo::Symbol)

Construct a `Bidiagonal` matrix from the main diagonal of `A` and
its first super- (if `uplo=:U`) or sub-diagonal (if `uplo=:L`).

# Examples
```jldoctest
julia> A = [1 1 1 1; 2 2 2 2; 3 3 3 3; 4 4 4 4]
4×4 Array{Int64,2}:
 1  1  1  1
 2  2  2  2
 3  3  3  3
 4  4  4  4

julia> Bidiagonal(A, :U) # contains the main diagonal and first superdiagonal of A
4×4 Bidiagonal{Int64,Array{Int64,1}}:
 1  1  ⋅  ⋅
 ⋅  2  2  ⋅
 ⋅  ⋅  3  3
 ⋅  ⋅  ⋅  4

julia> Bidiagonal(A, :L) # contains the main diagonal and first subdiagonal of A
4×4 Bidiagonal{Int64,Array{Int64,1}}:
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

function getindex(A::Bidiagonal{T}, i::Integer, j::Integer) where T
    if !((1 <= i <= size(A,2)) && (1 <= j <= size(A,2)))
        throw(BoundsError(A,(i,j)))
    end
    if i == j
        return A.dv[i]
    elseif A.uplo == 'U' && (i == j - 1)
        return A.ev[i]
    elseif A.uplo == 'L' && (i == j + 1)
        return A.ev[j]
    else
        return zero(T)
    end
end

function setindex!(A::Bidiagonal, x, i::Integer, j::Integer)
    @boundscheck checkbounds(A, i, j)
    if i == j
        @inbounds A.dv[i] = x
    elseif A.uplo == 'U' && (i == j - 1)
        @inbounds A.ev[i] = x
    elseif A.uplo == 'L' && (i == j + 1)
        @inbounds A.ev[j] = x
    elseif !iszero(x)
        throw(ArgumentError(string("cannot set entry ($i, $j) off the ",
            "$(istriu(A) ? "upper" : "lower") bidiagonal band to a nonzero value ($x)")))
    end
    return x
end

## structured matrix methods ##
function Base.replace_in_print_matrix(A::Bidiagonal,i::Integer,j::Integer,s::AbstractString)
    if A.uplo == 'U'
        i==j || i==j-1 ? s : Base.replace_with_centered_mark(s)
    else
        i==j || i==j+1 ? s : Base.replace_with_centered_mark(s)
    end
end

#Converting from Bidiagonal to dense Matrix
function Matrix{T}(A::Bidiagonal) where T
    n = size(A, 1)
    B = zeros(T, n, n)
    if n == 0
        return B
    end
    for i = 1:n - 1
        B[i,i] = A.dv[i]
        if A.uplo == 'U'
            B[i, i + 1] = A.ev[i]
        else
            B[i + 1, i] = A.ev[i]
        end
    end
    B[n,n] = A.dv[n]
    return B
end
Matrix(A::Bidiagonal{T}) where {T} = Matrix{T}(A)
Array(A::Bidiagonal) = Matrix(A)
promote_rule(::Type{Matrix{T}}, ::Type{<:Bidiagonal{S}}) where {T,S} =
    @isdefined(T) && @isdefined(S) ? Matrix{promote_type(T,S)} : Matrix
promote_rule(::Type{Matrix}, ::Type{<:Bidiagonal}) = Matrix

#Converting from Bidiagonal to Tridiagonal
function Tridiagonal{T}(A::Bidiagonal) where T
    dv = convert(AbstractVector{T}, A.dv)
    ev = convert(AbstractVector{T}, A.ev)
    z = fill!(similar(ev), zero(T))
    A.uplo == 'U' ? Tridiagonal(z, dv, ev) : Tridiagonal(ev, dv, z)
end
promote_rule(::Type{<:Tridiagonal{T}}, ::Type{<:Bidiagonal{S}}) where {T,S} =
    @isdefined(T) && @isdefined(S) ? Tridiagonal{promote_type(T,S)} : Tridiagonal
promote_rule(::Type{<:Tridiagonal}, ::Type{<:Bidiagonal}) = Tridiagonal

# When asked to convert Bidiagonal to AbstractMatrix{T}, preserve structure by converting to Bidiagonal{T} <: AbstractMatrix{T}
AbstractMatrix{T}(A::Bidiagonal) where {T} = convert(Bidiagonal{T}, A)

convert(T::Type{<:Bidiagonal}, m::AbstractMatrix) = m isa T ? m : T(m)

# For B<:Bidiagonal, similar(B[, neweltype]) should yield a Bidiagonal matrix.
# On the other hand, similar(B, [neweltype,] shape...) should yield a sparse matrix.
# The first method below effects the former, and the second the latter.
similar(B::Bidiagonal, ::Type{T}) where {T} = Bidiagonal(similar(B.dv, T), similar(B.ev, T), B.uplo)
# The method below is moved to SparseArrays for now
# similar(B::Bidiagonal, ::Type{T}, dims::Union{Dims{1},Dims{2}}) where {T} = spzeros(T, dims...)


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
    # TODO: make this readable and one-line
    summary(io, M); println(io, ":")
    print(io, " diag:")
    print_matrix(io, (M.dv)')
    print(io, M.uplo == 'U' ? "\n super:" : "\n sub:")
    print_matrix(io, (M.ev)')
end

size(M::Bidiagonal) = (length(M.dv), length(M.dv))
function size(M::Bidiagonal, d::Integer)
    if d < 1
        throw(ArgumentError("dimension must be ≥ 1, got $d"))
    elseif d <= 2
        return length(M.dv)
    else
        return 1
    end
end

#Elementary operations
for func in (:conj, :copy, :real, :imag)
    @eval ($func)(M::Bidiagonal) = Bidiagonal(($func)(M.dv), ($func)(M.ev), M.uplo)
end

adjoint(B::Bidiagonal) = Adjoint(B)
transpose(B::Bidiagonal) = Transpose(B)
adjoint(B::Bidiagonal{<:Real}) = Bidiagonal(B.dv, B.ev, B.uplo == 'U' ? :L : :U)
transpose(B::Bidiagonal{<:Number}) = Bidiagonal(B.dv, B.ev, B.uplo == 'U' ? :L : :U)
function Base.copy(aB::Adjoint{<:Any,<:Bidiagonal})
    B = aB.parent
    return Bidiagonal(map(x -> copy.(adjoint.(x)), (B.dv, B.ev))..., B.uplo == 'U' ? :L : :U)
end
function Base.copy(tB::Transpose{<:Any,<:Bidiagonal})
    B = tB.parent
    return Bidiagonal(map(x -> copy.(transpose.(x)), (B.dv, B.ev))..., B.uplo == 'U' ? :L : :U)
end

iszero(M::Bidiagonal) = iszero(M.dv) && iszero(M.ev)
isone(M::Bidiagonal) = all(isone, M.dv) && iszero(M.ev)
istriu(M::Bidiagonal) = M.uplo == 'U' || iszero(M.ev)
istril(M::Bidiagonal) = M.uplo == 'L' || iszero(M.ev)
isdiag(M::Bidiagonal) = iszero(M.ev)

function tril!(M::Bidiagonal, k::Integer=0)
    n = length(M.dv)
    if !(-n - 1 <= k <= n - 1)
        throw(ArgumentError(string("the requested diagonal, $k, must be at least ",
            "$(-n - 1) and at most $(n - 1) in an $n-by-$n matrix")))
    elseif M.uplo == 'U' && k < 0
        fill!(M.dv,0)
        fill!(M.ev,0)
    elseif k < -1
        fill!(M.dv,0)
        fill!(M.ev,0)
    elseif M.uplo == 'U' && k == 0
        fill!(M.ev,0)
    elseif M.uplo == 'L' && k == -1
        fill!(M.dv,0)
    end
    return M
end

function triu!(M::Bidiagonal, k::Integer=0)
    n = length(M.dv)
    if !(-n + 1 <= k <= n + 1)
        throw(ArgumentError(string("the requested diagonal, $k, must be at least",
            "$(-n + 1) and at most $(n + 1) in an $n-by-$n matrix")))
    elseif M.uplo == 'L' && k > 0
        fill!(M.dv,0)
        fill!(M.ev,0)
    elseif k > 1
        fill!(M.dv,0)
        fill!(M.ev,0)
    elseif M.uplo == 'L' && k == 0
        fill!(M.ev,0)
    elseif M.uplo == 'U' && k == 1
        fill!(M.dv,0)
    end
    return M
end

function diag(M::Bidiagonal, n::Integer=0)
    # every branch call similar(..., ::Int) to make sure the
    # same vector type is returned independent of n
    if n == 0
        return copyto!(similar(M.dv, length(M.dv)), M.dv)
    elseif (n == 1 && M.uplo == 'U') ||  (n == -1 && M.uplo == 'L')
        return copyto!(similar(M.ev, length(M.ev)), M.ev)
    elseif -size(M,1) <= n <= size(M,1)
        return fill!(similar(M.dv, size(M,1)-abs(n)), 0)
    else
        throw(ArgumentError(string("requested diagonal, $n, must be at least $(-size(M, 1)) ",
            "and at most $(size(M, 2)) for an $(size(M, 1))-by-$(size(M, 2)) matrix")))
    end
end

function +(A::Bidiagonal, B::Bidiagonal)
    if A.uplo == B.uplo
        Bidiagonal(A.dv+B.dv, A.ev+B.ev, A.uplo)
    else
        newdv = A.dv+B.dv
        Tridiagonal((A.uplo == 'U' ? (typeof(newdv)(B.ev), newdv, typeof(newdv)(A.ev)) : (typeof(newdv)(A.ev), newdv, typeof(newdv)(B.ev)))...)
    end
end

function -(A::Bidiagonal, B::Bidiagonal)
    if A.uplo == B.uplo
        Bidiagonal(A.dv-B.dv, A.ev-B.ev, A.uplo)
    else
        newdv = A.dv-B.dv
        Tridiagonal((A.uplo == 'U' ? (typeof(newdv)(-B.ev), newdv, typeof(newdv)(A.ev)) : (typeof(newdv)(A.ev), newdv, typeof(newdv)(-B.ev)))...)
    end
end

-(A::Bidiagonal)=Bidiagonal(-A.dv,-A.ev,A.uplo)
*(A::Bidiagonal, B::Number) = Bidiagonal(A.dv*B, A.ev*B, A.uplo)
*(B::Number, A::Bidiagonal) = A*B
/(A::Bidiagonal, B::Number) = Bidiagonal(A.dv/B, A.ev/B, A.uplo)

function ==(A::Bidiagonal, B::Bidiagonal)
    if A.uplo == B.uplo
        return A.dv == B.dv && A.ev == B.ev
    else
        return iszero(A.ev) && iszero(B.ev) && A.dv == B.dv
    end
end

const BiTriSym = Union{Bidiagonal,Tridiagonal,SymTridiagonal}
const BiTri = Union{Bidiagonal,Tridiagonal}
@inline mul!(C::AbstractMatrix,   A::SymTridiagonal,     B::BiTriSym, alpha::Number, beta::Number) = A_mul_B_td!(C, A, B, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix,   A::BiTriSym,           B::BiTriSym, alpha::Number, beta::Number) = A_mul_B_td!(C, A, B, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix,   A::AbstractTriangular, B::BiTriSym, alpha::Number, beta::Number) = A_mul_B_td!(C, A, B, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix,   A::AbstractMatrix,     B::BiTriSym, alpha::Number, beta::Number) = A_mul_B_td!(C, A, B, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix,   A::Diagonal,           B::BiTriSym, alpha::Number, beta::Number) = A_mul_B_td!(C, A, B, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix, A::Adjoint{<:Any,<:Diagonal}, B::BiTriSym, alpha::Number, beta::Number) = A_mul_B_td!(C, A, B, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix, A::Transpose{<:Any,<:Diagonal}, B::BiTriSym, alpha::Number, beta::Number) = A_mul_B_td!(C, A, B, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix, A::Adjoint{<:Any,<:AbstractTriangular}, B::BiTriSym, alpha::Number, beta::Number) = A_mul_B_td!(C, A, B, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix, A::Transpose{<:Any,<:AbstractTriangular}, B::BiTriSym, alpha::Number, beta::Number) = A_mul_B_td!(C, A, B, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix, A::Adjoint{<:Any,<:AbstractVecOrMat}, B::BiTriSym, alpha::Number, beta::Number) = A_mul_B_td!(C, A, B, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix, A::Transpose{<:Any,<:AbstractVecOrMat}, B::BiTriSym, alpha::Number, beta::Number) = A_mul_B_td!(C, A, B, MulAddMul(alpha, beta))
@inline mul!(C::AbstractVector,   A::BiTriSym,              B::AbstractVector, alpha::Number, beta::Number) = A_mul_B_td!(C, A, B, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix,   A::BiTriSym,              B::AbstractVecOrMat, alpha::Number, beta::Number) = A_mul_B_td!(C, A, B, MulAddMul(alpha, beta))
@inline mul!(C::AbstractVecOrMat, A::BiTriSym,              B::AbstractVecOrMat, alpha::Number, beta::Number) = A_mul_B_td!(C, A, B, MulAddMul(alpha, beta))
@inline mul!(C::AbstractMatrix, A::BiTriSym, B::Transpose{<:Any,<:AbstractVecOrMat}, alpha::Number, beta::Number) = A_mul_B_td!(C, A, B, MulAddMul(alpha, beta)) # around bidiag line 330
@inline mul!(C::AbstractMatrix, A::BiTriSym, B::Adjoint{<:Any,<:AbstractVecOrMat}, alpha::Number, beta::Number) = A_mul_B_td!(C, A, B, MulAddMul(alpha, beta))
@inline mul!(C::AbstractVector, A::BiTriSym, B::Transpose{<:Any,<:AbstractVecOrMat}, alpha::Number, beta::Number) = throw(MethodError(mul!, (C, A, B)), MulAddMul(alpha, beta))

function check_A_mul_B!_sizes(C, A, B)
    require_one_based_indexing(C)
    require_one_based_indexing(A)
    require_one_based_indexing(B)
    nA, mA = size(A)
    nB, mB = size(B)
    nC, mC = size(C)
    if nA != nC
        throw(DimensionMismatch("sizes size(A)=$(size(A)) and size(C) = $(size(C)) must match at first entry."))
    elseif mA != nB
        throw(DimensionMismatch("second entry of size(A)=$(size(A)) and first entry of size(B) = $(size(B)) must match."))
    elseif mB != mC
        throw(DimensionMismatch("sizes size(B)=$(size(B)) and size(C) = $(size(C)) must match at first second entry."))
    end
end

# function to get the internally stored vectors for Bidiagonal and [Sym]Tridiagonal
# to avoid allocations in A_mul_B_td! below (#24324, #24578)
_diag(A::Tridiagonal, k) = k == -1 ? A.dl : k == 0 ? A.d : A.du
_diag(A::SymTridiagonal, k) = k == 0 ? A.dv : A.ev
function _diag(A::Bidiagonal, k)
    if k == 0
        return A.dv
    elseif (A.uplo == 'L' && k == -1) || (A.uplo == 'U' && k == 1)
        return A.ev
    else
        return diag(A, k)
    end
end

function A_mul_B_td!(C::AbstractMatrix, A::BiTriSym, B::BiTriSym,
                     _add::MulAddMul = MulAddMul())
    check_A_mul_B!_sizes(C, A, B)
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

function A_mul_B_td!(C::AbstractMatrix, A::BiTriSym, B::Diagonal,
                     _add::MulAddMul = MulAddMul())
    check_A_mul_B!_sizes(C, A, B)
    n = size(A,1)
    n <= 3 && return mul!(C, Array(A), Array(B), _add.alpha, _add.beta)
    _rmul_or_fill!(C, _add.beta)  # see the same use above
    iszero(_add.alpha) && return C
    Al = _diag(A, -1)
    Ad = _diag(A, 0)
    Au = _diag(A, 1)
    Bd = B.diag
    @inbounds begin
        # first row of C
        C[1,1] += _add(A[1,1]*B[1,1])
        C[1,2] += _add(A[1,2]*B[2,2])
        # second row of C
        C[2,1] += _add(A[2,1]*B[1,1])
        C[2,2] += _add(A[2,2]*B[2,2])
        C[2,3] += _add(A[2,3]*B[3,3])
        for j in 3:n-2
            C[j, j-1] += _add(Al[j-1]*Bd[j-1])
            C[j, j  ] += _add(Ad[j  ]*Bd[j  ])
            C[j, j+1] += _add(Au[j  ]*Bd[j+1])
        end
        # row before last of C
        C[n-1,n-2] += _add(A[n-1,n-2]*B[n-2,n-2])
        C[n-1,n-1] += _add(A[n-1,n-1]*B[n-1,n-1])
        C[n-1,n  ] += _add(A[n-1,  n]*B[n  ,n  ])
        # last row of C
        C[n,n-1] += _add(A[n,n-1]*B[n-1,n-1])
        C[n,n  ] += _add(A[n,n  ]*B[n,  n  ])
    end # inbounds
    C
end

function A_mul_B_td!(C::AbstractVecOrMat, A::BiTriSym, B::AbstractVecOrMat,
                     _add::MulAddMul = MulAddMul())
    require_one_based_indexing(C)
    require_one_based_indexing(B)
    nA = size(A,1)
    nB = size(B,2)
    if !(size(C,1) == size(B,1) == nA)
        throw(DimensionMismatch("A has first dimension $nA, B has $(size(B,1)), C has $(size(C,1)) but all must match"))
    end
    if size(C,2) != nB
        throw(DimensionMismatch("A has second dimension $nA, B has $(size(B,2)), C has $(size(C,2)) but all must match"))
    end
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

function A_mul_B_td!(C::AbstractMatrix, A::AbstractMatrix, B::BiTriSym,
                     _add::MulAddMul = MulAddMul())
    check_A_mul_B!_sizes(C, A, B)
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

function A_mul_B_td!(C::AbstractMatrix, A::Diagonal, B::BiTriSym,
                     _add::MulAddMul = MulAddMul())
    check_A_mul_B!_sizes(C, A, B)
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

function *(A::AbstractTriangular, B::Union{SymTridiagonal, Tridiagonal})
    TS = promote_op(matprod, eltype(A), eltype(B))
    A_mul_B_td!(zeros(TS, size(A)...), A, B)
end

const UpperOrUnitUpperTriangular = Union{UpperTriangular, UnitUpperTriangular}
const LowerOrUnitLowerTriangular = Union{LowerTriangular, UnitLowerTriangular}
const AdjOrTransUpperOrUnitUpperTriangular = Union{Adjoint{<:Any, <:UpperOrUnitUpperTriangular}, Transpose{<:Any, <:UpperOrUnitUpperTriangular}}
const AdjOrTransLowerOrUnitLowerTriangular = Union{Adjoint{<:Any, <:LowerOrUnitLowerTriangular}, Transpose{<:Any, <:LowerOrUnitLowerTriangular}}

function *(A::UpperOrUnitUpperTriangular, B::Bidiagonal)
    TS = promote_op(matprod, eltype(A), eltype(B))
    if B.uplo == 'U'
        A_mul_B_td!(UpperTriangular(zeros(TS, size(A)...)), A, B)
    else
        A_mul_B_td!(zeros(TS, size(A)...), A, B)
    end
end

function *(A::AdjOrTransUpperOrUnitUpperTriangular, B::Bidiagonal)
    TS = promote_op(matprod, eltype(A), eltype(B))
    if B.uplo == 'L'
        A_mul_B_td!(LowerTriangular(zeros(TS, size(A)...)), A, B)
    else
        A_mul_B_td!(zeros(TS, size(A)...), A, B)
    end
end

function *(A::LowerOrUnitLowerTriangular, B::Bidiagonal)
    TS = promote_op(matprod, eltype(A), eltype(B))
    if B.uplo == 'L'
        A_mul_B_td!(LowerTriangular(zeros(TS, size(A)...)), A, B)
    else
        A_mul_B_td!(zeros(TS, size(A)...), A, B)
    end
end

function *(A::AdjOrTransLowerOrUnitLowerTriangular, B::Bidiagonal)
    TS = promote_op(matprod, eltype(A), eltype(B))
    if B.uplo == 'U'
        A_mul_B_td!(UpperTriangular(zeros(TS, size(A)...)), A, B)
    else
        A_mul_B_td!(zeros(TS, size(A)...), A, B)
    end
end

function *(A::Union{SymTridiagonal, Tridiagonal}, B::AbstractTriangular)
    TS = promote_op(matprod, eltype(A), eltype(B))
    A_mul_B_td!(zeros(TS, size(A)...), A, B)
end

function *(A::Bidiagonal, B::UpperOrUnitUpperTriangular)
    TS = promote_op(matprod, eltype(A), eltype(B))
    if A.uplo == 'U'
        A_mul_B_td!(UpperTriangular(zeros(TS, size(A)...)), A, B)
    else
        A_mul_B_td!(zeros(TS, size(A)...), A, B)
    end
end

function *(A::Bidiagonal, B::AdjOrTransUpperOrUnitUpperTriangular)
    TS = promote_op(matprod, eltype(A), eltype(B))
    if A.uplo == 'L'
        A_mul_B_td!(LowerTriangular(zeros(TS, size(A)...)), A, B)
    else
        A_mul_B_td!(zeros(TS, size(A)...), A, B)
    end
end

function *(A::Bidiagonal, B::LowerOrUnitLowerTriangular)
    TS = promote_op(matprod, eltype(A), eltype(B))
    if A.uplo == 'L'
        A_mul_B_td!(LowerTriangular(zeros(TS, size(A)...)), A, B)
    else
        A_mul_B_td!(zeros(TS, size(A)...), A, B)
    end
end

function *(A::Bidiagonal, B::AdjOrTransLowerOrUnitLowerTriangular)
    TS = promote_op(matprod, eltype(A), eltype(B))
    if A.uplo == 'U'
        A_mul_B_td!(UpperTriangular(zeros(TS, size(A)...)), A, B)
    else
        A_mul_B_td!(zeros(TS, size(A)...), A, B)
    end
end

function *(A::BiTri, B::Diagonal)
    TS = promote_op(matprod, eltype(A), eltype(B))
    A_mul_B_td!(similar(A, TS), A, B)
end

function *(A::Diagonal, B::BiTri)
    TS = promote_op(matprod, eltype(A), eltype(B))
    A_mul_B_td!(similar(B, TS), A, B)
end

function *(A::Diagonal, B::SymTridiagonal)
    TS = promote_op(matprod, eltype(A), eltype(B))
    A_mul_B_td!(Tridiagonal(zeros(TS, size(A, 1)-1), zeros(TS, size(A, 1)), zeros(TS, size(A, 1)-1)), A, B)
end

function *(A::SymTridiagonal, B::Diagonal)
    TS = promote_op(matprod, eltype(A), eltype(B))
    A_mul_B_td!(Tridiagonal(zeros(TS, size(A, 1)-1), zeros(TS, size(A, 1)), zeros(TS, size(A, 1)-1)), A, B)
end

function dot(x::AbstractVector, B::Bidiagonal, y::AbstractVector)
    require_one_based_indexing(x, y)
    nx, ny = length(x), length(y)
    (nx == size(B, 1) == ny) || throw(DimensionMismatch())
    if iszero(nx)
        return dot(zero(eltype(x)), zero(eltype(B)), zero(eltype(y)))
    end
    ev, dv = B.ev, B.dv
    if B.uplo == 'U'
        x₀ = x[1]
        r = dot(x[1], dv[1], y[1])
        @inbounds for j in 2:nx-1
            x₋, x₀ = x₀, x[j]
            r += dot(adjoint(ev[j-1])*x₋ + adjoint(dv[j])*x₀, y[j])
        end
        r += dot(adjoint(ev[nx-1])*x₀ + adjoint(dv[nx])*x[nx], y[nx])
        return r
    else # B.uplo == 'L'
        x₀ = x[1]
        x₊ = x[2]
        r = dot(adjoint(dv[1])*x₀ + adjoint(ev[1])*x₊, y[1])
        @inbounds for j in 2:nx-1
            x₀, x₊ = x₊, x[j+1]
            r += dot(adjoint(dv[j])*x₀ + adjoint(ev[j])*x₊, y[j])
        end
        r += dot(x₊, dv[nx], y[nx])
        return r
    end
end

#Linear solvers
ldiv!(A::Union{Bidiagonal, AbstractTriangular}, b::AbstractVector) = naivesub!(A, b)
ldiv!(A::Transpose{<:Any,<:Bidiagonal}, b::AbstractVector) = ldiv!(copy(A), b)
ldiv!(A::Adjoint{<:Any,<:Bidiagonal}, b::AbstractVector) = ldiv!(copy(A), b)
function ldiv!(A::Union{Bidiagonal,AbstractTriangular}, B::AbstractMatrix)
    require_one_based_indexing(A, B)
    nA,mA = size(A)
    tmp = similar(B,size(B,1))
    n = size(B, 1)
    if nA != n
        throw(DimensionMismatch("size of A is ($nA,$mA), corresponding dimension of B is $n"))
    end
    for i = 1:size(B,2)
        copyto!(tmp, 1, B, (i - 1)*n + 1, n)
        ldiv!(A, tmp)
        copyto!(B, (i - 1)*n + 1, tmp, 1, n) # Modify this when array view are implemented.
    end
    B
end
function ldiv!(adjA::Adjoint{<:Any,<:Union{Bidiagonal,AbstractTriangular}}, B::AbstractMatrix)
    require_one_based_indexing(adjA, B)
    A = adjA.parent
    nA,mA = size(A)
    tmp = similar(B,size(B,1))
    n = size(B, 1)
    if mA != n
        throw(DimensionMismatch("size of adjoint of A is ($mA,$nA), corresponding dimension of B is $n"))
    end
    for i = 1:size(B,2)
        copyto!(tmp, 1, B, (i - 1)*n + 1, n)
        ldiv!(adjoint(A), tmp)
        copyto!(B, (i - 1)*n + 1, tmp, 1, n) # Modify this when array view are implemented.
    end
    B
end
function ldiv!(transA::Transpose{<:Any,<:Union{Bidiagonal,AbstractTriangular}}, B::AbstractMatrix)
    require_one_based_indexing(transA, B)
    A = transA.parent
    nA,mA = size(A)
    tmp = similar(B,size(B,1))
    n = size(B, 1)
    if mA != n
        throw(DimensionMismatch("size of transpose of A is ($mA,$nA), corresponding dimension of B is $n"))
    end
    for i = 1:size(B,2)
        copyto!(tmp, 1, B, (i - 1)*n + 1, n)
        ldiv!(transpose(A), tmp)
        copyto!(B, (i - 1)*n + 1, tmp, 1, n) # Modify this when array view are implemented.
    end
    B
end
#Generic solver using naive substitution
function naivesub!(A::Bidiagonal{T}, b::AbstractVector, x::AbstractVector = b) where T
    require_one_based_indexing(A, b, x)
    N = size(A, 2)
    if N != length(b) || N != length(x)
        throw(DimensionMismatch("second dimension of A, $N, does not match one of the lengths of x, $(length(x)), or b, $(length(b))"))
    end

    if N == 0
        return x
    end

    @inbounds begin
        if A.uplo == 'L' #do forward substitution
            x[1] = xj1 = A.dv[1]\b[1]
            for j = 2:N
                xj  = b[j]
                xj -= A.ev[j - 1] * xj1
                dvj = A.dv[j]
                if iszero(dvj)
                    throw(SingularException(j))
                end
                xj   = dvj\xj
                x[j] = xj1 = xj
            end
        else #do backward substitution
            x[N] = xj1 = A.dv[N]\b[N]
            for j = (N - 1):-1:1
                xj  = b[j]
                xj -= A.ev[j] * xj1
                dvj = A.dv[j]
                if iszero(dvj)
                    throw(SingularException(j))
                end
                xj   = dvj\xj
                x[j] = xj1 = xj
            end
        end
    end
    return x
end

### Generic promotion methods and fallbacks
function \(A::Bidiagonal{<:Number}, B::AbstractVecOrMat{<:Number})
    TA, TB = eltype(A), eltype(B)
    TAB = typeof((zero(TA)*zero(TB) + zero(TA)*zero(TB))/one(TA))
    ldiv!(convert(AbstractArray{TAB}, A), copy_oftype(B, TAB))
end
\(A::Bidiagonal, B::AbstractVecOrMat) = ldiv!(A, copy(B))
function \(transA::Transpose{<:Number,<:Bidiagonal{<:Number}}, B::AbstractVecOrMat{<:Number})
    A = transA.parent
    TA, TB = eltype(A), eltype(B)
    TAB = typeof((zero(TA)*zero(TB) + zero(TA)*zero(TB))/one(TA))
    ldiv!(transpose(convert(AbstractArray{TAB}, A)), copy_oftype(B, TAB))
end
\(transA::Transpose{<:Any,<:Bidiagonal}, B::AbstractVecOrMat) = ldiv!(transpose(transA.parent), copy(B))
function \(adjA::Adjoint{<:Number,<:Bidiagonal{<:Number}}, B::AbstractVecOrMat{<:Number})
    A = adjA.parent
    TA, TB = eltype(A), eltype(B)
    TAB = typeof((zero(TA)*zero(TB) + zero(TA)*zero(TB))/one(TA))
    ldiv!(adjoint(convert(AbstractArray{TAB}, A)), copy_oftype(B, TAB))
end
\(adjA::Adjoint{<:Any,<:Bidiagonal}, B::AbstractVecOrMat) = ldiv!(adjoint(adjA.parent), copy(B))

factorize(A::Bidiagonal) = A

# Eigensystems
eigvals(M::Bidiagonal) = M.dv
function eigvecs(M::Bidiagonal{T}) where T
    n = length(M.dv)
    Q = Matrix{T}(undef, n,n)
    blks = [0; findall(x -> x == 0, M.ev); n]
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
