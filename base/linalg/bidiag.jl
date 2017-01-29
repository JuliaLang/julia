# This file is a part of Julia. License is MIT: http://julialang.org/license

# Bidiagonal matrices
type Bidiagonal{T} <: AbstractMatrix{T}
    dv::Vector{T} # diagonal
    ev::Vector{T} # sub/super diagonal
    isupper::Bool # is upper bidiagonal (true) or lower (false)
    function Bidiagonal{T}(dv::Vector{T}, ev::Vector{T}, isupper::Bool) where T
        if length(ev) != length(dv)-1
            throw(DimensionMismatch("length of diagonal vector is $(length(dv)), length of off-diagonal vector is $(length(ev))"))
        end
        new(dv, ev, isupper)
    end
end
"""
    Bidiagonal(dv, ev, isupper::Bool)

Constructs an upper (`isupper=true`) or lower (`isupper=false`) bidiagonal matrix using the
given diagonal (`dv`) and off-diagonal (`ev`) vectors.  The result is of type `Bidiagonal`
and provides efficient specialized linear solvers, but may be converted into a regular
matrix with [`convert(Array, _)`](@ref) (or `Array(_)` for short). `ev`'s length
must be one less than the length of `dv`.

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

julia> Bu = Bidiagonal(dv, ev, true) # ev is on the first superdiagonal
4×4 Bidiagonal{Int64}:
 1  7  ⋅  ⋅
 ⋅  2  8  ⋅
 ⋅  ⋅  3  9
 ⋅  ⋅  ⋅  4

julia> Bl = Bidiagonal(dv, ev, false) # ev is on the first subdiagonal
4×4 Bidiagonal{Int64}:
 1  ⋅  ⋅  ⋅
 7  2  ⋅  ⋅
 ⋅  8  3  ⋅
 ⋅  ⋅  9  4
```
"""
Bidiagonal(dv::AbstractVector{T}, ev::AbstractVector{T}, isupper::Bool) where T = Bidiagonal{T}(collect(dv), collect(ev), isupper)
Bidiagonal(dv::AbstractVector, ev::AbstractVector) = throw(ArgumentError("did you want an upper or lower Bidiagonal? Try again with an additional true (upper) or false (lower) argument."))

"""
    Bidiagonal(dv, ev, uplo::Char)

Constructs an upper (`uplo='U'`) or lower (`uplo='L'`) bidiagonal matrix using the
given diagonal (`dv`) and off-diagonal (`ev`) vectors.  The result is of type `Bidiagonal`
and provides efficient specialized linear solvers, but may be converted into a regular
matrix with [`convert(Array, _)`](@ref) (or `Array(_)` for short). `ev`'s
length must be one less than the length of `dv`.

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

julia> Bu = Bidiagonal(dv, ev, 'U') #e is on the first superdiagonal
4×4 Bidiagonal{Int64}:
 1  7  ⋅  ⋅
 ⋅  2  8  ⋅
 ⋅  ⋅  3  9
 ⋅  ⋅  ⋅  4

julia> Bl = Bidiagonal(dv, ev, 'L') #e is on the first subdiagonal
4×4 Bidiagonal{Int64}:
 1  ⋅  ⋅  ⋅
 7  2  ⋅  ⋅
 ⋅  8  3  ⋅
 ⋅  ⋅  9  4
```
"""
#Convert from BLAS uplo flag to boolean internal
Bidiagonal(dv::AbstractVector, ev::AbstractVector, uplo::Char) = begin
    if uplo === 'U'
        isupper = true
    elseif uplo === 'L'
        isupper = false
    else
        throw(ArgumentError("Bidiagonal uplo argument must be upper 'U' or lower 'L', got $(repr(uplo))"))
    end
    Bidiagonal(collect(dv), collect(ev), isupper)
end
function Bidiagonal(dv::AbstractVector{Td}, ev::AbstractVector{Te}, isupper::Bool) where {Td,Te}
    T = promote_type(Td,Te)
    Bidiagonal(convert(Vector{T}, dv), convert(Vector{T}, ev), isupper)
end

"""
    Bidiagonal(A, isupper::Bool)

Construct a `Bidiagonal` matrix from the main diagonal of `A` and
its first super- (if `isupper=true`) or sub-diagonal (if `isupper=false`).

# Example

```jldoctest
julia> A = [1 1 1 1; 2 2 2 2; 3 3 3 3; 4 4 4 4]
4×4 Array{Int64,2}:
 1  1  1  1
 2  2  2  2
 3  3  3  3
 4  4  4  4

julia> Bidiagonal(A, true) #contains the main diagonal and first superdiagonal of A
4×4 Bidiagonal{Int64}:
 1  1  ⋅  ⋅
 ⋅  2  2  ⋅
 ⋅  ⋅  3  3
 ⋅  ⋅  ⋅  4

julia> Bidiagonal(A, false) #contains the main diagonal and first subdiagonal of A
4×4 Bidiagonal{Int64}:
 1  ⋅  ⋅  ⋅
 2  2  ⋅  ⋅
 ⋅  3  3  ⋅
 ⋅  ⋅  4  4
```
"""
Bidiagonal(A::AbstractMatrix, isupper::Bool)=Bidiagonal(diag(A), diag(A, isupper?1:-1), isupper)

function getindex{T}(A::Bidiagonal{T}, i::Integer, j::Integer)
    if !((1 <= i <= size(A,2)) && (1 <= j <= size(A,2)))
        throw(BoundsError(A,(i,j)))
    end
    if i == j
        return A.dv[i]
    elseif (istriu(A) && (i == j - 1)) || (istril(A) && (i == j + 1))
        return A.ev[min(i,j)]
    else
        return zero(T)
    end
end

function setindex!(A::Bidiagonal, x, i::Integer, j::Integer)
    if i == j
        A.dv[i] = x
    elseif (istriu(A) && (i == j - 1)) || (istril(A) && (i == j + 1))
        return A.ev[min(i,j)] = x
    else
        throw(ArgumentError("cannot set elements outside main and $(istriu(A) ? "super": "sub") diagonals."))
    end
end

## structured matrix methods ##
function Base.replace_in_print_matrix(A::Bidiagonal,i::Integer,j::Integer,s::AbstractString)
    if A.isupper
        i==j || i==j-1 ? s : Base.replace_with_centered_mark(s)
    else
        i==j || i==j+1 ? s : Base.replace_with_centered_mark(s)
    end
end

#Converting from Bidiagonal to dense Matrix
function convert{T}(::Type{Matrix{T}}, A::Bidiagonal)
    n = size(A, 1)
    B = zeros(T, n, n)
    for i = 1:n - 1
        B[i,i] = A.dv[i]
        if A.isupper
            B[i, i + 1] = A.ev[i]
        else
            B[i + 1, i] = A.ev[i]
        end
    end
    B[n,n] = A.dv[n]
    return B
end
convert{T}(::Type{Matrix}, A::Bidiagonal{T}) = convert(Matrix{T}, A)
convert(::Type{Array}, A::Bidiagonal) = convert(Matrix, A)
full(A::Bidiagonal) = convert(Array, A)
promote_rule{T,S}(::Type{Matrix{T}}, ::Type{Bidiagonal{S}})=Matrix{promote_type(T,S)}

#Converting from Bidiagonal to Tridiagonal
Tridiagonal(M::Bidiagonal{T}) where T = convert(Tridiagonal{T}, M)
function convert{T}(::Type{Tridiagonal{T}}, A::Bidiagonal)
    z = zeros(T, size(A)[1]-1)
    A.isupper ? Tridiagonal(z, convert(Vector{T},A.dv), convert(Vector{T},A.ev)) : Tridiagonal(convert(Vector{T},A.ev), convert(Vector{T},A.dv), z)
end
promote_rule{T,S}(::Type{Tridiagonal{T}}, ::Type{Bidiagonal{S}})=Tridiagonal{promote_type(T,S)}

# No-op for trivial conversion Bidiagonal{T} -> Bidiagonal{T}
convert{T}(::Type{Bidiagonal{T}}, A::Bidiagonal{T}) = A
# Convert Bidiagonal{Told} to Bidiagonal{Tnew} by constructing a new instance with converted elements
convert{Tnew,Told}(::Type{Bidiagonal{Tnew}}, A::Bidiagonal{Told}) = Bidiagonal(convert(Vector{Tnew}, A.dv), convert(Vector{Tnew}, A.ev), A.isupper)
# When asked to convert Bidiagonal{Told} to AbstractMatrix{Tnew}, preserve structure by converting to Bidiagonal{Tnew} <: AbstractMatrix{Tnew}
convert{Tnew,Told}(::Type{AbstractMatrix{Tnew}}, A::Bidiagonal{Told}) = convert(Bidiagonal{Tnew}, A)

broadcast(::typeof(big), B::Bidiagonal) = Bidiagonal(big.(B.dv), big.(B.ev), B.isupper)

similar{T}(B::Bidiagonal, ::Type{T}) = Bidiagonal{T}(similar(B.dv, T), similar(B.ev, T), B.isupper)

###################
# LAPACK routines #
###################

#Singular values
svdvals!{T<:BlasReal}(M::Bidiagonal{T}) = LAPACK.bdsdc!(M.isupper ? 'U' : 'L', 'N', M.dv, M.ev)[1]
function svdfact!{T<:BlasReal}(M::Bidiagonal{T}; thin::Bool=true)
    d, e, U, Vt, Q, iQ = LAPACK.bdsdc!(M.isupper ? 'U' : 'L', 'I', M.dv, M.ev)
    SVD(U, d, Vt)
end
svdfact(M::Bidiagonal; thin::Bool=true) = svdfact!(copy(M),thin=thin)

####################
# Generic routines #
####################

function show(io::IO, M::Bidiagonal)
    # TODO: make this readable and one-line
    println(io, summary(M), ":")
    print(io, " diag:")
    print_matrix(io, (M.dv)')
    print(io, M.isupper?"\n super:":"\n sub:")
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
broadcast(::typeof(abs), M::Bidiagonal) = Bidiagonal(abs.(M.dv), abs.(M.ev), abs.(M.isupper))
broadcast(::typeof(round), M::Bidiagonal) = Bidiagonal(round.(M.dv), round.(M.ev), M.isupper)
broadcast(::typeof(trunc), M::Bidiagonal) = Bidiagonal(trunc.(M.dv), trunc.(M.ev), M.isupper)
broadcast(::typeof(floor), M::Bidiagonal) = Bidiagonal(floor.(M.dv), floor.(M.ev), M.isupper)
broadcast(::typeof(ceil), M::Bidiagonal) = Bidiagonal(ceil.(M.dv), ceil.(M.ev), M.isupper)
for func in (:conj, :copy, :real, :imag)
    @eval ($func)(M::Bidiagonal) = Bidiagonal(($func)(M.dv), ($func)(M.ev), M.isupper)
end
broadcast{T<:Integer}(::typeof(round), ::Type{T}, M::Bidiagonal) = Bidiagonal(round.(T, M.dv), round.(T, M.ev), M.isupper)
broadcast{T<:Integer}(::typeof(trunc), ::Type{T}, M::Bidiagonal) = Bidiagonal(trunc.(T, M.dv), trunc.(T, M.ev), M.isupper)
broadcast{T<:Integer}(::typeof(floor), ::Type{T}, M::Bidiagonal) = Bidiagonal(floor.(T, M.dv), floor.(T, M.ev), M.isupper)
broadcast{T<:Integer}(::typeof(ceil), ::Type{T}, M::Bidiagonal) = Bidiagonal(ceil.(T, M.dv), ceil.(T, M.ev), M.isupper)

transpose(M::Bidiagonal) = Bidiagonal(M.dv, M.ev, !M.isupper)
ctranspose(M::Bidiagonal) = Bidiagonal(conj(M.dv), conj(M.ev), !M.isupper)

istriu(M::Bidiagonal) = M.isupper || iszero(M.ev)
istril(M::Bidiagonal) = !M.isupper || iszero(M.ev)

function tril!(M::Bidiagonal, k::Integer=0)
    n = length(M.dv)
    if abs(k) > n
        throw(ArgumentError("requested diagonal, $k, out of bounds in matrix of size ($n,$n)"))
    elseif M.isupper && k < 0
        fill!(M.dv,0)
        fill!(M.ev,0)
    elseif k < -1
        fill!(M.dv,0)
        fill!(M.ev,0)
    elseif M.isupper && k == 0
        fill!(M.ev,0)
    elseif !M.isupper && k == -1
        fill!(M.dv,0)
    end
    return M
end

function triu!(M::Bidiagonal, k::Integer=0)
    n = length(M.dv)
    if abs(k) > n
        throw(ArgumentError("requested diagonal, $k, out of bounds in matrix of size ($n,$n)"))
    elseif !M.isupper && k > 0
        fill!(M.dv,0)
        fill!(M.ev,0)
    elseif k > 1
        fill!(M.dv,0)
        fill!(M.ev,0)
    elseif !M.isupper && k == 0
        fill!(M.ev,0)
    elseif M.isupper && k == 1
        fill!(M.dv,0)
    end
    return M
end

function diag{T}(M::Bidiagonal{T}, n::Integer=0)
    if n == 0
        return M.dv
    elseif n == 1
        return M.isupper ? M.ev : zeros(T, size(M,1)-1)
    elseif n == -1
        return M.isupper ? zeros(T, size(M,1)-1) : M.ev
    elseif -size(M,1) < n < size(M,1)
        return zeros(T, size(M,1)-abs(n))
    else
        throw(ArgumentError("matrix size is $(size(M)), n is $n"))
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

-(A::Bidiagonal)=Bidiagonal(-A.dv,-A.ev,A.isupper)
*(A::Bidiagonal, B::Number) = Bidiagonal(A.dv*B, A.ev*B, A.isupper)
*(B::Number, A::Bidiagonal) = A*B
/(A::Bidiagonal, B::Number) = Bidiagonal(A.dv/B, A.ev/B, A.isupper)
==(A::Bidiagonal, B::Bidiagonal) = (A.dv==B.dv) && (A.ev==B.ev) && (A.isupper==B.isupper)

BiTriSym = Union{Bidiagonal, Tridiagonal, SymTridiagonal}
BiTri = Union{Bidiagonal, Tridiagonal}
A_mul_B!(C::AbstractMatrix, A::SymTridiagonal, B::BiTriSym) = A_mul_B_td!(C, A, B)
A_mul_B!(C::AbstractMatrix, A::BiTri, B::BiTriSym) = A_mul_B_td!(C, A, B)
A_mul_B!(C::AbstractMatrix, A::BiTriSym, B::BiTriSym) = A_mul_B_td!(C, A, B)
A_mul_B!(C::AbstractMatrix, A::AbstractTriangular, B::BiTriSym) = A_mul_B_td!(C, A, B)
A_mul_B!(C::AbstractMatrix, A::AbstractMatrix, B::BiTriSym) = A_mul_B_td!(C, A, B)
A_mul_B!(C::AbstractVector, A::BiTri, B::AbstractVector) = A_mul_B_td!(C, A, B)
A_mul_B!(C::AbstractMatrix, A::BiTri, B::AbstractVecOrMat) = A_mul_B_td!(C, A, B)
A_mul_B!(C::AbstractVecOrMat, A::BiTri, B::AbstractVecOrMat) = A_mul_B_td!(C, A, B)

\(::Diagonal, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
\(::Bidiagonal, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
\{TA<:Number,TB<:Number}(::Bidiagonal{TA}, ::RowVector{TB}) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))

At_ldiv_B(::Bidiagonal, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
At_ldiv_B{TA<:Number,TB<:Number}(::Bidiagonal{TA}, ::RowVector{TB}) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))

Ac_ldiv_B(::Bidiagonal, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
Ac_ldiv_B{TA<:Number,TB<:Number}(::Bidiagonal{TA}, ::RowVector{TB}) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))

function check_A_mul_B!_sizes(C, A, B)
    nA, mA = size(A)
    nB, mB = size(B)
    nC, mC = size(C)
    if !(nA == nC)
        throw(DimensionMismatch("sizes size(A)=$(size(A)) and size(C) = $(size(C)) must match at first entry."))
    elseif !(mA == nB)
        throw(DimensionMismatch("second entry of size(A)=$(size(A)) and first entry of size(B) = $(size(B)) must match."))
    elseif !(mB == mC)
        throw(DimensionMismatch("sizes size(B)=$(size(B)) and size(C) = $(size(C)) must match at first second entry."))
    end
end

function A_mul_B_td!(C::AbstractMatrix, A::BiTriSym, B::BiTriSym)
    check_A_mul_B!_sizes(C, A, B)
    n = size(A,1)
    n <= 3 && return A_mul_B!(C, Array(A), Array(B))
    fill!(C, zero(eltype(C)))
    Al = diag(A, -1)
    Ad = diag(A, 0)
    Au = diag(A, 1)
    Bl = diag(B, -1)
    Bd = diag(B, 0)
    Bu = diag(B, 1)
    @inbounds begin
        # first row of C
        C[1,1] = A[1,1]*B[1,1] + A[1, 2]*B[2, 1]
        C[1,2] = A[1,1]*B[1,2] + A[1,2]*B[2,2]
        C[1,3] = A[1,2]*B[2,3]
        # second row of C
        C[2,1] = A[2,1]*B[1,1] + A[2,2]*B[2,1]
        C[2,2] = A[2,1]*B[1,2] + A[2,2]*B[2,2] + A[2,3]*B[3,2]
        C[2,3] = A[2,2]*B[2,3] + A[2,3]*B[3,3]
        C[2,4] = A[2,3]*B[3,4]
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
            C[j,j-2]  = Ajj₋1*Bj₋1j₋2
            C[j, j-1] = Ajj₋1*Bj₋1j₋1 + Ajj*Bjj₋1
            C[j, j  ] = Ajj₋1*Bj₋1j   + Ajj*Bjj       + Ajj₊1*Bj₊1j
            C[j, j+1] = Ajj  *Bjj₊1   + Ajj₊1*Bj₊1j₊1
            C[j, j+2] = Ajj₊1*Bj₊1j₊2
        end
        # row before last of C
        C[n-1,n-3] = A[n-1,n-2]*B[n-2,n-3]
        C[n-1,n-2] = A[n-1,n-1]*B[n-1,n-2] + A[n-1,n-2]*B[n-2,n-2]
        C[n-1,n-1] = A[n-1,n-2]*B[n-2,n-1] + A[n-1,n-1]*B[n-1,n-1] + A[n-1,n]*B[n,n-1]
        C[n-1,n  ] = A[n-1,n-1]*B[n-1,n  ] + A[n-1,  n]*B[n  ,n  ]
        # last row of C
        C[n,n-2] = A[n,n-1]*B[n-1,n-2]
        C[n,n-1] = A[n,n-1]*B[n-1,n-1] + A[n,n]*B[n,n-1]
        C[n,n  ] = A[n,n-1]*B[n-1,n  ] + A[n,n]*B[n,n  ]
    end # inbounds
    C
end

function A_mul_B_td!(C::AbstractVecOrMat, A::BiTriSym, B::AbstractVecOrMat)
    nA = size(A,1)
    nB = size(B,2)
    if !(size(C,1) == size(B,1) == nA)
        throw(DimensionMismatch("A has first dimension $nA, B has $(size(B,1)), C has $(size(C,1)) but all must match"))
    end
    if size(C,2) != nB
        throw(DimensionMismatch("A has second dimension $nA, B has $(size(B,2)), C has $(size(C,2)) but all must match"))
    end
    nA <= 3 && return A_mul_B!(C, Array(A), Array(B))
    l = diag(A, -1)
    d = diag(A, 0)
    u = diag(A, 1)
    @inbounds begin
        for j = 1:nB
            b₀, b₊ = B[1, j], B[2, j]
            C[1, j] = d[1]*b₀ + u[1]*b₊
            for i = 2:nA - 1
                b₋, b₀, b₊ = b₀, b₊, B[i + 1, j]
                C[i, j] = l[i - 1]*b₋ + d[i]*b₀ + u[i]*b₊
            end
            C[nA, j] = l[nA - 1]*b₀ + d[nA]*b₊
        end
    end
    C
end

function A_mul_B_td!(C::AbstractMatrix, A::AbstractMatrix, B::BiTriSym)
    check_A_mul_B!_sizes(C, A, B)
    n = size(A,1)
    n <= 3 && return A_mul_B!(C, Array(A), Array(B))
    m = size(B,2)
    Bl = diag(B, -1)
    Bd = diag(B, 0)
    Bu = diag(B, 1)
    @inbounds begin
        # first and last column of C
        B11 = Bd[1]
        B21 = Bl[1]
        Bmm = Bd[m]
        Bm₋1m = Bu[m-1]
        for i in 1:n
            C[i, 1] = A[i,1] * B11 + A[i, 2] * B21
            C[i, m] = A[i, m-1] * Bm₋1m + A[i, m] * Bmm
        end
        # middle columns of C
        for j = 2:m-1
            Bj₋1j = Bu[j-1]
            Bjj = Bd[j]
            Bj₊1j = Bl[j]
            for i = 1:n
                C[i, j] = A[i, j-1] * Bj₋1j + A[i, j]*Bjj + A[i, j+1] * Bj₊1j
            end
        end
    end # inbounds
    C
end

SpecialMatrix = Union{Bidiagonal, SymTridiagonal, Tridiagonal}
# to avoid ambiguity warning, but shouldn't be necessary
*(A::AbstractTriangular, B::SpecialMatrix) = Array(A) * Array(B)
*(A::SpecialMatrix, B::SpecialMatrix) = Array(A) * Array(B)

#Generic multiplication
for func in (:*, :Ac_mul_B, :A_mul_Bc, :/, :A_rdiv_Bc)
    @eval ($func){T}(A::Bidiagonal{T}, B::AbstractVector{T}) = ($func)(Array(A), B)
end

#Linear solvers
A_ldiv_B!(A::Union{Bidiagonal, AbstractTriangular}, b::AbstractVector) = naivesub!(A, b)
At_ldiv_B!(A::Bidiagonal, b::AbstractVector) = A_ldiv_B!(transpose(A), b)
Ac_ldiv_B!(A::Bidiagonal, b::AbstractVector) = A_ldiv_B!(ctranspose(A), b)
function A_ldiv_B!(A::Union{Bidiagonal, AbstractTriangular}, B::AbstractMatrix)
    nA,mA = size(A)
    tmp = similar(B,size(B,1))
    n = size(B, 1)
    if nA != n
        throw(DimensionMismatch("size of A is ($nA,$mA), corresponding dimension of B is $n"))
    end
    for i = 1:size(B,2)
        copy!(tmp, 1, B, (i - 1)*n + 1, n)
        A_ldiv_B!(A, tmp)
        copy!(B, (i - 1)*n + 1, tmp, 1, n) # Modify this when array view are implemented.
    end
    B
end
for func in (:Ac_ldiv_B!, :At_ldiv_B!)
    @eval function ($func)(A::Union{Bidiagonal, AbstractTriangular}, B::AbstractMatrix)
        nA,mA = size(A)
        tmp = similar(B,size(B,1))
        n = size(B, 1)
        if mA != n
            throw(DimensionMismatch("size of A' is ($mA,$nA), corresponding dimension of B is $n"))
        end
        for i = 1:size(B,2)
            copy!(tmp, 1, B, (i - 1)*n + 1, n)
            ($func)(A, tmp)
            copy!(B, (i - 1)*n + 1, tmp, 1, n) # Modify this when array view are implemented.
        end
        B
    end
end
#Generic solver using naive substitution
function naivesub!{T}(A::Bidiagonal{T}, b::AbstractVector, x::AbstractVector = b)
    N = size(A, 2)
    if N != length(b) || N != length(x)
        throw(DimensionMismatch("second dimension of A, $N, does not match one of the lengths of x, $(length(x)), or b, $(length(b))"))
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

### Generic promotion methods and fallbacks
for (f,g) in ((:\, :A_ldiv_B!), (:At_ldiv_B, :At_ldiv_B!), (:Ac_ldiv_B, :Ac_ldiv_B!))
    @eval begin
        function ($f){TA<:Number,TB<:Number}(A::Bidiagonal{TA}, B::AbstractVecOrMat{TB})
            TAB = typeof((zero(TA)*zero(TB) + zero(TA)*zero(TB))/one(TA))
            ($g)(convert(AbstractArray{TAB}, A), copy_oftype(B, TAB))
        end
        ($f)(A::Bidiagonal, B::AbstractVecOrMat) = ($g)(A, copy(B))
    end
end

factorize(A::Bidiagonal) = A

# Eigensystems
eigvals(M::Bidiagonal) = M.dv
function eigvecs{T}(M::Bidiagonal{T})
    n = length(M.dv)
    Q = Array{T}(n, n)
    blks = [0; find(x -> x == 0, M.ev); n]
    v = zeros(T, n)
    if M.isupper
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
eigfact(M::Bidiagonal) = Eigen(eigvals(M), eigvecs(M))

# fill! methods
_valuefields{T <: Diagonal}(S::Type{T}) = [:diag]
_valuefields{T <: Bidiagonal}(S::Type{T}) = [:dv, :ev]
_valuefields{T <: Tridiagonal}(S::Type{T}) = [:dl, :d, :du]
_valuefields{T <: SymTridiagonal}(S::Type{T}) = [:dv, :ev]
_valuefields{T <: AbstractTriangular}(S::Type{T}) = [:data]

SpecialArrays = Union{Diagonal,
    Bidiagonal,
    Tridiagonal,
    SymTridiagonal,
    AbstractTriangular}

@generated function fillslots!(A::SpecialArrays, x)
    ex = :(xT = convert(eltype(A), x))
    for field in _valuefields(A)
        ex = :($ex; fill!(A.$field, xT))
    end
    :($ex;return A)
end

# for historical reasons:
fill!(a::AbstractTriangular, x) = fillslots!(a, x);
fill!(D::Diagonal, x) = fillslots!(D, x);

_small_enough(A::Bidiagonal) = size(A, 1) <= 1
_small_enough(A::Tridiagonal) = size(A, 1) <= 2
_small_enough(A::SymTridiagonal) = size(A, 1) <= 2

function fill!(A::Union{Bidiagonal, Tridiagonal, SymTridiagonal} ,x)
    xT = convert(eltype(A), x)
    (xT == zero(eltype(A)) || _small_enough(A)) && return fillslots!(A, xT)
    throw(ArgumentError("array A of type $(typeof(A)) and size $(size(A)) can
    not be filled with x=$x, since some of its entries are constrained."))
end
