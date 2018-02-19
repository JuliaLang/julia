# This file is a part of Julia. License is MIT: https://julialang.org/license

# LQ Factorizations

struct LQ{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    τ::Vector{T}
    LQ{T,S}(factors::AbstractMatrix{T}, τ::Vector{T}) where {T,S<:AbstractMatrix} = new(factors, τ)
end

struct LQPackedQ{T,S<:AbstractMatrix} <: AbstractMatrix{T}
    factors::Matrix{T}
    τ::Vector{T}
    LQPackedQ{T,S}(factors::AbstractMatrix{T}, τ::Vector{T}) where {T,S<:AbstractMatrix} = new(factors, τ)
end

LQ(factors::AbstractMatrix{T}, τ::Vector{T}) where {T} = LQ{T,typeof(factors)}(factors, τ)
LQPackedQ(factors::AbstractMatrix{T}, τ::Vector{T}) where {T} = LQPackedQ{T,typeof(factors)}(factors, τ)

"""
    lqfact!(A) -> LQ

Compute the LQ factorization of `A`, using the input
matrix as a workspace. See also [`lq`](@ref).
"""
lqfact!(A::StridedMatrix{<:BlasFloat}) = LQ(LAPACK.gelqf!(A)...)
"""
    lqfact(A) -> LQ

Compute the LQ factorization of `A`. See also [`lq`](@ref).
"""
lqfact(A::StridedMatrix{<:BlasFloat})  = lqfact!(copy(A))
lqfact(x::Number) = lqfact(fill(x,1,1))

"""
    lq(A; full = false) -> L, Q

Perform an LQ factorization of `A` such that `A = L*Q`. The default (`full = false`)
computes a factorization with possibly-rectangular `L` and `Q`, commonly the "thin"
factorization. The LQ factorization is the QR factorization of `transpose(A)`. If the explicit,
full/square form of `Q` is requested via `full = true`, `L` is not extended with zeros.

!!! note
    While in QR factorization the "thin" factorization is so named due to yielding
    either a square or "tall"/"thin" rectangular factor `Q`, in LQ factorization the
    "thin" factorization somewhat confusingly produces either a square or "short"/"wide"
    rectangular factor `Q`. "Thin" factorizations more broadly are also
    referred to as "reduced" factorizatons.
"""
function lq(A::Union{Number,AbstractMatrix}; full::Bool = false, thin::Union{Bool,Nothing} = nothing)
    # DEPRECATION TODO: remove deprecated thin argument and associated logic after 0.7
    if thin != nothing
        Base.depwarn(string("the `thin` keyword argument in `lq(A; thin = $(thin))` has ",
            "been deprecated in favor of `full`, which has the opposite meaning, ",
            "e.g. `lq(A; full = $(!thin))`."), :lq)
        full::Bool = !thin
    end
    F = lqfact(A)
    L, Q = F.L, F.Q
    return L, !full ? Array(Q) : lmul!(Q, Matrix{eltype(Q)}(I, size(Q.factors, 2), size(Q.factors, 2)))
end

copy(A::LQ) = LQ(copy(A.factors), copy(A.τ))

LQ{T}(A::LQ) where {T} = LQ(convert(AbstractMatrix{T}, A.factors), convert(Vector{T}, A.τ))
Factorization{T}(A::LQ{T}) where {T} = A
Factorization{T}(A::LQ) where {T} = LQ{T}(A)
AbstractMatrix(A::LQ) = A.L*A.Q
AbstractArray(A::LQ) = AbstractMatrix(A)
Matrix(A::LQ) = Array(AbstractArray(A))
Array(A::LQ) = Matrix(A)

adjoint(A::LQ) = Adjoint(A)
Base.copy(F::Adjoint{T,<:LQ{T}}) where {T} =
    QR{T,typeof(F.parent.factors)}(copy(adjoint(F.parent.factors)), copy(F.parent.τ))

function getproperty(F::LQ, d::Symbol)
    m, n = size(F)
    if d == :L
        return tril!(getfield(F, :factors)[1:m, 1:min(m,n)])
    elseif d == :Q
        return LQPackedQ(getfield(F, :factors), getfield(F, :τ))
    else
        return getfield(F, d)
    end
end

Base.propertynames(F::LQ, private::Bool=false) =
    (:L, :Q, (private ? fieldnames(typeof(F)) : ())...)

getindex(A::LQPackedQ, i::Integer, j::Integer) =
    lmul!(A, setindex!(zeros(eltype(A), size(A, 2)), 1, j))[i]

function show(io::IO, C::LQ)
    println(io, "$(typeof(C)) with factors L and Q:")
    show(io, C.L)
    println(io)
    show(io, C.Q)
end

LQPackedQ{T}(Q::LQPackedQ) where {T} = LQPackedQ(convert(AbstractMatrix{T}, Q.factors), convert(Vector{T}, Q.τ))
AbstractMatrix{T}(Q::LQPackedQ) where {T} = LQPackedQ{T}(Q)
Matrix(A::LQPackedQ) = LAPACK.orglq!(copy(A.factors),A.τ)
Array(A::LQPackedQ) = Matrix(A)

size(F::LQ, dim::Integer) = size(getfield(F, :factors), dim)
size(F::LQ)               = size(getfield(F, :factors))

# size(Q::LQPackedQ) yields the shape of Q's square form
function size(Q::LQPackedQ)
    n = size(Q.factors, 2)
    return n, n
end
function size(Q::LQPackedQ, dim::Integer)
    if dim < 1
        throw(BoundsError())
    elseif dim <= 2 # && 1 <= dim
        return size(Q.factors, 2)
    else # 2 < dim
        return 1
    end
end


## Multiplication by LQ
lmul!(A::LQ, B::StridedVecOrMat) =
    lmul!(LowerTriangular(A.L), lmul!(A.Q, B))
function *(A::LQ{TA}, B::StridedVecOrMat{TB}) where {TA,TB}
    TAB = promote_type(TA, TB)
    lmul!(Factorization{TAB}(A), copy_oftype(B, TAB))
end

## Multiplication by Q
### QB
lmul!(A::LQPackedQ{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat} = LAPACK.ormlq!('L','N',A.factors,A.τ,B)
function (*)(A::LQPackedQ, B::StridedVecOrMat)
    TAB = promote_type(eltype(A), eltype(B))
    lmul!(AbstractMatrix{TAB}(A), copy_oftype(B, TAB))
end

### QcB
lmul!(adjA::Adjoint{<:Any,<:LQPackedQ{T}}, B::StridedVecOrMat{T}) where {T<:BlasReal} =
    (A = adjA.parent; LAPACK.ormlq!('L','T',A.factors,A.τ,B))
lmul!(adjA::Adjoint{<:Any,<:LQPackedQ{T}}, B::StridedVecOrMat{T}) where {T<:BlasComplex} =
    (A = adjA.parent; LAPACK.ormlq!('L','C',A.factors,A.τ,B))

function *(adjA::Adjoint{<:Any,<:LQPackedQ}, B::StridedVecOrMat)
    A = adjA.parent
    TAB = promote_type(eltype(A), eltype(B))
    if size(B,1) == size(A.factors,2)
        lmul!(adjoint(AbstractMatrix{TAB}(A)), copy_oftype(B, TAB))
    elseif size(B,1) == size(A.factors,1)
        lmul!(adjoint(AbstractMatrix{TAB}(A)), [B; zeros(TAB, size(A.factors, 2) - size(A.factors, 1), size(B, 2))])
    else
        throw(DimensionMismatch("first dimension of B, $(size(B,1)), must equal one of the dimensions of A, $(size(A))"))
    end
end

### QBc/QcBc
function *(A::LQPackedQ, adjB::Adjoint{<:Any,<:StridedVecOrMat})
    B = adjB.parent
    TAB = promote_type(eltype(A), eltype(B))
    BB = similar(B, TAB, (size(B, 2), size(B, 1)))
    adjoint!(BB, B)
    return lmul!(A, BB)
end
function *(adjA::Adjoint{<:Any,<:LQPackedQ}, adjB::Adjoint{<:Any,<:StridedVecOrMat})
    A, B = adjA.parent, adjB.parent
    TAB = promote_type(eltype(A), eltype(B))
    BB = similar(B, TAB, (size(B, 2), size(B, 1)))
    adjoint!(BB, B)
    return lmul!(adjoint(A), BB)
end

# in-place right-application of LQPackedQs
# these methods require that the applied-to matrix's (A's) number of columns
# match the number of columns (nQ) of the LQPackedQ (Q) (necessary for in-place
# operation, and the underlying LAPACK routine (ormlq) treats the implicit Q
# as its (nQ-by-nQ) square form)
rmul!(A::StridedMatrix{T}, B::LQPackedQ{T}) where {T<:BlasFloat} =
    LAPACK.ormlq!('R', 'N', B.factors, B.τ, A)
rmul!(A::StridedMatrix{T}, adjB::Adjoint{<:Any,<:LQPackedQ{T}}) where {T<:BlasReal} =
    (B = adjB.parent; LAPACK.ormlq!('R', 'T', B.factors, B.τ, A))
rmul!(A::StridedMatrix{T}, adjB::Adjoint{<:Any,<:LQPackedQ{T}}) where {T<:BlasComplex} =
    (B = adjB.parent; LAPACK.ormlq!('R', 'C', B.factors, B.τ, A))

# out-of-place right application of LQPackedQs
#
# LQPackedQ's out-of-place multiplication behavior is context dependent. specifically,
# if the inner dimension in the multiplication is the LQPackedQ's second dimension,
# the LQPackedQ behaves like its square form. if the inner dimension in the
# multiplication is the LQPackedQ's first dimension, the LQPackedQ behaves like either
# its square form or its truncated form depending on the shape of the other object
# involved in the multiplication. we treat these cases separately.
#
# (1) the inner dimension in the multiplication is the LQPackedQ's second dimension.
# in this case, the LQPackedQ behaves like its square form.
#
function *(A::StridedVecOrMat, adjQ::Adjoint{<:Any,<:LQPackedQ})
    Q = adjQ.parent
    TR = promote_type(eltype(A), eltype(Q))
    return rmul!(copy_oftype(A, TR), adjoint(AbstractMatrix{TR}(Q)))
end
function *(adjA::Adjoint{<:Any,<:StridedMatrix}, adjQ::Adjoint{<:Any,<:LQPackedQ})
    A, Q = adjA.parent, adjQ.parent
    TR = promote_type(eltype(A), eltype(Q))
    C = adjoint!(similar(A, TR, reverse(size(A))), A)
    return rmul!(C, adjoint(AbstractMatrix{TR}(Q)))
end
#
# (2) the inner dimension in the multiplication is the LQPackedQ's first dimension.
# in this case, the LQPackedQ behaves like either its square form or its
# truncated form depending on the shape of the other object in the multiplication.
#
# these methods: (1) check whether the applied-to matrix's (A's) appropriate dimension
# (columns for A_*, rows for Ac_*) matches the number of columns (nQ) of the LQPackedQ (Q),
# and if so effectively apply Q's square form to A without additional shenanigans; and
# (2) if the preceding dimensions do not match, check whether the appropriate dimension of
# A instead matches the number of rows of the matrix of which Q is a factor (i.e.
# size(Q.factors, 1)), and if so implicitly apply Q's truncated form to A by zero extending
# A as necessary for check (1) to pass (if possible) and then applying Q's square form
#
function *(A::StridedVecOrMat, Q::LQPackedQ)
    TR = promote_type(eltype(A), eltype(Q))
    if size(A, 2) == size(Q.factors, 2)
        C = copy_oftype(A, TR)
    elseif size(A, 2) == size(Q.factors, 1)
        C = zeros(TR, size(A, 1), size(Q.factors, 2))
        copyto!(C, 1, A, 1, length(A))
    else
        _rightappdimmismatch("columns")
    end
    return rmul!(C, AbstractMatrix{TR}(Q))
end
function *(adjA::Adjoint{<:Any,<:StridedMatrix}, Q::LQPackedQ)
    A = adjA.parent
    TR = promote_type(eltype(A), eltype(Q))
    if size(A, 1) == size(Q.factors, 2)
        C = adjoint!(similar(A, TR, reverse(size(A))), A)
    elseif size(A, 1) == size(Q.factors, 1)
        C = zeros(TR, size(A, 2), size(Q.factors, 2))
        adjoint!(view(C, :, 1:size(A, 1)), A)
    else
        _rightappdimmismatch("rows")
    end
    return rmul!(C, AbstractMatrix{TR}(Q))
end
_rightappdimmismatch(rowsorcols) =
    throw(DimensionMismatch(string("the number of $(rowsorcols) of the matrix on the left ",
        "must match either (1) the number of columns of the (LQPackedQ) matrix on the right ",
        "or (2) the number of rows of that (LQPackedQ) matrix's internal representation ",
        "(the factorization's originating matrix's number of rows)")))


function (\)(A::LQ{TA}, b::StridedVector{Tb}) where {TA,Tb}
    S = promote_type(TA,Tb)
    m = checksquare(A)
    m == length(b) || throw(DimensionMismatch("left hand side has $m rows, but right hand side has length $(length(b))"))
    AA = Factorization{S}(A)
    x = ldiv!(AA, copy_oftype(b, S))
    return x
end
function (\)(A::LQ{TA},B::StridedMatrix{TB}) where {TA,TB}
    S = promote_type(TA,TB)
    m = checksquare(A)
    m == size(B,1) || throw(DimensionMismatch("left hand side has $m rows, but right hand side has $(size(B,1)) rows"))
    AA = Factorization{S}(A)
    X = ldiv!(AA, copy_oftype(B, S))
    return X
end
# With a real lhs and complex rhs with the same precision, we can reinterpret
# the complex rhs as a real rhs with twice the number of columns
function (\)(F::LQ{T}, B::VecOrMat{Complex{T}}) where T<:BlasReal
    c2r = reshape(copy(transpose(reinterpret(T, reshape(B, (1, length(B)))))), size(B, 1), 2*size(B, 2))
    x = ldiv!(F, c2r)
    return reshape(copy(reinterpret(Complex{T}, copy(transpose(reshape(x, div(length(x), 2), 2))))),
                           isa(B, AbstractVector) ? (size(F,2),) : (size(F,2), size(B,2)))
end


function ldiv!(A::LQ{T}, B::StridedVecOrMat{T}) where T
    lmul!(adjoint(A.Q), ldiv!(LowerTriangular(A.L),B))
    return B
end
