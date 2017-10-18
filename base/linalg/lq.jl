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
    lq(A; [thin=true]) -> L, Q

Perform an LQ factorization of `A` such that `A = L*Q`. The default is to compute
a "thin" factorization. The LQ factorization is the QR factorization of `A.'`.
`L` is not extendedwith zeros if the explicit, square form of `Q`
is requested via `thin = false`.

!!! note
    While in QR factorization the "thin" factorization is so named due to yielding
    either a square or "tall"/"thin" factor `Q`, in LQ factorization the "thin"
    factorization somewhat confusingly produces either a square or "short"/"wide"
    factor `Q`. "Thin" factorizations more broadly are also (more descriptively)
    referred to as "truncated" or "reduced" factorizatons.
"""
function lq(A::Union{Number,AbstractMatrix}; thin::Bool = true)
    F = lqfact(A)
    L, Q = F[:L], F[:Q]
    return L, thin ? Array(Q) : A_mul_B!(Q, eye(eltype(Q), size(Q.factors, 2)))
end

copy(A::LQ) = LQ(copy(A.factors), copy(A.τ))

convert(::Type{LQ{T}},A::LQ) where {T} = LQ(convert(AbstractMatrix{T}, A.factors), convert(Vector{T}, A.τ))
convert(::Type{Factorization{T}}, A::LQ{T}) where {T} = A
convert(::Type{Factorization{T}}, A::LQ) where {T} = convert(LQ{T}, A)
convert(::Type{AbstractMatrix}, A::LQ) = A[:L]*A[:Q]
convert(::Type{AbstractArray}, A::LQ) = convert(AbstractMatrix, A)
convert(::Type{Matrix}, A::LQ) = convert(Array, convert(AbstractArray, A))
convert(::Type{Array}, A::LQ) = convert(Matrix, A)
full(A::LQ) = convert(AbstractArray, A)

adjoint(A::LQ{T}) where {T} = QR{T,typeof(A.factors)}(A.factors', A.τ)

function getindex(A::LQ, d::Symbol)
    m, n = size(A)
    if d == :L
        return tril!(A.factors[1:m, 1:min(m,n)])
    elseif d == :Q
        return LQPackedQ(A.factors,A.τ)
    else
        throw(KeyError(d))
    end
end

getindex(A::LQPackedQ, i::Integer, j::Integer) =
    A_mul_B!(A, setindex!(zeros(eltype(A), size(A, 2)), 1, j))[i]

getq(A::LQ) = LQPackedQ(A.factors, A.τ)

function show(io::IO, C::LQ)
    println(io, "$(typeof(C)) with factors L and Q:")
    show(io, C[:L])
    println(io)
    show(io, C[:Q])
end

convert(::Type{LQPackedQ{T}}, Q::LQPackedQ) where {T} = LQPackedQ(convert(AbstractMatrix{T}, Q.factors), convert(Vector{T}, Q.τ))
convert(::Type{AbstractMatrix{T}}, Q::LQPackedQ) where {T} = convert(LQPackedQ{T}, Q)
convert(::Type{Matrix}, A::LQPackedQ) = LAPACK.orglq!(copy(A.factors),A.τ)
convert(::Type{Array}, A::LQPackedQ) = convert(Matrix, A)

full(Q::LQPackedQ; thin::Bool = true) =
    thin ? Array(Q) : A_mul_B!(Q, eye(eltype(Q), size(Q.factors, 2)))

size(A::LQ, dim::Integer) = size(A.factors, dim)
size(A::LQ) = size(A.factors)

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
A_mul_B!(A::LQ{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat} =
    A[:L] * LAPACK.ormlq!('L', 'N', A.factors, A.τ, B)
A_mul_B!(A::LQ{T}, B::QR{T}) where {T<:BlasFloat} =
    A[:L] * LAPACK.ormlq!('L', 'N', A.factors, A.τ, Matrix(B))
A_mul_B!(A::QR{T}, B::LQ{T}) where {T<:BlasFloat} =
    A_mul_B!(zeros(eltype(A), size(A)), Matrix(A), Matrix(B))
function *(A::LQ{TA}, B::StridedVecOrMat{TB}) where {TA,TB}
    TAB = promote_type(TA, TB)
    A_mul_B!(convert(Factorization{TAB},A), copy_oftype(B, TAB))
end
function *(A::LQ{TA},B::QR{TB}) where {TA,TB}
    TAB = promote_type(TA, TB)
    A_mul_B!(convert(Factorization{TAB},A), convert(Factorization{TAB},B))
end
function *(A::QR{TA},B::LQ{TB}) where {TA,TB}
    TAB = promote_type(TA, TB)
    A_mul_B!(convert(Factorization{TAB},A), convert(Factorization{TAB},B))
end

## Multiplication by Q
### QB
A_mul_B!(A::LQPackedQ{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat} = LAPACK.ormlq!('L','N',A.factors,A.τ,B)
function (*)(A::LQPackedQ, B::StridedVecOrMat)
    TAB = promote_type(eltype(A), eltype(B))
    A_mul_B!(convert(AbstractMatrix{TAB}, A), copy_oftype(B, TAB))
end

### QcB
Ac_mul_B!(A::LQPackedQ{T}, B::StridedVecOrMat{T}) where {T<:BlasReal} = LAPACK.ormlq!('L','T',A.factors,A.τ,B)
Ac_mul_B!(A::LQPackedQ{T}, B::StridedVecOrMat{T}) where {T<:BlasComplex} = LAPACK.ormlq!('L','C',A.factors,A.τ,B)
function Ac_mul_B(A::LQPackedQ, B::StridedVecOrMat)
    TAB = promote_type(eltype(A), eltype(B))
    if size(B,1) == size(A.factors,2)
        Ac_mul_B!(convert(AbstractMatrix{TAB}, A), copy_oftype(B, TAB))
    elseif size(B,1) == size(A.factors,1)
        Ac_mul_B!(convert(AbstractMatrix{TAB}, A), [B; zeros(TAB, size(A.factors, 2) - size(A.factors, 1), size(B, 2))])
    else
        throw(DimensionMismatch("first dimension of B, $(size(B,1)), must equal one of the dimensions of A, $(size(A))"))
    end
end

### QBc/QcBc
for (f1, f2) in ((:A_mul_Bc, :A_mul_B!),
                 (:Ac_mul_Bc, :Ac_mul_B!))
    @eval begin
        function ($f1)(A::LQPackedQ, B::StridedVecOrMat)
            TAB = promote_type(eltype(A), eltype(B))
            BB = similar(B, TAB, (size(B, 2), size(B, 1)))
            adjoint!(BB, B)
            return ($f2)(A, BB)
        end
    end
end

# in-place right-application of LQPackedQs
# these methods require that the applied-to matrix's (A's) number of columns
# match the number of columns (nQ) of the LQPackedQ (Q) (necessary for in-place
# operation, and the underlying LAPACK routine (ormlq) treats the implicit Q
# as its (nQ-by-nQ) square form)
A_mul_B!(A::StridedMatrix{T}, B::LQPackedQ{T}) where {T<:BlasFloat} =
    LAPACK.ormlq!('R', 'N', B.factors, B.τ, A)
A_mul_Bc!(A::StridedMatrix{T}, B::LQPackedQ{T}) where {T<:BlasReal} =
    LAPACK.ormlq!('R', 'T', B.factors, B.τ, A)
A_mul_Bc!(A::StridedMatrix{T}, B::LQPackedQ{T}) where {T<:BlasComplex} =
    LAPACK.ormlq!('R', 'C', B.factors, B.τ, A)

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
function A_mul_Bc(A::StridedVecOrMat, Q::LQPackedQ)
    TR = promote_type(eltype(A), eltype(Q))
    return A_mul_Bc!(copy_oftype(A, TR), convert(AbstractMatrix{TR}, Q))
end
function Ac_mul_Bc(A::StridedMatrix, Q::LQPackedQ)
    TR = promote_type(eltype(A), eltype(Q))
    C = adjoint!(similar(A, TR, reverse(size(A))), A)
    return A_mul_Bc!(C, convert(AbstractMatrix{TR}, Q))
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
        copy!(C, 1, A, 1, length(A))
    else
        _rightappdimmismatch("columns")
    end
    return A_mul_B!(C, convert(AbstractMatrix{TR}, Q))
end
function Ac_mul_B(A::StridedMatrix, Q::LQPackedQ)
    TR = promote_type(eltype(A), eltype(Q))
    if size(A, 1) == size(Q.factors, 2)
        C = adjoint!(similar(A, TR, reverse(size(A))), A)
    elseif size(A, 1) == size(Q.factors, 1)
        C = zeros(TR, size(A, 2), size(Q.factors, 2))
        adjoint!(view(C, :, 1:size(A, 1)), A)
    else
        _rightappdimmismatch("rows")
    end
    return A_mul_B!(C, convert(AbstractMatrix{TR}, Q))
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
    AA = convert(Factorization{S}, A)
    x = A_ldiv_B!(AA, copy_oftype(b, S))
    return x
end
function (\)(A::LQ{TA},B::StridedMatrix{TB}) where {TA,TB}
    S = promote_type(TA,TB)
    m = checksquare(A)
    m == size(B,1) || throw(DimensionMismatch("left hand side has $m rows, but right hand side has $(size(B,1)) rows"))
    AA = convert(Factorization{S}, A)
    X = A_ldiv_B!(AA, copy_oftype(B, S))
    return X
end
# With a real lhs and complex rhs with the same precision, we can reinterpret
# the complex rhs as a real rhs with twice the number of columns
function (\)(F::LQ{T}, B::VecOrMat{Complex{T}}) where T<:BlasReal
    c2r = reshape(transpose(reinterpret(T, reshape(B, (1, length(B))))), size(B, 1), 2*size(B, 2))
    x = A_ldiv_B!(F, c2r)
    return reshape(collect(reinterpret(Complex{T}, transpose(reshape(x, div(length(x), 2), 2)))),
                           isa(B, AbstractVector) ? (size(F,2),) : (size(F,2), size(B,2)))
end


function A_ldiv_B!(A::LQ{T}, B::StridedVecOrMat{T}) where T
    Ac_mul_B!(A[:Q], A_ldiv_B!(LowerTriangular(A[:L]),B))
    return B
end
