# This file is a part of Julia. License is MIT: https://julialang.org/license

# Methods operating on different special matrix types


# Usually, reducedim_initarray calls similar, which yields a sparse matrix for a
# Diagonal/Bidiagonal/Tridiagonal/SymTridiagonal matrix. However, reducedim should
# yield a dense vector to increase performance.
Base.reducedim_initarray(A::Union{Diagonal,Bidiagonal,Tridiagonal,SymTridiagonal}, region, init, ::Type{R}) where {R} = fill(convert(R, init), Base.reduced_indices(A,region))


# Interconversion between special matrix types

# conversions from Diagonal to other special matrix types
Bidiagonal(A::Diagonal) = Bidiagonal(A.diag, fill!(similar(A.diag, length(A.diag)-1), 0), :U)
SymTridiagonal(A::Diagonal) = SymTridiagonal(A.diag, fill!(similar(A.diag, length(A.diag)-1), 0))
Tridiagonal(A::Diagonal) = Tridiagonal(fill!(similar(A.diag, length(A.diag)-1), 0), A.diag,
                                       fill!(similar(A.diag, length(A.diag)-1), 0))

# conversions from Bidiagonal to other special matrix types
Diagonal(A::Bidiagonal) = Diagonal(A.dv)
SymTridiagonal(A::Bidiagonal) =
    iszero(A.ev) ? SymTridiagonal(A.dv, A.ev) :
        throw(ArgumentError("matrix cannot be represented as SymTridiagonal"))
Tridiagonal(A::Bidiagonal) =
    Tridiagonal(A.uplo == 'U' ? fill!(similar(A.ev), 0) : A.ev, A.dv,
                A.uplo == 'U' ? A.ev : fill!(similar(A.ev), 0))

# conversions from SymTridiagonal to other special matrix types
Diagonal(A::SymTridiagonal) = Diagonal(A.dv)

# These can fail when ev has the same length as dv
# TODO: Revisit when a good solution for #42477 is found
Bidiagonal(A::SymTridiagonal) =
    iszero(A.ev) ? Bidiagonal(A.dv, A.ev, :U) :
        throw(ArgumentError("matrix cannot be represented as Bidiagonal"))
Tridiagonal(A::SymTridiagonal) =
    Tridiagonal(copy(A.ev), A.dv, A.ev)

# conversions from Tridiagonal to other special matrix types
Diagonal(A::Tridiagonal) = Diagonal(A.d)
Bidiagonal(A::Tridiagonal) =
    iszero(A.dl) ? Bidiagonal(A.d, A.du, :U) :
    iszero(A.du) ? Bidiagonal(A.d, A.dl, :L) :
        throw(ArgumentError("matrix cannot be represented as Bidiagonal"))

# conversions from AbstractTriangular to special matrix types
Bidiagonal(A::AbstractTriangular) =
    isbanded(A, 0, 1) ? Bidiagonal(diag(A, 0), diag(A,  1), :U) : # is upper bidiagonal
    isbanded(A, -1, 0) ? Bidiagonal(diag(A, 0), diag(A, -1), :L) : # is lower bidiagonal
        throw(ArgumentError("matrix cannot be represented as Bidiagonal"))

_lucopy(A::Bidiagonal, T)     = copymutable_oftype(Tridiagonal(A), T)
_lucopy(A::Diagonal, T)       = copymutable_oftype(Tridiagonal(A), T)
function _lucopy(A::SymTridiagonal, T)
    du = copy_similar(_evview(A), T)
    dl = copy.(transpose.(du))
    d  = copy_similar(A.dv, T)
    return Tridiagonal(dl, d, du)
end

const ConvertibleSpecialMatrix = Union{Diagonal,Bidiagonal,SymTridiagonal,Tridiagonal,AbstractTriangular}
const PossibleTriangularMatrix = Union{Diagonal, Bidiagonal, AbstractTriangular}

convert(T::Type{<:Diagonal},       m::ConvertibleSpecialMatrix) = m isa T ? m :
    isdiag(m) ? T(m) : throw(ArgumentError("matrix cannot be represented as Diagonal"))
convert(T::Type{<:SymTridiagonal}, m::ConvertibleSpecialMatrix) = m isa T ? m :
    issymmetric(m) && isbanded(m, -1, 1) ? T(m) : throw(ArgumentError("matrix cannot be represented as SymTridiagonal"))
convert(T::Type{<:Tridiagonal},    m::ConvertibleSpecialMatrix) = m isa T ? m :
    isbanded(m, -1, 1) ? T(m) : throw(ArgumentError("matrix cannot be represented as Tridiagonal"))

convert(T::Type{<:LowerTriangular}, m::Union{LowerTriangular,UnitLowerTriangular}) = m isa T ? m : T(m)
convert(T::Type{<:UpperTriangular}, m::Union{UpperTriangular,UnitUpperTriangular}) = m isa T ? m : T(m)

convert(T::Type{<:LowerTriangular}, m::PossibleTriangularMatrix) = m isa T ? m :
    istril(m) ? T(m) : throw(ArgumentError("matrix cannot be represented as LowerTriangular"))
convert(T::Type{<:UpperTriangular}, m::PossibleTriangularMatrix) = m isa T ? m :
    istriu(m) ? T(m) : throw(ArgumentError("matrix cannot be represented as UpperTriangular"))

# Constructs two method definitions taking into account (assumed) commutativity
# e.g. @commutative f(x::S, y::T) where {S,T} = x+y is the same is defining
#     f(x::S, y::T) where {S,T} = x+y
#     f(y::T, x::S) where {S,T} = f(x, y)
macro commutative(myexpr)
    @assert myexpr.head===:(=) || myexpr.head===:function # Make sure it is a function definition
    y = copy(myexpr.args[1].args[2:end])
    reverse!(y)
    reversed_call = Expr(:(=), Expr(:call,myexpr.args[1].args[1],y...), myexpr.args[1])
    esc(Expr(:block, myexpr, reversed_call))
end

for op in (:+, :-)
    for (matrixtype, uplo, converttype) in ((:UpperTriangular, 'U', :UpperTriangular),
                                            (:UnitUpperTriangular, 'U', :UpperTriangular),
                                            (:LowerTriangular, 'L', :LowerTriangular),
                                            (:UnitLowerTriangular, 'L', :LowerTriangular))
        @eval begin
            function ($op)(A::$matrixtype, B::Bidiagonal)
                if B.uplo == $uplo
                    ($op)(A, convert($converttype, B))
                else
                    ($op).(A, B)
                end
            end

            function ($op)(A::Bidiagonal, B::$matrixtype)
                if A.uplo == $uplo
                    ($op)(convert($converttype, A), B)
                else
                    ($op).(A, B)
                end
            end
        end
    end
end

# specialized +/- for structured matrices. If these are removed, it falls
# back to broadcasting which has ~2-10x speed regressions.
# For the other structure matrix pairs, broadcasting works well.

# For structured matrix types with different non-zero diagonals the underlying
# representations must be promoted to the same type.
# For example, in Diagonal + Bidiagonal only the main diagonal is touched so
# the off diagonal could be a different type after the operation resulting in
# an error. See issue #28994

function (+)(A::Bidiagonal, B::Diagonal)
    newdv = A.dv + B.diag
    Bidiagonal(newdv, typeof(newdv)(A.ev), A.uplo)
end

function (-)(A::Bidiagonal, B::Diagonal)
    newdv = A.dv - B.diag
    Bidiagonal(newdv, typeof(newdv)(A.ev), A.uplo)
end

function (+)(A::Diagonal, B::Bidiagonal)
    newdv = A.diag + B.dv
    Bidiagonal(newdv, typeof(newdv)(B.ev), B.uplo)
end

function (-)(A::Diagonal, B::Bidiagonal)
    newdv = A.diag-B.dv
    Bidiagonal(newdv, typeof(newdv)(-B.ev), B.uplo)
end

function (+)(A::Diagonal, B::SymTridiagonal)
    newdv = A.diag+B.dv
    SymTridiagonal(A.diag+B.dv, typeof(newdv)(B.ev))
end

function (-)(A::Diagonal, B::SymTridiagonal)
    newdv = A.diag-B.dv
    SymTridiagonal(newdv, typeof(newdv)(-B.ev))
end

function (+)(A::SymTridiagonal, B::Diagonal)
    newdv = A.dv+B.diag
    SymTridiagonal(newdv, typeof(newdv)(A.ev))
end

function (-)(A::SymTridiagonal, B::Diagonal)
    newdv = A.dv-B.diag
    SymTridiagonal(newdv, typeof(newdv)(A.ev))
end

# this set doesn't have the aforementioned problem

+(A::Tridiagonal, B::SymTridiagonal) = Tridiagonal(A.dl+_evview(B), A.d+B.dv, A.du+_evview(B))
-(A::Tridiagonal, B::SymTridiagonal) = Tridiagonal(A.dl-_evview(B), A.d-B.dv, A.du-_evview(B))
+(A::SymTridiagonal, B::Tridiagonal) = Tridiagonal(_evview(A)+B.dl, A.dv+B.d, _evview(A)+B.du)
-(A::SymTridiagonal, B::Tridiagonal) = Tridiagonal(_evview(A)-B.dl, A.dv-B.d, _evview(A)-B.du)


function (+)(A::Diagonal, B::Tridiagonal)
    newdv = A.diag+B.d
    Tridiagonal(typeof(newdv)(B.dl), newdv, typeof(newdv)(B.du))
end

function (-)(A::Diagonal, B::Tridiagonal)
    newdv = A.diag-B.d
    Tridiagonal(typeof(newdv)(-B.dl), newdv, typeof(newdv)(-B.du))
end

function (+)(A::Tridiagonal, B::Diagonal)
    newdv = A.d+B.diag
    Tridiagonal(typeof(newdv)(A.dl), newdv, typeof(newdv)(A.du))
end

function (-)(A::Tridiagonal, B::Diagonal)
    newdv = A.d-B.diag
    Tridiagonal(typeof(newdv)(A.dl), newdv, typeof(newdv)(A.du))
end

function (+)(A::Bidiagonal, B::Tridiagonal)
    newdv = A.dv+B.d
    Tridiagonal((A.uplo == 'U' ? (typeof(newdv)(B.dl), newdv, A.ev+B.du) : (A.ev+B.dl, newdv, typeof(newdv)(B.du)))...)
end

function (-)(A::Bidiagonal, B::Tridiagonal)
    newdv = A.dv-B.d
    Tridiagonal((A.uplo == 'U' ? (typeof(newdv)(-B.dl), newdv, A.ev-B.du) : (A.ev-B.dl, newdv, typeof(newdv)(-B.du)))...)
end

function (+)(A::Tridiagonal, B::Bidiagonal)
    newdv = A.d+B.dv
    Tridiagonal((B.uplo == 'U' ? (typeof(newdv)(A.dl), newdv, A.du+B.ev) : (A.dl+B.ev, newdv, typeof(newdv)(A.du)))...)
end

function (-)(A::Tridiagonal, B::Bidiagonal)
    newdv = A.d-B.dv
    Tridiagonal((B.uplo == 'U' ? (typeof(newdv)(A.dl), newdv, A.du-B.ev) : (A.dl-B.ev, newdv, typeof(newdv)(A.du)))...)
end

function (+)(A::Bidiagonal, B::SymTridiagonal)
    newdv = A.dv+B.dv
    Tridiagonal((A.uplo == 'U' ? (typeof(newdv)(_evview(B)), A.dv+B.dv, A.ev+_evview(B)) : (A.ev+_evview(B), A.dv+B.dv, typeof(newdv)(_evview(B))))...)
end

function (-)(A::Bidiagonal, B::SymTridiagonal)
    newdv = A.dv-B.dv
    Tridiagonal((A.uplo == 'U' ? (typeof(newdv)(-_evview(B)), newdv, A.ev-_evview(B)) : (A.ev-_evview(B), newdv, typeof(newdv)(-_evview(B))))...)
end

function (+)(A::SymTridiagonal, B::Bidiagonal)
    newdv = A.dv+B.dv
    Tridiagonal((B.uplo == 'U' ? (typeof(newdv)(_evview(A)), newdv, _evview(A)+B.ev) : (_evview(A)+B.ev, newdv, typeof(newdv)(_evview(A))))...)
end

function (-)(A::SymTridiagonal, B::Bidiagonal)
    newdv = A.dv-B.dv
    Tridiagonal((B.uplo == 'U' ? (typeof(newdv)(_evview(A)), newdv, _evview(A)-B.ev) : (_evview(A)-B.ev, newdv, typeof(newdv)(_evview(A))))...)
end

# fixing uniform scaling problems from #28994
# {<:Number} is required due to the test case from PR #27289 where eltype is a matrix.

function (+)(A::Tridiagonal{<:Number}, B::UniformScaling)
    newd = A.d .+ B.λ
    Tridiagonal(typeof(newd)(A.dl), newd, typeof(newd)(A.du))
end

function (+)(A::SymTridiagonal{<:Number}, B::UniformScaling)
    newdv = A.dv .+ B.λ
    SymTridiagonal(newdv, typeof(newdv)(A.ev))
end

function (+)(A::Bidiagonal{<:Number}, B::UniformScaling)
    newdv = A.dv .+ B.λ
    Bidiagonal(newdv, typeof(newdv)(A.ev), A.uplo)
end

function (+)(A::Diagonal{<:Number}, B::UniformScaling)
    Diagonal(A.diag .+ B.λ)
end

function (+)(A::UniformScaling, B::Tridiagonal{<:Number})
    newd = A.λ .+ B.d
    Tridiagonal(typeof(newd)(B.dl), newd, typeof(newd)(B.du))
end

function (+)(A::UniformScaling, B::SymTridiagonal{<:Number})
    newdv = A.λ .+ B.dv
    SymTridiagonal(newdv, typeof(newdv)(B.ev))
end

function (+)(A::UniformScaling, B::Bidiagonal{<:Number})
    newdv = A.λ .+ B.dv
    Bidiagonal(newdv, typeof(newdv)(B.ev), B.uplo)
end

function (+)(A::UniformScaling, B::Diagonal{<:Number})
    Diagonal(A.λ .+ B.diag)
end

function (-)(A::UniformScaling, B::Tridiagonal{<:Number})
    newd = A.λ .- B.d
    Tridiagonal(typeof(newd)(-B.dl), newd, typeof(newd)(-B.du))
end

function (-)(A::UniformScaling, B::SymTridiagonal{<:Number})
    newdv = A.λ .- B.dv
    SymTridiagonal(newdv, typeof(newdv)(-B.ev))
end

function (-)(A::UniformScaling, B::Bidiagonal{<:Number})
    newdv = A.λ .- B.dv
    Bidiagonal(newdv, typeof(newdv)(-B.ev), B.uplo)
end

function (-)(A::UniformScaling, B::Diagonal{<:Number})
    Diagonal(A.λ .- B.diag)
end

lmul!(Q::AbstractQ, B::AbstractTriangular) = lmul!(Q, full!(B))
lmul!(Q::QRPackedQ, B::AbstractTriangular) = lmul!(Q, full!(B)) # disambiguation
lmul!(Q::Adjoint{<:Any,<:AbstractQ}, B::AbstractTriangular) = lmul!(Q, full!(B))
lmul!(Q::Adjoint{<:Any,<:QRPackedQ}, B::AbstractTriangular) = lmul!(Q, full!(B)) # disambiguation

function _qlmul(Q::AbstractQ, B)
    TQB = promote_type(eltype(Q), eltype(B))
    if size(Q.factors, 1) == size(B, 1)
        Bnew = Matrix{TQB}(B)
    elseif size(Q.factors, 2) == size(B, 1)
        Bnew = [Matrix{TQB}(B); zeros(TQB, size(Q.factors, 1) - size(B,1), size(B, 2))]
    else
        throw(DimensionMismatch("first dimension of matrix must have size either $(size(Q.factors, 1)) or $(size(Q.factors, 2))"))
    end
    lmul!(convert(AbstractMatrix{TQB}, Q), Bnew)
end
function _qlmul(adjQ::Adjoint{<:Any,<:AbstractQ}, B)
    TQB = promote_type(eltype(adjQ), eltype(B))
    lmul!(adjoint(convert(AbstractMatrix{TQB}, parent(adjQ))), Matrix{TQB}(B))
end

*(Q::AbstractQ, B::AbstractTriangular) = _qlmul(Q, B)
*(Q::Adjoint{<:Any,<:AbstractQ}, B::AbstractTriangular) = _qlmul(Q, B)
*(Q::AbstractQ, B::BiTriSym) = _qlmul(Q, B)
*(Q::Adjoint{<:Any,<:AbstractQ}, B::BiTriSym) = _qlmul(Q, B)
*(Q::AbstractQ, B::Diagonal) = _qlmul(Q, B)
*(Q::Adjoint{<:Any,<:AbstractQ}, B::Diagonal) = _qlmul(Q, B)

rmul!(A::AbstractTriangular, Q::AbstractQ) = rmul!(full!(A), Q)
rmul!(A::AbstractTriangular, Q::Adjoint{<:Any,<:AbstractQ}) = rmul!(full!(A), Q)

function _qrmul(A, Q::AbstractQ)
    TAQ = promote_type(eltype(A), eltype(Q))
    return rmul!(Matrix{TAQ}(A), convert(AbstractMatrix{TAQ}, Q))
end
function _qrmul(A, adjQ::Adjoint{<:Any,<:AbstractQ})
    Q = adjQ.parent
    TAQ = promote_type(eltype(A), eltype(Q))
    if size(A,2) == size(Q.factors, 1)
        Anew = Matrix{TAQ}(A)
    elseif size(A,2) == size(Q.factors,2)
        Anew = [Matrix{TAQ}(A) zeros(TAQ, size(A, 1), size(Q.factors, 1) - size(Q.factors, 2))]
    else
        throw(DimensionMismatch("matrix A has dimensions $(size(A)) but matrix B has dimensions $(size(Q))"))
    end
    return rmul!(Anew, adjoint(convert(AbstractMatrix{TAQ}, Q)))
end

*(A::AbstractTriangular, Q::AbstractQ) = _qrmul(A, Q)
*(A::AbstractTriangular, Q::Adjoint{<:Any,<:AbstractQ}) = _qrmul(A, Q)
*(A::BiTriSym, Q::AbstractQ) = _qrmul(A, Q)
*(A::BiTriSym, Q::Adjoint{<:Any,<:AbstractQ}) = _qrmul(A, Q)
*(A::Diagonal, Q::AbstractQ) = _qrmul(A, Q)
*(A::Diagonal, Q::Adjoint{<:Any,<:AbstractQ}) = _qrmul(A, Q)

*(Q::AbstractQ, B::AbstractQ) = _qlmul(Q, B)
*(Q::Adjoint{<:Any,<:AbstractQ}, B::AbstractQ) = _qrmul(Q, B)
*(Q::AbstractQ, B::Adjoint{<:Any,<:AbstractQ}) = _qlmul(Q, B)
*(Q::Adjoint{<:Any,<:AbstractQ}, B::Adjoint{<:Any,<:AbstractQ}) = _qrmul(Q, B)

# fill[stored]! methods
fillstored!(A::Diagonal, x) = (fill!(A.diag, x); A)
fillstored!(A::Bidiagonal, x) = (fill!(A.dv, x); fill!(A.ev, x); A)
fillstored!(A::Tridiagonal, x) = (fill!(A.dl, x); fill!(A.d, x); fill!(A.du, x); A)
fillstored!(A::SymTridiagonal, x) = (fill!(A.dv, x); fill!(A.ev, x); A)

_small_enough(A::Union{Diagonal, Bidiagonal}) = size(A, 1) <= 1
_small_enough(A::Tridiagonal) = size(A, 1) <= 2
_small_enough(A::SymTridiagonal) = size(A, 1) <= 2

function fill!(A::Union{Diagonal,Bidiagonal,Tridiagonal,SymTridiagonal}, x)
    xT = convert(eltype(A), x)
    (iszero(xT) || _small_enough(A)) && return fillstored!(A, xT)
    throw(ArgumentError("array of type $(typeof(A)) and size $(size(A)) can
    not be filled with $x, since some of its entries are constrained."))
end

one(D::Diagonal) = Diagonal(one.(D.diag))
one(A::Bidiagonal{T}) where T = Bidiagonal(fill!(similar(A.dv, typeof(one(T))), one(T)), fill!(similar(A.ev, typeof(one(T))), zero(one(T))), A.uplo)
one(A::Tridiagonal{T}) where T = Tridiagonal(fill!(similar(A.du, typeof(one(T))), zero(one(T))), fill!(similar(A.d, typeof(one(T))), one(T)), fill!(similar(A.dl, typeof(one(T))), zero(one(T))))
one(A::SymTridiagonal{T}) where T = SymTridiagonal(fill!(similar(A.dv, typeof(one(T))), one(T)), fill!(similar(A.ev, typeof(one(T))), zero(one(T))))
for t in (:LowerTriangular, :UnitLowerTriangular, :UpperTriangular, :UnitUpperTriangular)
    @eval one(A::$t) = $t(one(parent(A)))
    @eval oneunit(A::$t) = $t(oneunit(parent(A)))
end

zero(D::Diagonal) = Diagonal(zero.(D.diag))
oneunit(D::Diagonal) = Diagonal(oneunit.(D.diag))

# equals and approx equals methods for structured matrices
# SymTridiagonal == Tridiagonal is already defined in tridiag.jl

==(A::Diagonal, B::Bidiagonal) = iszero(B.ev) && A.diag == B.dv
==(A::Diagonal, B::SymTridiagonal) = iszero(_evview(B)) && A.diag == B.dv
==(B::Bidiagonal, A::Diagonal) = A == B
==(A::Diagonal, B::Tridiagonal) = iszero(B.dl) && iszero(B.du) && A.diag == B.d
==(B::Tridiagonal, A::Diagonal) = A == B

function ==(A::Bidiagonal, B::Tridiagonal)
    if A.uplo == 'U'
        return iszero(B.dl) && A.dv == B.d && A.ev == B.du
    else
        return iszero(B.du) && A.dv == B.d && A.ev == B.dl
    end
end
==(B::Tridiagonal, A::Bidiagonal) = A == B

==(A::Bidiagonal, B::SymTridiagonal) = iszero(_evview(B)) && iszero(A.ev) && A.dv == B.dv
==(B::SymTridiagonal, A::Bidiagonal) = A == B

# concatenation
const _SpecialArrays = Union{Diagonal, Bidiagonal, Tridiagonal, SymTridiagonal}
const _Symmetric_DenseArrays{T,A<:Matrix} = Symmetric{T,A}
const _Hermitian_DenseArrays{T,A<:Matrix} = Hermitian{T,A}
const _Triangular_DenseArrays{T,A<:Matrix} = AbstractTriangular{T,A}
const _Annotated_DenseArrays = Union{_SpecialArrays, _Triangular_DenseArrays, _Symmetric_DenseArrays, _Hermitian_DenseArrays}
const _Annotated_Typed_DenseArrays{T} = Union{_Triangular_DenseArrays{T}, _Symmetric_DenseArrays{T}, _Hermitian_DenseArrays{T}}
const _DenseConcatGroup = Union{Number, Vector, Adjoint{<:Any,<:Vector}, Transpose{<:Any,<:Vector}, Matrix, _Annotated_DenseArrays}
const _TypedDenseConcatGroup{T} = Union{Vector{T}, Adjoint{T,Vector{T}}, Transpose{T,Vector{T}}, Matrix{T}, _Annotated_Typed_DenseArrays{T}}

promote_to_array_type(::Tuple{Vararg{Union{_DenseConcatGroup,UniformScaling}}}) = Matrix

Base._cat(dims, xs::_DenseConcatGroup...) = Base.cat_t(promote_eltype(xs...), xs...; dims=dims)
vcat(A::Vector...) = Base.typed_vcat(promote_eltype(A...), A...)
vcat(A::_DenseConcatGroup...) = Base.typed_vcat(promote_eltype(A...), A...)
hcat(A::Vector...) = Base.typed_hcat(promote_eltype(A...), A...)
hcat(A::_DenseConcatGroup...) = Base.typed_hcat(promote_eltype(A...), A...)
hvcat(rows::Tuple{Vararg{Int}}, xs::_DenseConcatGroup...) = Base.typed_hvcat(promote_eltype(xs...), rows, xs...)
# For performance, specially handle the case where the matrices/vectors have homogeneous eltype
Base._cat(dims, xs::_TypedDenseConcatGroup{T}...) where {T} = Base.cat_t(T, xs...; dims=dims)
vcat(A::_TypedDenseConcatGroup{T}...) where {T} = Base.typed_vcat(T, A...)
hcat(A::_TypedDenseConcatGroup{T}...) where {T} = Base.typed_hcat(T, A...)
hvcat(rows::Tuple{Vararg{Int}}, xs::_TypedDenseConcatGroup{T}...) where {T} = Base.typed_hvcat(T, rows, xs...)

# factorizations
function cholesky(S::RealHermSymComplexHerm{<:Real,<:SymTridiagonal}, ::NoPivot = NoPivot(); check::Bool = true)
    T = choltype(eltype(S))
    B = Bidiagonal{T}(diag(S, 0), diag(S, S.uplo == 'U' ? 1 : -1), sym_uplo(S.uplo))
    cholesky!(Hermitian(B, sym_uplo(S.uplo)), NoPivot(); check = check)
end
