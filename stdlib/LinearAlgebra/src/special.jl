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
Diagonal(A::Bidiagonal) =
    iszero(A.ev) ? Diagonal(A.dv) :
        throw(ArgumentError("matrix cannot be represented as Diagonal"))
SymTridiagonal(A::Bidiagonal) =
    iszero(A.ev) ? SymTridiagonal(A.dv, A.ev) :
        throw(ArgumentError("matrix cannot be represented as SymTridiagonal"))
Tridiagonal(A::Bidiagonal) =
    Tridiagonal(A.uplo == 'U' ? fill!(similar(A.ev), 0) : A.ev, A.dv,
                A.uplo == 'U' ? A.ev : fill!(similar(A.ev), 0))

# conversions from SymTridiagonal to other special matrix types
Diagonal(A::SymTridiagonal) =
    iszero(A.ev) ? Diagonal(A.dv) :
        throw(ArgumentError("matrix cannot be represented as Diagonal"))
Bidiagonal(A::SymTridiagonal) =
    iszero(A.ev) ? Bidiagonal(A.dv, A.ev, :U) :
        throw(ArgumentError("matrix cannot be represented as Bidiagonal"))
Tridiagonal(A::SymTridiagonal) =
    Tridiagonal(copy(A.ev), A.dv, A.ev)

# conversions from Tridiagonal to other special matrix types
Diagonal(A::Tridiagonal) =
    iszero(A.dl) && iszero(A.du) ? Diagonal(A.d) :
        throw(ArgumentError("matrix cannot be represented as Diagonal"))
Bidiagonal(A::Tridiagonal) =
    iszero(A.dl) ? Bidiagonal(A.d, A.du, :U) :
    iszero(A.du) ? Bidiagonal(A.d, A.dl, :L) :
        throw(ArgumentError("matrix cannot be represented as Bidiagonal"))
SymTridiagonal(A::Tridiagonal) =
    A.dl == A.du ? SymTridiagonal(A.d, A.dl) :
        throw(ArgumentError("matrix cannot be represented as SymTridiagonal"))

# conversions from AbstractTriangular to special matrix types
Diagonal(A::AbstractTriangular) =
    isdiag(A) ? Diagonal(diag(A)) :
        throw(ArgumentError("matrix cannot be represented as Diagonal"))
Bidiagonal(A::AbstractTriangular) =
    isbanded(A, 0, 1) ? Bidiagonal(diag(A, 0), diag(A,  1), :U) : # is upper bidiagonal
    isbanded(A, -1, 0) ? Bidiagonal(diag(A, 0), diag(A, -1), :L) : # is lower bidiagonal
        throw(ArgumentError("matrix cannot be represented as Bidiagonal"))
SymTridiagonal(A::AbstractTriangular) = SymTridiagonal(Tridiagonal(A))
Tridiagonal(A::AbstractTriangular) =
    isbanded(A, -1, 1) ? Tridiagonal(diag(A, -1), diag(A, 0), diag(A, 1)) : # is tridiagonal
        throw(ArgumentError("matrix cannot be represented as Tridiagonal"))
UpperTriangular(A::Bidiagonal) =
    A.uplo == 'U' ? UpperTriangular{eltype(A), typeof(A)}(A) :
        throw(ArgumentError("matrix cannot be represented as UpperTriangular"))
LowerTriangular(A::Bidiagonal) =
    A.uplo == 'L' ? LowerTriangular{eltype(A), typeof(A)}(A) :
        throw(ArgumentError("matrix cannot be represented as LowerTriangular"))

const ConvertibleSpecialMatrix = Union{Diagonal,Bidiagonal,SymTridiagonal,Tridiagonal,AbstractTriangular}
const PossibleTriangularMatrix = Union{Diagonal, Bidiagonal, AbstractTriangular}

convert(T::Type{<:Diagonal},       m::ConvertibleSpecialMatrix) = m isa T ? m : T(m)
convert(T::Type{<:SymTridiagonal}, m::ConvertibleSpecialMatrix) = m isa T ? m : T(m)
convert(T::Type{<:Tridiagonal},    m::ConvertibleSpecialMatrix) = m isa T ? m : T(m)

convert(T::Type{<:LowerTriangular}, m::Union{LowerTriangular,UnitLowerTriangular}) = m isa T ? m : T(m)
convert(T::Type{<:UpperTriangular}, m::Union{UpperTriangular,UnitUpperTriangular}) = m isa T ? m : T(m)

convert(T::Type{<:LowerTriangular}, m::PossibleTriangularMatrix) = m isa T ? m : T(m)
convert(T::Type{<:UpperTriangular}, m::PossibleTriangularMatrix) = m isa T ? m : T(m)

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

+(A::Tridiagonal, B::SymTridiagonal) = Tridiagonal(A.dl+B.ev, A.d+B.dv, A.du+B.ev)
-(A::Tridiagonal, B::SymTridiagonal) = Tridiagonal(A.dl-B.ev, A.d-B.dv, A.du-B.ev)
+(A::SymTridiagonal, B::Tridiagonal) = Tridiagonal(A.ev+B.dl, A.dv+B.d, A.ev+B.du)
-(A::SymTridiagonal, B::Tridiagonal) = Tridiagonal(A.ev-B.dl, A.dv-B.d, A.ev-B.du)


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
    Tridiagonal((A.uplo == 'U' ? (typeof(newdv)(B.ev), A.dv+B.dv, A.ev+B.ev) : (A.ev+B.ev, A.dv+B.dv, typeof(newdv)(B.ev)))...)
end

function (-)(A::Bidiagonal, B::SymTridiagonal)
    newdv = A.dv-B.dv
    Tridiagonal((A.uplo == 'U' ? (typeof(newdv)(-B.ev), newdv, A.ev-B.ev) : (A.ev-B.ev, newdv, typeof(newdv)(-B.ev)))...)
end

function (+)(A::SymTridiagonal, B::Bidiagonal)
    newdv = A.dv+B.dv
    Tridiagonal((B.uplo == 'U' ? (typeof(newdv)(A.ev), newdv, A.ev+B.ev) : (A.ev+B.ev, newdv, typeof(newdv)(A.ev)))...)
end

function (-)(A::SymTridiagonal, B::Bidiagonal)
    newdv = A.dv-B.dv
    Tridiagonal((B.uplo == 'U' ? (typeof(newdv)(A.ev), newdv, A.ev-B.ev) : (A.ev-B.ev, newdv, typeof(newdv)(A.ev)))...)
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

rmul!(A::AbstractTriangular, adjB::Adjoint{<:Any,<:Union{QRCompactWYQ,QRPackedQ}}) =
    (B = adjB.parent; rmul!(full!(A), adjoint(B)))
*(A::AbstractTriangular, adjB::Adjoint{<:Any,<:Union{QRCompactWYQ,QRPackedQ}}) =
    (B = adjB.parent; *(copyto!(similar(parent(A)), A), adjoint(B)))

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

one(A::Diagonal{T}) where T = Diagonal(fill!(similar(A.diag, typeof(one(T))), one(T)))
one(A::Bidiagonal{T}) where T = Bidiagonal(fill!(similar(A.dv, typeof(one(T))), one(T)), fill!(similar(A.ev, typeof(one(T))), zero(one(T))), A.uplo)
one(A::Tridiagonal{T}) where T = Tridiagonal(fill!(similar(A.du, typeof(one(T))), zero(one(T))), fill!(similar(A.d, typeof(one(T))), one(T)), fill!(similar(A.dl, typeof(one(T))), zero(one(T))))
one(A::SymTridiagonal{T}) where T = SymTridiagonal(fill!(similar(A.dv, typeof(one(T))), one(T)), fill!(similar(A.ev, typeof(one(T))), zero(one(T))))
# equals and approx equals methods for structured matrices
# SymTridiagonal == Tridiagonal is already defined in tridiag.jl

# SymTridiagonal and Bidiagonal have the same field names
==(A::Diagonal, B::Union{SymTridiagonal, Bidiagonal}) = iszero(B.ev) && A.diag == B.dv
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

==(A::Bidiagonal, B::SymTridiagonal) = iszero(B.ev) && iszero(A.ev) && A.dv == B.dv
==(B::SymTridiagonal, A::Bidiagonal) = A == B
