# This file is a part of Julia. License is MIT: https://julialang.org/license

# Methods operating on different special matrix types


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


const ConvertibleSpecialMatrix = Union{Diagonal,Bidiagonal,SymTridiagonal,Tridiagonal,AbstractTriangular}

convert(T::Type{<:Diagonal},       m::ConvertibleSpecialMatrix) = m isa T ? m : T(m)
convert(T::Type{<:SymTridiagonal}, m::ConvertibleSpecialMatrix) = m isa T ? m : T(m)
convert(T::Type{<:Tridiagonal},    m::ConvertibleSpecialMatrix) = m isa T ? m : T(m)

convert(T::Type{<:LowerTriangular}, m::Union{LowerTriangular,UnitLowerTriangular}) = m isa T ? m : T(m)
convert(T::Type{<:UpperTriangular}, m::Union{UpperTriangular,UnitUpperTriangular}) = m isa T ? m : T(m)

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
    SpecialMatrices = [:Diagonal, :Bidiagonal, :Tridiagonal, :Matrix]
    for (idx, matrixtype1) in enumerate(SpecialMatrices) # matrixtype1 is the sparser matrix type
        for matrixtype2 in SpecialMatrices[idx+1:end] # matrixtype2 is the denser matrix type
            @eval begin # TODO quite a few of these conversions are NOT defined
                ($op)(A::($matrixtype1), B::($matrixtype2)) = ($op)(convert(($matrixtype2), A), B)
                ($op)(A::($matrixtype2), B::($matrixtype1)) = ($op)(A, convert(($matrixtype2), B))
            end
        end
    end

    for  matrixtype1 in (:SymTridiagonal,)                      # matrixtype1 is the sparser matrix type
        for matrixtype2 in (:Tridiagonal, :Matrix) # matrixtype2 is the denser matrix type
            @eval begin
                ($op)(A::($matrixtype1), B::($matrixtype2)) = ($op)(convert(($matrixtype2), A), B)
                ($op)(A::($matrixtype2), B::($matrixtype1)) = ($op)(A, convert(($matrixtype2), B))
            end
        end
    end

    for matrixtype1 in (:Diagonal, :Bidiagonal) # matrixtype1 is the sparser matrix type
        for matrixtype2 in (:SymTridiagonal,)   # matrixtype2 is the denser matrix type
            @eval begin
                ($op)(A::($matrixtype1), B::($matrixtype2)) = ($op)(convert(($matrixtype2), A), B)
                ($op)(A::($matrixtype2), B::($matrixtype1)) = ($op)(A, convert(($matrixtype2), B))
            end
        end
    end

    for matrixtype1 in (:Diagonal,)
        for (matrixtype2,matrixtype3) in ((:UpperTriangular,:UpperTriangular),
                                          (:UnitUpperTriangular,:UpperTriangular),
                                          (:LowerTriangular,:LowerTriangular),
                                          (:UnitLowerTriangular,:LowerTriangular))
            @eval begin
                ($op)(A::($matrixtype1), B::($matrixtype2)) = ($op)(($matrixtype3)(A), B)
                ($op)(A::($matrixtype2), B::($matrixtype1)) = ($op)(A, ($matrixtype3)(B))
            end
        end
    end
    for matrixtype in (:SymTridiagonal,:Tridiagonal,:Bidiagonal,:Matrix)
        @eval begin
            ($op)(A::AbstractTriangular, B::($matrixtype)) = ($op)(copyto!(similar(parent(A)), A), B)
            ($op)(A::($matrixtype), B::AbstractTriangular) = ($op)(A, copyto!(similar(parent(B)), B))
        end
    end
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

_small_enough(A::Bidiagonal) = size(A, 1) <= 1
_small_enough(A::Tridiagonal) = size(A, 1) <= 2
_small_enough(A::SymTridiagonal) = size(A, 1) <= 2

# TODO: Add Diagonal to this method when 0.7 deprecations are removed
function fill!(A::Union{Bidiagonal,Tridiagonal,SymTridiagonal}, x)
    xT = convert(eltype(A), x)
    (iszero(xT) || _small_enough(A)) && return fillstored!(A, xT)
    throw(ArgumentError("array of type $(typeof(A)) and size $(size(A)) can
    not be filled with $x, since some of its entries are constrained."))
end
