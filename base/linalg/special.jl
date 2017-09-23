# This file is a part of Julia. License is MIT: https://julialang.org/license

# Methods operating on different special matrix types

# Interconversion between special matrix types
convert(::Type{Bidiagonal}, A::Diagonal) =
    Bidiagonal(A.diag, fill!(similar(A.diag, length(A.diag)-1), 0), :U)
convert(::Type{SymTridiagonal}, A::Diagonal) =
    SymTridiagonal(A.diag, fill!(similar(A.diag, length(A.diag)-1), 0))
convert(::Type{Tridiagonal}, A::Diagonal) =
    Tridiagonal(fill!(similar(A.diag, length(A.diag)-1), 0), A.diag,
                fill!(similar(A.diag, length(A.diag)-1), 0))

function convert(::Type{Diagonal}, A::Union{Bidiagonal, SymTridiagonal})
    if !iszero(A.ev)
        throw(ArgumentError("matrix cannot be represented as Diagonal"))
    end
    Diagonal(A.dv)
end

function convert(::Type{SymTridiagonal}, A::Bidiagonal)
    if !iszero(A.ev)
        throw(ArgumentError("matrix cannot be represented as SymTridiagonal"))
    end
    SymTridiagonal(A.dv, A.ev)
end

convert(::Type{Tridiagonal}, A::Bidiagonal{T}) where {T} =
    Tridiagonal(A.uplo == 'U' ? fill!(similar(A.ev), 0) : A.ev, A.dv,
                A.uplo == 'U' ? A.ev : fill!(similar(A.ev), 0))

function convert(::Type{Bidiagonal}, A::SymTridiagonal)
    if !iszero(A.ev)
        throw(ArgumentError("matrix cannot be represented as Bidiagonal"))
    end
    Bidiagonal(A.dv, A.ev, :U)
end

function convert(::Type{Diagonal}, A::Tridiagonal)
    if !(iszero(A.dl) && iszero(A.du))
        throw(ArgumentError("matrix cannot be represented as Diagonal"))
    end
    Diagonal(A.d)
end

function convert(::Type{Bidiagonal}, A::Tridiagonal)
    if iszero(A.dl)
        return Bidiagonal(A.d, A.du, :U)
    elseif iszero(A.du)
        return Bidiagonal(A.d, A.dl, :L)
    else
        throw(ArgumentError("matrix cannot be represented as Bidiagonal"))
    end
end

function convert(::Type{SymTridiagonal}, A::Tridiagonal)
    if A.dl != A.du
        throw(ArgumentError("matrix cannot be represented as SymTridiagonal"))
    end
    SymTridiagonal(A.d, A.dl)
end

function convert(::Type{Tridiagonal}, A::SymTridiagonal)
    Tridiagonal(copy(A.ev), A.dv, A.ev)
end

function convert(::Type{Diagonal}, A::AbstractTriangular)
    if !isdiag(A)
        throw(ArgumentError("matrix cannot be represented as Diagonal"))
    end
    Diagonal(diag(A))
end

function convert(::Type{Bidiagonal}, A::AbstractTriangular)
    if isbanded(A, 0, 1) # is upper bidiagonal
        return Bidiagonal(diag(A), diag(A, 1), :U)
    elseif isbanded(A, -1, 0) # is lower bidiagonal
        return Bidiagonal(diag(A), diag(A, -1), :L)
    else
        throw(ArgumentError("matrix cannot be represented as Bidiagonal"))
    end
end

convert(::Type{SymTridiagonal}, A::AbstractTriangular) =
    convert(SymTridiagonal, convert(Tridiagonal, A))

function convert(::Type{Tridiagonal}, A::AbstractTriangular)
    if isbanded(A, -1, 1) # is tridiagonal
        return Tridiagonal(diag(A, -1), diag(A), diag(A, 1))
    else
        throw(ArgumentError("matrix cannot be represented as Tridiagonal"))
    end
end

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
            ($op)(A::AbstractTriangular, B::($matrixtype)) = ($op)(full(A), B)
            ($op)(A::($matrixtype), B::AbstractTriangular) = ($op)(A, full(B))
        end
    end
end

A_mul_Bc!(A::AbstractTriangular, B::QRCompactWYQ) = A_mul_Bc!(full!(A),B)
A_mul_Bc!(A::AbstractTriangular, B::QRPackedQ) = A_mul_Bc!(full!(A),B)
A_mul_Bc(A::AbstractTriangular, B::Union{QRCompactWYQ,QRPackedQ}) = A_mul_Bc(full(A), B)
