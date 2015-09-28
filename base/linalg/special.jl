# This file is a part of Julia. License is MIT: http://julialang.org/license

#Methods operating on different special matrix types

#Interconversion between special matrix types
convert{T}(::Type{Bidiagonal}, A::Diagonal{T})=Bidiagonal(A.diag, zeros(T, size(A.diag,1)-1), true)
convert{T}(::Type{SymTridiagonal}, A::Diagonal{T})=SymTridiagonal(A.diag, zeros(T, size(A.diag,1)-1))
convert{T}(::Type{Tridiagonal}, A::Diagonal{T})=Tridiagonal(zeros(T, size(A.diag,1)-1), A.diag, zeros(T, size(A.diag,1)-1))
convert(::Type{LowerTriangular}, A::Bidiagonal) = !A.isupper ? LowerTriangular(full(A)) : throw(ArgumentError("Bidiagonal matrix must have lower off diagonal to be converted to LowerTriangular"))
convert(::Type{UpperTriangular}, A::Bidiagonal) = A.isupper ? UpperTriangular(full(A)) : throw(ArgumentError("Bidiagonal matrix must have upper off diagonal to be converted to UpperTriangular"))
convert(::Type{Matrix}, D::Diagonal) = diagm(D.diag)

function convert(::Type{UnitUpperTriangular}, A::Diagonal)
    if !all(A.diag .== one(eltype(A)))
        throw(ArgumentError("matrix cannot be represented as UnitUpperTriangular"))
    end
    UnitUpperTriangular(full(A))
end

function convert(::Type{UnitLowerTriangular}, A::Diagonal)
    if !all(A.diag .== one(eltype(A)))
        throw(ArgumentError("matrix cannot be represented as UnitLowerTriangular"))
    end
    UnitLowerTriangular(full(A))
end

function convert(::Type{Diagonal}, A::Union{Bidiagonal, SymTridiagonal})
    if !all(A.ev .== 0)
        throw(ArgumentError("matrix cannot be represented as Diagonal"))
    end
    Diagonal(A.dv)
end

function convert(::Type{SymTridiagonal}, A::Bidiagonal)
    if !all(A.ev .== 0)
        throw(ArgumentError("matrix cannot be represented as SymTridiagonal"))
    end
    SymTridiagonal(A.dv, A.ev)
end

convert{T}(::Type{Tridiagonal}, A::Bidiagonal{T})=Tridiagonal(A.isupper?zeros(T, size(A.dv,1)-1):A.ev, A.dv, A.isupper?A.ev:zeros(T, size(A.dv,1)-1))

function convert(::Type{Bidiagonal}, A::SymTridiagonal)
    if !all(A.ev .== 0)
        throw(ArgumentError("matrix cannot be represented as Bidiagonal"))
    end
    Bidiagonal(A.dv, A.ev, true)
end

function convert(::Type{Diagonal}, A::Tridiagonal)
    if !(all(A.dl .== 0) && all(A.du .== 0))
        throw(ArgumentError("matrix cannot be represented as Diagonal"))
    end
    Diagonal(A.d)
end

function convert(::Type{Bidiagonal}, A::Tridiagonal)
    if all(A.dl .== 0) return Bidiagonal(A.d, A.du, true)
    elseif all(A.du .== 0) return Bidiagonal(A.d, A.dl, false)
    else throw(ArgumentError("matrix cannot be represented as Bidiagonal"))
    end
end

function convert(::Type{SymTridiagonal}, A::Tridiagonal)
    if !all(A.dl .== A.du)
        throw(ArgumentError("matrix cannot be represented as SymTridiagonal"))
    end
    SymTridiagonal(A.d, A.dl)
end

function convert(::Type{Tridiagonal}, A::SymTridiagonal)
    Tridiagonal(copy(A.ev), A.dv, A.ev)
end

function convert(::Type{Diagonal}, A::AbstractTriangular)
    if full(A) != diagm(diag(A))
        throw(ArgumentError("matrix cannot be represented as Diagonal"))
    end
    Diagonal(diag(A))
end

function convert(::Type{Bidiagonal}, A::AbstractTriangular)
    fA = full(A)
    if fA == diagm(diag(A)) + diagm(diag(fA, 1), 1)
        return Bidiagonal(diag(A), diag(fA,1), true)
    elseif fA == diagm(diag(A)) + diagm(diag(fA, -1), -1)
        return Bidiagonal(diag(A), diag(fA,-1), false)
    else
        throw(ArgumentError("matrix cannot be represented as Bidiagonal"))
    end
end

convert(::Type{SymTridiagonal}, A::AbstractTriangular) = convert(SymTridiagonal, convert(Tridiagonal, A))

function convert(::Type{Tridiagonal}, A::AbstractTriangular)
    fA = full(A)
    if fA == diagm(diag(A)) + diagm(diag(fA, 1), 1) + diagm(diag(fA, -1), -1)
        return Tridiagonal(diag(fA, -1), diag(A), diag(fA,1))
    else
        throw(ArgumentError("matrix cannot be represented as Tridiagonal"))
    end
end

#Constructs two method definitions taking into account (assumed) commutativity
# e.g. @commutative f{S,T}(x::S, y::T) = x+y is the same is defining
#     f{S,T}(x::S, y::T) = x+y
#     f{S,T}(y::T, x::S) = f(x, y)
macro commutative(myexpr)
    @assert myexpr.head===:(=) || myexpr.head===:function #Make sure it is a function definition
    y = copy(myexpr.args[1].args[2:end])
    reverse!(y)
    reversed_call = Expr(:(=), Expr(:call,myexpr.args[1].args[1],y...), myexpr.args[1])
    esc(Expr(:block, myexpr, reversed_call))
end

for op in (:+, :-)
    SpecialMatrices = [:Diagonal, :Bidiagonal, :Tridiagonal, :Matrix]
    for (idx, matrixtype1) in enumerate(SpecialMatrices) #matrixtype1 is the sparser matrix type
        for matrixtype2 in SpecialMatrices[idx+1:end] #matrixtype2 is the denser matrix type
            @eval begin #TODO quite a few of these conversions are NOT defined...
                ($op)(A::($matrixtype1), B::($matrixtype2)) = ($op)(convert(($matrixtype2), A), B)
                ($op)(A::($matrixtype2), B::($matrixtype1)) = ($op)(A, convert(($matrixtype2), B))
            end
        end
    end

    for  matrixtype1 in (:SymTridiagonal,)                      #matrixtype1 is the sparser matrix type
        for matrixtype2 in (:Tridiagonal, :Matrix) #matrixtype2 is the denser matrix type
            @eval begin
                ($op)(A::($matrixtype1), B::($matrixtype2)) = ($op)(convert(($matrixtype2), A), B)
                ($op)(A::($matrixtype2), B::($matrixtype1)) = ($op)(A, convert(($matrixtype2), B))
            end
        end
    end

    for matrixtype1 in (:Diagonal, :Bidiagonal) #matrixtype1 is the sparser matrix type
        for matrixtype2 in (:SymTridiagonal,)   #matrixtype2 is the denser matrix type
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
                ($op)(A::($matrixtype1), B::($matrixtype2)) = ($op)(convert(($matrixtype3), A), B)
                ($op)(A::($matrixtype2), B::($matrixtype1)) = ($op)(A, convert(($matrixtype3), B))
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
