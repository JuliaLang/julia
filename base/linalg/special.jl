#Methods operating on different special matrix types

#Interconversion between special matrix types
import Base.convert
convert{T}(::Type{Bidiagonal}, A::Diagonal{T})=Bidiagonal(A.diag, zeros(T, size(A.diag,1)-1), true)
convert{T}(::Type{SymTridiagonal}, A::Diagonal{T})=SymTridiagonal(A.diag, zeros(T, size(A.diag,1)-1))
convert{T}(::Type{Tridiagonal}, A::Diagonal{T})=Tridiagonal(zeros(T, size(A.diag,1)-1), A.diag, zeros(T, size(A.diag,1)-1))
convert(::Type{Triangular}, A::Union(Diagonal, Bidiagonal, SymTridiagonal, Tridiagonal))=Triangular(full(A))
convert(::Type{Matrix}, D::Diagonal) = diagm(D.diag)

function convert(::Type{Diagonal}, A::Union(Bidiagonal, SymTridiagonal))
    all(A.ev .== 0) || throw(ArgumentError("Matrix cannot be represented as Diagonal"))
    Diagonal(A.dv)
end

function convert(::Type{SymTridiagonal}, A::Bidiagonal)
    all(A.ev .== 0) || throw(ArgumentError("Matrix cannot be represented as SymTridiagonal"))
    SymTridiagonal(A.dv, A.ev)
end

convert{T}(::Type{Tridiagonal}, A::Bidiagonal{T})=Tridiagonal(A.isupper?zeros(T, size(A.dv,1)-1):A.ev, A.dv, A.isupper?A.ev:zeros(T, size(A.dv,1)-1))

function convert(::Type{Bidiagonal}, A::SymTridiagonal)
    all(A.ev .== 0) || throw(ArgumentError("Matrix cannot be represented as Bidiagonal"))
    Bidiagonal(A.dv, A.ev, true)
end

function convert(::Type{Diagonal}, A::Tridiagonal)
    all(A.dl .== 0) && all(A.du .== 0) || throw(ArgumentError("Matrix cannot be represented as Diagonal"))
    Diagonal(A.d)
end

function convert(::Type{Bidiagonal}, A::Tridiagonal)
    if all(A.dl .== 0) return Bidiagonal(A.d, A.du, true)
    elseif all(A.du .== 0) return Bidiagonal(A.d, A.dl, true)
    else throw(ArgumentError("Matrix cannot be represented as Bidiagonal"))
    end
end

function convert(::Type{SymTridiagonal}, A::Tridiagonal)
    all(A.dl .== A.du) || throw(ArgumentError("Matrix cannot be represented as SymTridiagonal"))
    SymTridiagonal(A.d, A.dl)
end

function convert(::Type{Diagonal}, A::Triangular)
    full(A) == diagm(diag(A)) || throw(ArgumentError("Matrix cannot be represented as Diagonal"))
    Diagonal(diag(A))
end

function convert(::Type{Bidiagonal}, A::Triangular)
    fA = full(A)
    if fA == diagm(diag(A)) + diagm(diag(fA, 1), 1)
        return Bidiagonal(diag(A), diag(fA,1), true)
    elseif fA == diagm(diag(A)) + diagm(diag(fA, -1), -1)
        return Bidiagonal(diag(A), diag(fA,-1), true)
    else
        throw(ArgumentError("Matrix cannot be represented as Bidiagonal"))
    end
end

convert(::Type{SymTridiagonal}, A::Triangular) = convert(SymTridiagonal, convert(Tridiagonal, A))

function convert(::Type{Tridiagonal}, A::Triangular)
    fA = full(A)
    if fA == diagm(diag(A)) + diagm(diag(fA, 1), 1) + diagm(diag(fA, -1), -1) 
        return Tridiagonal(diag(fA, -1), diag(A), diag(fA,1))
    else
        throw(ArgumentError("Matrix cannot be represented as Tridiagonal"))
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
    #matrixtype1 is the sparser matrix type
    for (idx, matrixtype1) in enumerate([:Diagonal, :Bidiagonal, :Tridiagonal, :Triangular, :Matrix])
        #matrixtype2 is the denser matrix type
        for matrixtype2 in [:Diagonal, :Bidiagonal, :Tridiagonal, :Triangular, :Matrix][idx+1:end]
            @eval begin #TODO quite a few of these conversions are NOT defined...
                ($op)(A::($matrixtype1), B::($matrixtype2)) = ($op)(convert(($matrixtype2), A), B)
                ($op)(A::($matrixtype2), B::($matrixtype1)) = ($op)(A, convert(($matrixtype2), B))
            end
        end
    end

    #matrixtype1 is the sparser matrix type
    for (idx, matrixtype1) in enumerate([:SymTridiagonal])
        #matrixtype2 is the denser matrix type
        for matrixtype2 in [:Tridiagonal, :Triangular, :Matrix][idx+1:end]
            @eval begin
                ($op)(A::($matrixtype1), B::($matrixtype2)) = ($op)(convert(($matrixtype2), A), B)
                ($op)(A::($matrixtype2), B::($matrixtype1)) = ($op)(A, convert(($matrixtype2), B))
            end
        end
    end
end

