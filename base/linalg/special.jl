#Methods operating on different special matrix types

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

