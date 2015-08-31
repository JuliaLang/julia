# This file is a part of Julia. License is MIT: http://julialang.org/license

@generated function staged_t1(a,b)
    if a == Int
        return :(a+b)
    else
        return :(a*b)
    end
end

@test staged_t1(1,2) == 3
@test staged_t1(1.0,0.5) == 0.5
@test staged_t1(1,0.5) == 1.5

tinline(a,b) = staged_t1(a,b)

@test !isa(tinline(1,2),Expr)
@test tinline(1,0.5) == 1.5

@generated function splat(a,b...)
    :( ($a,$b,a,b) )
end

@test splat(1,2,3) == (Int,(Int,Int),1,(2,3))

stagediobuf = IOBuffer()
@generated function splat2(a...)
    print(stagediobuf, a)
    :(nothing)
end

const intstr = @sprintf("%s", Int)
splat2(1)
@test takebuf_string(stagediobuf) == "($intstr,)"
splat2(1,3)
@test takebuf_string(stagediobuf) == "($intstr,$intstr)"
splat2(5,2)
@test takebuf_string(stagediobuf) == ""
splat2(1:3,5.2)
@test takebuf_string(stagediobuf) == "(UnitRange{$intstr},Float64)"
splat2(3,5:2:7)
@test takebuf_string(stagediobuf) == "($intstr,StepRange{$intstr,$intstr})"
splat2(1,2,3,4)
@test takebuf_string(stagediobuf) == "($intstr,$intstr,$intstr,$intstr)"
splat2(1,2,3)
@test takebuf_string(stagediobuf) == "($intstr,$intstr,$intstr)"
splat2(1:5, 3, 3:3)
@test takebuf_string(stagediobuf) == "(UnitRange{$intstr},$intstr,UnitRange{$intstr})"
splat2(1:5, 3, 3:3)
@test takebuf_string(stagediobuf) == ""
splat2(1:5, 3:3, 3)
@test takebuf_string(stagediobuf) == "(UnitRange{$intstr},UnitRange{$intstr},$intstr)"
splat2(1:5, 3:3)
@test takebuf_string(stagediobuf) == "(UnitRange{$intstr},UnitRange{$intstr})"
splat2(3, 3:5)
@test takebuf_string(stagediobuf) == "($intstr,UnitRange{$intstr})"

# varargs specialization with parametric @generated functions (issue #8944)
@generated function splat3{T,N}(A::AbstractArray{T,N}, indx::RangeIndex...)
    print(stagediobuf, indx)
    :(nothing)
end
A = rand(5,5,3);
splat3(A, 1:2, 1:2, 1)
@test takebuf_string(stagediobuf) == "(UnitRange{$intstr},UnitRange{$intstr},$intstr)"
splat3(A, 1:2, 1, 1:2)
@test takebuf_string(stagediobuf) == "(UnitRange{$intstr},$intstr,UnitRange{$intstr})"

B = slice(A, 1:3, 2, 1:3);
@generated function mygetindex(S::SubArray, indexes::Real...)
    T, N, A, I = S.parameters
    if N != length(indexes)
        error("Wrong number of indexes supplied")
    end
    Ip = I.parameters
    NP = length(Ip)
    indexexprs = Array(Expr, NP)
    j = 1
    for i = 1:NP
        if Ip[i] == Int
            indexexprs[i] = :(S.indexes[$i])
        else
            indexexprs[i] = :(S.indexes[$i][indexes[$j]])
            j += 1
        end
    end
    ex = :(S.parent[$(indexexprs...)])
    ex
end
@test mygetindex(B,2,2) == A[2,2,2]

# issue #8497
module MyTest8497
internalfunction(x) = x+1
@generated function h(x)
    quote
        internalfunction(x)
    end
end
end
@test MyTest8497.h(3) == 4

# static parameters (issue #8505)
@generated function foo1{N,T}(a::Array{T,N})
    "N = $N, T = $T"
end
@generated function foo2{T,N}(a::Array{T,N})
    "N = $N, T = $T"
end
@test foo1(randn(3,3)) == "N = 2, T = Float64"
@test foo2(randn(3,3)) == "N = 2, T = Float64"

# issue #9088
@generated function f9088(x, a=5)
    :(x+a)
end
@test f9088(7) == 12

# issue #10502
@generated function f10502(x...)
    :($x)
end
f10502() = ()
@test f10502(1) == (Int,)
@test f10502(1,2) == (Int,Int)
@test f10502(1,2,3) == (Int,Int,Int)

# One-line @generated functions
@generated oneliner(x,y) = :($x, x, $y, y)
@test oneliner(1, 2.) == (Int, 1, Float64, 2.)

# issue #11982
@generated function f11982(T)
    string(T.parameters[1])
end
@test f11982(Float32) == "Float32"
@test f11982(Int32) == "Int32"

# @generated functions that throw (shouldn't segfault or throw)
module TestGeneratedThrow
    using Base.Test

    @generated function bar(x)
        error("I'm not happy with type $x")
    end

    foo() = (bar(rand() > 0.5 ? 1 : 1.0); error("foo"))
    function __init__()
        code_typed(foo,(); optimize = false)
        @test isa(Core.Inference.inference_stack,Core.Inference.EmptyCallStack)
        cfunction(foo,Void,())
    end
end
