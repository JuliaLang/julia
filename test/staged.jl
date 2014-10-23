stagedfunction staged_t1(a,b)
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

stagedfunction splat(a,b...)
    :( ($a,$b,a,b) )
end

@test splat(1,2,3) == (Int,(Int,Int),1,(2,3))

stagediobuf = IOBuffer()
stagedfunction splat2(a...)
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


A = rand(5,5,3);
B = slice(A, 1:3, 2, 1:3);
stagedfunction mygetindex(S::SubArray, indexes::Real...)
    T, N, A, I = S.parameters
    if N != length(indexes)
        error("Wrong number of indexes supplied")
    end
    NP = length(I)
    indexexprs = Array(Expr, NP)
    j = 1
    for i = 1:NP
        if I[i] == Int
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
stagedfunction h(x)
    quote
        internalfunction(x)
    end
end
end
@test MyTest8497.h(3) == 4

# static parameters (issue #8505)
stagedfunction f8505{T}(x::Vector{T})
    T
end
@test f8505([1.0]) === Float64
@test f8505([1]) === Int
