stagedfunction t1(a,b)
    if a == Int64
        return :(a+b)
    else
        return :(a*b)
    end
end

@test t1(1,2) == 3
@test t1(1.0,0.5) == 0.5
@test t1(1,0.5) == 1.5

tinline(a,b) = t1(a,b)

@test !isa(tinline(1,2),Expr)
@test tinline(1,0.5) == 1.5

stagedfunction splat(a,b...)
    :( ($a,$b,a,b) )
end

@test splat(1,2,3) == (Int64,(Int64,Int64),1,(2,3))

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