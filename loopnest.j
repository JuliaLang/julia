# ind={gensym(), gensym()}; e=loopnest(2, `(x[$(ind...)]=1), ind, (`(1:2), `(1:2)))
function loopnest(N::Int, expr, ind, itr)
    if N == 0
        return expr
    else
        loopnest(N-1, `(for $ind[N]=$itr[N]; $expr; end), ind, itr)
    end
end

# eval(nd_index(3, `DIMS, `IND, 2, `IND[1], `DIMS[1]))
# I[1] + (I[2] - 1)*dims[1] + (I[3] - 1)*dims[1]*dims[2] + ...
function nd_index(N::Int, dims, I, k::Int, expr, dexpr)
    if k == N+1
        return expr
    else
        nd_index(N, dims, I, k+1, 
                 `($expr + (ref($I, $k) - 1) * $dexpr), 
                 `($dexpr * ref($dims, $k)) 
                 )
    end
end
