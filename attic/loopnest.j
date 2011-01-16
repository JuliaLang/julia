# ind={gensym(), gensym()}; e=loopnest(2, :(x[$(ind...)]=1), ind, (:(1:2), :(1:2)))
function loopnest(N::Int, expr, ind, itr)
    if N == 0
        return expr
    else
        loopnest(N-1, :(for $ind[N]=$itr[N]; $expr; end), ind, itr)
    end
end

# eval(nd_index(3, :DIMS, :IND, 2, :(IND[1]), :(DIMS[1])))
# I[1] + (I[2] - 1)*dims[1] + (I[3] - 1)*dims[1]*dims[2] + ...
function nd_index(N, dims, I, k, expr, dexpr)
    if k == N+1
        return expr
    else
        nd_index(N, dims, I, k+1, 
                 :($expr + (ref($I, $k) - 1) * $dexpr), 
                 :($dexpr * ref($dims, $k)) 
                 )
    end
end

#eval(:(refN(A::Tensor{T,N}, ind::(Index...)) = $nd_index(:N, :(A.dims), :ind, 2, :(ind[1]), :(A.dims[1]) )) )

struct NDArray{T,N}
    data::Array{T,N}
    strides::(Index...)

    NDArray(A) = new(A, append((1,), cumprod(A.dims)))
end

function ref(A::NDArray, I::Index...)
    stride = A.strides

    index = 1
    for k=1:ndims(A.data)
        index += (I[k]-1) * stride[k]
    end

    return arrayref(A.data, index)
end

