### From array.jl

@ngenerate N function _checksize(A::AbstractArray, I::NTuple{N, Any}...)
    @nexprs N d->(size(A, d) == length(I_d) || throw(DimensionMismatch("Index $d has length $(length(I_d)), but size(A, $d) = $(size(A,d))")))
    nothing
end
checksize(A, I) = (_checksize(A, I); return nothing)
checksize(A, I, J) = (_checksize(A, I, J); return nothing)
checksize(A, I...) = (_checksize(A, I...); return nothing)

unsafe_getindex(v::Real, ind::Integer) = v
unsafe_getindex(v::Ranges, ind::Integer) = first(v) + (ind-1)*step(v)
unsafe_getindex(v::AbstractArray, ind::Integer) = v[ind]

# Version that uses cartesian indexing for src
@ngenerate N function _getindex!(dest::Array, src::AbstractArray, I::NTuple{N,Union(Real,AbstractVector)}...)
    checksize(dest, I...)
    checkbounds(src, I...)
    @nexprs N d->(J_d = to_index(I_d))
    k = 1
    @nloops N i dest d->(@inbounds j_d = unsafe_getindex(J_d, i_d)) begin
        @inbounds dest[k] = (@nref N src j)
        k += 1
    end
end

# Version that uses linear indexing for src
@ngenerate N function _getindex!(dest::Array, src::Array, I::NTuple{N,Union(Real,AbstractVector)}...)
    checksize(dest, I...)
    checkbounds(src, I...)
    @nexprs N d->(J_d = to_index(I_d))
    stride_1 = 1
    @nexprs N d->(stride_{d+1} = stride_d*size(src,d))
    @nexprs N d->(offset_d = 1)  # only really need offset_$N = 1
    k = 1
    @nloops N i dest d->(@inbounds offset_{d-1} = offset_d + (unsafe_getindex(J_d, i_d)-1)*stride_d) begin
        @inbounds dest[k] = src[offset_0]
        k += 1
    end
end

getindex!(dest, src, I) = (_getindex!(dest, src, I); return dest)
getindex!(dest, src, I, J) = (_getindex!(dest, src, I, J); return dest)
getindex!(dest, src, I...) = (_getindex!(dest, src, I...); return dest)

getindex(A::Array, I::Union(Real,AbstractVector)) = getindex!(similar(A, index_shape(I)), A, I)
getindex(A::Array, I::Union(Real,AbstractVector), J::Union(Real,AbstractVector)) = getindex!(similar(A, index_shape(I,J)), A, I, J)
getindex(A::Array, I::Union(Real,AbstractVector)...) = getindex!(similar(A, index_shape(I...)), A, I...)


@ngenerate N function _setindex!(A::Array, x, I::NTuple{N,Union(Real,AbstractArray)}...)
    checkbounds(A, I...)
    @nexprs N d->(J_d = to_index(I_d))
    stride_1 = 1
    @nexprs N d->(stride_{d+1} = stride_d*size(A,d))
    @nexprs N d->(offset_d = 1)  # really only need offset_$N = 1
    if !isa(x, AbstractArray)
        @nloops N i d->(1:length(J_d)) d->(@inbounds offset_{d-1} = offset_d + (unsafe_getindex(J_d, i_d)-1)*stride_d) begin
            @inbounds A[offset_0] = x
        end
    else
        X = x
        setindex_shape_check(X, I...)
        # TODO? A variant that can use cartesian indexing for RHS
        k = 1
        @nloops N i d->(1:length(J_d)) d->(@inbounds offset_{d-1} = offset_d + (unsafe_getindex(J_d, i_d)-1)*stride_d) begin
            @inbounds A[offset_0] = X[k]
            k += 1
        end
    end
end

setindex!(A::Array, x, I::Union(Real,AbstractArray)) = (_setindex!(A, x, I); return A)
setindex!(A::Array, x, I::Union(Real,AbstractArray), J::Union(Real,AbstractArray)) =
    (_setindex!(A, x, I, J); return A)
setindex!(A::Array, x, I::Union(Real,AbstractArray)...) = (_setindex!(A, x, I...); return A)


@ngenerate N function findn{T,N}(A::AbstractArray{T,N})
    nnzA = nnz(A)
    @nexprs N d->(I_d = Array(Int, nnzA))
    k = 1
    @nloops N i A begin
        @inbounds if (@nref N A i) != zero(T)
            @nexprs N d->(I_d[k] = i_d)
            k += 1
        end
    end
    @ntuple N I
end


### subarray.jl

# Here we want to skip creating the dict-based cached version,
# so use the ngenerate function
function gen_getindex_body(N::Int)
    quote
        strd_1 = 1
        @nexprs $N d->(@inbounds strd_{d+1} = strd_d*s.dims[d])
        ind -= 1
        indp = s.first_index
        @nexprs $N d->begin
            i = div(ind, strd_{$N-d+1})
            @inbounds indp += i*s.strides[$N-d+1]
            ind -= i*strd_{$N-d+1}
        end
        s.parent[indp]
    end
end

eval(ngenerate(:N, :(getindex{T}(s::SubArray{T,N}, ind::Integer)), gen_getindex_body, 2:5, false))


function gen_setindex!_body(N::Int)
    quote
        strd_1 = 1
        @nexprs $N d->(@inbounds strd_{d+1} = strd_d*s.dims[d])
        ind -= 1
        indp = s.first_index
        @nexprs $N d->begin
            i = div(ind, strd_{$N-d+1})
            @inbounds indp += i*s.strides[$N-d+1]
            ind -= i*strd_{$N-d+1}
        end
        s.parent[indp] = v
    end
end

eval(ngenerate(:N, :(setindex!{T}(s::SubArray{T,N}, v, ind::Integer)), gen_setindex!_body, 2:5, false))


### from abstractarray.jl

@ngenerate N function _fill!{T,N}(A::AbstractArray{T,N}, x)
    @nloops N i A begin
        @inbounds (@nref N A i) = x
    end
end

fill!(A::AbstractArray, x) = (_fill!(A, x); return A)


### from bitarray.jl

# note: we can gain some performance if the first dimension is a range;
# but we need to single-out the N=0 case due to how @ngenerate works
# case N = 0
function getindex(B::BitArray, I0::Range1)
    ndims(B) < 1 && error("wrong number of dimensions")
    checkbounds(B, I0)
    X = BitArray(length(I0))
    copy_chunks(X.chunks, 1, B.chunks, first(I0), length(I0))
    return X
end

# TODO: extend to I:Union(Real,AbstractArray)... (i.e. not necessarily contiguous)
@ngenerate N function getindex(B::BitArray, I0::Range1, I::NTuple{N,Union(Real,Range1)}...)
    ndims(B) < N+1 && error("wrong number of dimensions")
    checkbounds(B, I0, I...)
    X = BitArray(index_shape(I0, I...))

    I0 = to_index(I0)

    f0 = first(I0)
    l0 = length(I0)

    Base.@nexprs N d->(I_d = to_index(I_d))

    gap_lst_1 = 0
    @nexprs N d->(gap_lst_{d+1} = length(I_d))
    stride = 1
    ind = f0
    @nexprs N d->begin
        stride *= size(B, d)
        stride_lst_d = stride
        ind += stride * (first(I_d) - 1)
        gap_lst_{d+1} *= stride
    end

    storeind = 1
    @nloops(N, i, d->I_d,
        d->nothing, # PRE
        d->(ind += stride_lst_d - gap_lst_d), # POST
        begin # BODY
            copy_chunks(X.chunks, storeind, B.chunks, ind, l0)
            storeind += l0
        end)
    return X
end

@ngenerate N function getindex(B::BitArray, I::NTuple{N,Union(Real,AbstractVector)}...)
    checkbounds(B, I...)
    J = to_index(I...)
    X = BitArray(index_shape(J...))
    Xc = X.chunks

    ind = 1
    @nloops N i d->J[d] begin
        setindex_unchecked(Xc, (@nref N B i), ind)
        ind += 1
    end
    return X
end

# case N = 0
function setindex_array2bitarray_ranges(B::BitArray, X::BitArray, I0::Range1{Int})
    ndims(B) != 1 && error("wrong number of dimensions in assigment")
    lI = length(I0)
    length(X) != lI && error("array assignment dimensions mismatch")
    lI == 0 && return B
    f0 = first(I0)
    l0 = length(I0)
    copy_chunks(B.chunks, f0, X.chunks, 1, l0)
    return B
end

@ngenerate N function setindex_array2bitarray_ranges(B::BitArray, X::BitArray, I0::Range1{Int}, IR::NTuple{N,Range1{Int}}...)
    ndims(B) != N+1 && error("wrong number of dimensions in assigment")
    lI = length(I0)

    I = tuple(IR...)
    for r in I
        lI *= length(r)
    end
    length(X) != lI && error("array assignment dimensions mismatch")
    lI == 0 && return B
    f0 = first(I0)
    l0 = length(I0)

    gap_lst = Int[i==1 ? 0 : last(I[i-1])-first(I[i-1])+1 for i = 1:N+1]
    stride_lst = Array(Int, N)
    stride = 1
    ind = f0
    @inbounds for k = 1 : N
        stride *= size(B, k)
        stride_lst[k] = stride
        ind += stride * (first(I[k]) - 1)
        gap_lst[k+1] *= stride
    end

    refind = 1
    @nloops(N, i, d->I[d],
        d->nothing, # PRE
        d->(ind += stride_lst[d] - gap_lst[d]), # POST
        begin # BODY
            copy_chunks(B.chunks, ind, X.chunks, refind, l0)
            refind += l0
        end)

    return B
end

@ngenerate N function setindex!(B::BitArray, X::AbstractArray, I::NTuple{N,Union(Real,AbstractArray)}...)
    J = to_index(I...)
    nel = 1
    for idx in J
        nel *= length(idx)
    end
    length(X) != nel && error("argument dimensions must match")
    if ndims(X) > 1
        for i = 1:length(J)
            size(X,i) != length(J[i]) && error("argument dimensions must match")
        end
    end
    refind = 1
    @nloops N i d->J[d] begin
        (@nref N B i) = X[refind]
        refind += 1
    end
    return B
end

@ngenerate N function setindex!(B::BitArray, x, I::NTuple{N,Union(Real,AbstractArray)}...)
    x = convert(Bool, x)
    checkbounds(B, I...)
    J = to_index(I...)
    Bc = B.chunks
    @nloops N i d->J[d] begin
        (@nref N B i) = x # TODO: should avoid bounds checking
    end
    return B
end

@ngenerate N function findn{N}(B::BitArray{N})
    nnzB = nnz(B)
    I = ntuple(N, x->Array(Int, nnzB))
    if nnzB > 0
        count = 1
        @nloops N i B begin
            if (@nref N B i) # TODO: should avoid bounds checking
                @nexprs N d->(I[d][count] = i_d)
                count += 1
            end
        end
    end
    return I
end

for (V, PT, BT) in [((:N,), BitArray, BitArray), ((:T,:N), Array, StridedArray)]
    @eval begin
    @ngenerate N function permutedims!{$(V...)}(P::$PT{$(V...)}, B::$BT{$(V...)}, perm)
        dimsB = size(B)
        ndimsB = N
        (length(perm) == N && isperm(perm)) || error("no valid permutation of dimensions")
        dimsP = size(P)
        for i = 1:length(perm)
            dimsP[i] == dimsB[perm[i]] || error("destination tensor of incorrect size")
        end

        #calculates all the strides
        @nexprs N d->(strides_d = (d > 1 ? stride(B, perm[d-1]) : 0))
        strides_last = stride(B, perm[N])

        #Creates offset, because indexing starts at 1
        offset = strides_last
        @nexprs N d->(offset += strides_d)
        offset = 1 - offset

        if isa(B, SubArray)
            offset += B.first_index - 1
            B = B.parent
        end

        ind = 1
        counts_last = strides_last
        @nloops(N, i, P,
            d->begin # PRE
                counts_d = strides_d
            end,
            d->begin # POST
                if d < N
                    counts_{d+1} += strides_{d+1}
                else
                    counts_last += strides_last
                end
            end,
            begin # BODY
                sumc = 0
                @nexprs N d->(sumc += counts_d)
                # note: could use @inbounds, but it does not seem to
                #       improve performance
                P[ind] = B[sumc+counts_last+offset]
                ind += 1
            end)

        return P
    end
    end
end
