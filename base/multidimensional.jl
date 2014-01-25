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
@ngenerate N function _getindex!(dest::Array, src::AbstractArray, I::NTuple{N,Union(Int,AbstractVector)}...)
    checksize(dest, I...)
    k = 1
    @nloops N i dest d->(@inbounds j_d = unsafe_getindex(I_d, i_d)) begin
        @inbounds dest[k] = (@nref N src j)
        k += 1
    end
end

# Version that uses linear indexing for src
@ngenerate N function _getindex!(dest::Array, src::Array, I::NTuple{N,Union(Int,AbstractVector)}...)
    checksize(dest, I...)
    stride_1 = 1
    @nexprs N d->(stride_{d+1} = stride_d*size(src,d))
    @nexprs N d->(offset_d = 1)  # only really need offset_$N = 1
    k = 1
    @nloops N i dest d->(@inbounds offset_{d-1} = offset_d + (unsafe_getindex(I_d, i_d)-1)*stride_d) begin
        @inbounds dest[k] = src[offset_0]
        k += 1
    end
end

getindex!(dest, src, I) = (checkbounds(src, I); _getindex!(dest, src, to_index(I)); return dest)
getindex!(dest, src, I, J) = (checkbounds(src, I, J); _getindex!(dest, src, to_index(I), to_index(J)); return dest)
getindex!(dest, src, I...) = (checkbounds(src, I...); _getindex!(dest, src, to_index(I)...); return dest)

getindex(A::Array, I::Union(Real,AbstractVector)) = getindex!(similar(A, index_shape(I)), A, I)
function getindex(A::Array, I::Union(Real,AbstractVector)...)
    checkbounds(A, I...)
    Ii = to_index(I)
    dest = similar(A, index_shape(Ii...))
    _getindex!(dest, A, Ii...)
    dest
end
# Version of the above for 2d without the splats
function getindex(A::Array, I::Union(Real,AbstractVector), J::Union(Real,AbstractVector))
    checkbounds(A, I, J)
    Ii, Ji = to_index(I), to_index(J)
    dest = similar(A, index_shape(Ii,Ji))
    _getindex!(dest, A, Ii, Ji)
    dest
end


@ngenerate N function _setindex!(A::Array, x, I::NTuple{N,Union(Int,AbstractArray)}...)
    stride_1 = 1
    @nexprs N d->(stride_{d+1} = stride_d*size(A,d))
    @nexprs N d->(offset_d = 1)  # really only need offset_$N = 1
    if !isa(x, AbstractArray)
        @nloops N i d->(1:length(I_d)) d->(@inbounds offset_{d-1} = offset_d + (unsafe_getindex(I_d, i_d)-1)*stride_d) begin
            @inbounds A[offset_0] = x
        end
    else
        X = x
        setindex_shape_check(X, I...)
        # TODO? A variant that can use cartesian indexing for RHS
        k = 1
        @nloops N i d->(1:length(I_d)) d->(@inbounds offset_{d-1} = offset_d + (unsafe_getindex(I_d, i_d)-1)*stride_d) begin
            @inbounds A[offset_0] = X[k]
            k += 1
        end
    end
end

function setindex!(A::Array, x, I::Union(Real,AbstractArray), J::Union(Real,AbstractArray))
    checkbounds(A, I, J)
    _setindex!(A, x, to_index(I), to_index(J))
    A
end
function setindex!(A::Array, x, I::Union(Real,AbstractArray))
    checkbounds(A, I)
    _setindex!(A, x, to_index(I))
    A
end
function setindex!(A::Array, x, I::Union(Real,AbstractArray)...)
    checkbounds(A, I...)
    _setindex!(A, x, to_index(I)...)
    A
end


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
    @nexprs N d->(I_d = to_index(I_d))
    X = BitArray(index_shape(I...))
    Xc = X.chunks

    ind = 1
    @nloops N i d->I_d begin
        setindex_unchecked(Xc, (@nref N B i), ind)
        ind += 1
    end
    return X
end

# note: we can gain some performance if the first dimension is a range;
# case N = 0
function setindex!(B::BitArray, X::BitArray, I0::Range1)
    ndims(B) != 1 && error("wrong number of dimensions in assigment")
    I0 = to_index(I0)
    checkbounds(B, I0)
    lI = length(I0)
    length(X) != lI && error("array assignment dimensions mismatch")
    lI == 0 && return B
    f0 = first(I0)
    l0 = length(I0)
    copy_chunks(B.chunks, f0, X.chunks, 1, l0)
    return B
end

# TODO: extend to I:Union(Real,AbstractArray)... (i.e. not necessarily contiguous)
@ngenerate N function setindex!(B::BitArray, X::BitArray, I0::Range1, I::NTuple{N,Union(Real,Range1)}...)
    ndims(B) != N+1 && error("wrong number of dimensions in assigment")
    I0 = to_index(I0)
    lI = length(I0)

    @nexprs N d->begin
        I_d = to_index(I_d)
        lI *= length(I_d)
    end
    length(X) != lI && error("array assignment dimensions mismatch")
    checkbounds(B, I0, I...)
    lI == 0 && return B
    f0 = first(I0)
    l0 = length(I0)

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

    refind = 1
    @nloops(N, i, d->I_d,
        d->nothing, # PRE
        d->(ind += stride_lst_d - gap_lst_d), # POST
        begin # BODY
            copy_chunks(B.chunks, ind, X.chunks, refind, l0)
            refind += l0
        end)

    return B
end

@ngenerate N function setindex!(B::BitArray, X::AbstractArray, I::NTuple{N,Union(Real,AbstractArray)}...)
    checkbounds(B, I...)
    @nexprs N d->(I_d = to_index(I_d))
    nel = 1
    @nexprs N d->(nel *= length(I_d))
    length(X) != nel && error("argument dimensions must match")
    if ndims(X) > 1
        @nexprs N d->begin
            size(X,d) != length(I_d) && error("argument dimensions must match")
        end
    end
    refind = 1
    @nloops N i d->I_d begin
        (@nref N B i) = X[refind] # TODO: should avoid bounds checking
        refind += 1
    end
    return B
end

@ngenerate N function setindex!(B::BitArray, x, I::NTuple{N,Union(Real,AbstractArray)}...)
    x = convert(Bool, x)
    checkbounds(B, I...)
    @nexprs N d->(I_d = to_index(I_d))
    Bc = B.chunks
    @nloops N i d->I_d begin
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
        (length(perm) == N && isperm(perm)) || error("no valid permutation of dimensions")
        dimsP = size(P)
        for i = 1:length(perm)
            dimsP[i] == dimsB[perm[i]] || error("destination tensor of incorrect size")
        end

        #calculates all the strides
        strides_1 = 0
        @nexprs N d->(strides_{d+1} = stride(B, perm[d]))

        #Creates offset, because indexing starts at 1
        offset = 1 - sum(@ntuple N d->strides_{d+1})

        if isa(B, SubArray)
            offset += B.first_index - 1
            B = B.parent
        end

        ind = 1
        @nexprs 1 d->(counts_{N+1} = strides_{N+1}) # a trick to set counts_($N+1)
        @nloops(N, i, P,
            d->(counts_d = strides_d), # PRE
            d->(counts_{d+1} += strides_{d+1}), # POST
            begin # BODY
                sumc = sum(@ntuple N d->counts_{d+1})
                @inbounds P[ind] = B[sumc+offset]
                ind += 1
            end)

        return P
    end
    end
end
