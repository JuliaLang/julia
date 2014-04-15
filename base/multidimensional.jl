### From array.jl

@ngenerate N Nothing function checksize(A::AbstractArray, I::NTuple{N, Any}...)
    @nexprs N d->(size(A, d) == length(I_d) || throw(DimensionMismatch("index $d has length $(length(I_d)), but size(A, $d) = $(size(A,d))")))
    nothing
end

unsafe_getindex(v::Real, ind::Int) = v
unsafe_getindex(v::Range, ind::Int) = first(v) + (ind-1)*step(v)
unsafe_getindex(v::BitArray, ind::Int) = Base.unsafe_bitgetindex(v.chunks, ind)
unsafe_getindex(v::AbstractArray, ind::Int) = v[ind]
unsafe_getindex(v, ind::Real) = unsafe_getindex(v, to_index(ind))

unsafe_setindex!{T}(v::AbstractArray{T}, x::T, ind::Int) = (v[ind] = x; v)
unsafe_setindex!(v::BitArray, x::Bool, ind::Int) = (Base.unsafe_bitsetindex!(v.chunks, x, ind); v)
unsafe_setindex!{T}(v::AbstractArray{T}, x::T, ind::Real) = unsafe_setindex!(v, x, to_index(ind))

# Version that uses cartesian indexing for src
@ngenerate N typeof(dest) function _getindex!(dest::Array, src::AbstractArray, I::NTuple{N,Union(Int,AbstractVector)}...)
    checksize(dest, I...)
    k = 1
    @nloops N i dest d->(@inbounds j_d = unsafe_getindex(I_d, i_d)) begin
        @inbounds dest[k] = (@nref N src j)
        k += 1
    end
    dest
end

# Version that uses linear indexing for src
@ngenerate N typeof(dest) function _getindex!(dest::Array, src::Array, I::NTuple{N,Union(Int,AbstractVector)}...)
    checksize(dest, I...)
    stride_1 = 1
    @nexprs N d->(stride_{d+1} = stride_d*size(src,d))
    @nexprs N d->(offset_d = 1)  # only really need offset_$N = 1
    k = 1
    @nloops N i dest d->(@inbounds offset_{d-1} = offset_d + (unsafe_getindex(I_d, i_d)-1)*stride_d) begin
        @inbounds dest[k] = src[offset_0]
        k += 1
    end
    dest
end

# It's most efficient to call checkbounds first, then to_index, and finally
# allocate the output. Hence the different variants.
_getindex(A, I::(Union(Int,AbstractVector)...)) =
    _getindex!(similar(A, index_shape(I...)), A, I...)

@nsplat N function getindex(A::Array, I::NTuple{N,Union(Real,AbstractVector)}...)
    checkbounds(A, I...)
    _getindex(A, to_index(I...))
end

# Also a safe version of getindex!
@nsplat N function getindex!(dest, src, I::NTuple{N,Union(Real,AbstractVector)}...)
    checkbounds(src, I...)
    _getindex!(dest, src, to_index(I...)...)
end


@ngenerate N typeof(A) function setindex!(A::Array, x, J::NTuple{N,Union(Real,AbstractArray)}...)
    @ncall N checkbounds A J
    @nexprs N d->(I_d = to_index(J_d))
    stride_1 = 1
    @nexprs N d->(stride_{d+1} = stride_d*size(A,d))
    @nexprs N d->(offset_d = 1)  # really only need offset_$N = 1
    if !isa(x, AbstractArray)
        @nloops N i d->(1:length(I_d)) d->(@inbounds offset_{d-1} = offset_d + (unsafe_getindex(I_d, i_d)-1)*stride_d) begin
            @inbounds A[offset_0] = x
        end
    else
        X = x
        @ncall N setindex_shape_check X I
        # TODO? A variant that can use cartesian indexing for RHS
        k = 1
        @nloops N i d->(1:length(I_d)) d->(@inbounds offset_{d-1} = offset_d + (unsafe_getindex(I_d, i_d)-1)*stride_d) begin
            @inbounds A[offset_0] = X[k]
            k += 1
        end
    end
    A
end


@ngenerate N NTuple{N,Vector{Int}} function findn{T,N}(A::AbstractArray{T,N})
    nnzA = countnz(A)
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

eval(ngenerate(:N, nothing, :(getindex{T}(s::SubArray{T,N}, ind::Integer)), gen_getindex_body, 2:5, false))


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

eval(ngenerate(:N, nothing, :(setindex!{T}(s::SubArray{T,N}, v, ind::Integer)), gen_setindex!_body, 2:5, false))


### from abstractarray.jl

@ngenerate N typeof(A) function fill!{T,N}(A::AbstractArray{T,N}, x)
    @nloops N i A begin
        @inbounds (@nref N A i) = x
    end
    A
end

@ngenerate N typeof(dest) function copy!{T,N}(dest::AbstractArray{T,N}, src::AbstractArray{T,N})
    if @nall N d->(size(dest,d) == size(src,d))
        @nloops N i dest begin
            @inbounds (@nref N dest i) = (@nref N src i)
        end
    else
        invoke(copy!, (typeof(dest), Any), dest, src)
    end
    dest
end

### BitArrays

## getindex

# general scalar indexing with two or more indices
# (uses linear indexing, which - in the safe version - performs the final
# bounds check and is defined in bitarray.jl)
# (code is duplicated for safe and unsafe versions for performance reasons)

@ngenerate N Bool function unsafe_getindex(B::BitArray, I_0::Int, I::NTuple{N,Int}...)
    stride = 1
    index = I_0
    @nexprs N d->begin
        stride *= size(B,d)
        index += (I_d - 1) * stride
    end
    return unsafe_getindex(B, index)
end

@ngenerate N Bool function getindex(B::BitArray, I_0::Int, I::NTuple{N,Int}...)
    stride = 1
    index = I_0
    @nexprs N d->begin
        l = size(B,d)
        stride *= l
        1 <= I_{d-1} <= l || throw(BoundsError())
        index += (I_d - 1) * stride
    end
    return B[index]
end

# contiguous multidimensional indexing: if the first dimension is a range,
# we can get some performance from using copy_chunks!

function unsafe_getindex(B::BitArray, I0::UnitRange{Int})
    X = BitArray(length(I0))
    copy_chunks!(X.chunks, 1, B.chunks, first(I0), length(I0))
    return X
end

function getindex(B::BitArray, I0::UnitRange{Int})
    checkbounds(B, I0)
    return unsafe_getindex(B, I0)
end

getindex{T<:Real}(B::BitArray, I0::UnitRange{T}) = getindex(B, to_index(I0))

@ngenerate N BitArray{length(index_shape(I0, I...))} function unsafe_getindex(B::BitArray, I0::UnitRange{Int}, I::NTuple{N,Union(Int,UnitRange{Int})}...)
    X = BitArray(index_shape(I0, I...))

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

    storeind = 1
    @nloops(N, i, d->I_d,
        d->nothing, # PRE
        d->(ind += stride_lst_d - gap_lst_d), # POST
        begin # BODY
            copy_chunks!(X.chunks, storeind, B.chunks, ind, l0)
            storeind += l0
        end)
    return X
end

# general multidimensional non-scalar indexing

@ngenerate N BitArray{length(index_shape(I...))} function unsafe_getindex(B::BitArray, I::NTuple{N,Union(Int,AbstractVector{Int})}...)
    X = BitArray(index_shape(I...))
    Xc = X.chunks

    ind = 1
    @nloops N i d->I_d begin
        unsafe_bitsetindex!(Xc, (@ncall N unsafe_getindex B i), ind)
        ind += 1
    end
    return X
end

# general version with Real (or logical) indexing which dispatches on the appropriate method

@ngenerate N BitArray{length(index_shape(I...))} function getindex(B::BitArray, I::NTuple{N,Union(Real,AbstractVector)}...)
    checkbounds(B, I...)
    return unsafe_getindex(B, to_index(I...)...)
end

## setindex!

# general scalar indexing with two or more indices
# (uses linear indexing, which - in the safe version - performs the final
# bounds check and is defined in bitarray.jl)
# (code is duplicated for safe and unsafe versions for performance reasons)

@ngenerate N typeof(B) function unsafe_setindex!(B::BitArray, x::Bool, I_0::Int, I::NTuple{N,Int}...)
    stride = 1
    index = I_0
    @nexprs N d->begin
        stride *= size(B,d)
        index += (I_d - 1) * stride
    end
    unsafe_setindex!(B, x, index)
    return B
end

@ngenerate N typeof(B) function setindex!(B::BitArray, x::Bool, I_0::Int, I::NTuple{N,Int}...)
    stride = 1
    index = I_0
    @nexprs N d->begin
        l = size(B,d)
        stride *= l
        1 <= I_{d-1} <= l || throw(BoundsError())
        index += (I_d - 1) * stride
    end
    B[index] = x
    return B
end

# contiguous multidimensional indexing: if the first dimension is a range,
# we can get some performance from using copy_chunks!

function unsafe_setindex!(B::BitArray, X::BitArray, I0::UnitRange{Int})
    l0 = length(I0)
    l0 == 0 && return B
    f0 = first(I0)
    copy_chunks!(B.chunks, f0, X.chunks, 1, l0)
    return B
end

function unsafe_setindex!(B::BitArray, x::Bool, I0::UnitRange{Int})
    l0 = length(I0)
    l0 == 0 && return B
    f0 = first(I0)
    fill_chunks!(B.chunks, x, f0, l0)
    return B
end

@ngenerate N typeof(B) function unsafe_setindex!(B::BitArray, X::BitArray, I0::UnitRange{Int}, I::NTuple{N,Union(Int,UnitRange{Int})}...)
    length(X) == 0 && return B
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
            copy_chunks!(B.chunks, ind, X.chunks, refind, l0)
            refind += l0
        end)

    return B
end

@ngenerate N typeof(B) function unsafe_setindex!(B::BitArray, x::Bool, I0::UnitRange{Int}, I::NTuple{N,Union(Int,UnitRange{Int})}...)
    f0 = first(I0)
    l0 = length(I0)
    l0 == 0 && return B
    @nexprs N d->(length(I_d) == 0 && return B)

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

    @nloops(N, i, d->I_d,
        d->nothing, # PRE
        d->(ind += stride_lst_d - gap_lst_d), # POST
        begin # BODY
            fill_chunks!(B.chunks, x, ind, l0)
        end)

    return B
end


# general multidimensional non-scalar indexing

@ngenerate N typeof(B) function unsafe_setindex!(B::BitArray, X::AbstractArray, I::NTuple{N,Union(Int,AbstractArray{Int})}...)
    refind = 1
    @nloops N i d->I_d @inbounds begin
        @ncall N unsafe_setindex! B convert(Bool,X[refind]) i
        refind += 1
    end
    return B
end

@ngenerate N typeof(B) function unsafe_setindex!(B::BitArray, x::Bool, I::NTuple{N,Union(Int,AbstractArray{Int})}...)
    @nloops N i d->I_d begin
        @ncall N unsafe_setindex! B x i
    end
    return B
end

# general versions with Real (or logical) indexing which dispatch on the appropriate method

# this one is for disambiguation only
function setindex!(B::BitArray, x, i::Real)
    checkbounds(B, i)
    return unsafe_setindex!(B, convert(Bool,x), to_index(i))
end

@ngenerate N typeof(B) function setindex!(B::BitArray, x, I::NTuple{N,Union(Real,AbstractArray)}...)
    checkbounds(B, I...)
    #return unsafe_setindex!(B, convert(Bool,x), to_index(I...)...) # segfaults! (???)
    @nexprs N d->(J_d = to_index(I_d))
    return @ncall N unsafe_setindex! B convert(Bool,x) J
end


# this one is for disambiguation only
function setindex!(B::BitArray, X::AbstractArray, i::Real)
    checkbounds(B, i)
    j = to_index(i)
    setindex_shape_check(X, j)
    return unsafe_setindex!(B, X, j)
end

@ngenerate N typeof(B) function setindex!(B::BitArray, X::AbstractArray, I::NTuple{N,Union(Real,AbstractArray)}...)
    checkbounds(B, I...)
    @nexprs N d->(J_d = to_index(I_d))
    @ncall N setindex_shape_check X J
    return @ncall N unsafe_setindex! B X J
end



## findn

@ngenerate N NTuple{N,Vector{Int}} function findn{N}(B::BitArray{N})
    nnzB = countnz(B)
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

## isassigned

@ngenerate N Bool function isassigned(B::BitArray, I_0::Int, I::NTuple{N,Int}...)
    stride = 1
    index = I_0
    @nexprs N d->begin
        l = size(B,d)
        stride *= l
        1 <= I_{d-1} <= l || return false
        index += (I_d - 1) * stride
    end
    return isassigned(B, index)
end

## permutedims

for (V, PT, BT) in {((:N,), BitArray, BitArray), ((:T,:N), Array, StridedArray)}
    @eval @ngenerate N typeof(P) function permutedimsold!{$(V...)}(P::$PT{$(V...)}, B::$BT{$(V...)}, perm)
        dimsB = size(B)
        length(perm) == N || error("expected permutation of size $N, but length(perm)=$(length(perm))")
        isperm(perm) || error("input is not a permutation")
        dimsP = size(P)
        for i = 1:length(perm)
            dimsP[i] == dimsB[perm[i]] || throw(DimensionMismatch("destination tensor of incorrect size"))
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

@ngenerate N typeof(P) function permutedims!{T1,T2,N}(P::StridedArray{T1,N},B::StridedArray{T2,N}, perm)
    length(perm) == N || error("expected permutation of size $N, but length(perm)=$(length(perm))")
    isperm(perm) || error("input is not a permutation")
    dims = size(P)
    for i = 1:N
        dims[i] == size(B,perm[i]) || throw(DimensionMismatch("destination tensor of incorrect size"))
    end

    #calculates all the strides and dims as variables
    @nexprs N d->(stridesB_{d} = stride(B, perm[d]))
    @nexprs N d->(stridesP_{d} = stride(P, d))
    @nexprs N d->(dims_{d} = dims[d])
    
    # calculate blocking strategy
    if isa(P,BitArray)
        elszP=1
    else
        elszP=isbits(T1) ? sizeof(T1) : sizeof(Ptr)
    end
    if isa(B,BitArray)
        elszB=1
    else
        elszB=isbits(T2) ? sizeof(T2) : sizeof(Ptr)
    end
    if (elszB+elszP)*length(P)<=1<<15
        bdims=dims
    else
        bdims=blockdims(dims,elszP,(@ntuple N d->stridesP_{d}),elszB,(@ntuple N d->stridesB_{d}))
    end
    @nexprs N d->(bdims_{d} = bdims[d])

    if isa(B, SubArray)
        offsetB = B.first_index
        B = B.parent
    else
        offsetB = 1
    end
    if isa(P, SubArray)
        offsetP = P.first_index
        P = P.parent
    else
        offsetP = 1
    end

    @nexprs 1 d->(indB_{N} = offsetB)
    @nexprs 1 d->(indP_{N} = offsetP)
    @nloops(N, outer, d->1:bdims_{d}:dims_{d},
        d->(indB_{d-1} = indB_{d};indP_{d-1}=indP_{d}), # PRE
        d->(indB_{d} += bdims_{d}*stridesB_{d};indP_{d} += bdims_{d}*stridesP_{d}), # POST
        begin # BODY
            @nexprs 1 e->(ind2B_{N} = indB_0)
            @nexprs 1 e->(ind2P_{N} = indP_0)
            @nloops(N, inner, e->outer_{e}:min(outer_{e}+bdims_{e}-1,dims_{e}),
                e->(ind2B_{e-1} = ind2B_{e};ind2P_{e-1}=ind2P_{e}), # PRE
                e->(ind2B_{e} += stridesB_{e};ind2P_{e} += stridesP_{e}), # POST
                @inbounds P[ind2P_0]=B[ind2B_0]) #BODY
        end)
    return P
end

function blockdims{N}(dims::NTuple{N,Int},elszA::Int,stridesA::NTuple{N,Int},elszB::Int,stridesB::NTuple{N,Int})
    # blocking strategy for permutedims
    if N==0
        return ()
    else
        pA=sortperm(collect(stridesA))
        pB=sortperm(collect(stridesB))
        
        cacheline=64
        # determine cache
        effectivecachesize=25600 # 64*400 = ifloor(cachesize/1.28) with cachesize=32k and 1.28 safety margin to prevent complete cachefill
        
        # if smallest stride of A or B is not 1, then the effect size a subblock of A
        # or B will take in the cache depends not only on the element size but also on
        # the number of unused data that will be copied together with every element
        cachesizeA=min(elszA*stridesA[pA[1]],cacheline)
        cachesizeB=min(elszB*stridesB[pB[1]],cacheline)
    
        # check if complete data fits into cache:
        if (cachesizeA+cachesizeB)*prod(dims)<=effectivecachesize
            return dims
        end
    
        # cache-friendly blocking strategy:
        # bstep=ones(Int,N)
        # for i=1:N
        #     bstep[i]=max(1,div(cacheline,elszA*stridesA[i]),div(cacheline,elszB*stridesB[i]))
        #     # bstep is the number of elements along that dimension that can be expected to be
        #     # within a single cacheline for either array A or B; it would be suboptimal not to
        #     # use all of them immediately
        # end
        
        bdims=ones(Int,N)
        i=1
        j=1
        # loop will try to make blocks maximal along dimensions of minimal strides
        # for both A and B, until the blockdim equals the full dim along those
        # dimensions, and then continue with the next dimensions
        while true
            while bdims[pA[i]]==dims[pA[i]]
                i+=1
            end
            bdims[pA[i]]+=1#bstep[pA[i]]
            if (cachesizeA+cachesizeB)*prod(bdims)>effectivecachesize # this must become true at some point
                bdims[pA[i]]-=1#bstep[pA[i]]
                break
            end
            
            while bdims[pB[j]]==dims[pB[j]]
                j+=1
            end
            bdims[pB[j]]+=1#bstep[pB[j]]
            if (cachesizeA+cachesizeB)*prod(bdims)>effectivecachesize # this must become true at some point
                bdims[pB[j]]-=1#bstep[pB[j]]
                break
            end
        end
        return tuple(bdims...)
    end
end

## unique across dim

immutable Prehashed
    hash::Uint
end
hash(x::Prehashed) = x.hash

@ngenerate N typeof(A) function unique{T,N}(A::AbstractArray{T,N}, dim::Int)
    1 <= dim <= N || return copy(A)
    hashes = zeros(Uint, size(A, dim))

    # Compute hash for each row
    k = 0
    @nloops N i A d->(if d == dim; k = i_d; end) begin
       @inbounds hashes[k] = bitmix(hashes[k], hash((@nref N A i)))
    end

    # Collect index of first row for each hash
    uniquerow = Array(Int, size(A, dim))
    firstrow = Dict{Prehashed,Int}()
    for k = 1:size(A, dim)
        uniquerow[k] = get!(firstrow, Prehashed(hashes[k]), k)
    end
    uniquerows = collect(values(firstrow))

    # Check for collisions
    collided = falses(size(A, dim))
    @inbounds begin
        @nloops N i A d->(if d == dim
                              k = i_d
                              j_d = uniquerow[k]
                          else
                              j_d = i_d
                          end) begin
            if (@nref N A j) != (@nref N A i)
                collided[k] = true
            end
        end
    end

    if any(collided)
        nowcollided = BitArray(size(A, dim))
        while any(collided)
            # Collect index of first row for each collided hash
            empty!(firstrow)
            for j = 1:size(A, dim)
                collided[j] || continue
                uniquerow[j] = get!(firstrow, Prehashed(hashes[j]), j)
            end
            for v in values(firstrow)
                push!(uniquerows, v)
            end

            # Check for collisions
            fill!(nowcollided, false)
            @nloops N i A d->begin
                                 if d == dim
                                     k = i_d
                                     j_d = uniquerow[k]
                                     (!collided[k] || j_d == k) && continue
                                 else
                                     j_d = i_d
                                 end
                             end begin
                if (@nref N A j) != (@nref N A i)
                    nowcollided[k] = true
                end
            end
            (collided, nowcollided) = (nowcollided, collided)
        end
    end

    @nref N A d->d == dim ? sort!(uniquerows) : (1:size(A, d))
end
