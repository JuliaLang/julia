### TODO: implement bitarray functions in terms of cartesian and delete gen_cartesian_map

### From array.jl

@ngenerate N function _checksize(A::AbstractArray, I::NTuple{N, Any}...)
    @nexprs N d->(size(A, d) == length(I_d) || throw(DimensionMismatch("Index $d has length $(length(I_d)), but size(A, $d) = $(size(A,d))")))
    nothing
end
checksize(A, I) = (_checksize(A, I); return nothing)
checksize(A, I, J) = (_checksize(A, I, J); return nothing)
checksize(A, I...) = (_checksize(A, I...); return nothing)

# Version that uses cartesian indexing for src
@ngenerate N function _getindex!(dest::Array, src::AbstractArray, I::NTuple{N,Union(Real,AbstractVector)}...)
    checksize(dest, I...)
    checkbounds(src, I...)
    @nexprs N d->(J_d = to_index(I_d))
    k = 1
    @nloops N i dest d->(j_d = J_d[i_d]) begin
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
    @nloops N i dest d->(offset_{d-1} = offset_d + (J_d[i_d]-1)*stride_d) begin
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
        @nloops N i d->(1:length(J_d)) d->(offset_{d-1} = offset_d + (J_d[i_d]-1)*stride_d) begin
            @inbounds A[offset_0] = x
        end
    else
        X = x
        setindex_shape_check(X, I...)
        # TODO? A variant that can use cartesian indexing for RHS
        k = 1
        @nloops N i d->(1:length(J_d)) d->(offset_{d-1} = offset_d + (J_d[i_d]-1)*stride_d) begin
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

## code generator for specializing on the number of dimensions ##

#otherbodies are the bodies that reside between loops, if its a 2 dimension array.
function make_loop_nest(vars, ranges, body)
    otherbodies = cell(length(vars),2)
    #println(vars)
    for i = 1:2*length(vars)
        otherbodies[i] = nothing
    end
    make_loop_nest(vars, ranges, body, otherbodies)
end

function make_loop_nest(vars, ranges, body, otherbodies)
    expr = body
    len = size(otherbodies)[1]
    for i=1:length(vars)
        v = vars[i]
        r = ranges[i]
        l = otherbodies[i]
        j = otherbodies[i+len]
        expr = quote
            $l
            for ($v) = ($r)
                $expr
            end
            $j
        end
    end
    expr
end


## genbodies() is a function that creates an array (potentially 2d),
## where the first element is inside the inner most array, and the last
## element is outside most loop, and all the other arguments are
## between each loop. If it creates a 2d array, it just means that it
## specifies what it wants to do before and after each loop.
## If genbodies creates an array it must of length N.
function gen_cartesian_map(cache, genbodies, ranges, exargnames, exargs...)
    if ranges === ()
        ranges = (1,)
    end
    N = length(ranges)
    if !haskey(cache,N)
        if isdefined(genbodies,:code)
            mod = genbodies.code.module
        else
            mod = Main
        end
        dimargnames = { symbol(string("_d",i)) for i=1:N }
        ivars = { symbol(string("_i",i)) for i=1:N }
        bodies = genbodies(ivars)

        ## creating a 2d array, to pass as bodies
        if isa(bodies,Array)
            if (ndims(bodies)==2)
                #println("2d array noticed")
                body = bodies[1]
                bodies = bodies[2:end,:]
            elseif (ndims(bodies)==1)
                #println("1d array noticed")
                body = bodies[1]
                bodies_tmp = cell(N,2)
                for i = 1:N
                    bodies_tmp[i] = bodies[i+1]
                    bodies_tmp[i+N] = nothing
                end
                bodies = bodies_tmp
            end
        else
            #println("no array noticed")
            body = bodies
            bodies = cell(N,2)
            for i=1:2*N
                bodies[i] = nothing
            end
        end
        fexpr =
        quote
            local _F_
            function _F_($(dimargnames...), $(exargnames...))
                $(make_loop_nest(ivars, dimargnames, body, bodies))
            end
            _F_
        end
        f = eval(mod,fexpr)
        cache[N] = f
    else
        f = cache[N]
    end
    return f(ranges..., exargs...)
end


### from bitarray.jl

# note: we can gain some performance if the first dimension is a range;
# TODO: extend to I:Union(Real,AbstractArray)... (i.e. not necessarily contiguous)
let getindex_cache = nothing
    global getindex
    function getindex(B::BitArray, I0::Range1{Int}, I::Union(Real,Range1{Int})...)
        # the < should become a != once
        # the stricter indexing behaviour is enforced
        if ndims(B) < 1 + length(I)
            error("wrong number of dimensions")
        end
        checkbounds(B, I0, I...)
        X = BitArray(index_shape(I0, I...))
        nI = 1 + length(I)

        I = map(x->(isa(x,Real) ? (to_index(x):to_index(x)) : to_index(x)), I[1:nI-1])

        f0 = first(I0)
        l0 = length(I0)

        gap_lst = Int[last(r)-first(r)+1 for r in I]
        stride_lst = Array(Int, nI)
        stride = 1
        ind = f0
        for k = 1 : nI - 1
            stride *= size(B, k)
            stride_lst[k] = stride
            ind += stride * (first(I[k]) - 1)
            gap_lst[k] *= stride
        end
        # we only need nI-1 elements, the last one
        # is dummy (used in bodies[k,2] below)
        stride_lst[nI] = 0

        if ndims(X) == 1
            copy_chunks(X.chunks, 1, B.chunks, ind, l0)
            return X
        end

        if is(getindex_cache,nothing)
            getindex_cache = Dict()
        end

        gen_cartesian_map(getindex_cache,
            ivars->begin
                bodies = cell(nI, 2)
                bodies[1] = quote
                        copy_chunks(X.chunks, storeind, B.chunks, ind, l0)
                        storeind += l0
                        ind += stride_lst[loop_ind]
                    end
                for k = 2 : nI
                    bodies[k, 1] = quote
                        loop_ind -= 1
                    end
                    bodies[k, 2] = quote
                        ind -= gap_lst[loop_ind]
                        loop_ind += 1
                        ind += stride_lst[loop_ind]
                    end
                end
                return bodies
            end,
            I, (:B, :X, :storeind, :ind, :l0, :stride_lst, :gap_lst, :loop_ind),
            B, X, 1, ind, l0, stride_lst, gap_lst, nI)
        return X
    end
end

let getindex_cache = nothing
    global getindex
    function getindex(B::BitArray, I::Union(Real,AbstractVector)...)
        checkbounds(B, I...)
        I = to_index(I)
        X = BitArray(index_shape(I...))
        Xc = X.chunks

        if is(getindex_cache,nothing)
            getindex_cache = Dict()
        end
        gen_cartesian_map(getindex_cache, ivars -> quote
                #faster X[storeind] = B[$(ivars...)]
                setindex_unchecked(Xc, B[$(ivars...)], ind)
                ind += 1
            end, I, (:B, :Xc, :ind), B, Xc, 1)
        return X
    end
end

let setindex_cache = nothing
    global setindex_array2bitarray_ranges
    function setindex_array2bitarray_ranges(B::BitArray, X::BitArray, I0::Range1{Int}, I::Range1{Int}...)
        nI = 1 + length(I)
        if ndims(B) != nI
            error("wrong number of dimensions in assigment")
        end
        lI = length(I0)
        for r in I
            lI *= length(r)
        end
        if length(X) != lI
            error("array assignment dimensions mismatch")
        end
        if lI == 0
            return B
        end
        f0 = first(I0)
        l0 = length(I0)
        if nI == 1
            copy_chunks(B.chunks, f0, X.chunks, 1, l0)
            return B
        end
        if is(setindex_cache,nothing)
            setindex_cache = Dict()
        end
        gap_lst = [last(r)-first(r)+1 for r in I]
        stride_lst = Array(Int, nI)
        stride = 1
        ind = f0
        @inbounds for k = 1 : nI - 1
            stride *= size(B, k)
            stride_lst[k] = stride
            ind += stride * (first(I[k]) - 1)
            gap_lst[k] *= stride
        end
        # we only need nI-1 elements, the last one
        # is dummy (used in bodies[k,2] below)
        stride_lst[nI] = 0

        gen_cartesian_map(setindex_cache,
            ivars->begin
                bodies = cell(nI, 2)
                bodies[1] = quote
                        copy_chunks(B.chunks, ind, X.chunks, refind, l0)
                        refind += l0
                        ind += stride_lst[loop_ind]
                    end
                for k = 2 : nI
                    bodies[k, 1] = quote
                        loop_ind -= 1
                    end
                    bodies[k, 2] = quote
                        ind -= gap_lst[loop_ind]
                        loop_ind += 1
                        ind += stride_lst[loop_ind]
                    end
                end
                return bodies
            end,
            I, (:B, :X, :refind, :ind, :l0, :stride_lst, :gap_lst, :loop_ind),
            B, X, 1, ind, l0, stride_lst, gap_lst, nI)
        return B
    end
end

let setindex_cache = nothing
    global setindex!
    function setindex!(B::BitArray, X::AbstractArray, I::Union(Real,AbstractArray)...)
        I = to_index(I)
        nel = 1
        for idx in I
            nel *= length(idx)
        end
        if length(X) != nel
            error("argument dimensions must match")
        end
        if ndims(X) > 1
            for i = 1:length(I)
                if size(X,i) != length(I[i])
                    error("argument dimensions must match")
                end
            end
        end
        if is(setindex_cache,nothing)
            setindex_cache = Dict()
        end
        gen_cartesian_map(setindex_cache,
            ivars->:(B[$(ivars...)] = X[refind]; refind += 1),
            I,
            (:B, :X, :refind),
            B, X, 1)
        return B
    end
end

let setindex_cache = nothing
    global setindex!
    function setindex!(B::BitArray, x, I::Union(Real,AbstractArray)...)
        x = convert(Bool, x)
        checkbounds(B, I...)
        I = to_index(I)
        if is(setindex_cache,nothing)
            setindex_cache = Dict()
        end
        gen_cartesian_map(setindex_cache, ivars->:(B[$(ivars...)] = x),
            I,
            (:B, :x),
            B, x)
        return B
    end
end

let findn_cache = nothing
global findn
function findn(B::BitArray)
    ndimsB = ndims(B)
    nnzB = nnz(B)
    I = ntuple(ndimsB, x->Array(Int, nnzB))
    if nnzB > 0
        ranges = ntuple(ndims(B), d->(1:size(B,d)))

        if is(findn_cache,nothing)
            findn_cache = Dict()
        end

        gen_cartesian_map(findn_cache, findn_one, ranges,
                          (:B, :I, :count), B, I, 1)
    end
    return I
end
end

let permutedims_cache = nothing, stridenames::Array{Any,1} = {}
global permutedims!
function permutedims!(P::BitArray,B::BitArray, perm)
    dimsB = size(B)
    ndimsB = length(dimsB)
    (ndimsB == length(perm) && isperm(perm)) || error("no valid permutation of dimensions")
	dimsP = size(P)
    for i = 1:length(perm)
        dimsP[i] == dimsB[perm[i]] || error("destination tensor of incorrect size")
    end
	
    ranges = ntuple(ndimsB, i->(1:dimsP[i]))
    while length(stridenames) < ndimsB
        push!(stridenames, gensym())
    end

    #calculates all the strides
    strides = [ prod(dimsB[1:(perm[dim]-1)])::Int for dim = 1:length(perm) ]

    #Creates offset, because indexing starts at 1
    offset = 0
    for i in strides
        offset+=i
    end
    offset = 1-offset

    if isa(B,SubArray)
        offset += (B.first_index-1)
        B = B.parent
    end

    if is(permutedims_cache,nothing)
        permutedims_cache = Dict()
    end

    gen_cartesian_map(permutedims_cache, iv->permute_one_dim(iv,stridenames), ranges,
                      tuple(:B, :P, :perm, :offset, stridenames[1:ndimsB]...),
                      B, P, perm, offset, strides...)

    return P
end
function permutedims!(P::Array,B::StridedArray, perm)
    dimsB = size(B)
    ndimsB = length(dimsB)
    (ndimsB == length(perm) && isperm(perm)) || error("no valid permutation of dimensions")
	dimsP = size(P)
    for i = 1:length(perm)
        dimsP[i] == dimsB[perm[i]] || error("destination tensor of incorrect size")
    end
	
    ranges = ntuple(ndimsB, i->(1:dimsP[i]))
    while length(stridenames) < ndimsB
        push!(stridenames, gensym())
    end

    #calculates all the strides
    strides = [ stride(B, perm[dim]) for dim = 1:length(perm) ]

    #Creates offset, because indexing starts at 1
    offset = 0
    for i in strides
        offset+=i
    end
    offset = 1-offset

    if isa(B,SubArray)
        offset += (B.first_index-1)
        B = B.parent
    end

    if is(permutedims_cache,nothing)
        permutedims_cache = Dict()
    end

    gen_cartesian_map(permutedims_cache, iv->permute_one_dim(iv,stridenames), ranges,
                      tuple(:B, :P, :perm, :offset, stridenames[1:ndimsB]...),
                      B, P, perm, offset, strides...)

    return P
end
end # let
