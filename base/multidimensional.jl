### from abstractarray.jl


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


# Generate function bodies which look like this (example for a 3d array):
#    offset3 = 0
#    stride1 = 1
#    stride2 = stride1 * size(A,1)
#    stride3 = stride2 * size(A,2)
#    for i3 = ind3
#        offset2 = offset3 + (i3-1)*stride3
#        for i2 = ind2
#            offset1 = offset2 + (i2-1)*stride2
#            for i1 = ind1
#                linearind = offset1 + i1
#                <A function, "body", of linearind>
#            end
#        end
#    end
function make_arrayind_loop_nest(loopvars, offsetvars, stridevars, linearind, ranges, body, arrayname)
    # Initialize: calculate the strides
    offset = offsetvars[end]
    s = stridevars[1]
    exinit = quote
        $offset = 0
        $s = 1
    end
    for i = 2:length(ranges)
        sprev = s
        s = stridevars[i]
        exinit = quote
            $exinit
            $s = $sprev * size($arrayname, $i-1)
        end
    end
    # Build the innermost loop (iterating over the first index)
    v = loopvars[1]
    r = ranges[1]
    offset = offsetvars[1]
    exloop = quote
        for ($v) = ($r)
            $linearind = $offset + $v
            $body
        end
    end
    # Build the remaining loops
    for i = 2:length(ranges)
        v = loopvars[i]
        r = ranges[i]
        offset = offsetvars[i-1]
        offsetprev = offsetvars[i]
        s = stridevars[i]
        exloop = quote
            for ($v) = ($r)
                $offset = $offsetprev + ($v - 1) * $s
                $exloop
            end
        end
    end
    # Return the combined result
    return quote
        $exinit
        $exloop
    end
end

# Like gen_cartesian_map, except it builds a function creating a
# loop nest that computes a single linear index (instead of a
# multidimensional index).
# Important differences:
#   - genbody is a scalar-valued function of a single scalar argument,
#     the linear index. In gen_cartesian_map, this function can return
#     an array to specify "pre-loop" and "post-loop" operations, but
#     here those are handled explicitly in make_arrayind_loop_nest.
#   - exargnames[1] must be the array for which the linear index is
#     being created (it is used to calculate the strides, which in
#     turn are used for computing the linear index)
function gen_array_index_map(cache, genbody, ranges, exargnames, exargs...)
    N = length(ranges)
    if !haskey(cache,N)
        dimargnames = { symbol(string("_d",i)) for i=1:N }
        loopvars = { symbol(string("_l",i)) for i=1:N }
        offsetvars = { symbol(string("_offs",i)) for i=1:N }
        stridevars = { symbol(string("_stri",i)) for i=1:N }
        linearind = :_li
        body = genbody(linearind)
        fexpr = quote
            local _F_
            function _F_($(dimargnames...), $(exargnames...))
                $(make_arrayind_loop_nest(loopvars, offsetvars, stridevars, linearind, dimargnames, body, exargnames[1]))
            end
            return _F_
        end
        f = eval(fexpr)
        cache[N] = f
    else
        f = cache[N]
    end
    return f(ranges..., exargs...)
end



### From array.jl

# Multidimensional indexing
let getindex_cache = nothing
global getindex
function getindex(A::Array, I::Union(Real,AbstractVector)...)
    checkbounds(A, I...)
    I = to_index(I)
    X = similar(A, eltype(A), index_shape(I...))

    if is(getindex_cache,nothing)
        getindex_cache = Dict()
    end
    gen_array_index_map(getindex_cache, refind -> quote
            X[storeind] = A[$refind]
            storeind += 1
        end, I, (:A, :X, :storeind), A, X, 1)
    return X
end
end

let assign_cache = nothing, assign_scalar_cache = nothing
global setindex!
function setindex!(A::Array, x, I::Union(Real,AbstractArray)...)
    checkbounds(A, I...)
    I = to_index(I)
    if !isa(x,AbstractArray)
        if is(assign_scalar_cache,nothing)
            assign_scalar_cache = Dict()
        end
        gen_array_index_map(assign_scalar_cache, storeind -> quote
                              A[$storeind] = x
                            end,
                            I,
                            (:A, :x),
                            A, x)
    else
        if is(assign_cache,nothing)
            assign_cache = Dict()
        end
        X = x
        setindex_shape_check(X, I...)
        gen_array_index_map(assign_cache, storeind -> quote
                              A[$storeind] = X[refind]
                              refind += 1
                            end,
                            I,
                            (:A, :X, :refind),
                            A, X, 1)
    end
    return A
end
end


let findn_cache = nothing
function findn_one(ivars)
    s = { quote I[$i][count] = $(ivars[i]) end for i = 1:length(ivars)}
    quote
        Aind = A[$(ivars...)]
        if Aind != z
            $(s...)
            count +=1
        end
    end
end

global findn
function findn{T}(A::AbstractArray{T})
    ndimsA = ndims(A)
    nnzA = nnz(A)
    I = ntuple(ndimsA, x->Array(Int, nnzA))
    if nnzA > 0
        ranges = ntuple(ndims(A), d->(1:size(A,d)))

        if is(findn_cache,nothing)
            findn_cache = Dict()
        end

        gen_cartesian_map(findn_cache, findn_one, ranges,
                          (:A, :I, :count, :z), A,I,1, zero(T))
    end
    return I
end
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


### subarray.jl

function getindex{T}(s::SubArray{T,2}, ind::Integer)
    @inbounds strd2 = s.dims[1]
    ind -= 1
    i2 = div(ind,strd2)
    i1 = ind-i2*strd2
    s.parent[s.first_index + i1*s.strides[1] + i2*s.strides[2]]
end

function getindex{T}(s::SubArray{T,3}, ind::Integer)
    @inbounds strd2 = s.dims[1]
    @inbounds strd3 = strd2*s.dims[2]
    ind -= 1
    i3 = div(ind,strd3)
    ind -= i3*strd3
    i2 = div(ind,strd2)
    i1 = ind-i2*strd2
    s.parent[s.first_index + i1*s.strides[1] + i2*s.strides[2] + i3*s.strides[3]]
end

function getindex{T}(s::SubArray{T,4}, ind::Integer)
    @inbounds strd2 = s.dims[1]
    @inbounds strd3 = strd2*s.dims[2]
    @inbounds strd4 = strd3*s.dims[3]
    ind -= 1
    i4 = div(ind,strd4)
    ind -= i4*strd4
    i3 = div(ind,strd3)
    ind -= i3*strd3
    i2 = div(ind,strd2)
    i1 = ind-i2*strd2
    s.parent[s.first_index + i1*s.strides[1] + i2*s.strides[2] + i3*s.strides[3] + i4*s.strides[4]]
end

function getindex{T}(s::SubArray{T,5}, ind::Integer)
    @inbounds strd2 = s.dims[1]
    @inbounds strd3 = strd2*s.dims[2]
    @inbounds strd4 = strd3*s.dims[3]
    @inbounds strd5 = strd4*s.dims[4]
    ind -= 1
    i5 = div(ind,strd5)
    ind -= i5*strd5
    i4 = div(ind,strd4)
    ind -= i4*strd4
    i3 = div(ind,strd3)
    ind -= i3*strd3
    i2 = div(ind,strd2)
    i1 = ind-i2*strd2
    s.parent[s.first_index + i1*s.strides[1] + i2*s.strides[2] + i3*s.strides[3] + i4*s.strides[4] + i5*s.strides[5]]
end

function setindex!{T}(s::SubArray{T,2}, v, ind::Integer)
    @inbounds strd2 = s.dims[1]
    ind -= 1
    i2 = div(ind,strd2)
    i1 = ind-i2*strd2
    s.parent[s.first_index + i1*s.strides[1] + i2*s.strides[2]] = v
    s
end

function setindex!{T}(s::SubArray{T,3}, v, ind::Integer)
    @inbounds strd2 = s.dims[1]
    @inbounds strd3 = strd2*s.dims[2]
    ind -= 1
    i3 = div(ind,strd3)
    ind -= i3*strd3
    i2 = div(ind,strd2)
    i1 = ind-i2*strd2
    s.parent[s.first_index + i1*s.strides[1] + i2*s.strides[2] + i3*s.strides[3]] = v
    s
end

function setindex!{T}(s::SubArray{T,4}, v, ind::Integer)
    @inbounds strd2 = s.dims[1]
    @inbounds strd3 = strd2*s.dims[2]
    @inbounds strd4 = strd3*s.dims[3]
    ind -= 1
    i4 = div(ind,strd4)
    ind -= i4*strd4
    i3 = div(ind,strd3)
    ind -= i3*strd3
    i2 = div(ind,strd2)
    i1 = ind-i2*strd2
    s.parent[s.first_index + i1*s.strides[1] + i2*s.strides[2] + i3*s.strides[3] + i4*s.strides[4]] = v
    s
end

function setindex!{T}(s::SubArray{T,5}, v, ind::Integer)
    @inbounds strd2 = s.dims[1]
    @inbounds strd3 = strd2*s.dims[2]
    @inbounds strd4 = strd3*s.dims[3]
    @inbounds strd5 = strd4*s.dims[4]
    ind -= 1
    i5 = div(ind,strd5)
    ind -= i5*strd5
    i4 = div(ind,strd4)
    ind -= i4*strd4
    i3 = div(ind,strd3)
    ind -= i3*strd3
    i2 = div(ind,strd2)
    i1 = ind-i2*strd2
    s.parent[s.first_index + i1*s.strides[1] + i2*s.strides[2] + i3*s.strides[3] + i4*s.strides[4] + i5*s.strides[5]] = v
    s
end
