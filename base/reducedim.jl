# This file is a part of Julia. License is MIT: http://julialang.org/license

## Functions to compute the reduced shape

# for reductions that expand 0 dims to 1
reduced_dims(a::AbstractArray, region) = reduced_dims(size(a), region)

# for reductions that keep 0 dims as 0
reduced_dims0(a::AbstractArray, region) = reduced_dims0(size(a), region)

function reduced_dims{N}(siz::NTuple{N,Int}, d::Int, rd::Int)
    if d < 1
        throw(ArgumentError("dimension must be ≥ 1, got $d"))
    elseif d == 1
        return tuple(rd, siz[d+1:N]...)::typeof(siz)
    elseif 1 < d < N
        return tuple(siz[1:d-1]..., rd, siz[d+1:N]...)::typeof(siz)
    elseif d == N
        return tuple(siz[1:N-1]..., rd)::typeof(siz)
    else
        return siz
    end
end
reduced_dims{N}(siz::NTuple{N,Int}, d::Int) = reduced_dims(siz, d, 1)

function reduced_dims0{N}(siz::NTuple{N,Int}, d::Int)
    if d < 1
        throw(ArgumentError("dimension must be ≥ 1, got $d"))
    elseif d <= N
        return reduced_dims(siz, d, (siz[d] == 0 ? 0 : 1))
    else
        return siz
    end
end

function reduced_dims{N}(siz::NTuple{N,Int}, region)
    rsiz = [siz...]
    for i in region
        isa(i, Integer) || throw(ArgumentError("reduced dimension(s) must be integers"))
        d = convert(Int, i)::Int
        if d < 1
            throw(ArgumentError("region dimension(s) must be ≥ 1, got $d"))
        elseif d <= N
            rsiz[d] = 1
        end
    end
    tuple(rsiz...)::typeof(siz)
end

function reduced_dims0{N}(siz::NTuple{N,Int}, region)
    rsiz = [siz...]
    for i in region
        isa(i, Integer) || throw(ArgumentError("reduced dimension(s) must be integers"))
        d = convert(Int, i)::Int
        if d < 1
            throw(ArgumentError("region dimension(s) must be ≥ 1, got $d"))
        elseif d <= N
            rsiz[d] = (rsiz[d] == 0 ? 0 : 1)
        end
    end
    tuple(rsiz...)::typeof(siz)
end

function regionsize(a, region)
    s = 1
    for d in region
        s *= size(a,d)
    end
    s
end


###### Generic reduction functions #####

## initialization

for (Op, initfun) in ((:(typeof(+)), :zero), (:(typeof(*)), :one), (:(typeof(scalarmax)), :typemin), (:(typeof(scalarmin)), :typemax), (:(typeof(max)), :typemin), (:(typeof(min)), :typemax))
    @eval initarray!{T}(a::AbstractArray{T}, ::$(Op), init::Bool) = (init && fill!(a, $(initfun)(T)); a)
end

for (Op, initval) in ((:(typeof(&)), true), (:(typeof(|)), false))
    @eval initarray!(a::AbstractArray, ::$(Op), init::Bool) = (init && fill!(a, $initval); a)
end

reducedim_initarray{R}(A::AbstractArray, region, v0, ::Type{R}) = fill!(similar(A,R,reduced_dims(A,region)), v0)
reducedim_initarray{T}(A::AbstractArray, region, v0::T) = reducedim_initarray(A, region, v0, T)

reducedim_initarray0{R}(A::AbstractArray, region, v0, ::Type{R}) = fill!(similar(A,R,reduced_dims0(A,region)), v0)
reducedim_initarray0{T}(A::AbstractArray, region, v0::T) = reducedim_initarray0(A, region, v0, T)

# TODO: better way to handle reducedim initialization
#
# The current scheme is basically following Steven G. Johnson's original implementation
#
promote_union(T::Union) = promote_type(T.types...)
promote_union(T) = T

function reducedim_init{S}(f, op::typeof(+), A::AbstractArray{S}, region)
    _reducedim_init(f, op, zero, sum, A, region)
end
function reducedim_init{S}(f, op::typeof(*), A::AbstractArray{S}, region)
    _reducedim_init(f, op, one, prod, A, region)
end
function _reducedim_init(f, op, fv, fop, A, region)
    T = promote_union(eltype(A))
    if method_exists(zero, Tuple{Type{T}})
        x = f(zero(T))
        z = op(fv(x), fv(x))
        Tr = typeof(z) == typeof(x) && !isbits(T) ? T : typeof(z)
    else
        z = fv(fop(f, A))
        Tr = typeof(z)
    end
    return reducedim_initarray(A, region, z, Tr)
end

reducedim_init{T}(f, op::typeof(max), A::AbstractArray{T}, region) = reducedim_init(f, scalarmax, A, region)
reducedim_init{T}(f, op::typeof(min), A::AbstractArray{T}, region) = reducedim_init(f, scalarmin, A, region)
reducedim_init{T}(f::Union{typeof(abs),typeof(abs2)}, op::typeof(max), A::AbstractArray{T}, region) = reducedim_init(f, scalarmax, A, region)

reducedim_init{T}(f, op::typeof(scalarmax), A::AbstractArray{T}, region) = reducedim_initarray0(A, region, typemin(f(zero(T))))
reducedim_init{T}(f, op::typeof(scalarmin), A::AbstractArray{T}, region) = reducedim_initarray0(A, region, typemax(f(zero(T))))
reducedim_init{T}(f::Union{typeof(abs),typeof(abs2)}, op::typeof(scalarmax), A::AbstractArray{T}, region) =
    reducedim_initarray(A, region, zero(f(zero(T))))

reducedim_init(f, op::typeof(&), A::AbstractArray, region) = reducedim_initarray(A, region, true)
reducedim_init(f, op::typeof(|), A::AbstractArray, region) = reducedim_initarray(A, region, false)

# specialize to make initialization more efficient for common cases

for (IT, RT) in ((CommonReduceResult, :(eltype(A))), (SmallSigned, :Int), (SmallUnsigned, :UInt))
    T = Union{[AbstractArray{t} for t in IT.types]..., [AbstractArray{Complex{t}} for t in IT.types]...}
    @eval begin
        reducedim_init(f::typeof(identity), op::typeof(+), A::$T, region) =
            reducedim_initarray(A, region, zero($RT))
        reducedim_init(f::typeof(identity), op::typeof(*), A::$T, region) =
            reducedim_initarray(A, region, one($RT))
        reducedim_init(f::Union{typeof(abs),typeof(abs2)}, op::typeof(+), A::$T, region) =
            reducedim_initarray(A, region, real(zero($RT)))
        reducedim_init(f::Union{typeof(abs),typeof(abs2)}, op::typeof(*), A::$T, region) =
            reducedim_initarray(A, region, real(one($RT)))
    end
end
reducedim_init(f::Union{typeof(identity),typeof(abs),typeof(abs2)}, op::typeof(+), A::AbstractArray{Bool}, region) =
    reducedim_initarray(A, region, 0)


## generic (map)reduction

has_fast_linear_indexing(a::AbstractArray) = false
has_fast_linear_indexing(a::Array) = true

function check_reducedims(R, A)
    # Check whether R has compatible dimensions w.r.t. A for reduction
    #
    # It returns an integer value (useful for choosing implementation)
    # - If it reduces only along leading dimensions, e.g. sum(A, 1) or sum(A, (1, 2)),
    #   it returns the length of the leading slice. For the two examples above,
    #   it will be size(A, 1) or size(A, 1) * size(A, 2).
    # - Otherwise, e.g. sum(A, 2) or sum(A, (1, 3)), it returns 0.
    #
    ndims(R) <= ndims(A) || throw(DimensionMismatch("Cannot reduce $(ndims(A))-dimensional array to $(ndims(R)) dimensions"))
    lsiz = 1
    had_nonreduc = false
    for i = 1:ndims(A)
        sRi = size(R, i)
        sAi = size(A, i)
        if sRi == 1
            if sAi > 1
                if had_nonreduc
                    lsiz = 0  # to reduce along i, but some previous dimensions were non-reducing
                else
                    lsiz *= sAi  # if lsiz was set to zero, it will stay to be zero
                end
            end
        else
            sRi == sAi ||
                throw(DimensionMismatch("Reduction on array of size $(size(A)) with output of size $(size(R))"))
            had_nonreduc = true
        end
    end
    return lsiz
end

function _mapreducedim!{T,N}(f, op, R::AbstractArray, A::AbstractArray{T,N})
    lsiz = check_reducedims(R,A)
    isempty(A) && return R
    sizA1 = size(A, 1)

    if has_fast_linear_indexing(A) && lsiz > 16
        # use mapreduce_impl, which is probably better tuned to achieve higher performance
        nslices = div(length(A), lsiz)
        ibase = 0
        for i = 1:nslices
            @inbounds R[i] = op(R[i], mapreduce_impl(f, op, A, ibase+1, ibase+lsiz))
            ibase += lsiz
        end
    elseif size(R, 1) == 1 && sizA1 > 1
        # keep the accumulator as a local variable when reducing along the first dimension
        sizeR1 = size_skip1(size(R), A)
        sizeA1 = size_skip1(size(A), A)
        @inbounds for IA in CartesianRange(sizeA1)
            IR = min(sizeR1, IA)
            r = R[1,IR]
            @simd for i = 1:size(A, 1)  # fixme (iter): update when #15459 is implemented (and if it does't affect @simd)
                r = op(r, f(A[i, IA]))
            end
            R[1,IR] = r
        end
    else
        sizeR1 = Base.size_skip1(size(R), A)
        sizeA1 = Base.size_skip1(size(A), A)
        @inbounds for IA in CartesianRange(sizeA1)
            IR = min(IA, sizeR1)
            @simd for i = 1:size(A, 1)  # fixme (iter): update when #15459 is implemented (and if it does't affect @simd)
                R[i,IR] = op(R[i,IR], f(A[i,IA]))
            end
        end
    end
    return R
end

mapreducedim!(f, op, R::AbstractArray, A::AbstractArray) =
    (_mapreducedim!(f, op, R, A); R)

reducedim!{RT}(op, R::AbstractArray{RT}, A::AbstractArray) =
    mapreducedim!(identity, op, R, A, zero(RT))

mapreducedim(f, op, A::AbstractArray, region, v0) =
    mapreducedim!(f, op, reducedim_initarray(A, region, v0), A)
mapreducedim{T}(f, op, A::AbstractArray{T}, region) =
    mapreducedim!(f, op, reducedim_init(f, op, A, region), A)

reducedim(op, A::AbstractArray, region, v0) = mapreducedim(identity, op, A, region, v0)
reducedim(op, A::AbstractArray, region) = mapreducedim(identity, op, A, region)


##### Specific reduction functions #####

for (fname, op) in [(:sum, :+), (:prod, :*),
                    (:maximum, :scalarmax), (:minimum, :scalarmin),
                    (:all, :&), (:any, :|)]

    fname! = Symbol(fname, '!')
    @eval begin
        $(fname!)(f::Function, r::AbstractArray, A::AbstractArray; init::Bool=true) =
            mapreducedim!(f, $(op), initarray!(r, $(op), init), A)
        $(fname!)(r::AbstractArray, A::AbstractArray; init::Bool=true) = $(fname!)(identity, r, A; init=init)

        $(fname)(f::Function, A::AbstractArray, region) =
            mapreducedim(f, $(op), A, region)
        $(fname)(A::AbstractArray, region) = $(fname)(identity, A, region)
    end
end

for (fname, fbase, fun) in [(:sumabs, :sum, :abs),
                            (:sumabs2, :sum, :abs2),
                            (:maxabs, :maximum, :abs),
                            (:minabs, :minimum, :abs)]
    fname! = Symbol(fname, '!')
    fbase! = Symbol(fbase, '!')
    @eval begin
        $(fname!)(r::AbstractArray, A::AbstractArray; init::Bool=true) =
            $(fbase!)($(fun), r, A; init=init)
        $(fname)(A::AbstractArray, region) = $(fbase)($(fun), A, region)
    end
end


##### findmin & findmax #####

function findminmax!{T,N}(f, Rval, Rind, A::AbstractArray{T,N})
    (isempty(Rval) || isempty(A)) && return Rval, Rind
    (ndims(Rval) <= N && ndims(Rind) <= N) || throw(DimensionMismatch("Cannot find-reduce $(ndims(A))-dimensional array to $(ndims(Rval)),$(ndims(Rind)) dimensions"))
    for i = 1:N
        (size(Rval, i) == size(A, i) || size(Rval, i) == 1) || throw(DimensionMismatch("Find-reduction on array of size $(size(A)) with output of size $(size(Rval))"))
        size(Rval, i) == size(Rind, i) || throw(DimensionMismatch("Find-reduction: outputs must be of the same size"))
    end
    # If we're reducing along dimension 1, for efficiency we can make use of a temporary.
    # Otherwise, keep the result in Rval/Rind so that we traverse A in storage order.
    sizeR1 = size_skip1(size(Rval), A)
    sizeA1 = size_skip1(size(A), A)
    k = 0
    if size(Rval, 1) < size(A, 1)
        @inbounds for IA in CartesianRange(sizeA1)
            IR = min(sizeR1, IA)
            tmpRv = Rval[1,IR]
            tmpRi = Rind[1,IR]
            for i = 1:size(A,1)  # fixme (iter): update when #15459 is implemented (and if it does't affect @simd)
                k += 1
                tmpAv = A[i,IA]
                if f(tmpAv, tmpRv)
                    tmpRv = tmpAv
                    tmpRi = k
                end
            end
            Rval[1,IR] = tmpRv
            Rind[1,IR] = tmpRi
        end
    else
        @inbounds for IA in CartesianRange(sizeA1)
            IR = min(sizeR1, IA)
            for i = 1:size(A, 1)  # fixme (iter): update when #15459 is implemented (and if it does't affect @simd)
                k += 1
                tmpAv = A[i,IA]
                if f(tmpAv, Rval[i,IR])
                    Rval[i,IR] = tmpAv
                    Rind[i,IR] = k
                end
            end
        end
    end
    Rval, Rind
end


"""
    findmin!(rval, rind, A, [init=true]) -> (minval, index)

Find the minimum of `A` and the corresponding linear index along singleton
dimensions of `rval` and `rind`, and store the results in `rval` and `rind`.
"""
function findmin!{R}(rval::AbstractArray{R},
                     rind::AbstractArray,
                     A::AbstractArray;
                     init::Bool=true)
    findminmax!(<, initarray!(rval, scalarmin, init), rind, A)
end

function findmin{T}(A::AbstractArray{T}, region)
    if isempty(A)
        return (similar(A, reduced_dims0(A, region)),
                zeros(Int, reduced_dims0(A, region)))
    end
    return findminmax!(<, reducedim_initarray0(A, region, typemax(T)),
            zeros(Int, reduced_dims0(A, region)), A)
end

"""
    findmax!(rval, rind, A, [init=true]) -> (maxval, index)

Find the maximum of `A` and the corresponding linear index along singleton
dimensions of `rval` and `rind`, and store the results in `rval` and `rind`.
"""
function findmax!{R}(rval::AbstractArray{R},
                     rind::AbstractArray,
                     A::AbstractArray;
                     init::Bool=true)
    findminmax!(>, initarray!(rval, scalarmax, init), rind, A)
end

function findmax{T}(A::AbstractArray{T}, region)
    if isempty(A)
        return (similar(A, reduced_dims0(A,region)),
                zeros(Int, reduced_dims0(A,region)))
    end
    return findminmax!(>, reducedim_initarray0(A, region, typemin(T)),
            zeros(Int, reduced_dims0(A, region)), A)
end

size_skip1{T}(dims::Tuple{}, Aref::AbstractArray{T,0}) = CartesianIndex(())
size_skip1{T,N}(dims::NTuple{N,Int}, Aref::AbstractArray{T,N}) = CartesianIndex(skip1(dims...))
@inline size_skip1{T,M,N}(dims::NTuple{M,Int}, Aref::AbstractArray{T,N}) = size_skip1(tuple(dims..., 1), Aref)
skip1(x, t...) = t
