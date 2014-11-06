## Functions to compute the reduced shape

# for reductions that expand 0 dims to 1
reduced_dims(a::AbstractArray, region) = reduced_dims(size(a), region)

# for reductions that keep 0 dims as 0
reduced_dims0(a::AbstractArray, region) = reduced_dims0(size(a), region)
reduced_dims{N}(siz::NTuple{N,Int}, d::Int, rd::Int) = (d == 1 ? tuple(rd, siz[d+1:N]...) :
                                                        d == N ? tuple(siz[1:N-1]..., rd) :
                                                        1 < d < N ? tuple(siz[1:d-1]..., rd, siz[d+1:N]...) :
                                                        siz)::typeof(siz)
reduced_dims{N}(siz::NTuple{N,Int}, d::Int) = reduced_dims(siz, d, 1)
reduced_dims0{N}(siz::NTuple{N,Int}, d::Int) = 1 <= d <= N ? reduced_dims(siz, d, (siz[d] == 0 ? 0 : 1)) : siz

function reduced_dims{N}(siz::NTuple{N,Int}, region)
    rsiz = [siz...]
    for i in region
        if 1 <= i <= N
            rsiz[i] = 1
        end
    end
    tuple(rsiz...)::typeof(siz)
end

function reduced_dims0{N}(siz::NTuple{N,Int}, region)
    rsiz = [siz...]
    for i in region
        if i <= i <= N
            rsiz[i] = (rsiz[i] == 0 ? 0 : 1)
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

for (Op, initfun) in ((:AddFun, :zero), (:MulFun, :one), (:MaxFun, :typemin), (:MinFun, :typemax))
    @eval initarray!{T}(a::AbstractArray{T}, ::$(Op), init::Bool) = (init && fill!(a, $(initfun)(T)); a)
end

for (Op, initval) in ((:AndFun, true), (:OrFun, false))
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
promote_union(T::UnionType) = promote_type(T.types...)
promote_union(T) = T
function reducedim_init{S}(f, op::AddFun, A::AbstractArray{S}, region)
    T = promote_union(S)
    if method_exists(zero, (Type{T},))
        x = f(zero(T))
        z = zero(x) + zero(x)
        Tr = typeof(z) == typeof(x) && !isbits(T) ? T : typeof(z)
    else
        z = zero(sum(f, A))
        Tr = typeof(z)
    end
    return reducedim_initarray(A, region, z, Tr)
end

function reducedim_init{S}(f, op::MulFun, A::AbstractArray{S}, region)
    T = promote_union(S)
    if method_exists(zero, (Type{T},))
        x = f(zero(T))
        z = one(x) * one(x)
        Tr = typeof(z) == typeof(x) && !isbits(T) ? T : typeof(z)
    else
        z = one(prod(f, A))
        Tr = typeof(z)
    end
    return reducedim_initarray(A, region, z, Tr)
end

reducedim_init{T}(f, op::MaxFun, A::AbstractArray{T}, region) = reducedim_initarray0(A, region, typemin(f(zero(T))))
reducedim_init{T}(f, op::MinFun, A::AbstractArray{T}, region) = reducedim_initarray0(A, region, typemax(f(zero(T))))
reducedim_init{T}(f::Union(AbsFun,Abs2Fun), op::MaxFun, A::AbstractArray{T}, region) =
    reducedim_initarray(A, region, zero(f(zero(T))))

reducedim_init(f, op::AndFun, A::AbstractArray, region) = reducedim_initarray(A, region, true)
reducedim_init(f, op::OrFun, A::AbstractArray, region) = reducedim_initarray(A, region, false)

# specialize to make initialization more efficient for common cases

for (IT, RT) in ((CommonReduceResult, :(eltype(A))), (SmallSigned, :Int), (SmallUnsigned, :UInt))
    T = Union([AbstractArray{t} for t in IT.types]..., [AbstractArray{Complex{t}} for t in IT.types]...)
    @eval begin
        reducedim_init(f::IdFun, op::AddFun, A::$T, region) =
            reducedim_initarray(A, region, zero($RT))
        reducedim_init(f::IdFun, op::MulFun, A::$T, region) =
            reducedim_initarray(A, region, one($RT))
        reducedim_init(f::Union(AbsFun,Abs2Fun), op::AddFun, A::$T, region) =
            reducedim_initarray(A, region, real(zero($RT)))
        reducedim_init(f::Union(AbsFun,Abs2Fun), op::MulFun, A::$T, region) =
            reducedim_initarray(A, region, real(one($RT)))
    end
end
reducedim_init(f::Union(IdFun,AbsFun,Abs2Fun), op::AddFun, A::AbstractArray{Bool}, region) =
    reducedim_initarray(A, region, 0)


## generic (map)reduction

has_fast_linear_indexing(a::AbstractArray) = false
has_fast_linear_indexing(a::Array) = true

function check_reducdims(R, A)
    # Check whether R has compatible dimensions w.r.t. A for reduction
    #
    # It returns an integer value value (useful for choosing implementation)
    # - If it reduces only along leading dimensions, e.g. sum(A, 1) or sum(A, (1, 2)),
    #   it returns the length of the leading slice. For the two examples above,
    #   it will be size(A, 1) or size(A, 1) * size(A, 2).
    # - Otherwise, e.g. sum(A, 2) or sum(A, (1, 3)), it returns 0.
    #
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

@ngenerate N typeof(R) function _mapreducedim!{T,N}(f, op, R::AbstractArray, A::AbstractArray{T,N})
    lsiz = check_reducdims(R, A)
    isempty(A) && return R
    @nextract N sizeR d->size(R,d)
    sizA1 = size(A, 1)

    if has_fast_linear_indexing(A) && lsiz > 16
        # use mapreduce_impl, which is probably better tuned to achieve higher performance
        nslices = div(length(A), lsiz)
        ibase = 0
        for i = 1:nslices
            @inbounds R[i] = mapreduce_impl(f, op, A, ibase+1, ibase+lsiz)
            ibase += lsiz
        end
    elseif size(R, 1) == 1 && sizA1 > 1
        # keep the accumulator as a local variable when reducing along the first dimension
        @nloops N i d->(d>1? (1:size(A,d)) : (1:1)) d->(j_d = sizeR_d==1 ? 1 : i_d) begin
            @inbounds r = (@nref N R j)
            for i_1 = 1:sizA1
                @inbounds v = f(@nref N A i)
                r = op(r, v)
            end
            @inbounds (@nref N R j) = r
        end
    else
        # general implementation
        @nloops N i A d->(j_d = sizeR_d==1 ? 1 : i_d) begin
            @inbounds v = f(@nref N A i)
            @inbounds (@nref N R j) = op((@nref N R j), v)
        end
    end
    return R
end

mapreducedim!(f, op, R::AbstractArray, A::AbstractArray) = _mapreducedim!(f, op, R, A)

to_op(op) = op
function to_op(op::Function)
    is(op, +) ? AddFun() :
    is(op, *) ? MulFun() :
    is(op, &) ? AndFun() :
    is(op, |) ? OrFun() : op
end

mapreducedim!(f::Function, op, R::AbstractArray, A::AbstractArray) =
    _mapreducedim!(f, to_op(op), R, A)

reducedim!{RT}(op, R::AbstractArray{RT}, A::AbstractArray) =
    mapreducedim!(IdFun(), op, R, A, zero(RT))

mapreducedim(f, op, A::AbstractArray, region, v0) =
    mapreducedim!(f, op, reducedim_initarray(A, region, v0), A)
mapreducedim{T}(f, op, A::AbstractArray{T}, region) =
    mapreducedim!(f, op, reducedim_init(f, to_op(op), A, region), A)

reducedim(op, A::AbstractArray, region, v0) = mapreducedim(IdFun(), op, A, region, v0)
reducedim(op, A::AbstractArray, region) = mapreducedim(IdFun(), op, A, region)


##### Specific reduction functions #####

for (fname, Op) in [(:sum, :AddFun), (:prod, :MulFun),
                    (:maximum, :MaxFun), (:minimum, :MinFun),
                    (:all, :AndFun), (:any, :OrFun)]

    fname! = symbol(string(fname, '!'))
    @eval begin
        $(fname!)(f::Union(Function,Func{1}), r::AbstractArray, A::AbstractArray; init::Bool=true) =
            mapreducedim!(f, $(Op)(), initarray!(r, $(Op)(), init), A)
        $(fname!)(r::AbstractArray, A::AbstractArray; init::Bool=true) = $(fname!)(IdFun(), r, A; init=init)

        $(fname)(f::Union(Function,Func{1}), A::AbstractArray, region) =
            mapreducedim(f, $(Op)(), A, region)
        $(fname)(A::AbstractArray, region) = $(fname)(IdFun(), A, region)
    end
end

for (fname, fbase, Fun) in [(:sumabs, :sum, :AbsFun),
                            (:sumabs2, :sum, :Abs2Fun),
                            (:maxabs, :maximum, :AbsFun),
                            (:minabs, :minimum, :AbsFun)]
    fname! = symbol(string(fname, '!'))
    fbase! = symbol(string(fbase, '!'))
    @eval begin
        $(fname!)(r::AbstractArray, A::AbstractArray; init::Bool=true) =
            $(fbase!)($(Fun)(), r, A; init=init)
        $(fname)(A::AbstractArray, region) = $(fbase)($(Fun)(), A, region)
    end
end


##### findmin & findmax #####

# Generate the body for a reduction function reduce!(f, Rval, Rind, A), using a comparison operator f
# Rind contains the index of A from which Rval was taken
function gen_findreduction_body(N, f::Function)
    F = Expr(:quote, f)
    quote
        (isempty(Rval) || isempty(A)) && return Rval, Rind
        for i = 1:$N
            (size(Rval, i) == size(A, i) || size(Rval, i) == 1) || throw(DimensionMismatch("Find-reduction on array of size $(size(A)) with output of size $(size(Rval))"))
            size(Rval, i) == size(Rind, i) || throw(DimensionMismatch("Find-reduction: outputs must be of the same size"))
        end
        @nexprs $N d->(sizeR_d = size(Rval,d))
        # If we're reducing along dimension 1, for efficiency we can make use of a temporary.
        # Otherwise, keep the result in Rval/Rind so that we traverse A in storage order.
        k = 0
        @inbounds if size(Rval, 1) < size(A, 1)
            @nloops $N i d->(d>1? (1:size(A,d)) : (1:1)) d->(j_d = sizeR_d==1 ? 1 : i_d) begin
                tmpRv = (@nref $N Rval j)
                tmpRi = (@nref $N Rind j)
                for i_1 = 1:size(A,1)
                    k += 1
                    tmpAv = (@nref $N A i)
                    if ($F)(tmpAv, tmpRv)
                        tmpRv = tmpAv
                        tmpRi = k
                    end
                end
                (@nref $N Rval j) = tmpRv
                (@nref $N Rind j) = tmpRi
            end
        else
            @nloops $N i A d->(j_d = sizeR_d==1 ? 1 : i_d) begin
                k += 1
                tmpAv = (@nref $N A i)
                if ($F)(tmpAv, (@nref $N Rval j))
                    (@nref $N Rval j) = tmpAv
                    (@nref $N Rind j) = k
                end
            end
        end
        Rval, Rind
    end
end

eval(ngenerate(:N, :(typeof((Rval,Rind))), :(_findmin!{T,N}(Rval::AbstractArray, Rind::AbstractArray, A::AbstractArray{T,N})), N->gen_findreduction_body(N, <)))
findmin!{R}(rval::AbstractArray{R}, rind::AbstractArray, A::AbstractArray; init::Bool=true) = _findmin!(initarray!(rval, typemax(R), init), rind, A)
findmin{T}(A::AbstractArray{T}, region) =
    isempty(A) ? (similar(A,reduced_dims0(A,region)), zeros(Int,reduced_dims0(A,region))) :
                  _findmin!(reducedim_initarray0(A, region, typemax(T)), zeros(Int,reduced_dims0(A,region)), A)

eval(ngenerate(:N, :(typeof((Rval,Rind))), :(_findmax!{T,N}(Rval::AbstractArray, Rind::AbstractArray, A::AbstractArray{T,N})), N->gen_findreduction_body(N, >)))
findmax!{R}(rval::AbstractArray{R}, rind::AbstractArray, A::AbstractArray; init::Bool=true) = _findmax!(initarray!(rval, typemin(R), init), rind, A)
findmax{T}(A::AbstractArray{T}, region) =
    isempty(A) ? (similar(A,reduced_dims0(A,region)), zeros(Int,reduced_dims0(A,region))) :
                  _findmax!(reducedim_initarray0(A, region, typemin(T)), zeros(Int,reduced_dims0(A,region)), A)


