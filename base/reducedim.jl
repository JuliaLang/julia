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

reducedim(f::Function, A, region, initial) = reducedim!(f, reduction_init(A, region, initial), A)

reducedim(f::Function, A, region, initial, R) = reducedim!(f, fill!(R, initial), A)

function reducedim!_function(N::Int, f::Function)
    body = gen_reduction_body(N, f)
    @eval begin
        local _F_
        function _F_(R, A)
            $body
        end
        _F_
    end
end

let reducedim_cache = Dict()
# reducedim! assumes that R has already been initialized with a seed value
global reducedim!
function reducedim!(f::Function, R, A)
    if isempty(R)
        return R
    end
    ndimsA = ndims(A)
    key = (ndimsA, f)
    if !haskey(reducedim_cache,key)
        func = reducedim!_function(ndimsA, f)
        reducedim_cache[key] = func
    else
        func = reducedim_cache[key]
    end
    func(R, A)::typeof(R)
end
end  # let reducedim_cache

# Generate the body for a reduction function reduce!(f, R, A), using binary operation f,
# where R is the output and A is the input.
# R must have already been set to the appropriate size and initialized with the seed value
function gen_reduction_body(N, f::Function)
    F = Expr(:quote, f)
    quote
        (isempty(R) || isempty(A)) && return R
        for i = 1:$N
            (size(R, i) == size(A, i) || size(R, i) == 1) || throw(DimensionMismatch("Reduction on array of size $(size(A)) with output of size $(size(R))"))
        end
        @nextract $N sizeR d->size(R,d)
        # If we're reducing along dimension 1, for efficiency we can make use of a temporary.
        # Otherwise, keep the result in R so that we traverse A in storage order.
        if size(R, 1) < size(A, 1)
            @nloops $N i d->(d>1? (1:size(A,d)) : (1:1)) d->(j_d = sizeR_d==1 ? 1 : i_d) begin
                @inbounds tmp = (@nref $N R j)
                for i_1 = 1:size(A,1)
                    @inbounds tmp = ($F)(tmp, (@nref $N A i))
                end
                @inbounds (@nref $N R j) = tmp
            end
        else
            @nloops $N i A d->(j_d = sizeR_d==1 ? 1 : i_d) begin
                @inbounds (@nref $N R j) = ($F)((@nref $N R j), (@nref $N A i))
            end
        end
        R
    end
end

reduction_init{T}(A::AbstractArray, region, initial::T, Tr=T) = fill!(similar(A,Tr,reduced_dims(A,region)), initial)

function initarray!{T}(a::AbstractArray{T}, v::T, init::Bool) 
    if init 
        fill!(a, v)
    end
    return a
end


##### Specific reduction functions #####

## sum

@ngenerate N typeof(R) function _sum!{T,N}(f, R::AbstractArray, A::AbstractArray{T,N})
    (isempty(R) || isempty(A)) && return R
    rdims = 0
    for i = 1:N
        if size(R, i) == size(A, i)
        elseif size(R, i) == 1
            rdims += 1
        else
            throw(DimensionMismatch("sum of array of size $(size(A)) with output of size $(size(R))"))
        end
    end
    @nextract N sizeR d->size(R,d)
    # If we're reducing along dimension 1 and dimension 1 is
    # sufficiently large, use the pairwise implementation. Otherwise,
    # keep the result in R so that we traverse A in storage order.
    sz1 = size(A, 1)
    if size(R, 1) < sz1 && sz1 >= 16 && rdims == 1
        for i = 1:div(length(A), sz1)
            @inbounds R[i] = mapreduce_impl(f, AddFun(), A, (i-1)*sz1+1, i*sz1)
        end
    else
        @nloops N i A d->(j_d = sizeR_d==1 ? 1 : i_d) begin
            @inbounds (@nref N R j) += evaluate(f, @nref N A i)
        end
    end
    R
end

function sum{T}(f::Union(Function,Func{1}), A::AbstractArray{T}, region)
    if method_exists(zero, (Type{T},))
        fz = evaluate(f, zero(T))
        z = fz + fz
        Tr = typeof(z) == typeof(fz) && !isbits(T) ? T : typeof(z)
    else
        # TODO: handle more heterogeneous sums.  e.g. sum(A, 1) where
        # A is a Matrix{Any} with one column of numbers and one of vectors
        z = zero(sum(f, A))
        Tr = typeof(z)
    end
    _sum!(f, reduction_init(A, region, z, Tr), A)
end

sum!{R}(f::Union(Function,Func{1}), r::AbstractArray{R}, A::AbstractArray; init::Bool=true) =
    _sum!(f, initarray!(r, zero(R), init), A)

for (fname, func) in ((:sum, :IdFun), (:sumabs, :AbsFun), (:sumabs2, :Abs2Fun))
    @eval begin
        $fname(A::AbstractArray, region) = sum($func(), A, region)
        $(symbol("$(fname)!")){R}(r::AbstractArray{R}, A::AbstractArray; init::Bool=true) =
            sum!($func(), r, A; init=init)
    end
end


## prod

eval(ngenerate(:N, :(typeof(R)), :(_prod!{T,N}(R::AbstractArray, A::AbstractArray{T,N})), N->gen_reduction_body(N, *)))
prod!{R}(r::AbstractArray{R}, A::AbstractArray; init::Bool=true) = _prod!(initarray!(r, one(R), init), A)

function prod{T}(A::AbstractArray{T}, region)
    if method_exists(one, (Type{T},))
        z = one(T) * one(T)
        Tr = typeof(z) == typeof(one(T)) ? T : typeof(z)
    else
        # TODO: handle more heterogeneous products.  e.g. prod(A, 1) where
        # A is a Matrix{Any} with one column of numbers and one of vectors
        z = one(prod(A))
        Tr = typeof(z)
    end
    _prod!(reduction_init(A, region, z, Tr), A)
end

prod(A::AbstractArray{Bool}, region) = error("use all() instead of prod() for boolean arrays")


## maximum & minimum

eval(ngenerate(:N, :(typeof(R)), :(_maximum!{T,N}(R::AbstractArray, A::AbstractArray{T,N})), N->gen_reduction_body(N, scalarmax)))
maximum!{R}(r::AbstractArray{R}, A::AbstractArray; init::Bool=true) = _maximum!(initarray!(r, typemin(R), init), A)
maximum{T}(A::AbstractArray{T}, region) =
    isempty(A) ? similar(A,reduced_dims0(A,region)) : _maximum!(reduction_init(A, region, typemin(T)), A)

eval(ngenerate(:N, :(typeof(R)), :(_minimum!{T,N}(R::AbstractArray, A::AbstractArray{T,N})), N->gen_reduction_body(N, scalarmin)))
minimum!{R}(r::AbstractArray{R}, A::AbstractArray; init::Bool=true) = _minimum!(initarray!(r, typemax(R), init), A)
minimum{T}(A::AbstractArray{T}, region) =
    isempty(A) ? similar(A, reduced_dims0(A, region)) : _minimum!(reduction_init(A, region, typemax(T)), A)


## all & any

eval(ngenerate(:N, :(typeof(R)), :(_all!{N}(R::AbstractArray, A::AbstractArray{Bool,N})), N->gen_reduction_body(N, &)))
all!(r::AbstractArray, A::AbstractArray{Bool}; init::Bool=true) = _all!(initarray!(r, true, init), A)
all(A::AbstractArray{Bool}, region) = _all!(reduction_init(A, region, true), A)

eval(ngenerate(:N, :(typeof(R)), :(_any!{N}(R::AbstractArray, A::AbstractArray{Bool,N})), N->gen_reduction_body(N, |)))
any!(r::AbstractArray, A::AbstractArray{Bool}; init::Bool=true) = _any!(initarray!(r, false, init), A)
any(A::AbstractArray{Bool}, region) = _any!(reduction_init(A, region, false), A)


## findmin & findmax

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
                  _findmin!(reduction_init(A, region, typemax(T)), zeros(Int,reduced_dims0(A,region)), A)

eval(ngenerate(:N, :(typeof((Rval,Rind))), :(_findmax!{T,N}(Rval::AbstractArray, Rind::AbstractArray, A::AbstractArray{T,N})), N->gen_findreduction_body(N, >)))
findmax!{R}(rval::AbstractArray{R}, rind::AbstractArray, A::AbstractArray; init::Bool=true) = _findmax!(initarray!(rval, typemin(R), init), rind, A)
findmax{T}(A::AbstractArray{T}, region) = 
    isempty(A) ? (similar(A,reduced_dims0(A,region)), zeros(Int,reduced_dims0(A,region))) :
                  _findmax!(reduction_init(A, region, typemin(T)), zeros(Int,reduced_dims0(A,region)), A)
