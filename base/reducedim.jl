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


### Generic reduction functions

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

reduction_init{T}(A::AbstractArray, region, initial::T) = fill!(similar(A,T,reduced_dims(A,region)), initial)


### Pre-generated cases
# For performance, these bypass reducedim_cache

all(A::AbstractArray{Bool}, region) = all!(reduction_init(A,region,true), A)
eval(ngenerate(:N, :(typeof(R)), :(all!{N}(R::AbstractArray, A::AbstractArray{Bool,N})), N->gen_reduction_body(N, &)))
any(A::AbstractArray{Bool}, region) = any!(reduction_init(A,region,false), A)
eval(ngenerate(:N, :(typeof(R)), :(any!{N}(R::AbstractArray, A::AbstractArray{Bool,N})), N->gen_reduction_body(N, |)))
maximum{T}(A::AbstractArray{T}, region) =
    isempty(A) ? similar(A,reduced_dims0(A,region)) : maximum!(reduction_init(A,region,typemin(T)), A)
eval(ngenerate(:N, :(typeof(R)), :(maximum!{T,N}(R::AbstractArray, A::AbstractArray{T,N})), N->gen_reduction_body(N, scalarmax)))
minimum{T}(A::AbstractArray{T}, region) =
    isempty(A) ? similar(A,reduced_dims0(A,region)) : minimum!(reduction_init(A,region,typemax(T)), A)
eval(ngenerate(:N, :(typeof(R)), :(minimum!{T,N}(R::AbstractArray, A::AbstractArray{T,N})), N->gen_reduction_body(N, scalarmin)))
sum{T}(A::AbstractArray{T}, region) = sum!(reduction_init(A,region,zero(T)), A)
sum(A::AbstractArray{Bool}, region) = sum!(reduction_init(A,region,0), A)
eval(ngenerate(:N, :(typeof(R)), :(sum!{T,N}(R::AbstractArray, A::AbstractArray{T,N})), N->gen_reduction_body(N, +)))
prod{T}(A::AbstractArray{T}, region) = prod!(reduction_init(A,region,one(T)), A)
eval(ngenerate(:N, :(typeof(R)), :(prod!{T,N}(R::AbstractArray, A::AbstractArray{T,N})), N->gen_reduction_body(N, *)))

prod(A::AbstractArray{Bool}, region) = error("use all() instead of prod() for boolean arrays")
