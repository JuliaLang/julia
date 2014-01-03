
## Functions to compute the reduced shape

# for reductions that expand 0 dims to 1
reduced_dims(a::AbstractArray, region) = reduced_dims(size(a), region)

# for reductions that keep 0 dims as 0
reduced_dims0(a::AbstractArray, region) = reduced_dims0(size(a), region)

reduced_dims{N}(siz::NTuple{N,Int}, d::Int, rd::Int) = d == 1 ? tuple(rd, siz[d+1:N]...) :
                                                       d == N ? tuple(siz[1:N-1]..., rd) :
                                                       1 < d < N ? tuple(siz[1:d-1]..., rd, siz[d+1:N]...) : 
                                                       siz

reduced_dims{N}(siz::NTuple{N,Int}, d::Int) = reduced_dims(siz, d, 1)

reduced_dims0{N}(siz::NTuple{N,Int}, d::Int) = reduced_dims(siz, d, (siz[d] == 0 ? 0 : 1))

function reduced_dims{N}(siz::NTuple{N,Int}, region)
    rsiz = [siz...]
    for i in region
        if 1 <= i <= N
            rsiz[i] = 1
        end
    end
    tuple(rsiz...)
end

function reduced_dims0{N}(siz::NTuple{N,Int}, region)
    rsiz = [siz...]
    for i in region
        if i <= i <= N
            rsiz[i] = (rsiz[i] == 0 ? 0 : 1)
        end
    end
    tuple(rsiz...)
end


# reduction codes

reducedim(f::Function, A, region, v0) =
    reducedim(f, A, region, v0, similar(A, reduced_dims(A, region)))

maximum{T}(A::AbstractArray{T}, region) =
    isempty(A) ? similar(A,reduced_dims0(A,region)) : reducedim(scalarmax,A,region,typemin(T))
minimum{T}(A::AbstractArray{T}, region) =
    isempty(A) ? similar(A,reduced_dims0(A,region)) : reducedim(scalarmin,A,region,typemax(T))
sum{T}(A::AbstractArray{T}, region)  = reducedim(+,A,region,zero(T))
prod{T}(A::AbstractArray{T}, region) = reducedim(*,A,region,one(T))

all(A::AbstractArray{Bool}, region) = reducedim(&,A,region,true)
any(A::AbstractArray{Bool}, region) = reducedim(|,A,region,false)
sum(A::AbstractArray{Bool}, region) = reducedim(+,A,region,0,similar(A,Int,reduced_dims(A,region)))

prod(A::AbstractArray{Bool}, region) =
    error("use all() instead of prod() for boolean arrays")

# TODO:
# - find out why inner loop with dimsA[i] instead of size(A,i) is way too slow

let reducedim_cache = nothing
# generate the body of the N-d loop to compute a reduction
function gen_reducedim_func(n, f)
    ivars = { symbol(string("i",i)) for i=1:n }
    # limits and vars for reduction loop
    lo    = { symbol(string("lo",i)) for i=1:n }
    hi    = { symbol(string("hi",i)) for i=1:n }
    rvars = { symbol(string("r",i)) for i=1:n }
    setlims = { quote
        # each dim of reduction is either 1:sizeA or ivar:ivar
        if in($i,region)
            $(lo[i]) = 1
            $(hi[i]) = size(A,$i)
        else
            $(lo[i]) = $(hi[i]) = $(ivars[i])
        end
               end for i=1:n }
    rranges = { :( $(lo[i]):$(hi[i]) ) for i=1:n }  # lo:hi for all dims
    body =
    quote
        _tot = v0
        $(setlims...)
        $(make_loop_nest(rvars, rranges,
                         :(_tot = ($f)(_tot, A[$(rvars...)]))))
        R[_ind] = _tot
        _ind += 1
    end
    quote
        local _F_
        function _F_(f, A, region, R, v0)
            _ind = 1
            $(make_loop_nest(ivars, { :(1:size(R,$i)) for i=1:n }, body))
        end
        _F_
    end
end

global reducedim
function reducedim(f::Function, A, region, v0, R)
    ndimsA = ndims(A)

    if is(reducedim_cache,nothing)
        reducedim_cache = Dict()
    end

    key = ndimsA
    fname = :f

    if  (is(f,+)     && (fname=:+;true)) ||
        (is(f,*)     && (fname=:*;true)) ||
        (is(f,scalarmax)   && (fname=:scalarmax;true)) ||
        (is(f,scalarmin)   && (fname=:scalarmin;true)) ||
        (is(f,&)     && (fname=:&;true)) ||
        (is(f,|)     && (fname=:|;true))
        key = (fname, ndimsA)
    end

    if !haskey(reducedim_cache,key)
        fexpr = gen_reducedim_func(ndimsA, fname)
        func = eval(fexpr)
        reducedim_cache[key] = func
    else
        func = reducedim_cache[key]
    end

    func(f, A, region, R, v0)

    return R
end
end

