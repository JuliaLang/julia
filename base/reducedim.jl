
# for reductions that expand 0 dims to 1
reduced_dims(A, region) = ntuple(ndims(A), i->(in(i, region) ? 1 :
                                               size(A,i)))

# keep 0 dims in place
reduced_dims0(A, region) = ntuple(ndims(A), i->(size(A,i)==0 ? 0 :
                                                in(i, region) ? 1 :
                                                size(A,i)))

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

