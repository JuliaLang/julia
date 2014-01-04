
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


##### (original) reduction codes for arbitrary arrays

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



##### Below are optimized codes for contiguous arrays

function rcompress_dims{N}(siz::NTuple{N,Int}, region)
    isrd = fill(false, N)
    for k in region
        if 1 <= k <= N
            isrd[k] = true
        end
    end
    
    sdims = Int[]
    sizehint(sdims, N)
    b = isrd[1]
    n = siz[1]
    i = 2
    while i <= N
        if isrd[i] == b
            n *= siz[i]
        else
            b = !b
            push!(sdims, n)
            n = siz[i]
        end
        i += 1
    end
    if n != 1
        push!(sdims, n)
    end
    return (isrd[1], sdims)
end

function generate_reducedim_funcs(fname, comb, sker, ker0!, ker1!)
    # Parameters:
    #
    # - fname:  the interface function name (e.g. sum, maximum)
    # - comb:   the combination operation (e.g. +)
    # - sker:   a kernel function that reduces a vector (or a range of it) to a scalar
    # - ker0!:  a kernel that initializes an accumulator array using the first column of terms
    # - ker1!:  a kernel that accumulates a term column to an accumulating column
    #

    fname! = symbol("$(fname)!")
    fa! = symbol("$(fname)_a!")
    fb! = symbol("$(fname)_b!")
    
    quote
        global $(fname!)
        function $(fname!)(dst::Array, a::Array, dim::Integer)
            nd = ndims(a)
            siz = size(a)
            if 1 <= dim <= nd
                if dim == 1
                    $(fa!)(true, dst, 0, a, 0, prod(siz[2:nd]), siz[1])
                elseif dim == nd
                    $(fb!)(true, dst, 0, a, 0, siz[nd], prod(siz[1:nd-1]))
                else
                    $(fb!)(true, dst, 0, a, 0, prod(siz[dim+1:nd]), siz[dim], prod(siz[1:dim-1]))
                end
            else
                $(ker0!)(dst, 1, a, 1, length(a))
            end
            dst
        end

        function $(fname!)(dst::Array, a::Array, region)
            if length(region) == 1
                $(fname!)(dst, a, region[1])
            else
                isrd1, secs = rcompress_dims(size(a), region)
                if isrd1
                    $(fa!)(true, dst, 0, a, 0, secs[end:-1:1]...)
                else
                    $(fb!)(true, dst, 0, a, 0, secs[end:-1:1]...)
                end                
            end
            dst
        end
    
        # $(fa!)
        global $(fa!)
        function $(fa!)(isinit::Bool, dst::Array, od::Int, a::Array, oa::Int, n1::Int)
            if isinit
                dst[od+1] = $(sker)(a, oa+1, oa+n1)
            else
                dst[od+1] = $(comb)(dst[od+1], $(sker)(a, oa+1, oa+n1))
            end
        end

        function $(fa!)(isinit::Bool, dst::Array, od::Int, a::Array, oa::Int, n1::Int, n2::Int)
            if isinit
                for j = 1:n1
                    alast = oa + n2
                    dst[od+j] = $(sker)(a, oa+1, alast)
                    oa = alast
                end
            else
                for j = 1:n1
                    alast = oa + n2
                    dst[od+j] = $(comb)(dst[od+j], $(sker)(a, oa+1, alast))
                    oa = alast
                end
            end
        end

        function $(fa!)(isinit::Bool, dst::Array, od::Int, a::Array, oa::Int, n1::Int, n2::Int, n3::Int, ns::Int...)
            as::Int = *(n2, n3, ns...)
            if length(ns) & 1 == 0
                $(fa!)(isinit, dst, od, a, oa, n2, n3, ns...)
                oa += as    
    
                for j = 2:n1
                    $(fa!)(false, dst, od, a, oa, n2, n3, ns...)
                    oa += as
                end                                 
            else 
                ds::Int = *(n3, ns[2:2:end]...)
                for j = 1:n1
                    $(fa!)(isinit, dst, od, a, oa, n2, n3, ns...)
                    od += ds
                    oa += as
                end   
            end
        end

        # $(fb!)
        global $(fb!)
        function $(fb!)(isinit::Bool, dst::Array, od::Int, a::Array, oa::Int, n1::Int)
            if isinit
                $(ker0!)(dst, od+1, a, oa+1, n1)
            else
                $(ker1!)(dst, od+1, a, oa+1, n1)
            end
        end

        function $(fb!)(isinit::Bool, dst::Array, od::Int, a::Array, oa::Int, n1::Int, n2::Int)
            if isinit
                $(ker0!)(dst, od+1, a, oa+1, n2)
            else
                $(ker1!)(dst, od+1, a, oa+1, n2)        
            end
            oa += n2
    
            for j = 2:n1
                $(ker1!)(dst, od+1, a, oa+1, n2)
                oa += n2
            end
        end

        function $(fb!)(isinit::Bool, dst::Array, od::Int, a::Array, oa::Int, n1::Int, n2::Int, n3::Int, ns::Int...)
            as = *(n2, n3, ns...)
            if length(ns) & 1 == 0
                ds::Int = *(n3, ns[2:2:end]...)
                for j = 1:n1
                    $(fb!)(isinit, dst, od, a, oa, n2, n3, ns...)
                    od += ds
                    oa += as
                end
            else
                $(fb!)(isinit, dst, od, a, oa, n2, n3, ns...)
                oa += as
    
                for j = 2:n1
                    $(fb!)(false, dst, od, a, oa, n2, n3, ns...)
                    oa += as
                end
            end
        end
    end
end

macro code_reducedim(fname, comb, sker, ker0, ker1)
    esc(generate_reducedim_funcs(fname, comb, sker, ker0, ker1))
end


# sum

function vcopy!(dst::Array, od::Int, a::Array, oa::Int, n::Int)
    for i = 1:n
        @inbounds dst[od] = a[oa]
        od += 1
        oa += 1
    end    
end

vcopy!{T}(dst::Array{T}, od::Int, a::Array{T}, oa::Int, n::Int) = copy!(dst, od, a, oa, n)

function vadd!(dst::Array, od::Int, a::Array, oa::Int, n::Int)
    for i = 1:n
        @inbounds dst[od] += a[oa]
        od += 1
        oa += 1
    end
end

@code_reducedim sum (+) sum_seq vcopy! vadd!

function sum{R}(rt::Type{R}, a::Array, region)
    dst = Array(R, reduced_dims(a, region))
    if !isempty(dst)
        if isempty(a)
            fill!(dst, zero(R))
        else
            sum!(dst, a, region)
        end
    end
    return dst
end

sum{T}(a::Array{T}, region) = sum(T, a, region)
sum(a::Array{Bool}, region) = sum(Int, a, region)


# maximum & minimum

function vmax!(dst::Array, od::Int, a::Array, oa::Int, n::Int)
    for i = 1:n
        @inbounds dst[od] = scalarmax(dst[od], a[oa])
        od += 1
        oa += 1
    end
end

function vmin!(dst::Array, od::Int, a::Array, oa::Int, n::Int)
    for i = 1:n
        @inbounds dst[od] = scalarmin(dst[od], a[oa])
        od += 1
        oa += 1
    end
end

@code_reducedim maximum scalarmax maximum vcopy! vmax!
@code_reducedim minimum scalarmin minimum vcopy! vmin!



