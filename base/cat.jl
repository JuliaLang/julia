vcat() = Array(Any, 0)
hcat() = Array(Any, 0)
cat(catdim::Integer) = Array(Any, 0)
hvcat(rows::(Int...)) = Array(Any, 0)

# auxiliary cat functions
catrange(catdim, x::AbstractArray) = 1:size(x,catdim)
catrange(catdim, x) = 1

cattype(x::AbstractArray) = eltype(x)
cattype(x) = typeof(x)

stagedfunction vcatshape{T,N}(n::Int,x::AbstractArray{T,N})
    Expr(:tuple,:(n),[:(Int(size(x,$i))) for i=2:N]...)
end
vcatshape(n,x) = (n,)

stagedfunction hcatshape{T,N}(n::Int,x::AbstractArray{T,N})
    Expr(:tuple,:(Int(size(x,1))),:(n),[:(Int(size(x,$i))) for i=3:N]...)
end
hcatshape(n,x) = (1,n)

stagedfunction catshape{T,N}(catdim, n::Int, x::AbstractArray{T,N})
    quote
        if catdim<$N
            $(Expr(:tuple,[:(catdim==$i ? n : Int(size(x,$i))) for i=1:N]...))
        else
            tuple(Int[size(x,i) for i=1:catdim-1]..., n)::Dims
        end
    end
end
catshape(catdim, n, x) = tuple(ones(Int,catdim-1)..., n)::Dims

stagedfunction hvcatshape{T,N}(nr::Int,nc::Int,x::AbstractArray{T,N})
    Expr(:tuple,:(nr),:(nc),[:(Int(size(x,$i))) for i=3:N]...)
end
hvcatshape(nr,nc,x) = (nr, nc)

@inline function vcatsize(X::(Any...,))
    catlength::Int = 0
    N = 1
    y = X[1]
    @inbounds for k = 1:length(X)
        x = X[k]
        if isa(x, AbstractArray)
            catlength += size(x, 1)
            Nk = ndims(x)
            Nk > N && (y = x; N = Nk)
        else
            catlength += 1
        end
    end
    Base.vcatshape(catlength,y)
end
@inline function hcatsize(X::(Any...,))
    catlength::Int = 0
    N = 1
    y = X[1]
    @inbounds for k = 1:length(X)
        x = X[k]
        if isa(x, AbstractArray)
            catlength += size(x, 2)
            Nk = ndims(x)
            Nk > N && (y = x; N = Nk)
        else
            catlength += 1
        end
    end
    hcatshape(catlength,y)
end
@inline function catsize(catdim::Integer, X::(Any...,))
    catlength::Int = 0
    N = 1
    y = X[1]
    @inbounds for k = 1:length(X)
        x = X[k]
        if isa(x, AbstractArray)
            catlength += size(x, catdim)
            Nk = ndims(x)
            Nk > N && (y = x; N = Nk)
        else
            catlength += 1
        end
    end
    catshape(catdim,catlength,y)
end

@inline function hvcatsize(rows::(Integer...), X::(Any...,))
    nb = 0
    @inbounds for i = 1:length(rows)
        nb += rows[i]
    end
    nb == length(X) || throw(ArgumentError("Insufficient number of blocks"))

    nc::Int = 0
    @inbounds for k = 1:rows[1]
        x = X[k]
        if isa(x, AbstractArray)
            nc += size(x, 2)
        else
            nc += 1
        end
    end

    k = 1
    nr::Int = 0
    @inbounds for i = 1:length(rows)
        x = X[k]
        if isa(x, AbstractArray)
            nr += size(x, 1)
        else
            nr += 1
        end
        k += rows[i]
    end

    N = 1
    y = X[1]
    for k = 1:length(X)
        @inbounds x = X[k]
        if isa(x, AbstractArray)
            Nk = ndims(x)
            Nk > N && (y = x; N = Nk)
        end
    end
    hvcatshape(nr, nc, y)
end

cat_eltype(::()) = Any
cat_eltype{T}(X::(T...,)) = T
cat_eltype{T}(X::(AbstractArray{T}...,)) = T
@inline function cat_eltype(X::(Any...,))
    T = cat_eltype((X[1],))
    for k = 2:length(X)
        @inbounds x = X[k]
        T = promote_type(T, cattype(x))
    end
    T
end

cat_containertype(::()) = Array
cat_containertype(::(Any...,)) = Array

cat_container(T, sz, ::Type{Array}) = Array(T, sz)

stagedfunction vcat_fill!{T,N}(C::AbstractArray{T,N}, catrange, x)
    ranges = [:(:) for d=2:N]
    return quote
        $(Expr(:meta,:inline))
        C[catrange,$(ranges...)] = x
    end
end

stagedfunction hcat_fill!{T,N}(C::AbstractArray{T,N}, catrange, x)
    ranges = [:(:) for d=3:N]
    return quote
        $(Expr(:meta,:inline))
        C[:,catrange,$(ranges...)] = x
    end
end

stagedfunction hvcat_fill!{T,N}(C::AbstractArray{T,N}, rowrange, columnrange, x)
    ranges = [:(:) for d=3:N]
    return quote
        $(Expr(:meta,:inline))
        C[rowrange, columnrange, $(ranges...)] = x
    end
end

stagedfunction cat_fill!{T,N}(C::AbstractArray{T,N}, catdim::Integer, catrange, x)
    ranges = [d==N ? :(catrange) : :(1:size(C,$d)) for d=1:N]
    ex = :(C[$(ranges...)] = x)
    for n = N-1:-1:1
        ranges = [d==n ? :(catrange) : :(1:size(C,$d)) for d=1:N]
        ex = Expr(:if,:(catdim==$n),:(C[$(ranges...)] = x),ex)
    end
    return quote
        $(Expr(:meta,:inline))
        $ex
    end
end

## vcat
function typed_vcat(T::Type, X...)
    C = cat_container(T, vcatsize(X), cat_containertype(X))
    offset = 0
    for k = 1:length(X)
        @inbounds x = X[k]
        r = offset+catrange(1, x)
        vcat_fill!(C, r, x)
        offset = last(r)
    end
    C
end

function vcat(X...)
    C = cat_container(cat_eltype(X), vcatsize(X), cat_containertype(X))
    offset = 0
    for k = 1:length(X)
        @inbounds x = X[k]
        r = offset+catrange(1, x)
        vcat_fill!(C, r, x)
        offset = last(r)
    end
    C
end

## hcat
function typed_hcat(T::Type, X...)
    C=cat_container(T, hcatsize(X), cat_containertype(X))
    offset = 0
    for k = 1:length(X)
        @inbounds x = X[k]
        r = offset+catrange(2, x)
        hcat_fill!(C, r, x)
        offset = last(r)
    end
    C
end

function hcat(X...)
    C=cat_container(cat_eltype(X), hcatsize(X), cat_containertype(X))
    offset = 0
    for k = 1:length(X)
        @inbounds x = X[k]
        r = offset+catrange(2, x)
        hcat_fill!(C, r, x)
        offset = last(r)
    end
    C
end

# cat: general case
function cat(catdim::Integer, X...)
    C=cat_container(cat_eltype(X), catsize(catdim, X), cat_containertype(X))
    offset = 0
    for k = 1:length(X)
        @inbounds x = X[k]
        r = offset+catrange(catdim, x)
        cat_fill!(C, catdim, r, x)
        offset = last(r)
    end
    C
end

## hvcat: 2d horizontal and vertical concatenation
function typed_hvcat(T::Type, rows::(Int...), X...)
    C = cat_container(T, hvcatsize(rows,X), cat_containertype(X))
    rowoffset = 0
    k = 1
    for nbc in rows
        rowrange = rowoffset + catrange(1, X[k])
        columnoffset = 0
        for j = 1:nbc
            @inbounds x = X[k]
            columnrange = columnoffset + catrange(2, x)
            hvcat_fill!(C, rowrange, columnrange, x)
            columnoffset = last(columnrange)
            k+=1
        end
        columnoffset == size(C,2) || throw(ArgumentError("block row $(i) has mismatched number of columns"))
        rowoffset = last(rowrange)
    end
    k == length(X)+1 || throw(ArgumentError("mismatched number of blocks"))
    rowoffset == size(C,1) || throw(ArgumentError("mismatched number of rows"))
    C
end

function hvcat(rows::(Int...), X...)
    C = cat_container(cat_eltype(X), hvcatsize(rows,X), cat_containertype(X))
    rowoffset = 0
    k = 1
    for nbc in rows
        rowrange = rowoffset + catrange(1, X[k])
        columnoffset = 0
        for j = 1:nbc
            @inbounds x = X[k]
            columnrange = columnoffset + catrange(2, x)
            hvcat_fill!(C, rowrange, columnrange, x)
            columnoffset = last(columnrange)
            k+=1
        end
        columnoffset == size(C,2) || throw(ArgumentError("block row $(i) has mismatched number of columns"))
        rowoffset = last(rowrange)
    end
    k == length(X)+1 || throw(ArgumentError("mismatched number of blocks"))
    rowoffset == size(C,1) || throw(ArgumentError("mismatched number of rows"))
    C
end

function hvcat(nbc::Integer, A...)
    # nbc = # of block columns
    n = length(A)
    if mod(n,nbc) != 0
        throw(ArgumentError("all rows must have the same number of block columns"))
    end
    nbr = div(n,nbc)
    hvcat(ntuple(i->nbc, nbr), A...)
end

## dcat: cat along diagonals
function dcat(catdims, X...)
    dims = Int[d for d in catdims]
    M = length(dims)
    N = maximum(dims)

    catsizes = zeros(Int, N)
    for k = 1:length(X)
        x = X[k]
        for j = 1:M
            catdim = dims[j]
            catsizes[catdim] += length(catrange(catdim, x))
        end
        isa(x,AbstractArray) && (N = max(N,ndims(x)))
    end
    for d = 1:N
        catsizes[d] == 0 && (catsizes[d]=length(catrange(d, X[1])))
    end
    C=cat_container(cat_eltype(X), tuple(catsizes...), cat_containertype(X))
    M > 1 && fill!(C,0)

    offsets = zeros(Int, M)
    catranges = Array(Union(Int,UnitRange{Int}), N)
    for d = 1:N
        catranges[d] = 1:catsizes[d]
    end
    for k = 1:length(X)
        x=X[k]
        for j=1:M
            catdim = dims[j]
            catranges[catdim] = offsets[j] + catrange(catdim, x)
            offsets[j] = last(catranges[catdim])
        end
        C[catranges...] = x
    end
    C
end

blkdiag(X...) = dcat([1,2], X...)
