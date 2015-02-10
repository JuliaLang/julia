vcat() = Array(Any, 0)
hcat() = Array(Any, 0)
cat(catdim::Integer) = Array(Any, 0)
hvcat(rows::(Int...)) = Array(Any, 0)

# auxiliary cat functions
vcatsize(x::AbstractArray) = size(x)
vcatsize(x) = (1,)

hcatsize(x::AbstractVector) = (length(x),1)
hcatsize(x::AbstractArray) = size(x)
hcatsize(x) = (1,1)

catsize(catdim, x::AbstractArray) = ndims(x)>catdim ? size(x) : size(x, (1:catdim)...)
catsize(catdim, x) = tuple(ones(Int,catdim)...)

catrange(catdim, x::AbstractArray) = 1:size(x,catdim)
catrange(catdim, x) = 1

vcatshape(::()) = (0,)
@inline function vcatshape(X::(AbstractVector...,))
    s1::Int = 0
    for x in X
        s1 += length(x)
    end
    (s1,)
end
@inline function vcatshape(X::(AbstractMatrix...,))
    s1::Int = 0
    for x in X
        s1 += size(x,1)
    end
    s2::Int = size(X[1],2)
    (s1, s2)
end
vcatshape(X::(Number...,)) = (length(X),)
vcatshape{T}(X::(T...,)) = (length(X),)
@inline function vcatshape(X::(Any...,))
    sz::Dims = vcatsize(X[1])
    for k = 2:length(X)
        sz = vcatshape_add(sz, vcatsize(X[k]))
    end
    sz
end

stagedfunction vcatshape_add{N1,N2}(sz::NTuple{N1,Int}, sz2::NTuple{N2,Int})
    newsize = Expr(:tuple, :(sz[1]+sz2[1]), [:(sz[$i]) for i=2:N1]..., [1 for i=N1+1:N2]...)
    quote
        $(Expr(:meta,:inline))
        $newsize
    end
end

hcatshape(::()) = (1,0)
hcatshape(X::(Number...,)) = (1,length(X))
hcatshape{T}(X::(T...,)) = (1,length(X))
hcatshape(X::(AbstractVector...,)) = (length(X[1]), length(X))
@inline function hcatshape(X::(AbstractVecOrMat...,))
    s2::Int = 0
    for x in X
        s2 += size(x,2)
    end
    s1::Int = size(X[1],1)
    (s1, s2)
end
@inline function hcatshape(X::(Any...))
    sz::Dims = hcatsize(X[1])
    for k = 2:length(X)
        sz = hcatshape_add(sz, hcatsize(X[k]))
    end
    sz
end

stagedfunction hcatshape_add{N1,N2}(sz::NTuple{N1,Int}, sz2::NTuple{N2, Int})
    newsize = Expr(:tuple, :(sz[1]), :(sz[2]+sz2[2]), [:(sz[$i]) for i=3:N1]..., [1 for i=N1+1:N2]...)
    quote
        $(Expr(:meta,:inline))
        $newsize
    end
end

catshape(catdim, ::()) = tuple(ones(Int,catdim-1)...,0)
catshape(catdim, X::(Number...,)) = tuple(ones(Int,catdim-1)...,length(X))
@inline function catshape(catdim, X::(Any...))
    sz::Dims = catsize(catdim, X[1])
    for k = 2:length(X)
        sz = catshape_add(catdim, sz, catsize(catdim, X[k]))
    end
    sz
end

stagedfunction catshape_add{N1,N2}(catdim, sz::NTuple{N1,Int}, sz2::NTuple{N2,Int})
    newsize = Expr(:tuple, [:($i==catdim ? (sz[$i]+sz2[$i]) : sz[$i]) for i=1:N1]..., [1 for i=N1+1:N2]...)
    quote
        $(Expr(:meta,:inline))
        $newsize
    end
end

hvcatshape(rows::(Integer...),::()) = (length(rows),0)
hvcatshape(rows::(Integer...), X::(Number...,)) = (length(rows), rows[1])
hvcatshape{T}(rows::(Integer...), X::(T...,)) = (length(rows), rows[1])
@inline function hvcatshape(rows::(Integer...), X::(AbstractVecOrMat...,))
    nc::Int = 0
    for k = 1:rows[1]
        nc += size(X[k], 2)
    end
    k = 1
    nr::Int = 0
    for i = 1:length(rows)
        nr += size(X[k], 1)
        k += rows[i]
    end
    return (nr, nc)
end
@inline function hvcatshape(rows::(Integer...), X::(Any...))
    k=1
    rowsz::Dims = hcatsize(X[k])
    k+=1
    for j = 2:rows[1]
        rowsz = hcatshape_add(rowsz, hcatsize(X[k]))
        k+=1
    end
    sz::Dims = rowsz
    for i = 2:length(rows)
        rowsz = hcatsize(X[k])
        k+=1
        for j = 2:rows[i]
            rowsz = hcatshape_add(rowsz, hcatsize(X[k]))
            k+=1
        end
        sz = vcatshape_add(sz, rowsz)
    end
    sz
end

cat_eltypeof{T}(::(T...,)) = T
cat_eltypeof{T}(::(AbstractArray{T}...,)) = T
@inline function cat_eltypeof(X::(Any...,))
    T = cat_eltypeof((X[1],))
    for k = 2:length(X)
        T = promote_type(T, cat_eltypeof((X[k],)))
    end
    T
end

cat_containertypeof(::()) = Array
cat_containertypeof(::(Any...,)) = Array

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
    C = cat_container(T, vcatshape(X), cat_containertypeof(X))
    offset = 0
    for i = 1:length(X)
        x = X[i]
        r = offset+catrange(1, x)
        vcat_fill!(C, r, x)
        offset = last(r)
    end
    C
end

function vcat(X...)
    C = cat_container(cat_eltypeof(X), vcatshape(X), cat_containertypeof(X))
    offset = 0
    for i = 1:length(X)
        x = X[i]
        r = offset+catrange(1, x)
        vcat_fill!(C, r, x)
        offset = last(r)
    end
    C
end

## hcat
function typed_hcat(T::Type, X...)
    C=cat_container(T, hcatshape(X), cat_containertypeof(X))
    offset = 0
    for i = 1:length(X)
        x = X[i]
        r = offset+catrange(2, x)
        hcat_fill!(C, r, x)
        offset = last(r)
    end
    C
end

function hcat(X...)
    C=cat_container(cat_eltypeof(X), hcatshape(X), cat_containertypeof(X))
    offset = 0
    for i = 1:length(X)
        x = X[i]
        r = offset+catrange(2, x)
        hcat_fill!(C, r, x)
        offset = last(r)
    end
    C
end

# cat: general case
function cat(catdim::Integer, X...)
    C=cat_container(cat_eltypeof(X), catshape(catdim, X), cat_containertypeof(X))
    offset = 0
    for i = 1:length(X)
        x = X[i]
        r = offset+catrange(catdim, x)
        cat_fill!(C, catdim, r, x)
        offset = last(r)
    end
    C
end

## hvcat: 2d horizontal and vertical concatenation
function typed_hvcat(T::Type, rows::(Int...), X...)
    C = cat_container(T, hvcatshape(rows,X), cat_containertypeof(X))
    rowoffset = 0
    k = 1
    for nbc in rows
        rowrange = rowoffset + catrange(1, X[k])
        columnoffset = 0
        for j = 1:nbc
            x = X[k]
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
    C = cat_container(cat_eltypeof(X), hvcatshape(rows,X), cat_containertypeof(X))
    rowoffset = 0
    k = 1
    for nbc in rows
        rowrange = rowoffset + catrange(1, X[k])
        columnoffset = 0
        for j = 1:nbc
            x = X[k]
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
    C=cat_container(cat_eltypeof(X), tuple(catsizes...), cat_containertypeof(X))
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
