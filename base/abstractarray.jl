## abstractarray.jl : Generic array interfaces.

## Type aliases for convenience ##

typealias AbstractVector{T} AbstractArray{T,1}
typealias AbstractMatrix{T} AbstractArray{T,2}

typealias Indices{T<:Integer} Union(Integer, AbstractVector{T})
typealias Region Union(Int,Dims)

typealias RangeIndex Union(Int, Range{Int}, Range1{Int})

## Basic functions ##

size{T,n}(t::AbstractArray{T,n}, d) = (d>n ? 1 : size(t)[d])
eltype{T,n}(::AbstractArray{T,n}) = T
eltype{T,n}(::Type{AbstractArray{T,n}}) = T
eltype{T<:AbstractArray}(::Type{T}) = eltype(super(T))
ndims{T,n}(::AbstractArray{T,n}) = n
ndims{T,n}(::Type{AbstractArray{T,n}}) = n
ndims{T<:AbstractArray}(::Type{T}) = ndims(super(T))
length(t::AbstractArray) = prod(size(t))
first(a::AbstractArray) = a[1]
last(a::AbstractArray) = a[end]

function stride(a::AbstractArray, i::Integer)
    s = 1
    if i > ndims(a)
        return numel(a)
    end
    for n=1:(i-1)
        s *= size(a, n)
    end
    return s
end
strides{T}(a::AbstractArray{T,1}) = (1,)
strides{T}(a::AbstractArray{T,2}) = (1, size(a,1))
strides{T}(a::AbstractArray{T,3}) = (1, size(a,1), size(a,1)*size(a,2))
strides   (a::AbstractArray)      = ntuple(ndims(a), i->stride(a,i))

isinteger{T<:Integer}(::AbstractArray{T}) = true
isinteger(::AbstractArray) = false
isreal{T<:Real}(::AbstractArray{T}) = true
isreal(::AbstractArray) = false
iscomplex{T<:Complex}(::AbstractArray{T}) = true
iscomplex(::AbstractArray) = false
isbool(::AbstractArray{Bool}) = true
isbool(::AbstractArray) = false

## Constructors ##

# default arguments to similar()
similar{T}(a::AbstractArray{T})               = similar(a, T, size(a))
similar   (a::AbstractArray, T)               = similar(a, T, size(a))
similar{T}(a::AbstractArray{T}, dims::Dims)   = similar(a, T, dims)
similar{T}(a::AbstractArray{T}, dims::Int...) = similar(a, T, dims)
similar   (a::AbstractArray, T, dims::Int...) = similar(a, T, dims)

function reshape(a::AbstractArray, dims::Dims)
    if prod(dims) != numel(a)
        error("reshape: invalid dimensions")
    end
    b = similar(a, dims)
    for i=1:numel(a)
        b[i] = a[i]
    end
    return b
end
reshape(a::AbstractArray, dims::Int...) = reshape(a, dims)

vec(a::AbstractArray) = reshape(a,max(size(a)))

rowvec{T}(a::AbstractArray{T,2}, i::Int) = vec(a[i,:])
colvec{T}(a::AbstractArray{T,2}, i::Int) = vec(a[:,i])

function squeeze(A::AbstractArray)
    d = ()
    for i = size(A)
        if i != 1
            d = tuple(d..., i)
        end
    end
    reshape(A, d)
end

function fill!(A::AbstractArray, x)
    for i = 1:numel(A)
        A[i] = x
    end
    return A
end

function copy_to(dest::AbstractArray, src)
    i = 1
    for x in src
        dest[i] = x
        i += 1
    end
    return dest
end

copy(a::AbstractArray) = copy_to(similar(a), a)

zero{T}(x::AbstractArray{T}) = fill!(similar(x), zero(T))

## iteration support for arrays as ranges ##

start(a::AbstractArray) = 1
next(a::AbstractArray,i) = (a[i],i+1)
done(a::AbstractArray,i) = (i > numel(a))
isempty(a::AbstractArray) = (numel(a) == 0)

## Conversions ##

int     (x::AbstractArray) = copy_to(similar(x,Int)    , x)
int8    (x::AbstractArray) = copy_to(similar(x,Int8)   , x)
uint8   (x::AbstractArray) = copy_to(similar(x,Uint8)  , x)
int16   (x::AbstractArray) = copy_to(similar(x,Int16)  , x)
uint16  (x::AbstractArray) = copy_to(similar(x,Uint16) , x)
int32   (x::AbstractArray) = copy_to(similar(x,Int32)  , x)
uint32  (x::AbstractArray) = copy_to(similar(x,Uint32) , x)
int64   (x::AbstractArray) = copy_to(similar(x,Int64)  , x)
uint64  (x::AbstractArray) = copy_to(similar(x,Uint64) , x)
integer (x::AbstractArray) = copy_to(similar(x,typeof(integer(one(eltype(x))))), x)
unsigned(x::AbstractArray) = copy_to(similar(x,typeof(unsigned(one(eltype(x))))), x)
char   (x::AbstractArray) = copy_to(similar(x,Char)   , x)
float32(x::AbstractArray) = copy_to(similar(x,Float32), x)
float64(x::AbstractArray) = copy_to(similar(x,Float64), x)
float  (x::AbstractArray) = copy_to(similar(x,typeof(float(one(eltype(x))))), x)

full(x::AbstractArray) = x

## Unary operators ##

conj{T<:Real}(x::AbstractArray{T}) = x
conj!{T<:Real}(x::AbstractArray{T}) = x

real{T<:Real}(x::AbstractArray{T}) = x
imag{T<:Real}(x::AbstractArray{T}) = zero(x)

+{T<:Number}(x::AbstractArray{T}) = x
*{T<:Number}(x::AbstractArray{T}) = x

## Binary arithmetic operators ##

*(A::Number, B::AbstractArray) = A .* B
*(A::AbstractArray, B::Number) = A .* B

/(A::Number, B::AbstractArray) = A ./ B
/(A::AbstractArray, B::Number) = A ./ B

\(A::Number, B::AbstractArray) = B ./ A
\(A::AbstractArray, B::Number) = B ./ A

./(x::AbstractArray, y::AbstractArray ) = throw(MethodError(./, (x,y)))
./(x::Number,y::AbstractArray )         = throw(MethodError(./, (x,y)))
./(x::AbstractArray, y::Number)         = throw(MethodError(./, (x,y)))

.^(x::AbstractArray, y::AbstractArray ) = throw(MethodError(.^, (x,y)))
.^(x::Number,y::AbstractArray )         = throw(MethodError(.^, (x,y)))
.^(x::AbstractArray, y::Number)         = throw(MethodError(.^, (x,y)))

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
    N = length(ranges)
    if !has(cache,N)
        dimargnames = { gensym() for i=1:N }
        ivars = { gensym() for i=1:N }
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
                $make_loop_nest(ivars, dimargnames, body, bodies)
            end
            _F_
        end
        f = eval(fexpr)
        cache[N] = f
    else
        f = cache[N]
    end
    return f(ranges..., exargs...)
end

## Indexing: ref ##

ref(t::AbstractArray, i::Integer) = error("indexing not defined for ", typeof(t))
ref(t::AbstractArray, i::Real) = ref(t, iround(i))
ref(t::AbstractArray, i::Real, j::Real) = ref(t, iround(i), iround(j))
ref(t::AbstractArray, i::Real, j::Real, k::Real) = ref(t, iround(i), iround(j), iround(k))
ref(t::AbstractArray, r::Real...) = ref(t,map(iround,r)...)

# index A[:,:,...,i,:,:,...] where "i" is in dimension "d"
# TODO: more optimized special cases
slicedim(A::AbstractArray, d::Integer, i) =
    A[ntuple(ndims(A), n->(n==d ? i : (1:size(A,n))))...]

function flipdim(A::AbstractArray, d::Integer)
    nd = ndims(A)
    sd = d > nd ? 1 : size(A, d)
    if sd == 1
        return copy(A)
    end
    B = similar(A)
    nnd = 0
    for i = 1:nd
        nnd += int(size(A,i)==1 || i==d)
    end
    if nnd==nd
        # flip along the only non-singleton dimension
        for i = 1:sd
            B[i] = A[sd+1-i]
        end
        return B
    end
    alli = ntuple(nd, n->(n==d ? 0 : (1:size(B,n))))
    local ri
    b_ind = n->(n==d ? ri : alli[n])
    for i = 1:sd
        ri = sd+1-i
        B[ntuple(nd, b_ind)...] = slicedim(A, d, i)
    end
    return B
end

flipud(A::AbstractArray) = flipdim(A, 1)
fliplr(A::AbstractArray) = flipdim(A, 2)

circshift(a, shiftamt::Real) = circshift(a, [integer(shiftamt)])
function circshift(a, shiftamts)
    n = ndims(a)
    I = cell(n)
    for i=1:n
        s = size(a,i)
        d = i<=length(shiftamts) ? shiftamts[i] : 0
        I[i] = d==0 ? (1:s) : mod([-d:s-1-d], s)+1
    end
    a[I...]::typeof(a)
end

## Indexing: assign ##

# 1-d indexing is assumed defined on subtypes
assign(t::AbstractArray, x, i::Integer) =
    error("assign not defined for ",typeof(t))
assign(t::AbstractArray, x) = throw(MethodError(assign, (t, x)))

assign(t::AbstractArray, x, i::Real)          = (t[iround(i)] = x)
assign(t::AbstractArray, x, i::Real, j::Real) = (t[iround(i),iround(j)] = x)
assign(t::AbstractArray, x, i::Real, j::Real, k::Real) =
    (t[iround(i),iround(j),iround(k)] = x)
assign(t::AbstractArray, x, r::Real...)       = (t[map(iround,r)...] = x)

## Concatenation ##

#TODO: ERROR CHECK
cat(catdim::Integer) = Array(None, 0)

vcat() = Array(None, 0)
hcat() = Array(None, 0)

## cat: special cases
hcat{T}(X::T...) = [ X[j] for i=1, j=1:length(X) ]
vcat{T}(X::T...) = [ X[i] for i=1:length(X) ]

function hcat{T}(V::AbstractVector{T}...)
    height = length(V[1])
    for j = 2:length(V)
        if length(V[j]) != height; error("hcat: mismatched dimensions"); end
    end
    [ V[j][i]::T for i=1:length(V[1]), j=1:length(V) ]
end

function vcat{T}(V::AbstractVector{T}...)
    n = 0
    for Vk in V
        n += length(Vk)
    end
    a = similar(full(V[1]), n)
    pos = 1
    for k=1:length(V)
        Vk = V[k]
        for i=1:length(Vk)
            a[pos] = Vk[i]
            pos += 1
        end
    end
    a
end

function hcat{T}(A::Union(AbstractMatrix{T},AbstractVector{T})...)
    nargs = length(A)
    nrows = size(A[1], 1)
    ncols = 0
    dense = true
    for j = 1:nargs
        Aj = A[j]
        dense &= isa(Aj,Array)
        nd = ndims(Aj)
        ncols += (nd==2 ? size(Aj,2) : 1)
        if size(Aj, 1) != nrows; error("hcat: mismatched dimensions"); end
    end
    B = similar(full(A[1]), nrows, ncols)
    pos = 1
    if dense
        for k=1:nargs
            Ak = A[k]
            n = numel(Ak)
            copy_to(B, pos, Ak, 1, n)
            pos += n
        end
    else
        for k=1:nargs
            Ak = A[k]
            p1 = pos+(isa(Ak,AbstractMatrix) ? size(Ak, 2) : 1)-1
            B[:, pos:p1] = Ak
            pos = p1+1
        end
    end
    return B
end

function vcat{T}(A::AbstractMatrix{T}...)
    nargs = length(A)
    nrows = sum(a->size(a, 1), A)::Int
    ncols = size(A[1], 2)
    for j = 2:nargs
        if size(A[j], 2) != ncols; error("vcat: mismatched dimensions"); end
    end
    B = similar(full(A[1]), nrows, ncols)
    pos = 1
    for k=1:nargs
        Ak = A[k]
        p1 = pos+size(Ak,1)-1
        B[pos:p1, :] = Ak
        pos = p1+1
    end
    return B
end

## cat: general case

function cat(catdim::Integer, X...)
    nargs = length(X)
    dimsX = map((a->isa(a,AbstractArray) ? size(a) : (1,)), X)
    ndimsX = map((a->isa(a,AbstractArray) ? ndims(a) : 1), X)
    d_max = max(ndimsX)

    if catdim > d_max + 1
        for i=1:nargs
            if dimsX[1] != dimsX[i]
                error("cat: all inputs must have same dimensions when concatenating along a higher dimension");
            end
        end
    elseif nargs >= 2
        for d=1:d_max
            if d == catdim; continue; end
            len = d <= ndimsX[1] ? dimsX[1][d] : 1
            for i = 2:nargs
                if len != (d <= ndimsX[i] ? dimsX[i][d] : 1)
                    error("cat: dimension mismatch on dimension ", d)
                    #error("lala $d")
                end
            end
        end
    end

    cat_ranges = ntuple(nargs, i->(catdim <= ndimsX[i] ? dimsX[i][catdim] : 1))

    function compute_dims(d)
        if d == catdim
            if catdim <= d_max
                return sum(cat_ranges)
            else
                return nargs
            end
        else
            if d <= ndimsX[1]
                return dimsX[1][d]
            else
                return 1
            end
        end
    end

    ndimsC = max(catdim, d_max)
    dimsC = ntuple(ndimsC, compute_dims)::(Int...)
    typeC = promote_type(map(x->isa(x,AbstractArray) ? eltype(x) : typeof(x), X)...)
    C = similar(isa(X[1],AbstractArray) ? full(X[1]) : [X[1]], typeC, dimsC)

    range = 1
    for k=1:nargs
        nextrange = range+cat_ranges[k]
        cat_one = ntuple(ndimsC, i->(i != catdim ?
                                     (1:dimsC[i]) : (range:nextrange-1) ))
        C[cat_one...] = X[k]
        range = nextrange
    end
    return C
end

vcat(X...) = cat(1, X...)
hcat(X...) = cat(2, X...)

function cat(catdim::Integer, A::AbstractArray...)
    # ndims of all input arrays should be in [d-1, d]

    nargs = length(A)
    dimsA = map(size, A)
    ndimsA = map(ndims, A)
    d_max = max(ndimsA)

    if catdim > d_max + 1
        for i=1:nargs
            if dimsA[1] != dimsA[i]
                error("cat: all inputs must have same dimensions when concatenating along a higher dimension");
            end
        end
    elseif nargs >= 2
        for d=1:d_max
            if d == catdim; continue; end
            len = d <= ndimsA[1] ? dimsA[1][d] : 1
            for i = 2:nargs
                if len != (d <= ndimsA[i] ? dimsA[i][d] : 1)
                    error("cat: dimension mismatch on dimension ", d)
                end
            end
        end
    end

    cat_ranges = ntuple(nargs, i->(catdim <= ndimsA[i] ? dimsA[i][catdim] : 1))

    function compute_dims(d)
        if d == catdim
            if catdim <= d_max
                return sum(cat_ranges)
            else
                return nargs
            end
        else
            if d <= ndimsA[1]
                return dimsA[1][d]
            else
                return 1
            end
        end
    end

    ndimsC = max(catdim, d_max)
    dimsC = ntuple(ndimsC, compute_dims)::(Int...)
    typeC = promote_type(map(eltype, A)...)
    C = similar(full(A[1]), typeC, dimsC)

    range = 1
    for k=1:nargs
        nextrange = range+cat_ranges[k]
        cat_one = ntuple(ndimsC, i->(i != catdim ?
                                     (1:dimsC[i]) : (range:nextrange-1) ))
        C[cat_one...] = A[k]
        range = nextrange
    end
    return C
end

vcat(A::AbstractArray...) = cat(1, A...)
hcat(A::AbstractArray...) = cat(2, A...)

# 2d horizontal and vertical concatenation

function hvcat{T}(rows::(Int...), as::AbstractMatrix{T}...)
    nbr = length(rows)  # number of block rows

    nc = 0
    for i=1:rows[1]
        nc += size(as[i],2)
    end

    nr = 0
    a = 1
    for i = 1:nbr
        nr += size(as[a],1)
        a += rows[i]
    end

    out = similar(full(as[1]), T, nr, nc)

    a = 1
    r = 1
    for i = 1:nbr
        c = 1
        szi = size(as[a],1)
        for j = 1:rows[i]
            Aj = as[a+j-1]
            szj = size(Aj,2)
            if size(Aj,1) != szi
                error("hvcat: mismatched height in block row ", i)
            end
            if c-1+szj > nc
                error("hvcat: block row ", i, " has mismatched number of columns")
            end
            out[r:r-1+szi, c:c-1+szj] = Aj
            c += szj
        end
        if c != nc+1
            error("hvcat: block row ", i, " has mismatched number of columns")
        end
        r += szi
        a += rows[i]
    end
    out
end

hvcat(rows::(Int...)) = []

function hvcat{T<:Number}(rows::(Int...), xs::T...)
    nr = length(rows)
    nc = rows[1]

    a = Array(T, nr, nc)
    k = 1
    for i=1:nr
        if nc != rows[i]
            error("hvcat: row ", i, " has mismatched number of columns")
        end
        for j=1:nc
            a[i,j] = xs[k]
            k += 1
        end
    end
    a
end

function _jl_hvcat_fill(a, xs)
    k = 1
    nr, nc = size(a)
    for i=1:nr
        for j=1:nc
            a[i,j] = xs[k]
            k += 1
        end
    end
    a
end

function hvcat(rows::(Int...), xs::Number...)
    nr = length(rows)
    nc = rows[1]
    #error check
    for i = 2:nr
        if nc != rows[i]
            error("hvcat: row ", i, " has mismatched number of columns")
        end
    end
    T = typeof(xs[1])
    for i=2:length(xs)
        T = promote_type(T,typeof(xs[i]))
    end
    _jl_hvcat_fill(Array(T, nr, nc), xs)
end

## Reductions and scans ##

function isequal(A::AbstractArray, B::AbstractArray)
    if size(A) != size(B)
        return false
    end
    for i = 1:numel(A)
        if !isequal(A[i], B[i])
            return false
        end
    end
    return true
end

function isless(A::AbstractArray, B::AbstractArray)
    nA, nB = numel(A), numel(B)
    for i = 1:min(nA, nB)
        a, b = A[i], B[i]
        if !isequal(a, b)
            return isless(a, b)
        end
    end
    return nA < nB
end

function (==)(A::AbstractArray, B::AbstractArray)
    if size(A) != size(B)
        return false
    end
    for i = 1:numel(A)
        if !(A[i]==B[i])
            return false
        end
    end
    return true
end

function (!=)(A::AbstractArray, B::AbstractArray)
    if size(A) != size(B)
        return true
    end
    for i = 1:numel(A)
        if A[i]!=B[i]
            return true
        end
    end
    return false
end

(<)(A::AbstractArray, B::AbstractArray) =
    error("< not defined for arrays. Try .< or isless.")

(==)(A::AbstractArray, B) = error("Not defined. Try .== or isequal.")

(==)(A, B::AbstractArray) = error("Not defined. Try .== or isequal.")

for (f, op) = ((:cumsum, :+), (:cumprod, :*) )
    @eval function ($f)(v::AbstractVector)
        n = length(v)
        c = similar(v, n)
        if n == 0; return c; end

        c[1] = v[1]
        for i=2:n
           c[i] = ($op)(v[i], c[i-1])
        end
        return c
    end

    @eval function ($f)(A::AbstractArray, axis::Integer)
        dimsA = size(A)
        ndimsA = ndims(A)
        axis_size = dimsA[axis]
        axis_stride = stride(A, axis)

        if axis_size <= 1
            return A
        end

        B = similar(A)

        for i = 1:length(A)
            if div(i-1, axis_stride) % axis_size == 0
               B[i] = A[i]
            else
               B[i] = ($op)(A[i], B[i-axis_stride])
            end
        end

        return B
    end

    @eval ($f)(A::AbstractArray) = ($f)(A, 1)
end

## ipermute in terms of permute ##

function ipermute(A::AbstractArray,perm)
    iperm = Array(Int,length(perm))
    for i = 1:length(perm)
	iperm[perm[i]] = i
    end
    return permute(A,iperm)
end

## Other array functions ##

# fallback definition of hvcat in terms of hcat and vcat
function hvcat(rows::(Int...), as...)
    nbr = length(rows)  # number of block rows
    rs = cell(nbr)
    a = 1
    for i = 1:nbr
        rs[i] = hcat(as[a:a-1+rows[i]]...)
        a += rows[i]
    end
    vcat(rs...)
end

function repmat(a::AbstractMatrix, m::Int, n::Int)
    o,p = size(a)
    b = similar(a, o*m, p*n)
    for j=1:n
        d = (j-1)*p+1
        R = d:d+p-1
        for i=1:m
            c = (i-1)*o+1
            b[c:c+o-1, R] = a
        end
    end
    return b
end
repmat(a::AbstractVector, m::Int, n::Int) = repmat(reshape(a, length(a), 1), m, n)

sub2ind(dims) = 1
sub2ind(dims, i::Integer) = int(i)
sub2ind(dims, i::Integer, j::Integer) = sub2ind(int(i), int(j))
sub2ind(dims, i::Int, j::Int) = (j-1)*dims[1] + i
sub2ind(dims, i0::Integer, i1::Integer, i2::Integer) = sub2ind(int(i0),int(i1),int(i2))
sub2ind(dims, i0::Int, i1::Int, i2::Int) =
    i0 + dims[1]*((i1-1) + dims[2]*(i2-1))
sub2ind(dims, i0::Integer, i1::Integer, i2::Integer, i3::Integer) =
    sub2ind(int(i0),int(i1),int(i2),int(i3))
sub2ind(dims, i0::Int, i1::Int, i2::Int, i3::Int) =
    i0 + dims[1]*((i1-1) + dims[2]*((i2-1) + dims[3]*(i3-1)))

function sub2ind(dims, I::Integer...)
    ndims = length(dims)
    index = int(I[1])
    stride = 1
    for k=2:ndims
        stride = stride * dims[k-1]
        index += (int(I[k])-1) * stride
    end
    return index
end

sub2ind(dims, I::AbstractVector...) =
    [ sub2ind(dims, map(X->X[i], I)...) for i=1:length(I[1]) ]

ind2sub(dims::(Integer...), ind::Integer) = ind2sub(dims, int(ind))
ind2sub(dims::(), ind::Integer) = throw(BoundsError())
ind2sub(dims::(Integer,), ind::Int) = (ind,)
ind2sub(dims::(Integer,Integer), ind::Int) =
    (rem(ind-1,dims[1])+1, div(ind-1,dims[1])+1)
ind2sub(dims::(Integer,Integer,Integer), ind::Int) =
    (rem(ind-1,dims[1])+1, div(rem(ind-1,dims[1]*dims[2]), dims[1])+1,
     div(rem(ind-1,dims[1]*dims[2]*dims[3]), dims[1]*dims[2])+1)

function ind2sub(dims::(Integer,Integer...), ind::Int)
    ndims = length(dims)
    stride = dims[1]
    for i=2:ndims-1
        stride *= dims[i]
    end

    sub = ()
    for i=(ndims-1):-1:1
        rest = rem(ind-1, stride) + 1
        sub = tuple(div(ind - rest, stride) + 1, sub...)
        ind = rest
        stride = div(stride, dims[i])
    end
    return tuple(ind, sub...)
end

## iteration utilities ##

# slow, but useful
function cartesian_map(body, t::(Int...), it...)
    idx = length(t)-length(it)
    if idx == 1
        for i = 1:t[1]
            body(i, it...)
        end
    elseif idx == 2
        for j = 1:t[2]
            for i = 1:t[1]
                body(i, j, it...)
            end
        end
    elseif idx > 1
        for j = 1:t[idx]
            for i = 1:t[idx-1]
                cartesian_map(body, t, i, j, it...)
            end
        end
    else
        throw(ArgumentError("cartesian_map"))
    end
end

cartesian_map(body, t::()) = (body(); nothing)

function cartesian_map(body, t::(Int,))
    for i = 1:t[1]
        body(i)
    end
end

function cartesian_map(body, t::(Int,Int))
    for j = 1:t[2]
        for i = 1:t[1]
            body(i,j)
        end
    end
end

function cartesian_map(body, t::(Int,Int,Int))
    for k = 1:t[3]
        for j = 1:t[2]
            for i = 1:t[1]
                body(i,j,k)
            end
        end
    end
end

function bsxfun(f, a::AbstractArray, b::AbstractArray)
    nd = max(ndims(a),ndims(b))
    shp = Array(Int,nd)
    range = ()
    xa, xb = false, false
    for i=1:nd
        ai, bi = size(a,i), size(b,i)
        if ai == bi
            shp[i] = ai
        elseif ai == 1
            xa = true
            shp[i] = bi
            range = append(range,(bi,))
        elseif bi == 1
            xb = true
            shp[i] = ai
            range = append(range,(ai,))
        else
            error("argument dimensions do not match")
        end
    end
    if isempty(range)
        return f(a, b)
    end
    if numel(a) == 1
        return f(a[1], b)
    elseif numel(b) == 1
        return f(a, b[1])
    end
    c = Array(promote_type(eltype(a),eltype(b)), shp...)

    aidxs = { 1:size(a,i) for i=1:nd }
    bidxs = { 1:size(b,i) for i=1:nd }
    cidxs = { 1:size(c,i) for i=1:nd }

    sliceop = function (idxs::Int...)
        j = 1
        for i = 1:nd
            ai, bi = size(a,i), size(b,i)
            if ai == bi
            elseif ai == 1
                bidxs[i] = idxs[j]
                cidxs[i] = idxs[j]
                j+=1
            else
                aidxs[i] = idxs[j]
                cidxs[i] = idxs[j]
                j+=1
            end
        end
        if xb
            aa = a[aidxs...]; if numel(aa)==1; aa=aa[1]; end
        else
            aa = a
        end
        if xa
            bb = b[bidxs...]; if numel(bb)==1; bb=bb[1]; end
        else
            bb = b
        end
        c[cidxs...] = f(aa, bb)
    end
    cartesian_map(sliceop, range)
    c
end

bsxfun(f, a, b) = f(a, b)
bsxfun(f, a::AbstractArray, b) = f(a, b)
bsxfun(f, a, b::AbstractArray) = f(a, b)
