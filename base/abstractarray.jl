## Type aliases for convenience ##

typealias AbstractVector{T} AbstractArray{T,1}
typealias AbstractMatrix{T} AbstractArray{T,2}

## Basic functions ##

size{T,n}(t::AbstractArray{T,n}, d) = (d>n ? 1 : size(t)[d])
eltype(x) = Any
eltype{T,n}(::AbstractArray{T,n}) = T
eltype{T,n}(::Type{AbstractArray{T,n}}) = T
eltype{T<:AbstractArray}(::Type{T}) = eltype(super(T))
iseltype(x,T) = eltype(x) <: T
isinteger(x::AbstractArray) = all(isinteger,x)
isinteger{T<:Integer,n}(x::AbstractArray{T,n}) = true
isreal(x::AbstractArray) = all(isreal,x)
isreal{T<:Real,n}(x::AbstractArray{T,n}) = true
ndims{T,n}(::AbstractArray{T,n}) = n
ndims{T,n}(::Type{AbstractArray{T,n}}) = n
ndims{T<:AbstractArray}(::Type{T}) = ndims(super(T))
length(t::AbstractArray) = prod(size(t))::Int
endof(a::AbstractArray) = length(a)
first(a::AbstractArray) = a[1]
first(a) = next(a,start(a))[1]
last(a) = a[end]

function stride(a::AbstractArray, i::Integer)
    if i > ndims(a)
        return length(a)
    end
    s = 1
    for n=1:(i-1)
        s *= size(a, n)
    end
    return s
end

strides(a::AbstractArray) = ntuple(ndims(a), i->stride(a,i))::Dims

function isassigned(a::AbstractArray, i::Int...)
    # TODO
    try
        a[i...]
        true
    catch
        false
    end
end

# used to compute "end" for last index
function trailingsize(A, n)
    s = 1
    for i=n:ndims(A)
        s *= size(A,i)
    end
    return s
end

## Bounds checking ##
function checkbounds(sz::Int, I::Real)
    I = to_index(I)
    if I < 1 || I > sz
        throw(BoundsError())
    end
end

function checkbounds(sz::Int, I::AbstractVector{Bool})
    if length(I) > sz
        throw(BoundsError())
    end
end

function checkbounds{T<:Integer}(sz::Int, I::Ranges{T})
    if !isempty(I) && (minimum(I) < 1 || maximum(I) > sz)
        throw(BoundsError())
    end
end

function checkbounds{T <: Real}(sz::Int, I::AbstractArray{T})
    for i in I
        i = to_index(i)
        if i < 1 || i > sz
            throw(BoundsError())
        end
    end
end

function checkbounds(A::AbstractArray, I::AbstractArray{Bool})
    if !isequal(size(A), size(I)) throw(BoundsError()) end
end

checkbounds(A::AbstractArray, I) = checkbounds(length(A), I)

function checkbounds(A::AbstractMatrix, I, J)
    checkbounds(size(A,1), I)
    checkbounds(size(A,2), J)
end

function checkbounds(A::AbstractArray, I, J)
    checkbounds(size(A,1), I)
    sz = size(A,2)
    for i = 3:ndims(A)
        sz *= size(A, i) # TODO: sync. with decision on issue #1030
    end
    checkbounds(sz, J)
end

function checkbounds(A::AbstractArray, I::Union(Real,AbstractArray)...)
    n = length(I)
    if n > 0
        for dim = 1:(n-1)
            checkbounds(size(A,dim), I[dim])
        end
        sz = size(A,n)
        for i = n+1:ndims(A)
            sz *= size(A,i)     # TODO: sync. with decision on issue #1030
        end
        checkbounds(sz, I[n])
    end
end

## Bounds-checking without errors ##
in_bounds(l::Int, i::Integer) = 1 <= i <= l
function in_bounds(sz::Dims, I::Int...)
    n = length(I)
    for dim = 1:(n-1)
        if !(1 <= I[dim] <= sz[dim])
            return false
        end
    end
    s = sz[n]
    for i = n+1:length(sz)
        s *= sz[i]
    end
    1 <= I[n] <= s
end

## Constructors ##

# default arguments to similar()
similar{T}(a::AbstractArray{T})               = similar(a, T, size(a))
similar   (a::AbstractArray, T)               = similar(a, T, size(a))
similar{T}(a::AbstractArray{T}, dims::Dims)   = similar(a, T, dims)
similar{T}(a::AbstractArray{T}, dims::Int...) = similar(a, T, dims)
similar   (a::AbstractArray, T, dims::Int...) = similar(a, T, dims)

function reshape(a::AbstractArray, dims::Dims)
    if prod(dims) != length(a)
        error("reshape: invalid dimensions")
    end
    copy!(similar(a, dims), a)
end
reshape(a::AbstractArray, dims::Int...) = reshape(a, dims)

vec(a::AbstractArray) = reshape(a,length(a))
vec(a::AbstractVector) = a

function squeeze(A::AbstractArray, dims)
    d = ()
    for i in 1:ndims(A)
        if in(i,dims)
            if size(A,i) != 1
                error("squeezed dims must all be size 1")
            end
        else
            d = tuple(d..., size(A,i))
        end
    end
    reshape(A, d)
end

function fill!(A::AbstractArray, x)
    for i = 1:length(A)
        A[i] = x
    end
    return A
end

function copy!(dest::AbstractArray, src)
    i = 1
    for x in src
        dest[i] = x
        i += 1
    end
    return dest
end

# copy with minimal requirements on src
# if src is not an AbstractArray, moving to the offset might be O(n)
function copy!(dest::AbstractArray, doffs::Integer, src, soffs::Integer=1)
    st = start(src)
    for j = 1:(soffs-1)
        _, st = next(src, st)
    end
    i = doffs
    while !done(src,st)
        val, st = next(src, st)
        dest[i] = val
        i += 1
    end
    return dest
end

# NOTE: this is to avoid ambiguity with the deprecation of
#   copy!(dest::AbstractArray, src, doffs::Integer)
# Remove this when that deprecation is removed.
function copy!(dest::AbstractArray, doffs::Integer, src::Integer)
    dest[doffs] = src
    return dest
end

# this method must be separate from the above since src might not have a length
function copy!(dest::AbstractArray, doffs::Integer, src, soffs::Integer, n::Integer)
    n == 0 && return dest
    st = start(src)
    for j = 1:(soffs-1)
        _, st = next(src, st)
    end
    for i = doffs:(doffs+n-1)
        done(src,st) && throw(BoundsError())
        val, st = next(src, st)
        dest[i] = val
    end
    return dest
end

# if src is an AbstractArray and a source offset is passed, use indexing
function copy!(dest::AbstractArray, doffs::Integer, src::AbstractArray, soffs::Integer, n::Integer=length(src))
    for i = 0:(n-1)
        dest[doffs+i] = src[soffs+i]
    end
    return dest
end

copy(a::AbstractArray) = copy!(similar(a), a)
copy(a::AbstractArray{None}) = a # cannot be assigned into so is immutable

zero{T}(x::AbstractArray{T}) = fill!(similar(x), zero(T))

## iteration support for arrays as ranges ##

start(a::AbstractArray) = 1
next(a::AbstractArray,i) = (a[i],i+1)
done(a::AbstractArray,i) = (i > length(a))
isempty(a::AbstractArray) = (length(a) == 0)

## Conversions ##

for (f,t) in ((:char,   Char),
              (:int,    Int),
              (:int8,   Int8),
              (:int16,  Int16),
              (:int32,  Int32),
              (:int64,  Int64),
              (:int128, Int128),
              (:uint,   Uint),
              (:uint8,  Uint8),
              (:uint16, Uint16),
              (:uint32, Uint32),
              (:uint64, Uint64),
              (:uint128,Uint128))
    @eval begin
        ($f)(x::AbstractArray{$t}) = x

        function ($f)(x::AbstractArray)
            y = similar(x,$t)
            i = 1
            for e in x
                y[i] = ($f)(e)
                i += 1
            end
            y
        end
    end
end

for (f,t) in ((:integer, Integer),
              (:unsigned, Unsigned))
    @eval begin
        ($f){T<:$t}(x::AbstractArray{T}) = x

        function ($f)(x::AbstractArray)
            y = similar(x,typeof(($f)(one(eltype(x)))))
            i = 1
            for e in x
                y[i] = ($f)(e)
                i += 1
            end
            y
        end
    end
end

bool(x::AbstractArray{Bool}) = x
bool(x::AbstractArray) = copy!(similar(x,Bool), x)

for (f,t) in ((:float16,    Float16),
              (:float32,    Float32),
              (:float64,    Float64),
              (:complex64,  Complex64),
              (:complex128, Complex128))
    @eval ($f)(x::AbstractArray{$t}) = x
    @eval ($f)(x::AbstractArray) = copy!(similar(x,$t), x)
end

float{T<:FloatingPoint}(x::AbstractArray{T}) = x
complex{T<:Complex}(x::AbstractArray{T}) = x

float   (x::AbstractArray) = copy!(similar(x,typeof(float(one(eltype(x))))), x)
complex (x::AbstractArray) = copy!(similar(x,typeof(complex(one(eltype(x))))), x)

dense(x::AbstractArray) = x
full(x::AbstractArray) = x

## range conversions ##

for fn in _numeric_conversion_func_names
    @eval begin
        $fn(r::Range ) = Range($fn(r.start), $fn(r.step), r.len)
        $fn(r::Range1) = Range1($fn(r.start), r.len)
    end
end

## Unary operators ##

conj{T<:Real}(x::AbstractArray{T}) = x
conj!{T<:Real}(x::AbstractArray{T}) = x

real{T<:Real}(x::AbstractVector{T}) = x
real{T<:Real}(x::AbstractArray{T}) = x
imag{T<:Real}(x::AbstractVector{T}) = zero(x)
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
    if ranges === ()
        ranges = (1,)
    end
    N = length(ranges)
    if !haskey(cache,N)
        if isdefined(genbodies,:code)
            mod = genbodies.code.module
        else
            mod = Main
        end
        dimargnames = { symbol(string("_d",i)) for i=1:N }
        ivars = { symbol(string("_i",i)) for i=1:N }
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
                $(make_loop_nest(ivars, dimargnames, body, bodies))
            end
            _F_
        end
        f = eval(mod,fexpr)
        cache[N] = f
    else
        f = cache[N]
    end
    return f(ranges..., exargs...)
end


# Generate function bodies which look like this (example for a 3d array):
#    offset3 = 0
#    stride1 = 1
#    stride2 = stride1 * size(A,1)
#    stride3 = stride2 * size(A,2)
#    for i3 = ind3
#        offset2 = offset3 + (i3-1)*stride3
#        for i2 = ind2
#            offset1 = offset2 + (i2-1)*stride2
#            for i1 = ind1
#                linearind = offset1 + i1
#                <A function, "body", of linearind>
#            end
#        end
#    end
function make_arrayind_loop_nest(loopvars, offsetvars, stridevars, linearind, ranges, body, arrayname)
    # Initialize: calculate the strides
    offset = offsetvars[end]
    s = stridevars[1]
    exinit = quote
        $offset = 0
        $s = 1
    end
    for i = 2:length(ranges)
        sprev = s
        s = stridevars[i]
        exinit = quote
            $exinit
            $s = $sprev * size($arrayname, $i-1)
        end
    end
    # Build the innermost loop (iterating over the first index)
    v = loopvars[1]
    r = ranges[1]
    offset = offsetvars[1]
    exloop = quote
        for ($v) = ($r)
            $linearind = $offset + $v
            $body
        end
    end
    # Build the remaining loops
    for i = 2:length(ranges)
        v = loopvars[i]
        r = ranges[i]
        offset = offsetvars[i-1]
        offsetprev = offsetvars[i]
        s = stridevars[i]
        exloop = quote
            for ($v) = ($r)
                $offset = $offsetprev + ($v - 1) * $s
                $exloop
            end
        end
    end
    # Return the combined result
    return quote
        $exinit
        $exloop
    end
end

# Like gen_cartesian_map, except it builds a function creating a
# loop nest that computes a single linear index (instead of a
# multidimensional index).
# Important differences:
#   - genbody is a scalar-valued function of a single scalar argument,
#     the linear index. In gen_cartesian_map, this function can return
#     an array to specify "pre-loop" and "post-loop" operations, but
#     here those are handled explicitly in make_arrayind_loop_nest.
#   - exargnames[1] must be the array for which the linear index is
#     being created (it is used to calculate the strides, which in
#     turn are used for computing the linear index)
function gen_array_index_map(cache, genbody, ranges, exargnames, exargs...)
    N = length(ranges)
    if !haskey(cache,N)
        dimargnames = { symbol(string("_d",i)) for i=1:N }
        loopvars = { symbol(string("_l",i)) for i=1:N }
        offsetvars = { symbol(string("_offs",i)) for i=1:N }
        stridevars = { symbol(string("_stri",i)) for i=1:N }
        linearind = :_li
        body = genbody(linearind)
        fexpr = quote
            local _F_
            function _F_($(dimargnames...), $(exargnames...))
                $(make_arrayind_loop_nest(loopvars, offsetvars, stridevars, linearind, dimargnames, body, exargnames[1]))
            end
            return _F_
        end
        f = eval(fexpr)
        cache[N] = f
    else
        f = cache[N]
    end
    return f(ranges..., exargs...)
end




## Indexing: getindex ##

getindex(t::AbstractArray, i::Real) = error("indexing not defined for ", typeof(t))

# linear indexing with a single multi-dimensional index
function getindex(A::AbstractArray, I::AbstractArray)
    x = similar(A, size(I))
    for i=1:length(I)
        x[i] = A[I[i]]
    end
    return x
end

# index A[:,:,...,i,:,:,...] where "i" is in dimension "d"
# TODO: more optimized special cases
slicedim(A::AbstractArray, d::Integer, i) =
    A[[ n==d ? i : (1:size(A,n)) for n in 1:ndims(A) ]...]

function reverse(A::AbstractVector, s=1, n=length(A))
    B = similar(A)
    for i = 1:s-1
        B[i] = A[i]
    end
    for i = s:n
        B[i] = A[n+s-i]
    end
    for i = n+1:length(A)
        B[i] = A[i]
    end
    B
end

function flipdim(A::AbstractVector, d::Integer)
    d > 0 || error("dimension out of range")
    d == 1 || return copy(A)
    reverse(A)
end

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
    alli = [ 1:size(B,n) for n in 1:nd ]
    for i = 1:sd
        B[[ n==d ? sd+1-i : alli[n] for n in 1:nd ]...] = slicedim(A, d, i)
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

## Indexing: setindex! ##

# 1-d indexing is assumed defined on subtypes
setindex!(t::AbstractArray, x, i::Real) =
    error("setindex! not defined for ",typeof(t))
setindex!(t::AbstractArray, x) = throw(MethodError(setindex!, (t, x)))

## Indexing: handle more indices than dimensions if "extra" indices are 1

# Don't require vector/matrix subclasses to implement more than 1/2 indices,
# respectively, by handling the extra dimensions in AbstractMatrix.

function getindex(A::AbstractVector, i1,i2,i3...)
    if i2*prod(i3) != 1
        throw(BoundsError())
    end
    A[i1]
end
function getindex(A::AbstractMatrix, i1,i2,i3,i4...)
    if i3*prod(i4) != 1
        throw(BoundsError())
    end
    A[i1,i2]
end

function setindex!(A::AbstractVector, x, i1,i2,i3...)
    if i2*prod(i3) != 1
        throw(BoundsError())
    end
    A[i1] = x
end
function setindex!(A::AbstractMatrix, x, i1,i2,i3,i4...)
    if i3*prod(i4) != 1
        throw(BoundsError())
    end
    A[i1,i2] = x
end

## get (getindex with a default value) ##

typealias RangeVecIntList{A<:AbstractVector{Int}} Union((Union(Ranges, AbstractVector{Int})...), AbstractVector{Range1{Int}}, AbstractVector{Range{Int}}, AbstractVector{A})

get(A::AbstractArray, i::Integer, default) = in_bounds(length(A), i) ? A[i] : default
get(A::AbstractArray, I::(), default) = similar(A, typeof(default), 0)
get(A::AbstractArray, I::Dims, default) = in_bounds(size(A), I...) ? A[I...] : default

function get!{T}(X::AbstractArray{T}, A::AbstractArray, I::Union(Ranges, AbstractVector{Int}), default::T)
    ind = findin(I, 1:length(A))
    X[ind] = A[I[ind]]
    X[1:first(ind)-1] = default
    X[last(ind)+1:length(X)] = default
    X
end

get(A::AbstractArray, I::Ranges, default) = get!(similar(A, typeof(default), length(I)), A, I, default)

function get!{T}(X::AbstractArray{T}, A::AbstractArray, I::RangeVecIntList, default::T)
    fill!(X, default)
    dst, src = indcopy(size(A), I)
    X[dst...] = A[src...]
    X
end

get(A::AbstractArray, I::RangeVecIntList, default) = get!(similar(A, typeof(default), map(length, I)...), A, I, default)


## Concatenation ##

#TODO: ERROR CHECK
cat(catdim::Integer) = Array(None, 0)

vcat() = Array(None, 0)
hcat() = Array(None, 0)

## cat: special cases
hcat{T}(X::T...)         = T[ X[j] for i=1, j=1:length(X) ]
hcat{T<:Number}(X::T...) = T[ X[j] for i=1, j=1:length(X) ]
vcat{T}(X::T...)         = T[ X[i] for i=1:length(X) ]
vcat{T<:Number}(X::T...) = T[ X[i] for i=1:length(X) ]

function vcat(X::Number...)
    T = None
    for x in X
        T = promote_type(T,typeof(x))
    end
    hvcat_fill(Array(T,length(X)), X)
end

function hcat(X::Number...)
    T = None
    for x in X
        T = promote_type(T,typeof(x))
    end
    hvcat_fill(Array(T,1,length(X)), X)
end

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
            n = length(Ak)
            copy!(B, pos, Ak, 1, n)
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
    d_max = maximum(ndimsX)

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

    cat_ranges = [ catdim <= ndimsX[i] ? dimsX[i][catdim] : 1 for i=1:nargs ]

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
        cat_one = [ i != catdim ? (1:dimsC[i]) : (range:nextrange-1) for i=1:ndimsC ]
        C[cat_one...] = X[k]
        range = nextrange
    end
    return C
end

vcat(X...) = cat(1, X...)
hcat(X...) = cat(2, X...)

cat{T}(catdim::Integer, A::AbstractArray{T}...) = cat_t(catdim, T, A...)

cat(catdim::Integer, A::AbstractArray...) =
    cat_t(catdim, promote_type(map(eltype, A)...), A...)

function cat_t(catdim::Integer, typeC, A::AbstractArray...)
    # ndims of all input arrays should be in [d-1, d]

    nargs = length(A)
    dimsA = map(size, A)
    ndimsA = map(ndims, A)
    d_max = maximum(ndimsA)

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

    cat_ranges = [ catdim <= ndimsA[i] ? dimsA[i][catdim] : 1 for i=1:nargs ]

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
    C = similar(full(A[1]), typeC, dimsC)

    range = 1
    for k=1:nargs
        nextrange = range+cat_ranges[k]
        cat_one = [ i != catdim ? (1:dimsC[i]) : (range:nextrange-1) for i=1:ndimsC ]
        C[cat_one...] = A[k]
        range = nextrange
    end
    return C
end

vcat(A::AbstractArray...) = cat(1, A...)
hcat(A::AbstractArray...) = cat(2, A...)

# 2d horizontal and vertical concatenation

function hvcat(nbc::Integer, as...)
    # nbc = # of block columns
    n = length(as)
    if mod(n,nbc) != 0
        error("hvcat: not all rows have the same number of block columns")
    end
    nbr = div(n,nbc)
    hvcat(ntuple(nbr, i->nbc), as...)
end

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

function hvcat_fill(a, xs)
    k = 1
    nr, nc = size(a,1), size(a,2)
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
    hvcat_fill(Array(T, nr, nc), xs)
end

## Reductions and scans ##

function isequal(A::AbstractArray, B::AbstractArray)
    if A === B return true end
    if size(A) != size(B)
        return false
    end
    for i = 1:length(A)
        if !isequal(A[i], B[i])
            return false
        end
    end
    return true
end

function lexcmp(A::AbstractArray, B::AbstractArray)
    nA, nB = length(A), length(B)
    for i = 1:min(nA, nB)
        a, b = A[i], B[i]
        if !isequal(a, b)
            return isless(a, b) ? -1 : +1
        end
    end
    return cmp(nA, nB)
end

function (==)(A::AbstractArray, B::AbstractArray)
    if size(A) != size(B)
        return false
    end
    for i = 1:length(A)
        if !(A[i]==B[i])
            return false
        end
    end
    return true
end

_cumsum_type{T<:Number}(v::AbstractArray{T}) = typeof(+zero(T))
_cumsum_type(v) = typeof(v[1]+v[1])

for (f, fp, op) = ((:cumsum, :cumsum_pairwise, :+),
                   (:cumprod, :cumprod_pairwise, :*) )
    # in-place cumsum of c = s+v(i1:n), using pairwise summation as for sum
    @eval function ($fp)(v::AbstractVector, c::AbstractVector, s, i1, n)
        if n < 128
            @inbounds c[i1] = ($op)(s, v[i1])
            for i = i1+1:i1+n-1
                @inbounds c[i] = $(op)(c[i-1], v[i])
            end
        else
            n2 = div(n,2)
            ($fp)(v, c, s, i1, n2)
            ($fp)(v, c, c[(i1+n2)-1], i1+n2, n-n2)
        end
    end

    @eval function ($f)(v::AbstractVector)
        n = length(v)
        c = $(op===:+ ? (:(similar(v,_cumsum_type(v)))) :
                        (:(similar(v))))
        if n == 0; return c; end
        ($fp)(v, c, $(op==:+ ? :(zero(v[1])) : :(one(v[1]))), 1, n)
        return c
    end

    @eval function ($f)(A::AbstractArray, axis::Integer)
        dimsA = size(A)
        ndimsA = ndims(A)
        axis_size = dimsA[axis]
        axis_stride = 1
        for i = 1:(axis-1)
            axis_stride *= size(A,i)
        end

        B = $(op===:+ ? (:(similar(A,_cumsum_type(A)))) :
                        (:(similar(A))))

        if axis_size < 1
            return B
        end

        for i = 1:length(A)
            if div(i-1, axis_stride) % axis_size == 0
               B[i] = A[i]
            else
               B[i] = ($op)(B[i-axis_stride], A[i])
            end
        end

        return B
    end

    @eval ($f)(A::AbstractArray) = ($f)(A, 1)
end

for (f, op) = ((:cummin, :min), (:cummax, :max))
    @eval function ($f)(v::AbstractVector)
        n = length(v)
        cur_val = v[1]
        res = similar(v, n)
        res[1] = cur_val
        for i in 2:n
            cur_val = ($op)(v[i], cur_val)
            res[i] = cur_val
        end
        return res
    end

    @eval function ($f)(A::AbstractArray, axis::Integer)
        dimsA = size(A)
        ndimsA = ndims(A)
        axis_size = dimsA[axis]
        axis_stride = 1
        for i = 1:(axis-1)
            axis_stride *= size(A,i)
        end

        if axis_size < 1
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

## ipermutedims in terms of permutedims ##

function ipermutedims(A::AbstractArray,perm)
    iperm = Array(Int,length(perm))
    for i = 1:length(perm)
	iperm[perm[i]] = i
    end
    return permutedims(A,iperm)
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

function repmat(a::Union(AbstractVector,AbstractMatrix), m::Int, n::Int=1)
    o, p = size(a,1), size(a,2)
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

function repmat(a::AbstractVector, m::Int)
    o = length(a)
    b = similar(a, o*m)
    for i=1:m
        c = (i-1)*o+1
        b[c:c+o-1] = a
    end
    return b
end

sub2ind(dims) = 1
sub2ind(dims, i::Integer) = int(i)
sub2ind(dims, i::Integer, j::Integer) = sub2ind(dims, int(i), int(j))
sub2ind(dims, i::Int, j::Int) = (j-1)*dims[1] + i
sub2ind(dims, i0::Integer, i1::Integer, i2::Integer) = sub2ind(dims, int(i0),int(i1),int(i2))
sub2ind(dims, i0::Int, i1::Int, i2::Int) =
    i0 + dims[1]*((i1-1) + dims[2]*(i2-1))
sub2ind(dims, i0::Integer, i1::Integer, i2::Integer, i3::Integer) =
    sub2ind(dims, int(i0),int(i1),int(i2),int(i3))
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

function sub2ind{T<:Integer}(dims::Array{T}, sub::Array{T})
    ndims = length(dims)
    ind = sub[1]
    stride = 1
    for k in 2:ndims
        stride = stride * dims[k - 1]
        ind += (sub[k] - 1) * stride
    end
    return ind
end

sub2ind{T<:Integer}(dims, I::AbstractVector{T}...) =
    [ sub2ind(dims, map(X->X[i], I)...)::Int for i=1:length(I[1]) ]

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

ind2sub(dims::(Integer...), ind::Integer) = ind2sub(dims, int(ind))
ind2sub(dims::(), ind::Integer) = ind==1 ? () : throw(BoundsError())
ind2sub(dims::(Integer,), ind::Int) = (ind,)
ind2sub(dims::(Integer,Integer), ind::Int) =
    (rem(ind-1,dims[1])+1, div(ind-1,dims[1])+1)
ind2sub(dims::(Integer,Integer,Integer), ind::Int) =
    (rem(ind-1,dims[1])+1, div(rem(ind-1,dims[1]*dims[2]), dims[1])+1,
     div(rem(ind-1,dims[1]*dims[2]*dims[3]), dims[1]*dims[2])+1)

function ind2sub{T<:Integer}(dims::(Integer,Integer...), ind::AbstractVector{T})
    n = length(dims)
    l = length(ind)
    t = ntuple(n, x->Array(Int, l))
    for i = 1:l
        s = ind2sub(dims, ind[i])
        for j = 1:n
            t[j][i] = s[j]
        end
    end
    return t
end

function ind2sub!{T<:Integer}(sub::Array{T}, dims::Array{T}, ind::T)
    ndims = length(dims)
    stride = dims[1]
    for i in 2:(ndims - 1)
        stride *= dims[i]
    end
    for i in (ndims - 1):-1:1
        rest = rem1(ind, stride)
        sub[i + 1] = div(ind - rest, stride) + 1
        ind = rest
        stride = div(stride, dims[i])
    end
    sub[1] = ind
    return
end

indices(I) = I
indices(I::Int) = I
indices(I::Real) = convert(Int, I)
indices(I::AbstractArray{Bool,1}) = find(I)
indices(I::(Any,))            = (indices(I[1]), )
indices(I::(Any,Any,))        = (indices(I[1]), indices(I[2]))
indices(I::(Any,Any,Any))     = (indices(I[1]), indices(I[2]), indices(I[3]))
indices(I::(Any,Any,Any,Any)) = (indices(I[1]), indices(I[2]), indices(I[3]), indices(I[4]))
indices(I::Tuple) = map(indices, I)

# Generalized repmat
function repeat{T}(A::Array{T};
                   inner::Array{Int} = ones(Int, ndims(A)),
                   outer::Array{Int} = ones(Int, ndims(A)))
    ndims_in = ndims(A)
    length_inner = length(inner)
    length_outer = length(outer)
    ndims_out = max(ndims_in, length_inner, length_outer)

    if length_inner < ndims_in || length_outer < ndims_in
        msg = "Inner/outer repetitions must be set for all input dimensions"
        throw(ArgumentError(msg))
    end

    size_in = Array(Int, ndims_in)
    size_out = Array(Int, ndims_out)
    inner_size_out = Array(Int, ndims_out)

    for i in 1:ndims_in
        size_in[i] = size(A, i)
    end
    for i in 1:ndims_out
        t1 = ndims_in < i ? 1 : size_in[i]
        t2 = length_inner < i ? 1 : inner[i]
        t3 = length_outer < i ? 1 : outer[i]
        size_out[i] = t1 * t2 * t3
        inner_size_out[i] = t1 * t2
    end

    indices_in = Array(Int, ndims_in)
    indices_out = Array(Int, ndims_out)

    length_out = prod(size_out)
    R = Array(T, size_out...)

    for index_out in 1:length_out
        ind2sub!(indices_out, size_out, index_out)
        for t in 1:ndims_in
            # "Project" outer repetitions into inner repetitions
            indices_in[t] = mod1(indices_out[t], inner_size_out[t])
            # Find inner repetitions using flooring division
            if inner[t] != 1
                indices_in[t] = fld1(indices_in[t], inner[t])
            end
        end
        index_in = sub2ind(size_in, indices_in)
        R[index_out] = A[index_in]
    end

    return R
end

## iteration utilities ##

# slow, but useful
function cartesianmap(body, t::(Int...), it...)
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
                cartesianmap(body, t, i, j, it...)
            end
        end
    else
        throw(ArgumentError("cartesianmap"))
    end
end

cartesianmap(body, t::()) = (body(); nothing)

function cartesianmap(body, t::(Int,))
    for i = 1:t[1]
        body(i)
    end
end

function cartesianmap(body, t::(Int,Int))
    for j = 1:t[2]
        for i = 1:t[1]
            body(i,j)
        end
    end
end

function cartesianmap(body, t::(Int,Int,Int))
    for k = 1:t[3]
        for j = 1:t[2]
            for i = 1:t[1]
                body(i,j,k)
            end
        end
    end
end

# Basic AbstractArray functions

function nnz{T}(a::AbstractArray{T})
    n = 0
    for i = 1:length(a)
        @inbounds n += (a[i] != zero(T))
    end
    return n
end

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
sum(A::AbstractArray{Bool}) = sum(A, [1:ndims(A)])[1]
prod(A::AbstractArray{Bool}) =
    error("use all() instead of prod() for boolean arrays")
prod(A::AbstractArray{Bool}, region) =
    error("use all() instead of prod() for boolean arrays")

# Pairwise (cascade) summation of A[i1:i1+n-1], which has O(log n) error growth
# [vs O(n) for a simple loop] with negligible performance cost if
# the base case is large enough.  See, e.g.:
#        http://en.wikipedia.org/wiki/Pairwise_summation
#        Higham, Nicholas J. (1993), "The accuracy of floating point
#        summation", SIAM Journal on Scientific Computing 14 (4): 783–799.
# In fact, the root-mean-square error growth, assuming random roundoff
# errors, is only O(sqrt(log n)), which is nearly indistinguishable from O(1)
# in practice.  See:
#        Manfred Tasche and Hansmartin Zeuner, Handbook of
#        Analytic-Computational Methods in Applied Mathematics (2000).
function sum_pairwise(A::AbstractArray, i1,n)
    if n < 128
        @inbounds s = A[i1]
        for i = i1+1:i1+n-1
            @inbounds s += A[i]
        end
        return s
    else
        n2 = div(n,2)
        return sum_pairwise(A, i1, n2) + sum_pairwise(A, i1+n2, n-n2)
    end
end

function sum{T}(A::AbstractArray{T})
    n = length(A)
    n == 0 ? zero(T) : sum_pairwise(A, 1, n)
end

# Kahan (compensated) summation: O(1) error growth, at the expense
# of a considerable increase in computational expense.
function sum_kbn{T<:FloatingPoint}(A::AbstractArray{T})
    n = length(A)
    if (n == 0)
        return zero(T)
    end
    s = A[1]
    c = zero(T)
    for i in 2:n
        Ai = A[i]
        t = s + Ai
        if abs(s) >= abs(Ai)
            c += ((s-t) + Ai)
        else
            c += ((Ai-t) + s)
        end
        s = t
    end

    s + c
end

# Uses K-B-N summation
function cumsum_kbn{T<:FloatingPoint}(v::AbstractVector{T})
    n = length(v)
    r = similar(v, n)
    if n == 0; return r; end

    s = r[1] = v[1]
    c = zero(T)
    for i=2:n
        vi = v[i]
        t = s + vi
        if abs(s) >= abs(vi)
            c += ((s-t) + vi)
        else
            c += ((vi-t) + s)
        end
        s = t
        r[i] = s+c
    end
    return r
end

# Uses K-B-N summation
function cumsum_kbn{T<:FloatingPoint}(A::AbstractArray{T}, axis::Integer)
    dimsA = size(A)
    ndimsA = ndims(A)
    axis_size = dimsA[axis]
    axis_stride = 1
    for i = 1:(axis-1)
        axis_stride *= size(A,i)
    end

    if axis_size <= 1
        return A
    end

    B = similar(A)
    C = similar(A)

    for i = 1:length(A)
        if div(i-1, axis_stride) % axis_size == 0
            B[i] = A[i]
            C[i] = zero(T)
        else
            s = B[i-axis_stride]
            Ai = A[i]
            B[i] = t = s + Ai
            if abs(s) >= abs(Ai)
                C[i] = C[i-axis_stride] + ((s-t) + Ai)
            else
                C[i] = C[i-axis_stride] + ((Ai-t) + s)
            end
        end
    end

    return B + C
end

function prod{T}(A::AbstractArray{T})
    if isempty(A)
        return one(T)
    end
    v = A[1]
    for i=2:length(A)
        @inbounds v *= A[i]
    end
    v
end

function minimum{T<:Real}(A::AbstractArray{T})
    if isempty(A); error("minimum: argument is empty"); end
    v = A[1]
    for i=2:length(A)
        @inbounds x = A[i]
        if x < v || v!=v
            v = x
        end
    end
    v
end

function maximum{T<:Real}(A::AbstractArray{T})
    if isempty(A); error("maximum: argument is empty"); end
    v = A[1]
    for i=2:length(A)
        @inbounds x = A[i]
        if x > v || v!=v
            v = x
        end
    end
    v
end

## map over arrays ##

## transform any set of dimensions
## dims specifies which dimensions will be transformed. for example
## dims==1:2 will call f on all slices A[:,:,...]
mapslices(f::Function, A::AbstractArray, dims) = mapslices(f, A, [dims...])
function mapslices(f::Function, A::AbstractArray, dims::AbstractVector)
    if isempty(dims)
        return A
    end

    dimsA = [size(A)...]
    ndimsA = ndims(A)
    alldims = [1:ndimsA]

    if dims == alldims
        return f(A)
    end

    otherdims = setdiff(alldims, dims)

    idx = cell(ndimsA)
    fill!(idx, 1)
    Asliceshape = tuple(dimsA[dims]...)
    itershape   = tuple(dimsA[otherdims]...)
    for d in dims
        idx[d] = 1:size(A,d)
    end

    r1 = f(reshape(A[idx...], Asliceshape))

    # determine result size and allocate
    Rsize = copy(dimsA)
    # TODO: maybe support removing dimensions
    if isempty(size(r1))
        r1 = [r1]
    end
    Rsize[dims] = [size(r1)...; ones(Int,max(0,length(dims)-ndims(r1)))]
    R = similar(r1, tuple(Rsize...))

    ridx = cell(ndims(R))
    fill!(ridx, 1)
    for d in dims
        ridx[d] = 1:size(R,d)
    end

    R[ridx...] = r1

    first = true
    cartesianmap(itershape) do idxs...
        if first
            first = false
        else
            ia = [idxs...]
            idx[otherdims] = ia
            ridx[otherdims] = ia
            R[ridx...] = f(reshape(A[idx...], Asliceshape))
        end
    end

    return R
end


## 1 argument
function map_to!(f::Callable, first, dest::AbstractArray, A::AbstractArray)
    dest[1] = first
    for i=2:length(A)
        dest[i] = f(A[i])
    end
    return dest
end

function map(f::Callable, A::AbstractArray)
    if isempty(A); return {}; end
    first = f(A[1])
    dest = similar(A, typeof(first))
    return map_to!(f, first, dest, A)
end

## 2 argument
function map_to!(f::Callable, first, dest::AbstractArray, A::AbstractArray, B::AbstractArray)
    dest[1] = first
    for i=2:length(A)
        dest[i] = f(A[i], B[i])
    end
    return dest
end

function map(f::Callable, A::AbstractArray, B::AbstractArray)
    shp = promote_shape(size(A),size(B))
    if isempty(A)
        return similar(A, Any, shp)
    end
    first = f(A[1], B[1])
    dest = similar(A, typeof(first), shp)
    return map_to!(f, first, dest, A, B)
end

## N argument
function map_to!(f::Callable, first, dest::AbstractArray, As::AbstractArray...)
    n = length(As[1])
    i = 1
    ith = a->a[i]
    dest[1] = first
    for i=2:n
        dest[i] = f(map(ith, As)...)
    end
    return dest
end

function map(f::Callable, As::AbstractArray...)
    shape = mapreduce(size, promote_shape, As)
    if prod(shape) == 0
        return similar(As[1], Any, shape)
    end
    first = f(map(a->a[1], As)...)
    dest = similar(As[1], typeof(first), shape)
    return map_to!(f, first, dest, As...)
end
