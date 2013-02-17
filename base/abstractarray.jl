## Type aliases for convenience ##

typealias AbstractVector{T} AbstractArray{T,1}
typealias AbstractMatrix{T} AbstractArray{T,2}

## Basic functions ##

size{T,n}(t::AbstractArray{T,n}, d) = (d>n ? 1 : size(t)[d])
eltype{T,n}(::AbstractArray{T,n}) = T
eltype{T,n}(::Type{AbstractArray{T,n}}) = T
eltype{T<:AbstractArray}(::Type{T}) = eltype(super(T))
ndims{T,n}(::AbstractArray{T,n}) = n
ndims{T,n}(::Type{AbstractArray{T,n}}) = n
ndims{T<:AbstractArray}(::Type{T}) = ndims(super(T))
length(t::AbstractArray) = prod(size(t))
endof(a::AbstractArray) = length(a)
first(a::AbstractArray) = a[1]
last(a::AbstractArray) = a[end]

strides(a::AbstractArray) = ntuple(ndims(a), i->stride(a,i))::Dims

isinteger{T<:Integer}(::AbstractArray{T}) = true
isinteger(::AbstractArray) = false
isreal{T<:Real}(::AbstractArray{T}) = true
isreal(::AbstractArray) = false
iscomplex{T<:Complex}(::AbstractArray{T}) = true
iscomplex(::AbstractArray) = false
isbool(::AbstractArray{Bool}) = true
isbool(::AbstractArray) = false

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
function check_bounds(sz::Int, I::Real)
    I = to_index(I)
    if I < 1 || I > sz
        throw(BoundsError())
    end
end

function check_bounds(sz::Int, I::AbstractVector{Bool})
    if length(I) > sz
        throw(BoundsError())
    end
end

function check_bounds{T<:Integer}(sz::Int, I::Ranges{T})
    if min(I) < 1 || max(I) > sz
        throw(BoundsError())
    end
end

function check_bounds{T <: Real}(sz::Int, I::AbstractVector{T})
    for i in I
        i = to_index(i)
        if i < 1 || i > sz
            throw(BoundsError())
        end
    end
end

function check_bounds(A::AbstractArray, I::AbstractArray{Bool})
    if !isequal(size(A), size(I)) throw(BoundsError()) end
end

check_bounds(A::AbstractArray, I) = check_bounds(length(A), I)

function check_bounds(A::AbstractMatrix, I, J)
    check_bounds(size(A,1), I)
    check_bounds(size(A,2), J)
end

function check_bounds(A::AbstractArray, I, J)
    check_bounds(size(A,1), I)
    sz = size(A,2)
    for i = 3:ndims(A)
        sz *= size(A, i) # TODO: sync. with decision on issue #1030
    end
    check_bounds(sz, J)
end

function check_bounds(A::AbstractArray, I::Union(Real,AbstractArray)...)
    n = length(I)
    if n > 0
        for dim = 1:(n-1)
            check_bounds(size(A,dim), I[dim])
        end
        sz = size(A,n)
        for i = n+1:ndims(A)
            sz *= size(A,i)     # TODO: sync. with decision on issue #1030
        end
        check_bounds(sz, I[n])
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
    b = similar(a, dims)
    for i = 1:length(a)
        b[i] = a[i]
    end
    return b
end
reshape(a::AbstractArray, dims::Int...) = reshape(a, dims)

vec(a::AbstractArray) = reshape(a,length(a))

function squeeze(A::AbstractArray, dims)
    d = ()
    for i in 1:ndims(A)
        if contains(dims,i)
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

copy(a::AbstractArray) = copy!(similar(a), a)
copy(a::AbstractArray{None}) = a # cannot be assigned into so is immutable

zero{T}(x::AbstractArray{T}) = fill!(similar(x), zero(T))

## iteration support for arrays as ranges ##

start(a::AbstractArray) = 1
next(a::AbstractArray,i) = (a[i],i+1)
done(a::AbstractArray,i) = (i > length(a))
isempty(a::AbstractArray) = (length(a) == 0)

## Conversions ##

function iround_to{T}(dest::AbstractArray{T}, src)
    i = 1
    for x in src
        dest[i] = iround(T,x)
        i += 1
    end
    return dest
end

for (f,t) in ((:char,   Char),
              (:int,    Int),
              (:int8,   Int8),
              (:int16,  Int16),
              (:int32,  Int32),
              (:int64,  Int64),
              (:uint8,  Uint8),
              (:uint16, Uint16),
              (:uint32, Uint32),
              (:uint64, Uint64))
    @eval ($f)(x::AbstractArray{$t}) = x
    @eval ($f)(x::AbstractArray) = iround_to(similar(x,$t), x)
end

bool(x::AbstractArray{Bool}) = x
bool(x::AbstractArray) = copy!(similar(x,Bool), x)

for (f,t) in ((:float32,    Float32),
              (:float64,    Float64),
              (:complex64,  Complex64),
              (:complex128, Complex128))
    @eval ($f)(x::AbstractArray{$t}) = x
    @eval ($f)(x::AbstractArray) = copy!(similar(x,$t), x)
end

integer{T<:Integer}(x::AbstractArray{T}) = x
unsigned{T<:Unsigned}(x::AbstractArray{T}) = x
float{T<:FloatingPoint}(x::AbstractArray{T}) = x
complex{T<:Complex}(x::AbstractArray{T}) = x

integer (x::AbstractArray) = iround_to(similar(x,typeof(integer(one(eltype(x))))), x)
unsigned(x::AbstractArray) = iround_to(similar(x,typeof(unsigned(one(eltype(x))))), x)
float   (x::AbstractArray) = copy!(similar(x,typeof(float(one(eltype(x))))), x)
complex (x::AbstractArray) = copy!(similar(x,typeof(complex(one(eltype(x))))), x)

dense(x::AbstractArray) = x
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
    if !has(cache,N)
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




## Indexing: ref ##

ref(t::AbstractArray, i::Real) = error("indexing not defined for ", typeof(t))

# index A[:,:,...,i,:,:,...] where "i" is in dimension "d"
# TODO: more optimized special cases
slicedim(A::AbstractArray, d::Integer, i) =
    A[[ n==d ? i : (1:size(A,n)) for n in 1:ndims(A) ]...]

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

## Indexing: assign ##

# 1-d indexing is assumed defined on subtypes
assign(t::AbstractArray, x, i::Real) =
    error("assign not defined for ",typeof(t))
assign(t::AbstractArray, x) = throw(MethodError(assign, (t, x)))

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
    hvcat_fill(Array(T, nr, nc), xs)
end

## Reductions and scans ##

function isequal(A::AbstractArray, B::AbstractArray)
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

function cmp(A::AbstractArray, B::AbstractArray)
    nA, nB = length(A), length(B)
    for i = 1:min(nA, nB)
        a, b = A[i], B[i]
        if !isequal(a, b)
            return isless(a, b) ? -1 : +1
        end
    end
    return cmp(nA, nB)
end

isless(A::AbstractArray, B::AbstractArray) = cmp(A,B)<0

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

function (!=)(A::AbstractArray, B::AbstractArray)
    if size(A) != size(B)
        return true
    end
    for i = 1:length(A)
        if A[i]!=B[i]
            return true
        end
    end
    return false
end

for (f, op) = ((:cumsum, :+), (:cumprod, :*) )
    @eval function ($f)(v::AbstractVector)
        n = length(v)
        c = $(op===:+ ? (:(similar(v,typeof(+zero(eltype(v)))))) :
                        (:(similar(v))))
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
        axis_stride = 1
        for i = 1:(axis-1)
            axis_stride *= size(A,i)
        end

        B = $(op===:+ ? (:(similar(A,typeof(+zero(eltype(A)))))) :
                        (:(similar(A))))

        if axis_size < 1
            return B
        end

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

sub2ind{T<:Integer}(dims, I::AbstractVector{T}...) =
    [ sub2ind(dims, map(X->X[i], I)...)::Int for i=1:length(I[1]) ]

ind2sub(dims::(Integer...), ind::Integer) = ind2sub(dims, int(ind))
ind2sub(dims::(), ind::Integer) = ind==1 ? () : throw(BoundsError())
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

indices(I) = I
indices(I::AbstractArray{Bool,1}) = find(I)
indices(I::Tuple) = map(indices, I)

## iteration utilities ##

# TODO: something sensible should happen when each_col et. al. are used with a
# pure function argument
function each_col!(f::Function, a::AbstractMatrix)
    m = size(a,1)
    for i = 1:m:length(a)
        f(sub(a, i:(i+m-1)))
    end
    return a
end

function each_row!(f::Function, a::AbstractMatrix)
    m = size(a,1)
    for i = 1:m
        f(sub(a, i:m:length(a)))
    end
    return a
end

function each_vec!(f::Function, a::AbstractMatrix, dim::Integer)
    if dim == 1; return each_col!(f,a); end
    if dim == 2; return each_row!(f,a); end
    error("invalid matrix dimensions: ", dim)
end

each_col(f::Function, a::AbstractMatrix) = each_col!(f,copy(a))
each_row(f::Function, a::AbstractMatrix) = each_row!(f,copy(a))
each_vec(f::Function, a::AbstractMatrix, d::Integer) = each_vec!(f,copy(a),d)

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
            range = tuple(range..., bi)
        elseif bi == 1
            xb = true
            shp[i] = ai
            range = tuple(range..., ai)
        else
            error("argument dimensions do not match")
        end
    end
    if isempty(range)
        return f(a, b)
    end
    if length(a) == 1
        return f(a[1], b)
    elseif length(b) == 1
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
            aa = a[aidxs...]; if length(aa)==1; aa=aa[1]; end
        else
            aa = a
        end
        if xa
            bb = b[bidxs...]; if length(bb)==1; bb=bb[1]; end
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
bsxfun(f, a, b, c...) = bsxfun(f, bsxfun(f, a, b), c...)
