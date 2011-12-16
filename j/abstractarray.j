## abstractarray.j : Generic array interfaces.

## Type aliases for convenience ##

typealias AbstractVector{T} AbstractArray{T,1}
typealias AbstractMatrix{T} AbstractArray{T,2}

typealias Indices{T<:Int} Union(Int, AbstractVector{T})
typealias Region Union(Long,Dims)

typealias RangeIndex Union(Long, Range{Long}, Range1{Long})

## Basic functions ##

size(t::AbstractArray, d) = size(t)[d]
eltype{T,n}(::AbstractArray{T,n}) = T
ndims{T,n}(::AbstractArray{T,n}) = n
numel(t::AbstractArray) = prod(size(t))
length(a::AbstractArray) = numel(a)

function stride(a::AbstractArray, i::Int)
    s = 1
    for n=1:(i-1)
        s *= size(a, n)
    end
    return s
end
strides{T}(a::AbstractArray{T,1}) = (1,)
strides{T}(a::AbstractArray{T,2}) = (1, size(a,1))
strides{T}(a::AbstractArray{T,3}) = (1, size(a,1), size(a,1)*size(a,2))
strides   (a::AbstractArray)      = ntuple(ndims(a), i->stride(a,i))

isinteger{T<:Int}(::AbstractArray{T}) = true
isinteger(::AbstractArray) = false
isreal{T<:Real}(::AbstractArray{T}) = true
isreal(::AbstractArray) = false
iscomplex{T<:Complex}(::AbstractArray{T}) = true
iscomplex(::AbstractArray) = false

## Constructors ##

# default arguments to similar()
similar{T}(a::AbstractArray{T})                = similar(a, T, size(a))
similar   (a::AbstractArray, T)                = similar(a, T, size(a))
similar{T}(a::AbstractArray{T}, dims::Dims)    = similar(a, T, dims)
similar{T}(a::AbstractArray{T}, dims::Long...) = similar(a, T, dims)
similar   (a::AbstractArray, T, dims::Long...) = similar(a, T, dims)

empty(a::AbstractArray) = similar(a, 0)

function reshape(a::AbstractArray, dims::Dims)
    b = similar(a, dims)
    for i=1:numel(a)
        b[i] = a[i]
    end
    return b
end
reshape(a::AbstractArray, dims::Long...) = reshape(a, dims)

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

function copy_to(dest::AbstractArray, src::AbstractArray)
    for i=1:numel(src)
        dest[i] = copy(src[i])
    end
    return dest
end

copy(a::AbstractArray) = copy_to(similar(a), a)

zero{T}(x::AbstractArray{T}) = fill!(similar(x), zero(T))

## Conversions ##

int8   (x::AbstractArray) = copy_to(similar(x,Int8)   , x)
uint8  (x::AbstractArray) = copy_to(similar(x,Uint8)  , x)
int16  (x::AbstractArray) = copy_to(similar(x,Int16)  , x)
uint16 (x::AbstractArray) = copy_to(similar(x,Uint16) , x)
int32  (x::AbstractArray) = copy_to(similar(x,Int32)  , x)
uint32 (x::AbstractArray) = copy_to(similar(x,Uint32) , x)
int64  (x::AbstractArray) = copy_to(similar(x,Int64)  , x)
uint64 (x::AbstractArray) = copy_to(similar(x,Uint64) , x)
int    (x::AbstractArray) = copy_to(similar(x,typeof(int(one(eltype(x))))), x)
uint   (x::AbstractArray) = copy_to(similar(x,typeof(uint(one(eltype(x))))), x)
bool   (x::AbstractArray) = copy_to(similar(x,Bool)   , x)
char   (x::AbstractArray) = copy_to(similar(x,Char)   , x)
float32(x::AbstractArray) = copy_to(similar(x,Float32), x)
float64(x::AbstractArray) = copy_to(similar(x,Float64), x)
float  (x::AbstractArray) = copy_to(similar(x,typeof(float(one(eltype(x))))), x)

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

# ^ is difficult, since negative exponents give a different type

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
        dimargnames = { gensym() | i=1:N }
        ivars = { gensym() | i=1:N }
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

ref(t::AbstractArray) = t
ref(t::AbstractArray, i::Int) = error("indexing not defined for ", typeof(t))
ref(t::AbstractArray, i::Real) = ref(t, iround(i))
ref(t::AbstractArray, i::Real, j::Real) = ref(t, iround(i), iround(j))
ref(t::AbstractArray, i::Real, j::Real, k::Real) = ref(t, iround(i), iround(j), iround(k))
ref(t::AbstractArray, r::Real...) = ref(t,map(iround,r)...)

ref{T<:Int}(A::AbstractVector, I::AbstractVector{T}) = [ A[i] | i = I ]
ref{T<:Int}(A::AbstractArray{Any,1}, I::AbstractVector{T}) = { A[i] | i = I }

ref{T<:Int}(A::AbstractMatrix, I::Int, J::AbstractVector{T}) = [ A[i,j] | i = I, j = J ]
ref{T<:Int}(A::AbstractMatrix, I::AbstractVector{T}, J::Int) = [ A[i,j] | i = I, j = J ]
ref{T<:Int}(A::AbstractMatrix, I::AbstractVector{T}, J::AbstractVector{T}) = [ A[i,j] | i = I, j = J ]

ref(A::AbstractArray, i0::Int, i1::Int) = A[i0 + size(A,1)*(i1-1)]
ref(A::AbstractArray, i0::Int, i1::Int, i2::Int) =
    A[i0 + size(A,1)*((i1-1) + size(A,2)*(i2-1))]
ref(A::AbstractArray, i0::Int, i1::Int, i2::Int, i3::Int) =
    A[i0 + size(A,1)*((i1-1) + size(A,2)*((i2-1) + size(A,3)*(i3-1)))]

function ref(A::AbstractArray, I::Int...)
    ndims = length(I)
    index = I[1]
    stride = 1
    for k=2:ndims
        stride = stride * size(A, k-1)
        index += (I[k]-1) * stride
    end
    return A[index]
end

let ref_cache = nothing
global ref
function ref(A::AbstractArray, I::Indices...)
    X = similar(A, map(length, I))

    if is(ref_cache,nothing)
        ref_cache = HashTable()
    end
    gen_cartesian_map(ref_cache, ivars -> quote
            X[storeind] = A[$(ivars...)];
            storeind += 1
        end, I, (:A, :X, :storeind), A, X, 1)
    return X
end
end

# index A[:,:,...,i,:,:,...] where "i" is in dimension "d"
# TODO: more optimized special cases
slicedim(A::AbstractArray, d::Int, i) =
    A[ntuple(ndims(A), n->(n==d ? i : (1:size(A,n))))...]

function flipdim(A::AbstractArray, d::Int)
    nd = ndims(A)
    sd = d > nd ? 1 : size(A, d)
    if sd == 1
        return copy(A)
    end
    B = similar(A)
    nnd = 0
    for i = 1:nd
        nnd += count(size(A,i)==1 || i==d)
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

circshift(a, shiftamt::Int) = circshift(a, [shiftamt])
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
assign(t::AbstractArray, x, i::Int) =
    error("assign not defined for ",typeof(t))
assign(t::AbstractArray, x::AbstractArray, i::Int) =
    error("assign not defined for ",typeof(t))

assign(t::AbstractArray, x, i::Real)          = (t[iround(i)] = x)
assign(t::AbstractArray, x, i::Real, j::Real) = (t[iround(i),iround(j)] = x)
assign(t::AbstractArray, x, i::Real, j::Real, k::Real) =
    (t[iround(i),iround(j),iround(k)] = x)
assign(t::AbstractArray, x, r::Real...)       = (t[map(iround,r)...] = x)
assign(t::AbstractArray, x) = error("assign: too few arguments")

function assign{T<:Int}(A::AbstractVector, x, I::AbstractVector{T})
    for i = I
        A[i] = x
    end
    return A
end

function assign{T<:Int}(A::AbstractVector, X::AbstractArray, I::AbstractVector{T})
    for i = 1:length(I)
        A[I[i]] = X[i]
    end
    return A
end

assign(A::AbstractMatrix, x, i::Int, j::Int) = (A[(j-1)*size(A,1) + i] = x)
assign(A::AbstractMatrix, x::AbstractArray, i::Int, j::Int) = (A[(j-1)*size(A,1) + i] = x)

function assign(A::AbstractMatrix, x, I::Indices, J::Indices)
    for j=J, i=I
        A[i,j] = x
    end
    return A
end

function assign(A::AbstractMatrix, X::AbstractArray, I::Indices, J::Indices)
    count = 1
    for j=J, i=I
        A[i,j] = X[count]
        count += 1
    end
    return A
end

assign(A::AbstractArray, x, I0::Int, I::Int...) = assign_scalarND(A,x,I0,I...)
assign(A::AbstractArray, x::AbstractArray, I0::Int, I::Int...) =
    assign_scalarND(A,x,I0,I...)

assign(A::AbstractArray, x::AbstractArray, i0::Int, i1::Int) = A[i0 + size(A,1)*(i1-1)] = x
assign(A::AbstractArray, x, i0::Int, i1::Int) = A[i0 + size(A,1)*(i1-1)] = x

assign(A::AbstractArray, x, i0::Int, i1::Int, i2::Int) =
    A[i0 + size(A,1)*((i1-1) + size(A,2)*(i2-1))] = x
assign(A::AbstractArray, x::AbstractArray, i0::Int, i1::Int, i2::Int) =
    A[i0 + size(A,1)*((i1-1) + size(A,2)*(i2-1))] = x

assign(A::AbstractArray, x, i0::Int, i1::Int, i2::Int, i3::Int) =
    A[i0 + size(A,1)*((i1-1) + size(A,2)*((i2-1) + size(A,3)*(i3-1)))] = x
assign(A::AbstractArray, x::AbstractArray, i0::Int, i1::Int, i2::Int, i3::Int) =
    A[i0 + size(A,1)*((i1-1) + size(A,2)*((i2-1) + size(A,3)*(i3-1)))] = x

function assign_scalarND(A, x, I0, I...)
    index = I0
    stride = 1
    for k=1:length(I)
        stride = stride * size(A, k)
        index += (I[k]-1) * stride
    end
    A[index] = x
    return A
end

let assign_cache = nothing
global assign
function assign(A::AbstractArray, x, I0::Indices, I::Indices...)
    if is(assign_cache,nothing)
        assign_cache = HashTable()
    end
    gen_cartesian_map(assign_cache, ivars->:(A[$(ivars...)] = x),
                      tuple(I0, I...),
                      (:A, :x),
                      A, x)
    return A
end
end

let assign_cache = nothing
global assign
function assign(A::AbstractArray, X::AbstractArray, I0::Indices, I::Indices...)
    if is(assign_cache,nothing)
        assign_cache = HashTable()
    end
    gen_cartesian_map(assign_cache, ivars->:(A[$(ivars...)] = X[refind];
                                             refind += 1),
                      tuple(I0, I...),
                      (:A, :X, :refind),
                      A, X, 1)
    return A
end
end

## Concatenation ##

#TODO: ERROR CHECK
cat(catdim::Int) = Array(None, 0)

vcat() = Array(None, 0)
hcat() = Array(None, 0)

## cat: special cases
hcat{T}(X::T...) = [ X[j] | i=1, j=1:length(X) ]
vcat{T}(X::T...) = [ X[i] | i=1:length(X) ]

function hcat{T}(V::AbstractVector{T}...)
    height = length(V[1])
    for j = 2:length(V)
        if length(V[j]) != height; error("hcat: mismatched dimensions"); end
    end
    [ V[j][i] | i=1:length(V[1]), j=1:length(V) ]
end

function vcat{T}(V::AbstractVector{T}...)
    n = 0
    for Vk = V
        n += length(Vk)
    end
    a = similar(V[1], n)
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

function hcat{T}(A::AbstractMatrix{T}...)
    nargs = length(A)
    ncols = sum(a->size(a, 2), A)::Long
    nrows = size(A[1], 1)
    for j = 2:nargs
        if size(A[j], 1) != nrows; error("hcat: mismatched dimensions"); end
    end
    B = similar(A[1], nrows, ncols)
    pos = 1
    for k=1:nargs
        Ak = A[k]
        p1 = pos+size(Ak,2)-1
        B[:, pos:p1] = Ak
        pos = p1+1
    end
    return B
end

function vcat{T}(A::AbstractMatrix{T}...)
    nargs = length(A)
    nrows = sum(a->size(a, 1), A)::Long
    ncols = size(A[1], 2)
    for j = 2:nargs
        if size(A[j], 2) != ncols; error("vcat: mismatched dimensions"); end
    end
    B = similar(A[1], nrows, ncols)
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

function cat(catdim::Int, X...)
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
                    error("cat: dimension mismatch on dimension", d)
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
    dimsC = ntuple(ndimsC, compute_dims)::(Long...)
    typeC = promote_type(map(x->isa(x,AbstractArray) ? eltype(x) : typeof(x), X)...)
    C = similar(isa(X[1],AbstractArray) ? X[1] : [X[1]], typeC, dimsC)

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

function cat(catdim::Int, A::AbstractArray...)
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
    dimsC = ntuple(ndimsC, compute_dims)::(Long...)
    typeC = promote_type(map(eltype, A)...)
    C = similar(A[1], typeC, dimsC)

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

function hvcat{T}(rows::(Long...), as::AbstractMatrix{T}...)
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

    out = similar(as[1], T, nr, nc)

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

hvcat(rows::(Long...)) = []

function hvcat{T<:Number}(rows::(Long...), xs::T...)
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

function hvcat(rows::(Long...), xs::Number...)
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

## Reductions ##

contains(s::Number, n::Number) = (s == n)

areduce{T}(f::Function, A::AbstractArray{T}, region::Region, v0) =
        areduce(f,A,region,v0,T)

# TODO:
# - find out why inner loop with dimsA[i] instead of size(A,i) is way too slow

let areduce_cache = nothing
# generate the body of the N-d loop to compute a reduction
function gen_areduce_func(n, f)
    ivars = { gensym() | i=1:n }
    # limits and vars for reduction loop
    lo    = { gensym() | i=1:n }
    hi    = { gensym() | i=1:n }
    rvars = { gensym() | i=1:n }
    setlims = { quote
        # each dim of reduction is either 1:sizeA or ivar:ivar
        if contains(region,$i)
            $lo[i] = 1
            $hi[i] = size(A,$i)
        else
            $lo[i] = $hi[i] = $ivars[i]
        end
               end | i=1:n }
    rranges = { :( ($lo[i]):($hi[i]) ) | i=1:n }  # lo:hi for all dims
    body =
    quote
        _tot = v0
        $(setlims...)
        $make_loop_nest(rvars, rranges,
                        :(_tot = ($f)(_tot, A[$(rvars...)])))
        R[_ind] = _tot
        _ind += 1
    end
    quote
        local _F_
        function _F_(f, A, region, R, v0)
            _ind = 1
            $make_loop_nest(ivars, { :(1:size(R,$i)) | i=1:n }, body)
        end
        _F_
    end
end

global areduce
function areduce(f::Function, A::AbstractArray, region::Region, v0, RType::Type)
    dimsA = size(A)
    ndimsA = ndims(A)
    dimsR = ntuple(ndimsA, i->(contains(region, i) ? 1 : dimsA[i]))
    R = similar(A, RType, dimsR)

    if is(areduce_cache,nothing)
        areduce_cache = HashTable()
    end

    key = ndimsA
    fname = :f

    if  (is(f,+)     && (fname=:+;true)) ||
        (is(f,*)     && (fname=:*;true)) ||
        (is(f,max)   && (fname=:max;true)) ||
        (is(f,min)   && (fname=:min;true)) ||
        (is(f,sum)   && (fname=:+;true)) ||
        (is(f,prod)  && (fname=:*;true)) ||
        (is(f,any)   && (fname=:any;true)) ||
        (is(f,all)   && (fname=:all;true)) ||
        (is(f,count) && (fname=:count;true))
        key = (fname, ndimsA)
    end

    if !has(areduce_cache,key)
        fexpr = gen_areduce_func(ndimsA, fname)
        func = eval(fexpr)
        areduce_cache[key] = func
    else
        func = areduce_cache[key]
    end

    func(f, A, region, R, v0)

    return R
end
end

function sum{T}(A::AbstractArray{T})
    if isempty(A)
        return zero(T)
    end
    v = A[1]
    for i=2:numel(A)
        v += A[i]
    end
    v
end

function prod{T}(A::AbstractArray{T})
    if isempty(A)
        return one(T)
    end
    v = A[1]
    for i=2:numel(A)
        v *= A[i]
    end
    v
end

function min{T<:Int}(A::AbstractArray{T})
    v = typemax(T)
    for i=1:numel(A)
        x = A[i]
        if x < v
            v = x
        end
    end
    v
end

function max{T<:Int}(A::AbstractArray{T})
    v = typemin(T)
    for i=1:numel(A)
        x = A[i]
        if x > v
            v = x
        end
    end
    v
end

max{T}(A::AbstractArray{T}, b::(), region::Region) = areduce(max,A,region,typemin(T),T)
min{T}(A::AbstractArray{T}, b::(), region::Region) = areduce(min,A,region,typemax(T),T)
sum{T}(A::AbstractArray{T}, region::Region)  = areduce(+,A,region,zero(T))
prod{T}(A::AbstractArray{T}, region::Region) = areduce(*,A,region,one(T))

all(A::AbstractArray{Bool}, region::Region) = areduce(all,A,region,true)
any(A::AbstractArray{Bool}, region::Region) = areduce(any,A,region,false)
count(A::AbstractArray{Bool}, region::Region) = areduce(count,A,region,0,Long)

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
end

## iteration support for arrays as ranges ##

start(a::AbstractArray) = 1
next(a::AbstractArray,i) = (a[i],i+1)
done(a::AbstractArray,i) = (i > numel(a))
isempty(a::AbstractArray) = (numel(a) == 0)

## map over arrays ##

## 1 argument
function map_to(dest::AbstractArray, f, A::AbstractArray)
    for i=1:numel(A)
        dest[i] = f(A[i])
    end
    return dest
end
function map_to2(first, dest::AbstractArray, f, A::AbstractArray)
    dest[1] = first
    for i=2:numel(A)
        dest[i] = f(A[i])
    end
    return dest
end

function map(f, A::AbstractArray)
    if isempty(A); return A; end
    first = f(A[1])
    dest = similar(A, typeof(first))
    return map_to2(first, dest, f, A)
end

## 2 argument
function map_to(dest::AbstractArray, f, A::AbstractArray, B::AbstractArray)
    for i=1:numel(A)
        dest[i] = f(A[i], B[i])
    end
    return dest
end
function map_to2(first, dest::AbstractArray, f,
                 A::AbstractArray, B::AbstractArray)
    dest[1] = first
    for i=2:numel(A)
        dest[i] = f(A[i], B[i])
    end
    return dest
end

function map(f, A::AbstractArray, B::AbstractArray)
    if size(A) != size(B); error("argument dimensions must match"); end
    if isempty(A); return A; end
    first = f(A[1], B[1])
    dest = similar(A, typeof(first))
    return map_to2(first, dest, f, A, B)
end

function map_to(dest::AbstractArray, f, A::AbstractArray, B::Number)
    for i=1:numel(A)
        dest[i] = f(A[i], B)
    end
    return dest
end
function map_to2(first, dest::AbstractArray, f, A::AbstractArray, B::Number)
    dest[1] = first
    for i=2:numel(A)
        dest[i] = f(A[i], B)
    end
    return dest
end

function map(f, A::AbstractArray, B::Number)
    if isempty(A); return A; end
    first = f(A[1], B)
    dest = similar(A, typeof(first))
    return map_to2(first, dest, f, A, B)
end

function map_to(dest::AbstractArray, f, A::Number, B::AbstractArray)
    for i=1:numel(B)
        dest[i] = f(A, B[i])
    end
    return dest
end
function map_to2(first, dest::AbstractArray, f, A::Number, B::AbstractArray)
    dest[1] = first
    for i=2:numel(B)
        dest[i] = f(A, B[i])
    end
    return dest
end

function map(f, A::Number, B::AbstractArray)
    if isempty(A); return A; end
    first = f(A, B[1])
    dest = similar(B, typeof(first))
    return map_to2(first, dest, f, A, B)
end

## N argument
function map_to(dest::AbstractArray, f, As::AbstractArray...)
    n = numel(As[1])
    i = 1
    ith = a->a[i]
    for i=1:n
        dest[i] = f(map(ith, As)...)
    end
    return dest
end
function map_to2(first, dest::AbstractArray, f, As::AbstractArray...)
    n = numel(As[1])
    i = 1
    ith = a->a[i]
    dest[1] = first
    for i=2:n
        dest[i] = f(map(ith, As)...)
    end
    return dest
end

function map(f, As::AbstractArray...)
    if isempty(As[1]); return As[1]; end
    first = f(map(a->a[1], As)...)
    dest = similar(As[1], typeof(first))
    return map_to2(first, dest, f, As...)
end

## Transpose, Permute ##

let permute_cache = nothing
global permute
function permute(A::AbstractArray, perm)
    dimsA = size(A)
    ndimsA = length(dimsA)
    dimsP = ntuple(ndimsA, i->dimsA[perm[i]])
    P = similar(A, dimsP)
    ranges = ntuple(ndimsA, i->(Range1(1,dimsP[i])))
    stridenames = {gensym() | i = 1:ndimsA}

    #calculates all the strides
    strides = Array(Int32,0)
    for dim = 1:length(perm)
    	stride = 1
    	for dim_size = 1:(dim-1)
            stride = stride*dimsA[dim_size]
    	end
    	push(strides, stride)
    end

    #reorganizes the ordering of the strides
    strides = { (strides[perm[i]]) | i = 1:ndimsA}

    #Creates offset, because indexing starts at 1
    offset = 0
    for i = strides
        offset+=i
    end
    offset = 1-offset

    function permute_one(ivars)
        len = length(ivars)
        counts = { gensym() | i=1:len}
        toReturn = cell(len+1,2)
        for i = 1:numel(toReturn)
            toReturn[i] = nothing
        end

        tmp = counts[end]
        toReturn[len+1] = quote
            ind = 1
            $tmp = $stridenames[end]
        end

        #inner most loop
        toReturn[1] = quote
            P[ind] = A[+($counts...)+offset]
            ind+=1
            $counts[1]+= $stridenames[1]
        end
        for i = 1:len-1
            tmp = counts[i]
            val = i
            toReturn[(i+1)] = quote
                $tmp = $stridenames[val]
            end
            tmp2 = counts[i+1]
            val = i+1
            toReturn[(i+1)+(len+1)] = quote
                 $tmp2 += $stridenames[val]
            end
        end
        toReturn
    end


    if is(permute_cache,nothing)
	permute_cache = HashTable()
    end

    gen_cartesian_map(permute_cache, permute_one, ranges,
                      tuple(:A, :P, :perm, :offset, stridenames...),
                      A, P, perm, offset, strides...)

    return P
end
end # let

function ipermute(A::AbstractArray,perm)
    iperm = zeros(Int32,length(perm))
    for i = 1:length(perm)
	iperm[perm[i]] = i
    end
    return permute(A,iperm)
end

## Other array functions ##

# fallback definition of hvcat in terms of hcat and vcat
function hvcat(rows::(Long...), as...)
    nbr = length(rows)  # number of block rows
    rs = cell(nbr)
    a = 1
    for i = 1:nbr
        rs[i] = hcat(as[a:a-1+rows[i]]...)
        a += rows[i]
    end
    vcat(rs...)
end

function repmat(a::AbstractMatrix, m::Long, n::Long)
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

accumarray(I::AbstractVector, J::AbstractVector, V) = accumarray (I, J, V, max(I), max(J))

function accumarray{T<:Number}(I::AbstractVector, J::AbstractVector, V::T, m::Long, n::Long)
    A = similar(V, m, n)
    for k=1:length(I)
        A[I[k], J[k]] += V
    end
    return A
end

function accumarray(I::Indices, J::Indices, V::AbstractVector, m::Long, n::Long)
    A = similar(V, m, n)
    for k=1:length(I)
        A[I[k], J[k]] += V[k]
    end
    return A
end

sub2ind(dims) = 1
sub2ind(dims, i::Int) = long(i)
sub2ind(dims, i::Int, j::Int) = sub2ind(long(i), long(j))
sub2ind(dims, i::Long, j::Long) = (j-1)*dims[1] + i
sub2ind(dims, i0::Int, i1::Int, i2::Int) = sub2ind(long(i0),long(i1),long(i2))
sub2ind(dims, i0::Long, i1::Long, i2::Long) =
    i0 + dims[1]*((i1-1) + dims[2]*(i2-1))
sub2ind(dims, i0::Int, i1::Int, i2::Int, i3::Int) =
    sub2ind(long(i0),long(i1),long(i2),long(i3))
sub2ind(dims, i0::Long, i1::Long, i2::Long, i3::Long) =
    i0 + dims[1]*((i1-1) + dims[2]*((i2-1) + dims[3]*(i3-1)))

function sub2ind(dims, I::Int...)
    ndims = length(dims)
    index = long(I[1])
    stride = 1
    for k=2:ndims
        stride = stride * dims[k-1]
        index += (long(I[k])-1) * stride
    end
    return index
end

sub2ind(dims, I::AbstractVector...) =
    [ sub2ind(dims, map(X->X[i], I)...) | i=1:length(I[1]) ]

ind2sub(dims::(Int...), ind::Int) = ind2sub(dims, long(ind))
ind2sub(dims::(), ind::Int) = throw(BoundsError())
ind2sub(dims::(Int,), ind::Long) = (ind,)
ind2sub(dims::(Int,Int), ind::Long) =
    (rem(ind-1,dims[1])+1, div(ind-1,dims[1])+1)
ind2sub(dims::(Int,Int,Int), ind::Long) =
    (rem(ind-1,dims[1])+1, div(rem(ind-1,dims[1]*dims[2]), dims[1])+1,
     div(rem(ind-1,dims[1]*dims[2]*dims[3]), dims[1]*dims[2])+1)

function ind2sub(dims::(Int,Int...), ind::Long)
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
function cartesian_map(body, t::(Long...), it...)
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

function cartesian_map(body, t::(Long,))
    for i = 1:t[1]
        body(i)
    end
end

function cartesian_map(body, t::(Long,Long))
    for j = 1:t[2]
        for i = 1:t[1]
            body(i,j)
        end
    end
end

function cartesian_map(body, t::(Long,Long,Long))
    for k = 1:t[3]
        for j = 1:t[2]
            for i = 1:t[1]
                body(i,j,k)
            end
        end
    end
end
