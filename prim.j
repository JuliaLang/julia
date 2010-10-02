typealias Index Int32
typealias Size Int32

(<:)(T, S) = subtype(T,S)
(>:)(T, S) = subtype(S,T)

# bootstrapping versions of operators needed by for loops
(-)(x::Int32) = boxsi32(neg_int(unbox32(x)))
(+)(x::Int32, y::Int32) = boxsi32(add_int(unbox32(x), unbox32(y)))
(-)(x::Int32, y::Int32) = boxsi32(sub_int(unbox32(x), unbox32(y)))
(*)(x::Int32, y::Int32) = boxsi32(mul_int(unbox32(x), unbox32(y)))
div(x::Int32, y::Int32) = boxsi32(sdiv_int(unbox32(x), unbox32(y)))
< (x::Int32, y::Int32) = slt_int(unbox32(x),unbox32(y))
==(x::Int32, y::Int32) = eq_int(unbox32(x),unbox32(y))
floor(x::Float64) = ccall(dlsym(JuliaDLHandle,"floor"),Float64,(Float64,),x)

# fallback definitions for emulating N-arg operators with 2-arg definitions
(*)() = 1
(*)(x::Tensor) = x
(*)(a,b,c) = (*)((*)(a,b),c)
(*)(a,b,c,d) = (*)((*)((*)(a,b),c),d)
(*)(a,b,c,d,e) = (*)((*)((*)((*)(a,b),c),d),e)
function (*)(x1, x2, x3, xs...)
    accum = (*)((*)(x1,x2),x3)
    for x = xs
        accum = accum * x
    end
    accum
end

(+)() = 0
(+)(x::Tensor) = x
(+)(a,b,c) = (+)((+)(a,b),c)
(+)(a,b,c,d) = (+)((+)((+)(a,b),c),d)
(+)(a,b,c,d,e) = (+)((+)((+)((+)(a,b),c),d),e)
function (+)(x1, x2, x3, xs...)
    accum = (+)((+)(x1,x2),x3)
    for x = xs
        accum = accum + x
    end
    accum
end

# arithmetic and comparison promotion
(+)(x::Number, y::Number) = (+)(promote(x,y)...)
(*)(x::Number, y::Number) = (*)(promote(x,y)...)
(-)(x::Number, y::Number) = (-)(promote(x,y)...)
(/)(x::Number, y::Number) = (/)(promote(x,y)...)
# these are defined for the fundamental < and == so that if a method is
# not found for e.g. <=, it is translated to < and == first, then promotion
# is handled after.
(<) (x::Real, y::Real)     = (<)(promote(x,y)...)
(==)(x::Number, y::Number) = (==)(promote(x,y)...)

# if arguments are of the same type and no method exists, promotion
# is not applicable. it means the operator is not defined.
no_op_err(name, T) = error(strcat(name," not defined for ",string(T)))
(+){T<:Number}(x::T, y::T) = no_op_err("+", T)
(*){T<:Number}(x::T, y::T) = no_op_err("*", T)
(-){T<:Number}(x::T, y::T) = no_op_err("-", T)
(/){T<:Number}(x::T, y::T) = no_op_err("/", T)
(<){T<:Real}  (x::T, y::T) = no_op_err("<", T)
(==){T<:Number}(x::T, y::T) = no_op_err("==", T)

# .<op> defaults to <op>
(./)(x,y) = x/y
(.\)(x,y) = y./x
(.*)(x,y) = x*y

# generic div and % operations
div(x::Number, y::Number) = truncate(x/y)
(%)(x::Number, y::Number) = x-div(x,y)*y
mod(x,y) = x%y
(\)(x,y) = y/x

# general comparisons from == and < operators
!=(x, y) = !(x == y)
> (x, y) = (y < x)
<=(x, y) = (x < y) || (x == y)
>=(x, y) = (x > y) || (x == y)
<=(x::Real, y::Real) = (x < y) || (x == y)
>=(x::Real, y::Real) = (x > y) || (x == y)

# indexing tuples
length(t::Tuple) = tuplelen(t)
size(t::Tuple, d) = d==1 ? tuplelen(t) : error("invalid tuple dimension")
ref(t::Tuple, i::Index) = tupleref(t, i)

ref(t::Tuple, r::Range)  = accumtuple(t, r, start(r), r.step)
ref(t::Tuple, r::Range1) = accumtuple(t, r, start(r), 1)
function accumtuple(t::Tuple, r, i, step, elts...)
    if done(r, i)
        return elts
    end
    accumtuple(t, r, i+step, step, elts..., t[i])
end
ref(t::Tuple, r::RangeFrom) = t[Range(r.start,r.step,length(t))]
ref(t::Tuple, r::RangeTo)   = t[Range(1,r.step,r.stop)]
ref(t::Tuple, r::RangeBy)   = t[Range(1,r.step,length(t))]

# iterating over tuples
start(t::Tuple) = 1
done(t::Tuple, i) = (i > length(t))
next(t::Tuple, i) = (t[i], i+1)

# map on tuples
# 0 argument function
map(f) = f()
# 1 argument function
map(f, t::())                   = ()
map(f, t::(Any,))               = (f(t[1]),)
map(f, t::(Any, Any))           = (f(t[1]), f(t[2]))
map(f, t::(Any, Any, Any))      = (f(t[1]), f(t[2]), f(t[3]))
map(f, t::(Any, Any, Any, Any)) = (f(t[1]), f(t[2]), f(t[3]), f(t[4]))
map(f, t::Tuple) = maptuple(f, t...)
maptuple(f) = ()
maptuple(f, first, rest...) = tuple(f(first), maptuple(f, rest...)...)
# 2 argument function
map(f, t::(),        s::())        = ()
map(f, t::(Any,),    s::(Any,))    = (f(t[1],s[1]),)
map(f, t::(Any,Any), s::(Any,Any)) = (f(t[1],s[1]), f(t[2],s[2]))
# n argument function
function map(f, ts::Tuple...)
    function _map(f, ts, i)
        if i > length(ts[1])
            return ()
        end
        return tuple(f(map(x->x[i],ts)...), _map(f,ts,i+1)...)
    end
    return _map(f, ts, 1)
end

function ==(t1::Tuple, t2::Tuple)
    if length(t1) != length(t2)
        return false
    end
    for i = 1:length(t1)
        if t1[i] != t2[i]
            return false
        end
    end
    return true
end

function append(t1::Tuple, ts::Tuple...)
    if length(ts)==0
        return t1
    end
    return tuple(t1..., append(ts...)...)
end


# cell primitives
# these are used by the front end to implement {} and backquote
function append(a1::Array{Any,1}, as::Array{Any,1}...)
    n = arraylen(a1)
    for i = 1:length(as)
        n += arraylen(as[i])
    end
    a = Array(Any,n)
    for i = 1:arraylen(a1)
        arrayset(a,i,arrayref(a1,i))
    end
    i = arraylen(a1)+1
    for x = as
        for j = 1:length(x)
            arrayset(a,i,x[j])
            i += 1
        end
    end
    a
end

function cell_1d(xs...)
    n = length(xs)
    a = Array(Any,n)
    for i=1:n
        arrayset(a,i,xs[i])
    end
    a
end

function cell_2d(nr, nc, xs...)
    a = Array(Any,nr,nc)
    for i=1:(nr*nc)
        arrayset(a,i,xs[i])
    end
    a
end

expr(hd::Symbol, args...) = Expr(hd, {args...}, Any)

# map cell array
map(f, a::Array{Any,1}) = { f(a[i]) | i=1:length(a) }
map(f, a::Array{Any,1}, b::Array{Any,1}) =
    { f(a[i],b[i]) | i=1:min(length(a),length(b)) }

# copy
copy(x::Any) = x
copy(x::Tuple) = map(copy, x)
copy(e::Expr) = Expr(e.head, copy(e.args), e.type)

# sizeof
sizeof{T}(x::T) = sizeof(T)
sizeof(t::Type) = error(strcat("size of type ",string(t)," unknown"))

function assert(c)
    if !c
        error("Assertion failed.")
    end
    true
end

# system word size
word_size() = ccall(dlsym(JuliaDLHandle,"jl_word_size"), Int32, ())

# needed by type inference
symbol(s::Latin1String) = symbol(s.data)
symbol(s::UTF8String) = symbol(s.data)
symbol(a::Array{Uint8,1}) =
    ccall(dlsym(JuliaDLHandle,"jl_symbol"), Any, (Ptr{Uint8},), a)::Symbol
gensym() = ccall(dlsym(JuliaDLHandle,"jl_gensym"), Any, ())::Symbol
