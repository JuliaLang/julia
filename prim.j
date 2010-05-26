typealias Nullable{T} Union(T,())
typealias Index Int32
typealias Size  Int32
typealias String Array{Uint8,1}

(<:)(T, S) = subtype(T,S)
(:>)(T, S) = subtype(S,T)

ref(t::Tuple, i::Index) = tupleref(t, i)
length(t::Tuple) = tuplelen(t)

!(x::Bool) = eq_int(unbox8(x),trunc8(unbox32(0)))

# bootstrapping versions of operators needed by for loops
(-)(x::Int32) = boxsi32(neg_int(unbox32(x)))
(+)(x::Int32, y::Int32) = boxsi32(add_int(unbox32(x), unbox32(y)))
(-)(x::Int32, y::Int32) = boxsi32(sub_int(unbox32(x), unbox32(y)))
(*)(x::Int32, y::Int32) = boxsi32(mul_int(unbox32(x), unbox32(y)))
div(x::Int32, y::Int32) = boxsi32(sdiv_int(unbox32(x), unbox32(y)))
< (x::Int32, y::Int32) = slt_int(unbox32(x),unbox32(y))
==(x::Int32, y::Int32) = eq_int(unbox32(x),unbox32(y))

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

(<)(x::Number, y::Number)  = (<)(promote(x,y)...)
(>)(x::Number, y::Number)  = (>)(promote(x,y)...)
(<=)(x::Number, y::Number) = (<=)(promote(x,y)...)
(>=)(x::Number, y::Number) = (>=)(promote(x,y)...)
(==)(x::Number, y::Number) = (==)(promote(x,y)...)

# .<op> defaults to <op>
(./)(x,y) = x/y
(.*)(x,y) = x*y

# generic div and % operations
div(x::Number, y::Number) = truncate(x/y)
(%)(x::Number, y::Number) = x-div(x,y)*y

# general comparisons from == and < operators
!=(x, y) = !(x == y)
> (x::Real, y::Real) = (y < x)
<=(x::Real, y::Real) = (x < y) || x == y
>=(x::Real, y::Real) = (x > y) || x == y

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

ref(t::Tuple, r::Range) = accumtuple(t, r, start(r))
function accumtuple(t::Tuple, r::Range, i, elts...)
    if done(r, i)
        return elts
    end
    accumtuple(t, r, i+r.step, elts..., t[i])
end
ref(t::Tuple, r::RangeFrom) = t[Range(r.start,r.step,length(t))]
ref(t::Tuple, r::RangeTo)   = t[Range(1,r.step,r.stop)]
ref(t::Tuple, r::RangeBy)   = t[Range(1,r.step,length(t))]

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

print(x...) = for i=x; print(i); end

expr(hd::Symbol, args...) = Expr(hd, args, Any)

function cell_literal(xs...)
    n = length(xs)
    a = Array(Any,n)
    for i=1:n
        arrayset(a,i,xs[i])
    end
    a
end

symbol(s::String) =
    ccall(dlsym(JuliaDLHandle,"jl_symbol"), Any, (Ptr{Uint8},), s)::Symbol

string(x) =
    ccall(dlsym(JuliaDLHandle,"jl_cstr_to_array"), Any, (Ptr{Int8},),
          ccall(dlsym(JuliaDLHandle,"jl_print_to_string"), Ptr{Int8}, (Any,),
                x))

function print(e::Expr)
    hd = e.head
    if is(hd,`call)
        print(e.args[1], e.args[2:])
    elseif is(hd,`=)
        print(e.args[1], " = ", e.args[2])
    elseif is(hd,`quote)
        print("`", e.args[1])
    elseif is(hd,`null)
        print("()")
    elseif is(hd,`goto)
        print("goto ", e.args[1])
    elseif is(hd,`gotoifnot)
        print("unless ", e.args[1], " goto ", e.args[2])
    elseif is(hd,`label)
        print(e.args[1],": ")
    elseif is(hd,symbol("return"))
        print("return ", e.args[1])
    elseif is(hd,`string)
        print("\"", e.args[1], "\"")
    elseif is(hd,symbol("::"))
        print(e.args[1], "::", e.args[2])
    elseif is(hd,`body) || is(hd,`block)
        print("\nbegin\n")
        for a=e.args
            print("  ", a, "\n")
        end
        print("end\n")
    else
        print(hd, e.args)
    end
    if !is(e.type, Any)
        print("::", e.type)
    end
end

copy(x::Any) = x
copy(x::Tuple) = map(copy, x)
copy(e::Expr) = Expr(e.head, copy(e.args), e.type)
