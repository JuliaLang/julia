typealias Index Int32
typealias Size Int32
typealias Char Uint8
typealias String Array{Char,1}

(<:)(T, S) = subtype(T,S)
(>:)(T, S) = subtype(S,T)

ref(t::Tuple, i::Index) = tupleref(t, i)
length(t::Tuple) = tuplelen(t)

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
# these are defined for the fundamental < and == so that if a method is
# not found for e.g. <=, it is translated to < and == first, then promotion
# is handled after.
(<)(x::Number, y::Number)  = (<)(promote(x,y)...)
(==)(x::Number, y::Number) = (==)(promote(x,y)...)

# if arguments are of the same type and no method exists, promotion
# is not applicable. it means the operator is not defined.
no_op_err(name, T) = error(strcat(name," not defined for ",string(T)))
(+){T<:Number}(x::T, y::T) = no_op_err("+", T)
(*){T<:Number}(x::T, y::T) = no_op_err("*", T)
(-){T<:Number}(x::T, y::T) = no_op_err("-", T)
(/){T<:Number}(x::T, y::T) = no_op_err("/", T)
(<){T<:Number}(x::T, y::T) = no_op_err("<", T)
(==){T<:Number}(x::T, y::T) = no_op_err("==", T)

# .<op> defaults to <op>
(./)(x,y) = x/y
(.\)(x,y) = y./x
(.*)(x,y) = x*y

# generic div and % operations
div(x::Number, y::Number) = truncate(x/y)
(%)(x::Number, y::Number) = x-div(x,y)*y
(\)(x,y) = y/x

# general comparisons from == and < operators
!=(x, y) = !(x == y)
> (x, y) = (y < x)
<=(x, y) = (x < y) || (x == y)
>=(x, y) = (x > y) || (x == y)

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

function append(a1::Array{Any,1}, as::Array{Any,1}...)
    n = length(a1) + apply(+,map(length, as))
    a = Array(Any,n)
    for i = 1:length(a1)
        a[i] = a1[i]
    end
    i = length(a1)+1
    for x = as
        for j = 1:length(x)
            a[i] = x[j]
            i += 1
        end
    end
    a
end

print(x...) = for i=x; print(i); end

expr(hd::Symbol, args...) = Expr(hd, {args...}, Any)

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
    for i=1:numel(a)
        arrayset(a,i,xs[i])
    end
    a
end

function print_comma_array(ar, open, close)
    print(open)
    for i=1:length(ar)
        print(ar[i])
        if i < length(ar)
            print(",")
        end
    end
    print(close)
end

function print(e::Expr)
    hd = e.head
    if is(hd,`call)
        print(e.args[1])
        print_comma_array(e.args[2:],"(",")")
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
    elseif is(hd,`symbol)
        print(e.args[1])
    elseif is(hd,`body) || is(hd,`block)
        print("\nbegin\n")
        for a=e.args
            print("  ", a, "\n")
        end
        print("end\n")
    else
        print(hd)
        print_comma_array(e.args,"(",")")
    end
    if !is(e.type, Any)
        if isa(e.type, FuncKind)
            print("::F")
        elseif is(e.type, IntrinsicFunction)
            print("::I")
        else
            print("::", e.type)
        end
    end
end

inspect(x) = print(x)

copy(x::Any) = x
copy(x::Tuple) = map(copy, x)
copy(e::Expr) = Expr(e.head, copy(e.args), e.type)

# timing

clock() = ccall(dlsym(JuliaDLHandle,"clock_now"), Float64, ())

_TIMERS = ()

function tic()
    t0 = clock()
    global _TIMERS = (t0, _TIMERS)
    return t0
end

function _toc(noisy)
    t1 = clock()
    global _TIMERS
    if is(_TIMERS,())
        error("toc() without tic()")
    end
    t0 = _TIMERS[1]
    _TIMERS = _TIMERS[2]
    t = t1-t0
    if noisy
        print("elapsed time: ", t, " sec\n")
    end
    t
end

qtoc() = _toc(false)
toc()  = _toc(true)
