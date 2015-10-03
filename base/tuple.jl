# This file is a part of Julia. License is MIT: http://julialang.org/license

## indexing ##

length(t::Tuple) = nfields(t)
endof(t::Tuple) = length(t)
size(t::Tuple, d) = d==1 ? length(t) : throw(ArgumentError("invalid tuple dimension $d"))
getindex(t::Tuple, i::Int) = getfield(t, i)
getindex(t::Tuple, i::Real) = getfield(t, convert(Int, i))
getindex(t::Tuple, r::AbstractArray) = tuple([t[ri] for ri in r]...)
getindex(t::Tuple, b::AbstractArray{Bool}) = getindex(t,find(b))

## iterating ##

start(t::Tuple) = 1
done(t::Tuple, i::Int) = (length(t) < i)
next(t::Tuple, i::Int) = (t[i], i+1)

# this allows partial evaluation of bounded sequences of next() calls on tuples,
# while reducing to plain next() for arbitrary iterables.
indexed_next(t::Tuple, i::Int, state) = (t[i], i+1)
indexed_next(a::Array, i::Int, state) = (a[i], i+1)
indexed_next(I, i, state) = done(I,state) ? throw(BoundsError()) : next(I, state)

# eltype

eltype(::Type{Tuple{}}) = Bottom
eltype{T,_}(::Type{NTuple{_,T}}) = T

## mapping ##

const ntuple = function(f, n::Integer)
    n<=0 ? () :
    n==1 ? (f(1),) :
    n==2 ? (f(1),f(2),) :
    n==3 ? (f(1),f(2),f(3),) :
    tuple([f(i) for i = 1:Int(n)]...)
end

const _ntuple_tfunc_gensym = gensym()
const _ntuple_tfunc = function(argtypes, argexprs, vtypes, sv)
    n = false
    if isa(argexprs[2], Integer)
        try
            n = Int(argexprs[2])
        end
    end

    Core.Inference.setindex!(vtypes,
        Core.Inference.call(Core.Inference.VarState, Int, false),
        _ntuple_tfunc_gensym)
    F = Core.Inference.abstract_eval_call(
        Expr(:call, argexprs[1], _ntuple_tfunc_gensym),
        vtypes, sv)
    Core.Inference.delete!(vtypes,
        _ntuple_tfunc_gensym)

    if isa(n, Int)
        return NTuple{n, F}
    elseif F === Any
        return Tuple
    else
        return Tuple{Vararg{F}}
    end
end
if isdefined(Core, :Inference) && isdefined(Core.Inference, :add_tfunc)
    Core.Inference.add_tfunc(ntuple, 2, 2, _ntuple_tfunc)
end

# 0 argument function
map(f) = f()
# 1 argument function
map(f, t::Tuple{})              = ()
map(f, t::Tuple{Any,})          = (f(t[1]),)
map(f, t::Tuple{Any, Any})      = (f(t[1]), f(t[2]))
map(f, t::Tuple{Any, Any, Any}) = (f(t[1]), f(t[2]), f(t[3]))
map(f, t::Tuple)                = (f(t[1]), map(f,tail(t))...)
# 2 argument function
map(f, t::Tuple{},        s::Tuple{})        = ()
map(f, t::Tuple{Any,},    s::Tuple{Any,})    = (f(t[1],s[1]),)
map(f, t::Tuple{Any,Any}, s::Tuple{Any,Any}) = (f(t[1],s[1]), f(t[2],s[2]))
# n argument function
heads() = ()
heads(t::Tuple, ts::Tuple...) = (t[1], heads(ts...)...)
tails() = ()
tails(t::Tuple, ts::Tuple...) = (tail(t), tails(ts...)...)
map(f, ::Tuple{}, ts::Tuple...) = ()
map(f, ts::Tuple...) = (f(heads(ts...)...), map(f, tails(ts...)...)...)

## comparison ##

function isequal(t1::Tuple, t2::Tuple)
    if length(t1) != length(t2)
        return false
    end
    for i = 1:length(t1)
        if !isequal(t1[i], t2[i])
            return false
        end
    end
    return true
end

function ==(t1::Tuple, t2::Tuple)
    if length(t1) != length(t2)
        return false
    end
    for i = 1:length(t1)
        if !(t1[i] == t2[i])
            return false
        end
    end
    return true
end

const tuplehash_seed = UInt === UInt64 ? 0x77cfa1eef01bca90 : 0xf01bca90
hash( ::Tuple{}, h::UInt)        = h + tuplehash_seed
hash(x::Tuple{Any,}, h::UInt)    = hash(x[1], hash((), h))
hash(x::Tuple{Any,Any}, h::UInt) = hash(x[1], hash(x[2], hash((), h)))
hash(x::Tuple, h::UInt)          = hash(x[1], hash(x[2], hash(tail(tail(x)), h)))

function isless(t1::Tuple, t2::Tuple)
    n1, n2 = length(t1), length(t2)
    for i = 1:min(n1, n2)
        a, b = t1[i], t2[i]
        if !isequal(a, b)
            return isless(a, b)
        end
    end
    return n1 < n2
end

## functions ##

isempty(x::Tuple{}) = true
isempty(x::Tuple) = false

revargs() = ()
revargs(x, r...) = (revargs(r...)..., x)

reverse(t::Tuple) = revargs(t...)

## specialized reduction ##

# TODO: these definitions cannot yet be combined, since +(x...)
# where x might be any tuple matches too many methods.
sum(x::Tuple{Any, Vararg{Any}}) = +(x...)

# NOTE: should remove, but often used on array sizes
prod(x::Tuple{}) = 1
prod(x::Tuple{Any, Vararg{Any}}) = *(x...)

all(x::Tuple{}) = true
all(x::Tuple{Bool}) = x[1]
all(x::Tuple{Bool, Bool}) = x[1]&x[2]
all(x::Tuple{Bool, Bool, Bool}) = x[1]&x[2]&x[3]
# use generic reductions for the rest

any(x::Tuple{}) = false
any(x::Tuple{Bool}) = x[1]
any(x::Tuple{Bool, Bool}) = x[1]|x[2]
any(x::Tuple{Bool, Bool, Bool}) = x[1]|x[2]|x[3]
