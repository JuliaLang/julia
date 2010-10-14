## indexing ##

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

## iterating ##

start(t::Tuple) = 1
done(t::Tuple, i) = (i > length(t))
next(t::Tuple, i) = (t[i], i+1)

## mapping ##

ntuple(n, f) = n<=0 ? () :
               n==1 ? (f(1),) :
               n==2 ? (f(1),f(2),) :
               n==3 ? (f(1),f(2),f(3),) :
               n==4 ? (f(1),f(2),f(3),f(4),) :
               n==5 ? (f(1),f(2),f(3),f(4),f(5),) :
               tuple(ntuple(n-2,f)..., f(n-1), f(n))

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
map(f, t::(Any,Any,Any), s::(Any,Any,Any)) =
    (f(t[1],s[1]), f(t[2],s[2]), f(t[3],s[3]))
map(f, t::(Any,Any,Any,Any), s::(Any,Any,Any,Any)) =
    (f(t[1],s[1]), f(t[2],s[2]), f(t[3],s[3]), f(t[4],s[4]))
# n argument function
map(f, ts::Tuple...) = ntuple(length(ts[1]), n->f(map(t->t[n],ts)...))

## comparison ##

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

## functions ##

copy(x::Tuple) = map(copy, x)

function append(t1::Tuple, ts::Tuple...)
    if length(ts)==0
        return t1
    end
    return tuple(t1..., append(ts...)...)
end
