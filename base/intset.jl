type IntSet
    bits::Array{Uint32,1}
    limit::Int
    fill1s::Bool

    IntSet() = new(zeros(Uint32,256>>>5), 256, false)
end

IntSet(args...) = (s=IntSet(); for a in args; add!(s,a); end; s)

similar(s::IntSet) = IntSet()

copy(s::IntSet) = union!(IntSet(), s)

function sizehint(s::IntSet, top::Integer)
    if top >= s.limit
        lim = ((top+31) & -32)>>>5
        olsz = length(s.bits)
        if olsz < lim
            resize!(s.bits, lim)
            fill = s.fill1s ? uint32(-1) : uint32(0)
            for i=(olsz+1):lim; s.bits[i] = fill; end
        end
        s.limit = top
    end
    s
end

function add!(s::IntSet, n::Integer)
    if n >= s.limit
        if s.fill1s
            return s
        else
            lim = int(n + div(n,2))
            sizehint(s, lim)
        end
    end
    s.bits[n>>5 + 1] |= (uint32(1)<<(n&31))
    return s
end

function add_each!(s::IntSet, ns)
    for n in ns
        add!(s, n)
    end
    return s
end

function delete!(s::IntSet, n::Integer, deflt)
    if n >= s.limit
        if s.fill1s
            lim = int(n + div(n,2))
            sizehint(s, lim)
        else
            return deflt
        end
    end
    mask = uint32(1)<<(n&31)
    idx = n>>5 + 1
    b = s.bits[idx]
    if (b&mask)==0; return deflt; end
    s.bits[idx] = b&~mask
    return n
end

function delete!(s::IntSet, n::Integer)
    if delete!(s, n, n+1) == n+1
        throw(KeyError(n))
    end
    return n
end

function del_each!(s::IntSet, ns)
    for n in ns
        delete!(s, n)
    end
    return s
end

setdiff(a::IntSet, b::IntSet) = del_each!(copy(a),b)
symdiff(a::IntSet, b::IntSet) = a $ b

function empty!(s::IntSet)
    s.bits[:] = 0
    return s
end

function symdiff!(s::IntSet, n::Integer)
    if n >= s.limit
        lim = int(n + dim(n,2))
        sizehint(s, lim)
    end
    s.bits[n>>5 + 1] $= (uint32(1)<<(n&31))
    return s
end

function symdiff!(s::IntSet, ns)
   for n in ns
       symdiff!(s, n)
   end
   return s
end

function copy!(to::IntSet, from::IntSet)
    empty!(to)
    union!(to, from)
end

function has(s::IntSet, n::Integer)
    if n >= s.limit
        s.fill1s && n >= 0
    else
        (s.bits[n>>5 + 1] & (uint32(1)<<(n&31))) != 0
    end
end

start(s::IntSet) = int64(0)
done(s::IntSet, i) = (!s.fill1s && next(s,i)[1] >= s.limit) || i == typemax(Int)
function next(s::IntSet, i)
    if i >= s.limit
        n = i
    else
        n = ccall(:bitvector_next, Int64, (Ptr{Uint32}, Uint64, Uint64), s.bits, i, s.limit)
    end
    (n, n+1)
end

isempty(s::IntSet) =
    !s.fill1s && ccall(:bitvector_any1, Uint32, (Ptr{Uint32}, Uint64, Uint64), s.bits, 0, s.limit)==0

function first(s::IntSet)
    n = next(s,0)[1]
    if n >= s.limit
        error("first: set is empty")
    end
    return n
end

shift!(s::IntSet) = delete!(s, first(s))

length(s::IntSet) = int(ccall(:bitvector_count, Uint64, (Ptr{Uint32}, Uint64, Uint64), s.bits, 0, s.limit)) +
    (s.fill1s ? typemax(Int) - s.limit : 0)

function show(io::IO, s::IntSet)
    print(io, "IntSet(")
    first = true
    for n in s
        if n > s.limit
            break
        end
        if !first
            print(io, ", ")
        end
        print(io, n)
        first = false
    end
    if s.fill1s
        print(io, ", ..., ", typemax(Int)-1, ")")
    else
        print(io, ")")
    end
end


# Math functions
function union!(s::IntSet, s2::IntSet)
    if s2.limit > s.limit
        sizehint(s, s2.limit)
    end
    lim = length(s2.bits)
    for n = 1:lim
        s.bits[n] |= s2.bits[n]
    end
    if s2.fill1s
        for n=lim+1:length(s.bits)
            s.bits[n] = uint32(-1)
        end
    end
    s.fill1s |= s2.fill1s
    s
end

union(s1::IntSet, s2::IntSet) = (s1.limit >= s2.limit ? union!(copy(s1), s2) : union!(copy(s2), s1))

add_each!(s::IntSet, s2::IntSet) = union!(s, s2)

function intersect!(s::IntSet, s2::IntSet)
    if s2.limit > s.limit
        sizehint(s, s2.limit)
    end
    lim = length(s2.bits)
    for n = 1:lim
        s.bits[n] &= s2.bits[n]
    end
    if !s2.fill1s   
        for n=lim+1:length(s.bits)
            s.bits[n] = uint32(0)
        end
    end
    s.fill1s &= s2.fill1s
    s
end

intersect(s1::IntSet, s2::IntSet) = (s1.limit >= s2.limit ? intersect!(copy(s1), s2) : intersect!(copy(s2), s1))

function complement!(s::IntSet)
    for n = 1:length(s.bits)
        s.bits[n] = ~s.bits[n]
    end
    s.fill1s = !s.fill1s
    s
end

complement(s::IntSet) = complement!(copy(s))

function symdiff!(s::IntSet, s2::IntSet)
    if s2.limit > s.limit
        sizehint(s, s2.limit)
    end
    lim = length(s2.bits)
    for n = 1:lim
        s.bits[n] $= s2.bits[n]
    end
    if s2.fill1s
        for n=lim+1:length(s.bits)
            s.bits[n] = ~s.bits[n]
        end
    end
    s.fill1s $= s2.fill1s
    s
end

($)(s1::IntSet, s2::IntSet) =
    (s1.limit >= s2.limit ? symdiff!(copy(s1), s2) : symdiff!!(copy(s2), s1))

|(s::IntSet, s2::IntSet) = union(s, s2)
(&)(s::IntSet, s2::IntSet) = intersect(s, s2)
-(a::IntSet, b::IntSet) = setdiff(a,b)
~(s::IntSet) = complement(s)

function isequal(s1::IntSet, s2::IntSet)
    if s1.fill1s != s2.fill1s
        return false
    end
    lim1 = length(s1.bits)
    lim2 = length(s2.bits)
    for i = 1:min(lim1,lim2)
        if s1.bits[i] != s2.bits[i]
            return false
        end
    end
    filln = s1.fill1s ? uint32(-1) : uint32(0)
    if lim1 > lim2
        for i = lim2:lim1
            if s1.bits[i] != filln
                return false
            end
        end
    else
        for i = lim1:lim2
            if s2.bits[i] != filln
                return false
            end
        end
    end
    return true
end
