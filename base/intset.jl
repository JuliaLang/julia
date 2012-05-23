type IntSet
    bits::Array{Uint32,1}
    limit::Int
    fill1s::Bool

    IntSet() = IntSet(1024)
    IntSet(top::Integer) = (lim = (top+31) & -32;
                            new(zeros(Uint32,lim>>>5), top, false))
end
function IntSet(s::IntSet)
    s2::IntSet = IntSet(s.limit)
    s2.fill1s = s.fill1s
    or!(s2, s)
    s2
end
intset(args...) = add_each(IntSet(), args)

function grow(s::IntSet, top::Integer)
    if top >= s.limit
        lim = ((top+31) & -32)>>>5
        olsz = length(s.bits)
        if olsz < lim
            grow(s.bits, lim-olsz)
            fill = s.fill1s ? uint32(-1) : uint32(0)
            for i=(olsz+1):lim; s.bits[i] = fill; end
        end
        s.limit = top
    end
    s.limit
end

function add(s::IntSet, n::Integer)
    if n >= s.limit
        lim = int(n + div(n,2))
        grow(s, lim)
    end
    s.bits[n>>5 + 1] |= (uint32(1)<<(n&31))
    return s
end

function add_each(s::IntSet, ns)
    for n in ns
        add(s, n)
    end
    return s
end

function del(s::IntSet, n::Integer)
    if n < s.limit
        s.bits[n>>5 + 1] &= ~(uint32(1)<<(n&31))
    end
    return s
end

function del_each(s::IntSet, ns)
    for n in ns
        del(s, n)
    end
    return s
end

function del_all(s::IntSet)
    s.bits[:] = 0
    return s
end

function toggle(s::IntSet, n::Integer)
    if n >= s.limit
        lim = int(n + dim(n,2))
        grow(s, lim)
    end
    s.bits[n>>5 + 1] $= (uint32(1)<<(n&31))
   return s
end

function toggle_each(s::IntSet, ns)
   for n in ns
       toggle(s, n)
   end
   return s
end

function copy_to!(to::IntSet, from::IntSet)
    del_all(to)
    or!(to, from)
end

function has(s::IntSet, n::Integer)
    if n >= s.limit
        false
    else
        (s.bits[n>>5 + 1] & (uint32(1)<<(n&31))) != 0
    end
end

start(s::IntSet) = int64(0)
done(s::IntSet, i) = (next(s,i)[1] >= s.limit)
function next(s::IntSet, i)
    n = ccall(:bitvector_next, Int64, (Ptr{Uint32}, Uint64, Uint64), s.bits, i, s.limit)
    (n, n+1)
end

isempty(s::IntSet) =
    ccall(:bitvector_any1, Uint32, (Ptr{Uint32}, Uint64, Uint64), s.bits, 0, s.limit)==0

function choose(s::IntSet)
    n = next(s,0)[1]
    if n >= s.limit
        error("choose: set is empty")
    end
    return n
end

function pop(s::IntSet)
    n = choose(s)
    del(s, n)
    n
end

length(s::IntSet) =
    int(ccall(:bitvector_count, Uint64, (Ptr{Uint32}, Uint64, Uint64), s.bits, 0, s.limit))

function show(io, s::IntSet)
    print(io, "intset(")
    first = true
    for n in s
        if !first
            print(io, ", ")
        end
        print(io, n)
        first = false
    end
    if s.fill1s
        print(io, ", ...)")
    else
        print(io, ")")
    end
end


# Math functions
function or!(s::IntSet, s2::IntSet)
    if s2.limit > s.limit
        grow(s, s2.limit)
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

add_each(s::IntSet, s2::IntSet) = or!(s, s2)

function and!(s::IntSet, s2::IntSet)
    if s2.limit > s.limit
        grow(s, s2.limit)
    end
    lim = length(s2.bits)
    for n = 1:lim
        s.bits[n] &= s2.bits[n]
    end
    if ~s2.fill1s   
        for n=lim+1:length(s.bits)
            s.bits[n] = uint32(0)
        end
    end
    s.fill1s &= s2.fill1s
    s
end

function not!(s::IntSet)
    for n = 1:length(s.bits)
        s.bits[n] = ~s.bits[n]
    end
    s.fill1s = ~s.fill1s
    s
end

function xor!(s::IntSet, s2::IntSet)
    if s2.limit > s.limit
        grow(s, s2.limit)
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

~(s::IntSet) = not!(IntSet(s))
|(s1::IntSet, s2::IntSet) = (s1.limit >= s2.limit ? or!(IntSet(s1), s2) : or!(IntSet(s2), s1))
(&)(s1::IntSet, s2::IntSet) = (s1.limit >= s2.limit ? and!(IntSet(s1), s2) : and!(IntSet(s2), s1))
($)(s1::IntSet, s2::IntSet) = (s1.limit >= s2.limit ? xor!(IntSet(s1), s2) : xor!(IntSet(s2), s1))

union!(s1::IntSet, s2::IntSet) = or!(s1, s2)
union(s1::IntSet, s2::IntSet) = s1 | s2
intersection!(s1::IntSet, s2::IntSet) = and!(s1, s2)
intersection(s1::IntSet, s2::IntSet) = s1 & s2
complement!(s1::IntSet) = not!(s1)
complement(s1::IntSet) = ~s1

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
