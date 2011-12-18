type IntSet
    bits::Array{Uint32,1}
    limit::Int

    IntSet() = IntSet(1024)
    IntSet(max::Integer) = (lim = (max+31) & -32;
                        new(zeros(Uint32,lim>>>5), lim))
end
function IntSet(s::IntSet)
	s2::IntSet = IntSet(s.limit)
	or!(s2, s)
	s2
end
intset(args...) = add_each(IntSet(), args)

function grow(s::IntSet, max::Integer)
	if max >= s.limit
        olsz = length(s.bits)
        newbits = Array(Uint32,(max+31)>>>5)
        newbits[1:olsz] = s.bits
        for i=(olsz+1):length(newbits); newbits[i] = 0; end
        s.bits = newbits
        s.limit = max
	end
	s.limit
end

function add(s::IntSet, n::Integer)
    if n >= s.limit
        lim = int(n + div(n,2))
		resize(s, lim)
	end
    s.bits[n>>5 + 1] |= (1<<(n&31))
    return s
end

function add_each(s::IntSet, ns)
    for n = ns
        add(s, n)
    end
    return s
end

function del(s::IntSet, n::Integer)
    if n < s.limit
        s.bits[n>>5 + 1] &= ~(1<<(n&31))
    end
    return s
end

function del_each(s::IntSet, ns)
    for n = ns
        del(s, n)
    end
    return s
end

function del_all(s::IntSet)
    s.bits[:] = 0
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
        (s.bits[n>>5 + 1] & (1<<(n&31))) != 0
    end
end

start(s::IntSet) = int64(0)
done(s::IntSet, i) = (next(s,i)[1] >= s.limit)
function next(s::IntSet, i)
    n = ccall(:bitvector_next, Int64, (Ptr{Uint32}, Uint64, Uint64),
              s.bits, uint64(i), uint64(s.limit))
    (n, n+1)
end

isempty(s::IntSet) =
    ccall(:bitvector_any1, Uint32, (Ptr{Uint32}, Uint64, Uint64),
          s.bits, uint64(0), uint64(s.limit))==0

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

length(s::IntSet) = numel(s)
numel(s::IntSet) =
    int32(ccall(:bitvector_count, Uint64, (Ptr{Uint32}, Uint64, Uint64),
                s.bits, uint64(0), uint64(s.limit)))

function show(s::IntSet)
    print("intset(")
    first = true
    for n = s
        if !first
            print(", ")
        end
        print(n)
        first = false
    end
    print(")")
end


# Math functions
function or!(s::IntSet, s2::IntSet)
	if s2.limit > s.limit
		grow(s2, s.limit)
	end
	for n = 1:(s2.limit+31)>>>5
		s.bits[n] |= s2.bits[n]
	end
	s
end

function and!(s::IntSet, s2::IntSet)
	if s2.limit > s.limit
		grow(s2, s.limit)
	end
	for n = 1:(s2.limit+31)>>>5
		s.bits[n] &= s2.bits[n]
	end
	s
end

function not!(s::IntSet)
	for n = 1:(s.limit+31)>>>5
		s.bits[n] = ~s.bits[n]
	end
	s
end

function xor!(s::IntSet, s2::IntSet)
	if s2.limit > s.limit
		grow(s2, s.limit)
	end
	for n = 1:(s2.limit+31)>>>5
		s.bits[n] $= s2.bits[n]
	end
	s
end


|(s1::IntSet, s2::IntSet) = (s1.limit >= s2.limit ? or!(IntSet(s1), s2) : or!(IntSet(s2), s1))
&(s1::IntSet, s2::IntSet) = (s1.limit >= s2.limit ? and!(IntSet(s1), s2) : and!(IntSet(s2), s1))
~(s::IntSet) = not!(IntSet(s))
($)(s1::IntSet, s2::IntSet) = (s1.limit >= s2.limit ? xor!(IntSet(s1), s2) : xor!(IntSet(s2), s1))



