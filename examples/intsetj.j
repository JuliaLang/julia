lomask(n::Integer) = lomask(uint32(n))
lomask(n::Uint32) = ((1<<n)-1)
himask(n::Integer) = himask(uint32(n))
himask(n::Uint32) = (~lomask(32-n))

# This appears to be actually the same speed as the one below
#start(s::IntSet) = IntSet(s)
#done(s::IntSet, i) = isempty(i)
#function next(s::IntSet, i)
#	#n = ccall(:bitvector_next, Int64, (Ptr{Uint32}, Uint64, Uint64),
#	#          s.bits, uint64(first), uint64(s.limit))
#	#return (n, n+1)
#	n = bitvector_next(i.bits, uint64(0), uint64(i.limit))::Int64
#	del(i, n)
#	return (n, i)
#end

start(s::IntSet) = int64(0)
done(s::IntSet, i) = (next(s,i)[1] >= s.limit)
function next(s::IntSet, i)
    n = bitvector_next(s.bits, uint64(i), uint64(s.limit))
    (n, n+1)
end


function bitvector_next(b::Array{Uint32,1}, n0::Uint64, n::Uint64)
	local w::Uint32, i::Uint32, nb::Uint32, nw::Uint32
	i = n0>>5
	nb = n0&31
	nw = (n+31)>>5
	if i < nw-1 || (n&31)==0
		w = b[i+1]>>nb
	else
		w = (b[i+1]&lomask(n&31))>>nb
	end
	if w != 0
		nxt = int64(cttz_int(unbox32(w))::Int32 + n0)
		return nxt
	end
	if i == nw-1
		nxt = int64(n)
		return nxt
	end
	i += 1
	while i < nw-1
		w = b[i+1]
		if w != 0
			nxt = int64(cttz_int(unbox32(w))::Int32 + i<<5)
			return nxt
		end
		i += 1
	end
	w = b[i+1]
	nb = n & 31
	i = int64(cttz_int(unbox32(w))::Int32)
	if nb == 0
		nxt = int64(i + (n-32))
		return nxt
	end
	if i >= nb
		nxt = int64(n)
		return nxt
	end
	nxt = int64(i + (n-nb))
	return nxt
end

function isempty(s::IntSet)
	#ccall(:bitvector_any1, Uint32, (Ptr{Uint32}, Uint64, Uint64),
	#      s.bits, uint64(0), uint64(s.limit))==0
	!bitvector_any1(s.bits, uint64(0), uint64(s.limit))
end

function bitvector_any1(b::Array{Uint32,1}, offs::Uint64, nbits::Uint64)
    local nw::Uint32, tail::Uint32, mask::Uint32

    if (nbits == 0)
		return false;
	end
    nw = (offs+nbits+31)>>5;

    if (nw == 1)
        if (nbits == 32)
            mask = (uint32(0xffffffff)<<offs)
        else
            mask = (lomask(nbits)<<offs)
		end
		if ((b[1] & mask) != 0)
			return true
		end
        return false
	end

    mask = ~lomask(offs)
    if ((b[1] & mask) != 0)
		return true
	end

    for i = 1:nw-1
        if (b[i+1] != 0)
			return true
		end
	end

    tail = (offs+nbits)&31
    if (tail==0)
        if (b[nw] != 0)
			return true
		end
    else
        mask = lomask(tail);
        if ((b[nw] & mask) != 0)
			return true
		end
	end
    return false
end

function choose(s::IntSet)
    n = bitvector_next(s.bits, uint64(0), uint64(s.limit))::Int64
    if n >= s.limit
        error("choose: set is empty")
    end
    return n
end

length(s::IntSet) = numel(s)
numel(s::IntSet) =
    int32(ccall(:bitvector_count, Uint64, (Ptr{Uint32}, Uint64, Uint64),
                s.bits, uint64(0), uint64(s.limit)))

