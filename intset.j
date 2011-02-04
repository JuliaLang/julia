type IntSet
    bits::Array{Uint32,1}
    limit::Int32  # todo: should be Int64
    
    IntSet() = IntSet(1024)
    IntSet(max::Int32) = (lim = (max+31) & -32;
                          new(zeros(Uint32,lim>>5), lim))
end

function intset(args...)
    s = IntSet()
    for i = args
        add(s, i)
    end
    s
end

function add(s::IntSet, n::Int)
    if n >= s.limit
        lim = int32(n + div(n,2))
        olsz = length(s.bits)
        newbits = Array(Uint32,(lim+31)>>5)
        newbits[1:olsz] = s.bits
        for i=(olsz+1):length(newbits); newbits[i] = 0; end
        s.bits = newbits
        s.limit = lim
    end
    s.bits[n>>5 + 1] |= (1<<(n&31))
    s
end

function del(s::IntSet, n::Int)
    if n < s.limit
        s.bits[n>>5 + 1] &= ~(1<<(n&31))
    end
    s
end

function has(s::IntSet, n::Int)
    if n >= s.limit
        false
    else
        (s.bits[n>>5 + 1] & (1<<(n&31))) != 0
    end
end

start(s::IntSet) = 0
done(s::IntSet, i) = (next(s,i)[1] >= s.limit)
function next(s::IntSet, i) 
    n = ccall(:bitvector_next, Int32, (Ptr{Uint32}, Uint64, Uint64),
              s.bits, uint64(i), uint64(s.limit))
    (n, n+1)
end

isempty(s::IntSet) =
    ccall(:bitvector_any1, Uint32, (Ptr{Uint32}, Uint32, Uint32),
          s.bits, uint32(0), uint32(s.limit))==0

function choose(s::IntSet)
    n = next(s,0)[1]
    if n >= s.limit
        error("choose: set is empty")
    end
    n
end

numel(s::IntSet) =
    int32(ccall(:bitvector_count, Uint64, (Ptr{Uint32}, Uint32, Uint64),
                s.bits, uint32(0), uint64(s.limit)))

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
