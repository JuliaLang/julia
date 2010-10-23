struct IntSet
    bits::Ptr{Uint32}
    limit::Int32  # todo: should be Int64
end

intset(max::Int32) =
    IntSet(ccall(dlsym(JuliaDLHandle,"bitvector_new"), Ptr{Uint32},
                 (Uint64, Int32),
                 uint64(max), 1),
           max)

intset() = intset(1024)

function adjoin(s::IntSet, n::Int)
    if n >= s.limit
        lim = int32(n + div(n,2))
        s.bits = ccall(dlsym(JuliaDLHandle,"bitvector_resize"), Ptr{Uint32},
                       (Ptr{Uint32}, Uint64, Uint64, Int32),
                       s.bits, uint64(s.limit), uint64(lim), 1)
        s.limit = lim
    end
    ccall(dlsym(JuliaDLHandle,"bitvector_set"), Void,
          (Ptr{Uint32}, Uint64, Int32),
          s.bits, uint64(n), 1)
    s
end

function remove(s::IntSet, n::Int)
    if n < s.limit
        ccall(dlsym(JuliaDLHandle,"bitvector_set"), Void,
              (Ptr{Uint32}, Uint64, Int32),
              s.bits, uint64(n), 0)
    end
    s
end

contains(s::Scalar, n::Int) = (s == n)

function contains(s::IntSet, n::Int)
    if n >= s.limit
        false
    else
        (ccall(dlsym(JuliaDLHandle,"bitvector_get"), Int32,
               (Ptr{Uint32}, Uint64),
               s.bits, uint64(n)) != 0)
    end
end

start(s::IntSet) = 0
done(s::IntSet, i) = (next(s,i)[1] >= s.limit)
function next(s::IntSet, i) 
    n = ccall(dlsym(JuliaDLHandle,"bitvector_next"), Int32,
              (Ptr{Uint32}, Uint64, Uint64),
              s.bits, uint64(i), uint64(s.limit))
    (n, n+1)
end

isempty(s::IntSet) = (next(s,0)[1] >= s.limit)

function choose(s::IntSet)
    n = next(s,0)[1]
    if n >= s.limit
        error("choose: set is empty")
    end
    n
end
