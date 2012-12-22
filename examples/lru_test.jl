load("lru")

const TestLRU = UnboundedLRU{ASCIIString, ASCIIString}()
const TestBLRUs = BoundedLRU{ASCIIString, ASCIIString}(100)
const TestBLRUm = BoundedLRU{ASCIIString, ASCIIString}(1000)
const TestBLRUl = BoundedLRU{ASCIIString, ASCIIString}(10000)
const TestBLRUxl = BoundedLRU{ASCIIString, ASCIIString}(100000)

get_str(i) = ascii(vcat(map(x->[x>>4; x&0x0F], reinterpret(Uint8, [int32(i)]))...))

isbounded{L<:LRU}(::Type{L}) = any(map(n->n==:maxsize, L.names))
isbounded{L<:LRU}(l::L) = isbounded(L)

nmax = int(logspace(2, 5, 4))

println("LRU consistency tests")
for lru in (
            TestLRU,
            TestBLRUs,
            TestBLRUm,
            TestBLRUl,
            TestBLRUxl,
            )
    for n in nmax
        del_all(lru)
        @printf("  %s, %d items\n", lru, n)
        print("    Simple eviction: ")
        for i in 1:n
            str = get_str(i)
            lru[str] = str
            @assert lru.q[1].v == str
            if isbounded(lru) && length(lru) >= lru.maxsize
                tailstr = get_str(i-lru.maxsize+1)
                @assert lru.q[end].v == tailstr
            end
        end
        println("pass")

        print("    Lookup, random access: ")
        for i in 1:n
            str = get_str(randi(n))
            if has(lru, str) # the bounded LRUs can have cache misses
                blah = lru[str]
                @assert lru.q[1].v == blah
            end
        end
        println("pass")
    end
    del_all(lru)
end
