# This file is a part of Julia. License is MIT: http://julialang.org/license

using .LRUExample

TestLRU = LRUExample.UnboundedLRU{String, String}()
TestBLRU = LRUExample.BoundedLRU{String, String}(1000)

get_str(i) = String(vcat(map(x->[x>>4; x&0x0F], reinterpret(UInt8, [Int32(i)]))...))

isbounded{L<:LRUExample.LRU}(::Type{L}) = any(map(n->n==:maxsize, fieldnames(L)))
isbounded{L<:LRUExample.LRU}(l::L) = isbounded(L)

nmax = round.(Int, logspace(2, 5, 4))

function lrutest()
    #println("LRU consistency tests")
    for lru in (TestLRU,TestBLRU)
        for n in nmax
            empty!(lru)
            #@printf("  %s, %d items\n", lru, n)
            #print("    Simple eviction: ")
            for i in 1:n
                str = get_str(i)
                lru[str] = str
                @assert lru.q[1].v == str
                if isbounded(lru) && length(lru) >= lru.maxsize
                    tailstr = get_str(i-lru.maxsize+1)
                    @assert lru.q[end].v == tailstr
                end
            end
            #println("pass")

            #print("    Lookup, random access: ")
            for i in 1:n
                str = get_str(rand(1:n))
                if haskey(lru, str) # the bounded LRUs can have cache misses
                    blah = lru[str]
                    @assert lru.q[1].v == blah
                end
            end
            #println("pass")
        end
        empty!(lru)
    end
end

lrutest()
