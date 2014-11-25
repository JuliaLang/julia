# Sieve of Atkin for generating primes:
#     http://en.wikipedia.org/wiki/Sieve_of_Atkin
# Code very loosely based on this:
#     http://thomasinterestingblog.wordpress.com/2011/11/30/generating-primes-with-the-sieve-of-atkin-in-c/
#     http://dl.dropboxusercontent.com/u/29023244/atkin.cpp
#
function primesmask(s::AbstractVector{Bool})
    n = length(s)
    n < 2 && return s; s[2] = true
    n < 3 && return s; s[3] = true
    r = floor(Int,sqrt(n))
    for x = 1:r
        xx = x*x
        for y = 1:r
            yy = y*y
            i, j, k = 4xx+yy, 3xx+yy, 3xx-yy
            i <= n && (s[i] $= (i%12==1)|(i%12==5))
            j <= n && (s[j] $= (j%12==7))
            1 <= k <= n && (s[k] $= (x>y)&(k%12==11))
        end
    end
    for i = 5:r
        s[i] && (s[i*i:i*i:n] = false)
    end
    return s
end
primesmask(n::Int) = primesmask(falses(n))
primesmask(n::Integer) = n <= typemax(Int) ? primesmask(int(n)) :
    error("you want WAY too many primes ($n)")

primes(n::Union(Integer,AbstractVector{Bool})) = find(primesmask(n))

# Miller-Rabin for primality testing:
#     http://en.wikipedia.org/wiki/Miller–Rabin_primality_test
#
function isprime(n::Integer)
    n == 2 && return true
    (n < 2) | iseven(n) && return false
    s = trailing_zeros(n-1)
    d = (n-1) >>> s
    for a in witnesses(n)
        a < n || break
        x = powermod(a,d,n)
        x == 1 && continue
        t = s
        while x != n-1
            (t-=1) <= 0 && return false
            x = oftype(n, widemul(x,x) % n)
            x == 1 && return false
        end
    end
    return true
end

# Miller-Rabin witness choices based on:
#     http://mathoverflow.net/questions/101922/smallest-collection-of-bases-for-prime-testing-of-64-bit-numbers
#     http://primes.utm.edu/prove/merged.html
#     http://miller-rabin.appspot.com
#
witnesses(n::Union(UInt8,Int8,UInt16,Int16)) = (2,3)
witnesses(n::Union(UInt32,Int32)) = n < 1373653 ? (2,3) : (2,7,61)
witnesses(n::Union(UInt64,Int64)) =
        n < 1373653         ? (2,3) :
        n < 4759123141      ? (2,7,61) :
        n < 2152302898747   ? (2,3,5,7,11) :
        n < 3474749660383   ? (2,3,5,7,11,13) :
                              (2,325,9375,28178,450775,9780504,1795265022)

isprime(n::UInt128) =
    n <= typemax(UInt64) ? isprime(uint64(n)) : isprime(BigInt(n))
isprime(n::Int128) = n < 2 ? false :
    n <= typemax(Int64)  ? isprime(int64(n))  : isprime(BigInt(n))

# TODO: faster factorization algorithms?

const PRIMES = primes(10000)

function factor{T<:Integer}(n::T)
    0 < n || error("number to be factored must be positive")
    h = Dict{T,Int}()
    n == 1 && return h
    n <= 3 && (h[n] = 1; return h)
    local s::T, p::T
    s = isqrt(n)
    for p in PRIMES
        if p > s
            h[n] = 1
            return h
        end
        if n % p == 0
            while n % p == 0
                h[p] = get(h,p,0)+1
                n = oftype(n, div(n,p))
            end
            n == 1 && return h
            s = isqrt(n)
        end
    end
    p = PRIMES[end]+2
    while p <= s
        if n % p == 0
            while n % p == 0
                h[p] = get(h,p,0)+1
                n = oftype(n, div(n,p))
            end
            if n == 1
                return h
            end
            s = isqrt(n)
        end
        p += 2
    end
    h[n] = 1
    return h
end
