# This file is a part of Julia. License is MIT: http://julialang.org/license

# Sieve of Atkin for generating primes:
#     http://en.wikipedia.org/wiki/Sieve_of_Atkin
# Code very loosely based on this:
#     http://thomasinterestingblog.wordpress.com/2011/11/30/generating-primes-with-the-sieve-of-atkin-in-c/
#     http://dl.dropboxusercontent.com/u/29023244/atkin.cpp
#
function primesmask(sieve::AbstractVector{Bool})
    @inbounds begin
        limit = length(sieve)
        limit < 2 && return sieve; sieve[2] = true
        limit < 3 && return sieve; sieve[3] = true
        r = isqrt(limit)    # highest factor
        for x = 1:r; x² = x*x
            for y = 1:r; y² = y*y
                n = 4x² + y²
                if n ≤ limit && (n % 12 == 1 || n % 12 == 5)
                    sieve[n] = !sieve[n]
                end
                n = 3x² + y²
                if n ≤ limit && n % 12 == 7
                    sieve[n] = !sieve[n]
                end
                n = 3x² - y²
                if x > y && n ≤ limit && n % 12 == 11
                    sieve[n] = !sieve[n]
                end
            end
        end
        for i = 5:r; i² = i*i
            if sieve[i]
                for j = i²:i²:limit
                    sieve[j] = false
                end
            end
        end
    end
    return sieve
end

"Returns a collection of the prime numbers ≤ `limit`."
primes(limit::Integer) = limit <= typemax(Int) ? find(primesmask(falses(Int(limit)))) :
    throw(ArgumentError("requested number of primes must be ≤ $(typemax(Int)), got $limit"))

const PRIMES = primes(2^16)

# Small precomputed primes + Miller-Rabin for primality testing:
#     http://en.wikipedia.org/wiki/Miller–Rabin_primality_test
#
function isprime(n::Integer)
    (n < 3 || iseven(n)) && return n == 2
    n <= 2^16 && return PRIMES[searchsortedlast(PRIMES,n)] == n
    s = trailing_zeros(n-1)
    d = (n-1) >>> s
    for a in witnesses(n)
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
witnesses(n::Union{UInt8,Int8,UInt16,Int16}) = (2,3)
witnesses(n::Union{UInt32,Int32}) = n < 1373653 ? (2,3) : (2,7,61)
witnesses(n::Union{UInt64,Int64}) =
        n < 1373653         ? (2,3) :
        n < 4759123141      ? (2,7,61) :
        n < 2152302898747   ? (2,3,5,7,11) :
        n < 3474749660383   ? (2,3,5,7,11,13) :
                              (2,325,9375,28178,450775,9780504,1795265022)

isprime(n::UInt128) =
    n <= typemax(UInt64) ? isprime(UInt64(n)) : isprime(BigInt(n))
isprime(n::Int128) = n < 2 ? false :
    n <= typemax(Int64)  ? isprime(Int64(n))  : isprime(BigInt(n))


# Trial division of small (< 2^16) precomputed primes +
# Pollard rho's algorithm with Richard P. Brent optimizations
#     http://en.wikipedia.org/wiki/Trial_division
#     http://en.wikipedia.org/wiki/Pollard%27s_rho_algorithm
#     http://maths-people.anu.edu.au/~brent/pub/pub051.html
#
function factor{T<:Integer}(n::T)
    0 < n || throw(ArgumentError("number to be factored must be ≥ 0, got $n"))
    h = Dict{T,Int}()
    n == 1 && return h
    isprime(n) && (h[n] = 1; return h)
    local p::T
    for p in PRIMES
        if n % p == 0
            h[p] = get(h,p,0)+1
            n = div(n,p)
            while n % p == 0
                h[p] = get(h,p,0)+1
                n = div(n,p)
            end
            n == 1 && return h
            isprime(n) && (h[n] = 1; return h)
        end
    end
    T <: BigInt || widemul(n-1,n-1) <= typemax(n) ? pollardfactors!(n, h) : pollardfactors!(widen(n), h)
end

function pollardfactors!{T<:Integer,K<:Integer}(n::T, h::Dict{K,Int})
    while true
        local c::T = rand(1:(n-1)), G::T = 1, r::K = 1, y::T = rand(0:(n-1)), m::K = 1900
        local ys::T, q::T = 1, x::T
        while c == n - 2
            c = rand(1:(n-1))
        end
        while G == 1
            x = y
            for i in 1:r
                y = (y*y)%n
                y = (y+c)%n
            end
            local k::K = 0
            G = 1
            while k < r && G == 1
                for i in 1:(m>(r-k)?(r-k):m)
                    ys = y
                    y = (y*y)%n
                    y = (y+c)%n
                    q = (q*(x>y?x-y:y-x))%n
                end
                G = gcd(q,n)
                k = k + m
            end
            r = 2 * r
        end
        G == n && (G = 1)
        while G == 1
            ys = (ys*ys)%n
            ys = (ys+c)%n
            G = gcd(x>ys?x-ys:ys-x,n)
        end
        if G != n
            isprime(G) ? h[G] = get(h,G,0) + 1 : pollardfactors!(G,h)
            G2 = div(n,G)
            isprime(G2) ? h[G2] = get(h,G2,0) + 1 : pollardfactors!(G2,h)
            return h
        end
    end
end
