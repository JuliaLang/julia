# This file is a part of Julia. License is MIT: http://julialang.org/license

# Primes generating functions
#     https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
#     https://en.wikipedia.org/wiki/Wheel_factorization
#     http://primesieve.org
#     Jonathan Sorenson, "An analysis of two prime number sieves", Computer Science Technical Report Vol. 1028, 1991
const wheel         = [4,  2,  4,  2,  4,  6,  2,  6]
const wheel_primes  = [7, 11, 13, 17, 19, 23, 29, 31]
const wheel_indices = [0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7 ]

@inline wheel_index(n) = ( (d, r) = divrem(n - 1, 30); return 8d + wheel_indices[r+1] )
@inline wheel_prime(n) = ( (d, r) = ((n - 1) >>> 3, (n - 1) & 7); return 30d + wheel_primes[r+1] )

function _primesmask(limit::Int)
    limit < 7 && throw(ArgumentError("limit must be at least 7, got $limit"))
    n = wheel_index(limit)
    m = wheel_prime(n)
    sieve = ones(Bool, n)
    @inbounds for i = 1:wheel_index(isqrt(limit))
        if sieve[i]; p = wheel_prime(i)
            q = p * p
            j = (i - 1) & 7 + 1
            while q ≤ m
                sieve[wheel_index(q)] = false
                q = q + wheel[j] * p
                j = j & 7 + 1
            end
        end
    end
    return sieve
end

function _primesmask(lo::Int, hi::Int)
    7 ≤ lo ≤ hi || throw(ArgumentError("the condition 7 ≤ lo ≤ hi must be met"))
    lo == 7 && return _primesmask(hi)
    wlo, whi = wheel_index(lo - 1), wheel_index(hi)
    m = wheel_prime(whi)
    sieve = ones(Bool, whi - wlo)
    small_primes = primes(isqrt(hi))
    @inbounds for i in 4:length(small_primes)
        p = small_primes[i]
        j = wheel_index(2 * div(lo - p - 1, 2p) + 1)
        q = p * wheel_prime(j + 1); j = j & 7 + 1
        while q ≤ m
            sieve[wheel_index(q)-wlo] = false
            q = q + wheel[j] * p
            j = j & 7 + 1
        end
    end
    return sieve
end

# Sieve of the primes up to limit represented as an array of booleans
function primesmask(limit::Int)
    limit < 1 && throw(ArgumentError("limit must be at least 1, got $limit"))
    sieve = falses(limit)
    limit < 2 && return sieve; sieve[2] = true
    limit < 3 && return sieve; sieve[3] = true
    limit < 5 && return sieve; sieve[5] = true
    limit < 7 && return sieve
    wheel_sieve = _primesmask(limit)
    @inbounds for i in eachindex(wheel_sieve)
        Base.unsafe_setindex!(sieve, wheel_sieve[i], wheel_prime(i))
    end
    return sieve
end
primesmask(n::Integer) = n <= typemax(Int) ? primesmask(Int(n)) :
    throw(ArgumentError("requested number of primes must be ≤ $(typemax(Int)), got $n"))

function primesmask(lo::Int, hi::Int)
    0 < lo ≤ hi || throw(ArgumentError("the condition 0 < lo ≤ hi must be met"))
    sieve = falses(hi - lo + 1)
    lo ≤ 2 ≤ hi && (sieve[3-lo] = true)
    lo ≤ 3 ≤ hi && (sieve[4-lo] = true)
    lo ≤ 5 ≤ hi && (sieve[6-lo] = true)
    hi < 7 && return sieve
    wheel_sieve = _primesmask(max(7, lo), hi)
    lsi = lo - 1
    lwi = wheel_index(lsi)
    @inbounds for i in eachindex(wheel_sieve)
        Base.unsafe_setindex!(sieve, wheel_sieve[i], wheel_prime(i + lwi) - lsi)
    end
    return sieve
end
primesmask{T<:Integer}(lo::T, hi::T) = lo <= hi <= typemax(Int) ? primesmask(Int(lo), Int(hi)) :
    throw(ArgumentError("both endpoints of the interval to sieve must be ≤ $(typemax(Int)), got $lo and $hi"))

function primes(n::Int)
    list = Int[]
    n < 2 && return list; push!(list, 2)
    n < 3 && return list; push!(list, 3)
    n < 5 && return list; push!(list, 5)
    n < 7 && return list
    sizehint!(list, floor(Int, n / log(n)))
    sieve = _primesmask(n)
    @inbounds for i in eachindex(sieve)
        sieve[i] && push!(list, wheel_prime(i))
    end
    return list
end

function primes(lo::Int, hi::Int)
    lo ≤ hi || throw(ArgumentError("the condition lo ≤ hi must be met"))
    list = Int[]
    lo ≤ 2 ≤ hi && push!(list, 2)
    lo ≤ 3 ≤ hi && push!(list, 3)
    lo ≤ 5 ≤ hi && push!(list, 5)
    hi < 7 && return list
    sieve = _primesmask(max(7, lo), hi)
    lwi = wheel_index(lo - 1)
    @inbounds for i in eachindex(sieve)
        sieve[i] && push!(list, wheel_prime(i + lwi))
    end
    return list
end

const PRIMES = primes(2^16)

# Small precomputed primes + Miller-Rabin for primality testing:
#     https://en.wikipedia.org/wiki/Miller–Rabin_primality_test
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
#     https://en.wikipedia.org/wiki/Trial_division
#     https://en.wikipedia.org/wiki/Pollard%27s_rho_algorithm
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
