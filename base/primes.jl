function primesmask(n::Int)
    s = falses(n)
    n < 2 && return s; s[2] = true
    n < 3 && return s; s[3] = true
    r = ifloor(sqrt(n))
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
function primesmask(n::Integer)
    n <= typemax(Int) || error("primesmask: you want WAY too many primes ($n)")
    primesmask(int(n))
end

primes(n::Integer) = find(primesmask(n))

function isprime(n::Integer)
    n == 2 && return true
    n <= 2 | iseven(n) && return false
    s = trailing_zeros(n-1)
    d = (n-1) >>> s
    for a in witnesses(n)
        a < n || break
        x = powermod(a,d,n)
        x == 1 && continue
        t = s
        while x != n-1
            (t-=1) <= 0 && return false
            x = x*x % n
            x == 1 && return false
        end
    end
    return true
end
witnesses(n::Union(Uint8,Int8,Uint16,Int16)) = (2,3)
witnesses(n::Union(Uint32,Int32)) = n < 1373653 ? (2,3) : (2,7,61)
witnesses(n::Union(Uint64,Int64)) =
        n < 1373653         ? (2,3) :
        n < 4759123141      ? (2,7,61) :
        n < 2152302898747   ? (2,3,5,7,11) :
        n < 3474749660383   ? (2,3,5,7,11,13) :
                              (2,325,9375,28178,450775,9780504,1795265022)

# TODO: replace this factorization routine

const PRIMES = primes(10000)

function factor{T<:Integer}(n::T)
    0 < n || error("factor: number to be factored must be positive")
    h = (T=>Int)[]
    n == 1 && return h
    n <= 3 && (h[n] = 1; return h)
    local s::T, p::T
    s = isqrt(n)
    for p in PRIMES
        p <= s || break
        if n % p == 0
            while n % p == 0
                h[p] = get(h,p,0)+1
                n = div(n,p)
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
                n = div(n,p)
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
