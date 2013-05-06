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
primes(n::Int) = find(primesmask(n))

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
@eval begin
    witnesses(n::Union(Uint32,Int32)) = n < 1373653 ?
        $(map(int32,(2,3))) :
        $(map(int32,(2,7,61)))
    witnesses(n::Union(Uint64,Int64)) = n < 341550071728321 ?
        $(map(int64,(2,3,5,7,11,13,17))) :
        $(map(int64,(2,325,9375,28178,450775,9780504,1795265022)))
end

# TODO: replace this factorization routine

function factor{T<:Integer}(n::T)
    if n <= 0
        error("factor: number to be factored must be positive")
    end
    h = (T=>Int)[]
    if n == 1 return h end
    local p::T
    s = ifloor(sqrt(n))
    P = primes(n)
    for p in P
        if p > s
            break
        end
        if n % p == 0
            while n % p == 0
                h[p] = get(h,p,0)+1
                n = div(n,p)
            end
            if n == 1
                return h
            end
            s = ifloor(sqrt(n))
        end
    end
    p = P[end]+2
    while p <= s
        if n % p == 0
            while n % p == 0
                h[p] = get(h,p,0)+1
                n = div(n,p)
            end
            if n == 1
                return h
            end
            s = ifloor(sqrt(n))
        end
        p += 2
    end
    h[n] = 1
    return h
end
