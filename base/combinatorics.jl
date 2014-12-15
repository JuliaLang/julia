const _fact_table64 =
    Int64[1,2,6,24,120,720,5040,40320,362880,3628800,39916800,479001600,6227020800,
          87178291200,1307674368000,20922789888000,355687428096000,6402373705728000,
          121645100408832000,2432902008176640000]

const _fact_table128 =
    UInt128[0x00000000000000000000000000000001, 0x00000000000000000000000000000002,
            0x00000000000000000000000000000006, 0x00000000000000000000000000000018,
            0x00000000000000000000000000000078, 0x000000000000000000000000000002d0,
            0x000000000000000000000000000013b0, 0x00000000000000000000000000009d80,
            0x00000000000000000000000000058980, 0x00000000000000000000000000375f00,
            0x00000000000000000000000002611500, 0x0000000000000000000000001c8cfc00,
            0x0000000000000000000000017328cc00, 0x0000000000000000000000144c3b2800,
            0x00000000000000000000013077775800, 0x00000000000000000000130777758000,
            0x00000000000000000001437eeecd8000, 0x00000000000000000016beecca730000,
            0x000000000000000001b02b9306890000, 0x000000000000000021c3677c82b40000,
            0x0000000000000002c5077d36b8c40000, 0x000000000000003ceea4c2b3e0d80000,
            0x000000000000057970cd7e2933680000, 0x00000000000083629343d3dcd1c00000,
            0x00000000000cd4a0619fb0907bc00000, 0x00000000014d9849ea37eeac91800000,
            0x00000000232f0fcbb3e62c3358800000, 0x00000003d925ba47ad2cd59dae000000,
            0x0000006f99461a1e9e1432dcb6000000, 0x00000d13f6370f96865df5dd54000000,
            0x0001956ad0aae33a4560c5cd2c000000, 0x0032ad5a155c6748ac18b9a580000000,
            0x0688589cc0e9505e2f2fee5580000000, 0xde1bc4d19efcac82445da75b00000000]

function factorial_lookup(n::Integer, table, lim)
    n < 0 && throw(DomainError())
    n > lim && throw(OverflowError())
    n == 0 && return one(n)
    @inbounds f = table[n]
    return oftype(n, f)
end

factorial(n::Int128) = factorial_lookup(n, _fact_table128, 33)
factorial(n::UInt128) = factorial_lookup(n, _fact_table128, 34)
factorial(n::Union(Int64,UInt64)) = factorial_lookup(n, _fact_table64, 20)

if Int === Int32
factorial(n::Union(Int8,UInt8,Int16,UInt16)) = factorial(int32(n))
factorial(n::Union(Int32,UInt32)) = factorial_lookup(n, _fact_table64, 12)
else
factorial(n::Union(Int8,UInt8,Int16,UInt16,Int32,UInt32)) = factorial(int64(n))
end

function gamma(n::Union(Int8,UInt8,Int16,UInt16,Int32,UInt32,Int64,UInt64))
    n < 0 && throw(DomainError())
    n == 0 && return Inf
    n <= 2 && return 1.0
    n > 20 && return gamma(float64(n))
    @inbounds return float64(_fact_table64[n-1])
end

function factorial(n::Integer)
    n < 0 && throw(DomainError())
    local f::typeof(n*n), i::typeof(n*n)
    f = 1
    for i = 2:n
        f *= i
    end
    return f
end

# computes n!/k!
function factorial{T<:Integer}(n::T, k::T)
    if k < 0 || n < 0 || k > n
        return zero(T)
    end
    f = one(T)
    while n > k
        f *= n
        n -= 1
    end
    return f
end

function binomial{T<:Integer}(n::T, k::T)
    k < 0 && return zero(T)
    sgn = one(T)
    if n < 0
        n = -n + k -1
        if isodd(k)
            sgn = -sgn
        end
    end
    k > n && return zero(T)
    (k == 0 || k == n) && return sgn
    k == 1 && return sgn*n
    if k > (n>>1)
        k = (n - k)
    end
    x::T = nn = n - k + 1
    nn += 1
    rr = 2
    while rr <= k
        xt = div(widemul(x, nn), rr)
        x = xt
        x == xt || throw(OverflowError())
        rr += 1
        nn += 1
    end
    convert(T, copysign(x, sgn))
end

## other ordering related functions ##

function shuffle!(a::AbstractVector)
    for i = length(a):-1:2
        j = rand(1:i)
        a[i], a[j] = a[j], a[i]
    end
    return a
end

shuffle(a::AbstractVector) = shuffle!(copy(a))

function randperm(n::Integer)
    a = Array(typeof(n), n)
    a[1] = 1
    @inbounds for i = 2:n
        j = rand(1:i)
        a[i] = a[j]
        a[j] = i
    end
    return a
end

function randcycle(n::Integer)
    a = Array(typeof(n), n)
    a[1] = 1
    @inbounds for i = 2:n
        j = rand(1:i-1)
        a[i] = a[j]
        a[j] = i
    end
    return a
end

function nthperm!(a::AbstractVector, k::Integer)
    k -= 1 # make k 1-indexed
    k < 0 && error("permutation must be a positive number")
    n = length(a)
    n == 0 && return a
    f = factorial(oftype(k, n-1))
    for i=1:n-1
        j = div(k, f) + 1
        k = k % f
        f = div(f, n-i)

        j = j+i-1
        elt = a[j]
        for d = j:-1:i+1
            a[d] = a[d-1]
        end
        a[i] = elt
    end
    a
end
nthperm(a::AbstractVector, k::Integer) = nthperm!(copy(a),k)

function nthperm{T<:Integer}(p::AbstractVector{T})
    isperm(p) || error("argument is not a permutation")
    k, n = 1, length(p)
    for i = 1:n-1
        f = factorial(n-i)
        for j = i+1:n
            k += ifelse(p[j] < p[i], f, 0)
        end
    end
    return k
end

function invperm(a::AbstractVector)
    b = zero(a) # similar vector of zeros
    n = length(a)
    for i = 1:n
        j = a[i]
        ((1 <= j <= n) && b[j] == 0) ||
            error("argument is not a permutation")
        b[j] = i
    end
    b
end

function isperm(A)
    n = length(A)
    used = falses(n)
    for a in A
        (0 < a <= n) && (used[a] $= true) || return false
    end
    true
end

function permute!!{T<:Integer}(a, p::AbstractVector{T})
    count = 0
    start = 0
    while count < length(a)
        ptr = start = findnext(p, start+1)
        temp = a[start]
        next = p[start]
        count += 1
        while next != start
            a[ptr] = a[next]
            p[ptr] = 0
            ptr = next
            next = p[next]
            count += 1
        end
        a[ptr] = temp
        p[ptr] = 0
    end
    a
end

permute!(a, p::AbstractVector) = permute!!(a, copy(p))

function ipermute!!{T<:Integer}(a, p::AbstractVector{T})
    count = 0
    start = 0
    while count < length(a)
        start = findnext(p, start+1)
        temp = a[start]
        next = p[start]
        count += 1
        while next != start
            temp_next = a[next]
            a[next] = temp
            temp = temp_next
            ptr = p[next]
            p[next] = 0
            next = ptr
            count += 1
        end
        a[next] = temp
        p[next] = 0
    end
    a
end

ipermute!(a, p::AbstractVector) = ipermute!!(a, copy(p))

immutable Combinations{T}
    a::T
    t::Int
end

eltype(c::Combinations) = typeof(c.a)
eltype{T}(c::Combinations{UnitRange{T}}) = Array{T,1}
eltype{T}(c::Combinations{Range{T}}) = Array{T,1}

length(c::Combinations) = binomial(length(c.a),c.t)

function combinations(a, t::Integer)
    if t < 0
        # generate 0 combinations for negative argument
        t = length(a)+1
    end
    Combinations(a, t)
end

start(c::Combinations) = [1:c.t]
function next(c::Combinations, s)
    comb = c.a[s]
    if c.t == 0
        # special case to generate 1 result for t==0
        return (comb,[length(c.a)+2])
    end
    s = copy(s)
    for i = length(s):-1:1
        s[i] += 1
        if s[i] > (length(c.a) - (length(s)-i))
            continue
        end
        for j = i+1:endof(s)
            s[j] = s[j-1]+1
        end
        break
    end
    (comb,s)
end
done(c::Combinations, s) = !isempty(s) && s[1] > length(c.a)-c.t+1

immutable Permutations{T}
    a::T
end

eltype(c::Permutations) = typeof(c.a)
eltype{T}(c::Permutations{UnitRange{T}}) = Array{T,1}
eltype{T}(c::Permutations{Range{T}}) = Array{T,1}

length(c::Permutations) = factorial(length(c.a))

permutations(a) = Permutations(a)

start(p::Permutations) = [1:length(p.a)]
function next(p::Permutations, s)
    perm = p.a[s]
    if length(p.a) == 0
        # special case to generate 1 result for len==0
        return (perm,[1])
    end
    s = copy(s)
    k = length(s)-1
    while k > 0 && s[k] > s[k+1];  k -= 1;  end
    if k == 0
        s[1] = length(s)+1   # done
    else
        l = length(s)
        while s[k] >= s[l];  l -= 1;  end
        s[k],s[l] = s[l],s[k]
        reverse!(s,k+1)
    end
    (perm,s)
end
done(p::Permutations, s) = !isempty(s) && s[1] > length(p.a)


# Integer Partitions

immutable IntegerPartitions
    n::Int
end

length(p::IntegerPartitions) = npartitions(p.n)

partitions(n::Integer) = IntegerPartitions(n)

start(p::IntegerPartitions) = Int[]
done(p::IntegerPartitions, xs) = length(xs) == p.n
next(p::IntegerPartitions, xs) = (xs = nextpartition(p.n,xs); (xs,xs))

function nextpartition(n, as)
    if isempty(as);  return Int[n];  end

    xs = similar(as,0)
    sizehint!(xs,length(as)+1)

    for i = 1:length(as)-1
        if as[i+1] == 1
            x = as[i]-1
            push!(xs, x)
            n -= x
            while n > x
                push!(xs, x)
                n -= x
            end
            push!(xs, n)

            return xs
        end
        push!(xs, as[i])
        n -= as[i]
    end
    push!(xs, as[end]-1)
    push!(xs, 1)

    xs
end

let _npartitions = Dict{Int,Int}()
    global npartitions
    function npartitions(n::Int)
        if n < 0
            0
        elseif n < 2
            1
        elseif (np = get(_npartitions, n, 0)) > 0
            np
        else
            np = 0
            sgn = 1
            for k = 1:n
                np += sgn * (npartitions(n-k*(3k-1)>>1) + npartitions(n-k*(3k+1)>>1))
                sgn = -sgn
            end
            _npartitions[n] = np
        end
    end
end

# Algorithm H from TAoCP 7.2.1.4
# Partition n into m parts
# in colex order (lexicographic by reflected sequence)

immutable FixedPartitions
    n::Int
    m::Int
end

length(f::FixedPartitions) = npartitions(f.n,f.m)

partitions(n::Integer, m::Integer) = n >= 1 && m >= 1 ? FixedPartitions(n,m) : throw(DomainError())

start(f::FixedPartitions) = Int[]
function done(f::FixedPartitions, s::Vector{Int})
    f.m <= f.n || return true
    isempty(s) && return false
    return f.m == 1 || s[1]-1 <= s[end]
end
next(f::FixedPartitions, s::Vector{Int}) = (xs = nextfixedpartition(f.n,f.m,s); (xs,xs))

function nextfixedpartition(n, m, bs)
    as = copy(bs)
    if isempty(as)
        # First iteration
        as = [n-m+1, ones(Int, m-1)]
    elseif as[2] < as[1]-1
        # Most common iteration
        as[1] -= 1
        as[2] += 1
    else
        # Iterate
        local j
        s = as[1]+as[2]-1
        for j = 3:m
            if as[j] < as[1]-1; break; end
            s += as[j]
        end
        x = as[j] += 1
        for k = j-1:-1:2
            as[k] = x
            s -= x
        end
        as[1] = s
    end

    return as
end

let _nipartitions = Dict{(Int,Int),Int}()
    global npartitions
    function npartitions(n::Int,m::Int)
        if n < m || m == 0
            0
        elseif n == m
            1
        elseif (np = get(_nipartitions, (n,m), 0)) > 0
            np
        else
            _nipartitions[(n,m)] = npartitions(n-1,m-1) + npartitions(n-m,m)
        end
    end
end

# Algorithm H from TAoCP 7.2.1.5
# Set partitions

immutable SetPartitions{T<:AbstractVector}
    s::T
end

length(p::SetPartitions) = nsetpartitions(length(p.s))

partitions(s::AbstractVector) = SetPartitions(s)

start(p::SetPartitions) = (n = length(p.s); (zeros(Int32, n), ones(Int32, n-1), n, 1))
done(p::SetPartitions, s) = s[1][1] > 0
next(p::SetPartitions, s) = nextsetpartition(p.s, s...)

function nextsetpartition(s::AbstractVector, a, b, n, m)
    function makeparts(s, a, m)
        temp = [ similar(s,0) for k = 0:m ]
        for i = 1:n
            push!(temp[a[i]+1], s[i])
        end
        filter!(x->!isempty(x), temp)
    end

    if isempty(s);  return ([s], ([1], Int[], n, 1));  end

    part = makeparts(s,a,m)

    if a[end] != m
        a[end] += 1
    else
        local j
        for j = n-1:-1:1
            if a[j] != b[j]
                break
            end
        end
        a[j] += 1
        m = b[j] + (a[j] == b[j])
        for k = j+1:n-1
            a[k] = 0
            b[k] = m
        end
        a[end] = 0
    end

    return (part, (a,b,n,m))

end

let _nsetpartitions = Dict{Int,Int}()
    global nsetpartitions
    function nsetpartitions(n::Int)
        if n < 0
            0
        elseif n < 2
            1
        elseif (wn = get(_nsetpartitions, n, 0)) > 0
            wn
        else
            wn = 0
            for k = 0:n-1
                wn += binomial(n-1,k)*nsetpartitions(n-1-k)
            end
            _nsetpartitions[n] = wn
        end
    end
end

immutable FixedSetPartitions{T<:AbstractVector}
    s::T
    m::Int
end

length(p::FixedSetPartitions) = nfixedsetpartitions(length(p.s),p.m)

partitions(s::AbstractVector,m::Int) = length(s) >= 1 && m >= 1 ? FixedSetPartitions(s,m) : throw(DomainError())

function start(p::FixedSetPartitions)
    n = length(p.s)
    m = p.m
    m <= n ? (vcat(ones(Int, n-m),1:m), vcat(1,n-m+2:n), n) : (Int[], Int[], n)
end
# state consists of:
# vector a of length n describing to which partition every element of s belongs
# vector b of length n describing the first index b[i] that belongs to partition i
# integer n

done(p::FixedSetPartitions, s) = length(s[1]) == 0 || s[1][1] > 1
next(p::FixedSetPartitions, s) = nextfixedsetpartition(p.s,p.m, s...)

function nextfixedsetpartition(s::AbstractVector, m, a, b, n)
    function makeparts(s, a)
        part = [ similar(s,0) for k = 1:m ]
        for i = 1:n
            push!(part[a[i]], s[i])
        end
        return part
    end

    part = makeparts(s,a)

    if m == 1
        a[1] = 2
        return (part, (a, b, n))
    end

    if a[end] != m
        a[end] += 1
    else
        local j, k
        for j = n-1:-1:1
            if a[j]<m && b[a[j]+1]<j
                break
            end
        end
        if j>1
            a[j]+=1
            for p=j+1:n
                if b[a[p]]!=p
                    a[p]=1
                end
            end
        else
            for k=m:-1:2
                if b[k-1]<b[k]-1
                    break
                end
            end
            b[k]=b[k]-1
            b[k+1:m]=n-m+k+1:n
            a[1:n]=1
            a[b]=1:m
        end
    end

    return (part, (a,b,n))
end

function nfixedsetpartitions(n::Int,m::Int)
    numpart=0
    for k=0:m
        numpart+=(-1)^(m-k)*binomial(m,k)*(k^n)
    end
    numpart=div(numpart,factorial(m))
    return numpart
end


# For a list of integers i1, i2, i3, find the smallest
#     i1^n1 * i2^n2 * i3^n3 >= x
# for integer n1, n2, n3
function nextprod(a::Vector{Int}, x)
    if x > typemax(Int)
        error("unsafe for x bigger than typemax(Int)")
    end
    k = length(a)
    v = ones(Int, k)                  # current value of each counter
    mx = [nextpow(ai,x) for ai in a]  # maximum value of each counter
    v[1] = mx[1]                      # start at first case that is >= x
    p::widen(Int) = mx[1]             # initial value of product in this case
    best = p
    icarry = 1

    while v[end] < mx[end]
        if p >= x
            best = p < best ? p : best  # keep the best found yet
            carrytest = true
            while carrytest
                p = div(p, v[icarry])
                v[icarry] = 1
                icarry += 1
                p *= a[icarry]
                v[icarry] *= a[icarry]
                carrytest = v[icarry] > mx[icarry] && icarry < k
            end
            if p < x
                icarry = 1
            end
        else
            while p < x
                p *= a[1]
                v[1] *= a[1]
            end
        end
    end
    best = mx[end] < best ? mx[end] : best
    return int(best)  # could overflow, but best to have predictable return type
end

# For a list of integers i1, i2, i3, find the largest
#     i1^n1 * i2^n2 * i3^n3 <= x
# for integer n1, n2, n3
function prevprod(a::Vector{Int}, x)
    if x > typemax(Int)
        error("unsafe for x bigger than typemax(Int)")
    end
    k = length(a)
    v = ones(Int, k)                  # current value of each counter
    mx = [nextpow(ai,x) for ai in a]  # allow each counter to exceed p (sentinel)
    first = int(prevpow(a[1], x))     # start at best case in first factor
    v[1] = first
    p::widen(Int) = first
    best = p
    icarry = 1

    while v[end] < mx[end]
        while p <= x
            best = p > best ? p : best
            p *= a[1]
            v[1] *= a[1]
        end
        if p > x
            carrytest = true
            while carrytest
                p = div(p, v[icarry])
                v[icarry] = 1
                icarry += 1
                p *= a[icarry]
                v[icarry] *= a[icarry]
                carrytest = v[icarry] > mx[icarry] && icarry < k
            end
            if p <= x
                icarry = 1
            end
        end
    end
    best = x >= p > best ? p : best
    return int(best)
end
