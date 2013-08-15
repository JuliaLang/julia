function factorial(n::Integer)
    if n < 0
        return zero(n)
    end
    f = one(n)
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
    x = nn = n - k + 1.0
    nn += 1.0
    rr = 2.0
    while rr <= k
        x *= nn/rr
        rr += 1
        nn += 1
    end
    sgn*iround(T,x)
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
    for i = 2:n
        j = rand(1:i)
        a[i] = a[j]
        a[j] = i
    end
    return a
end

function randcycle(n::Integer)
    a = Array(typeof(n), n)
    a[1] = 1
    for i = 2:n
        j = rand(1:i-1)
        a[i] = a[j]
        a[j] = i
    end
    return a
end

function nthperm!(a::AbstractVector, k::Integer)
    k -= 1 # make k 1-indexed
    n = length(a)
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

# invert a permutation
function _invperm(a::AbstractVector)
    b = zero(a) # similar vector of zeros
    n = length(a)
    for i = 1:n
        j = a[i]
        if !(1 <= j <= n) || b[j] != 0
            b[1] = 0
            break
        end
        b[j] = i
    end
    return b
end

function invperm(a::AbstractVector)
    b = _invperm(a)
    if !isempty(b) && b[1] == 0
        error("invperm: input is not a permutation")
    end
    return b
end

function isperm(a::AbstractVector)
    b = _invperm(a)
    return isempty(b) || b[1]!=0
end

function permute!!(a, p::AbstractVector{Int})
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

permute!(a, p::AbstractVector{Int}) = permute!!(a, copy(p))

function ipermute!!(a, p::AbstractVector{Int})
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

ipermute!(a, p::AbstractVector{Int}) = ipermute!!(a, copy(p))

immutable Combinations{T}
    a::T
    t::Int
end

eltype(c::Combinations) = typeof(c.a)
eltype{T}(c::Combinations{Range1{T}}) = Array{T,1}
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
eltype{T}(c::Permutations{Range1{T}}) = Array{T,1}
eltype{T}(c::Permutations{Range{T}}) = Array{T,1}

length(c::Permutations) = factorial(length(c.a))

permutations(a) = Permutations(a)

start(p::Permutations) = [1:length(p.a)]
function next(p::Permutations, s)
    if length(p.a) == 0
        # special case to generate 1 result for len==0
        return (p.a,[1])
    end
    perm = p.a[s]
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
    sizehint(xs,length(as)+1)

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

const _npartitions = (Int=>Int)[]
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


# Algorithm H from TAoCP 7.2.1.4
# Partition n into m parts
# in colex order (lexicographic by reflected sequence)

immutable FixedPartitions
    n::Int
    m::Int
end

length(f::FixedPartitions) = npartitions(f.n,f.m)

partitions(n::Integer, m::Integer) = (@assert 2 <= m <= n; FixedPartitions(n,m))

start(f::FixedPartitions) = Int[]
done(f::FixedPartitions, s::Vector{Int}) = !isempty(s) && s[1]-1 <= s[end]
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

const _nipartitions = ((Int,Int)=>Int)[]
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


# Algorithm H from TAoCP 7.2.1.5
# Set partitions

immutable SetPartitions{T<:AbstractVector}
    s::T
end

length(p::SetPartitions) = nsetpartitions(length(p.s))

partitions(s::AbstractVector) = SetPartitions(s)

start(p::SetPartitions) = (n = length(p.s); (zeros(Int32, n), ones(Int32, n-1), n, 1))
done(p::SetPartitions, s) = !isempty(s) && s[1][1] > 0
next(p::SetPartitions, s) = nextsetpartition(p.s, s...)

function nextsetpartition(s::AbstractVector, a, b, n, m)
    function makeparts(s, a, m)
        temp = [ similar(s,0) for k = 0:m ]
        for i = 1:n
            push!(temp[a[i]+1], s[i])
        end
        filter!(x->!isempty(x), temp)
    end

    if isempty(s);  return ({s}, ([1], Int[], n, 1));  end

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


const _nsetpartitions = (Int=>Int)[]
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


# For a list of integers i1, i2, i3, find the smallest 
#     i1^n1 * i2^n2 * i3^n3 >= x
# for integer n1, n2, n3
function nextprod(a::Vector{Int}, x)
    if x > typemax(Int)
        error("Unsafe for x bigger than typemax(Int)")
    end
    k = length(a)
    v = ones(Int, k)            # current value of each counter
    mx = int(a.^nextpow(a, x))  # maximum value of each counter
    v[1] = mx[1]                # start at first case that is >= x
    p::morebits(Int) = mx[1]    # initial value of product in this case
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
        error("Unsafe for x bigger than typemax(Int)")
    end
    k = length(a)
    v = ones(Int, k)            # current value of each counter
    mx = int(a.^nextpow(a, x))  # allow each counter to exceed p (sentinel)
    first = int(a[1]^prevpow(a[1], x))  # start at best case in first factor 
    v[1] = first
    p::morebits(Int) = first
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
