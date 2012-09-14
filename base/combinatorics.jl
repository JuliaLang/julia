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

nPr(n, r) = factorial(n, n-r)

function binomial{T<:Integer}(n::T, k::T)
    if k < 0
        return zero(T)
    end
    sgn = one(T)
    if n < 0
        n = -n + k -1
        if isodd(k)
            sgn = -sgn
        end
    end
    if k > n # TODO: is this definitely right?
        return zero(T)
    end
    if k == 0 || k == n
        return sgn
    end
    if k == 1
        return sgn*n
    end
    if k > (n>>1)
        k = (n - k)
    end
    x = nn = n - k + 1.0
    nn += 1.0
    rr = 2.0
    while rr <= k
        x *= (nn/rr)
        rr += 1
        nn += 1
    end
    return sgn*iround(T,x)
end

const nCr = binomial

pascal(n) = [binomial(i+j-2,i-1) for i=1:n,j=1:n]

## other ordering related functions ##

function shuffle!(a::AbstractVector)
    for i = length(a):-1:2
        j = randi(i)
        a[i], a[j] = a[j], a[i]
    end
    return a
end

@in_place_matrix_op shuffle

function randperm(n::Integer)
    a = Array(typeof(n), n)
    a[1] = 1
    for i = 2:n
        j = randi(i)
        a[i] = a[j]
        a[j] = i
    end
    return a
end

function randcycle(n::Integer)
    a = Array(typeof(n), n)
    a[1] = 1
    for i = 2:n
        j = randi(i-1)
        a[i] = a[j]
        a[j] = i
    end
    return a
end

function nthperm!(a::AbstractVector, k::Integer)
    n = length(a)
    k -= 1   # make k 1-indexed
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
function invperm(a::AbstractVector)
    b = zero(a) # similar vector of zeros
    n = length(a)
    for i = 1:n
        j = a[i]
        if !(1 <= j <= n) || b[j] != 0
            error("invperm: input is not a permutation")
        end
        b[j] = i
    end
    return b
end

function isperm(a::AbstractVector)
    try
        invperm(a)
        true
    catch
        false
    end
end

# Algorithm T from TAoCP 7.2.1.3
function combinations(a::AbstractVector, t::Integer)
  # T1
  n = length(a)
  c = [0:t-1, n, 0]
  j = t
  if (t >= n) 
    # Algorithm T assumes t < n, just return a
    produce(a)
  else
    while true
      # T2
      produce([ a[c[i]+1] for i=1:t ])

      if j > 0
        x = j
      else
        # T3
        if c[1] + 1 < c[2]
          c[1] = c[1] + 1
          continue # to T2
        else
          j = 2
        end

        # T4
        need_j = true
        while need_j
          need_j = false

          c[j-1] = j-2
          x = c[j] + 1
          if x == c[j + 1]
            j = j + 1
            need_j = true # loop to T4
          end
        end

        # T5
        if j > t
          # terminate
          break
        end
      end

      # T6
      c[j] = x 
      j = j - 1
    end
  end
end

# Algorithm H from TAoCP 7.2.1.4
# Partition n into m parts
function integer_partitions(n::Int64, m::Int64) # why only Int64?
  if n < m || m < 2
    throw("Assumed n >= m >= 2!")
  end
  # H1
  a = [n - m + 1, ones(Int64, m), -1]
  # H2
  while true
    produce(a[1:m])
    if a[2] < a[1] - 1
      # H3
      a[1] = a[1] - 1
      a[2] = a[2] + 1
      continue # to H2
    end
    # H4
    j = 3
    s = a[1] + a[2] - 1
    if a[j] >= a[1] - 1
      while true
        s = s + a[j]
        j = j + 1
        if a[j] < a[1] - 1
          break # end loop
        end
      end
    end
    # H5
    if j > m 
      break # terminate
    end
    x = a[j] + 1
    a[j] = x
    j = j - 1
    # H6
    while j > 1
      a[j] = x
      s = s - x
      j = j - 1
    end
    a[1] = s
  end
end

# Algorithm H from TAoCP 7.2.1.5
# Set partitions
function partitions{T}(s::AbstractVector{T})

  n = length(s)
  # H1
  a = zeros(Int,n)
  b = ones(Int,n-1)
  m = 1

  while true
    # H2
    # convert from restricted growth string a[1:n] to set of sets
    temp = [ Array(T,0) for k = 1:n ]
    for k = 1:n
      push(temp[a[k]+1], s[k])
    end
    result = Array(Array{T,1},0)
    for arr in temp
      if !isempty(arr)
        push(result, arr)
      end
    end
    #produce(a[1:n]) # this is the string representing set assignment
    produce(result)

    if a[n] != m
      # H3
      a[n] = a[n] + 1
      continue # to H2
    end
    # H4
    j = n - 1
    while a[j] == b[j]
      j = j - 1
    end
    # H5
    if j == 1
      break # terminate
    end
    a[j] = a[j] + 1
    # H6
    m = b[j] + (a[j] == b[j])
    j = j + 1
    while j < n
      a[j] = 0
      b[j] = m
      j = j + 1
    end
    a[n] = 0
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
