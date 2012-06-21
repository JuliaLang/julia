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
    return sgn*convert(T,x)
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

# todo: should be O(n)
isperm(a::AbstractVector) = isequal([1:length(a)], sort(a))

# inverse permutation
function invperm(a::AbstractVector)
    b = zero(a) # similar vector of zeros
    try
        for i = 1:length(a)
            if b[a[i]] != 0 error() end
            b[a[i]] = i
        end
    catch
        # TODO: should catch more selectively, but at
        # the moment, this is just an ExceptionError.
        error("invperm: input must be a permutation")
    end
    return b
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
