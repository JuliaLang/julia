function factorial(n::Int)
    if n < 0
        return zero(n)
    end
    p = one(n)
    for i=2:n
        p*=i
    end
    p
end

function nPr{T <: Int}(n::T, r::T)
    if r < 0 || n < 0 || r > n
        return zero(T)
    end

    ans = one(T)
    while (r > 0)
        ans *= n
        n -= 1
        r -= 1
    end
    return ans
end

function nCr{T <: Int}(n::T, r::T)
    if r < 0
        return zero(T)
    end

    neg = false
    if n < 0
        n = (-n)+r-1
        if isodd(r)
            neg = true
        end
    end

    if r > n
        return zero(T)
    end
    if r == 0 || r == n
        return one(T)
    end

    if r > div(n,2)
        r = (n - r)
    end

    ans = nn = n - r + 1.0
    nn += 1.0
    rr = 2.0
    while (rr <= r)
        ans *= (nn/rr)
        rr += 1
        nn += 1
    end
    if neg
        return convert(T,-ans)
    end
    return convert(T,ans)
end

# sort() should be stable
# Thus, if a permutation is required, or records are being sorted
# a stable sort should be used.
# If only numbers are being sorted, a faster quicksort can be used.

sort_inplace{T <: Real}(a::Vector{T}) = quicksort(a, 1, length(a))

sort_inplace{T}(a::Vector{T}) = mergesort(a, 1, length(a), Array(T, length(a)))

sort(a::Vector) = sort_inplace(copy(a))

sortperm{T}(a::Vector{T}) =
    mergesort(copy(a), linspace(1,length(a)), 1, length(a),
              Array(T, length(a)), Array(Size, length(a)))

function insertionsort(a::Vector, lo, hi)
    for i=(lo+1):hi
        j = i
        x = a[i]
        while j > lo
            if x >= a[j-1]
                break
            end
            a[j] = a[j-1]
            j -= 1
        end
        a[j] = x
    end
    a
end

function quicksort(a::Vector, lo, hi)
    while hi > lo
        if (hi-lo <= 20)
            return insertionsort(a, lo, hi)
        end
        i, j = lo, hi
        pivot = a[div((lo+hi),2)]
        # Partition
        while i <= j
            while a[i] < pivot; i += 1; end
            while a[j] > pivot; j -= 1; end
            if i <= j
                a[i], a[j] = a[j], a[i]
                i += 1
                j -= 1
            end
        end
        # Recursion for quicksort
        if lo < j; quicksort(a, lo, j); end
        lo = i
    end
    return a
end

function insertionsort(a::Vector, p::Vector{Size}, lo, hi)
    for i=(lo+1):hi
        j = i
        x = a[i]
        xp = p[i]
        while j > lo
            if x >= a[j-1]
                break
            end
            a[j] = a[j-1]
            p[j] = p[j-1]
            j -= 1
        end
        a[j] = x
        p[j] = xp
    end
    (a, p)
end

function mergesort(a::Vector, p::Vector{Size}, lo, hi,
                   b::Vector, pb::Vector{Size})

    if lo < hi
        if (hi-lo <= 20)
            return insertionsort(a, p, lo, hi)
        end

        m = div ((lo + hi), 2)
        mergesort(a, p, lo, m, b, pb)
        mergesort(a, p, m+1, hi, b, pb)

        # merge(lo,m,hi)
        i = 1
        j = lo
        while (j <= m)
            b[i] = a[j]
            pb[i] = p[j]
            i += 1
            j += 1
        end

        i = 1
        k = lo
        while ((k < j) & (j <= hi))
            if (b[i] <= a[j])
                a[k] = b[i]
                p[k] = pb[i]
                i += 1
            else
                a[k] = a[j]
                p[k] = p[j]
                j += 1
            end
            k += 1
        end

        while (k < j)
            a[k] = b[i]
            p[k] = pb[i]
            k += 1
            i += 1
        end

    end # if lo<hi...

    return (a, p)
end

function mergesort(a::Vector, lo, hi, b::Vector)
    if lo < hi
        if (hi-lo <= 20)
            return insertionsort(a, lo, hi)
        end

        m = div ((lo + hi), 2)
        mergesort(a, lo, m, b)
        mergesort(a, m+1, hi, b)

        # merge(lo,m,hi)
        i = 1
        j = lo
        while (j <= m)
            b[i] = a[j]
            i += 1
            j += 1
        end

        i = 1
        k = lo
        while ((k < j) & (j <= hi))
            if (b[i] <= a[j])
                a[k] = b[i]
                i += 1
            else
                a[k] = a[j]
                j += 1
            end
            k += 1
        end

        while (k < j)
            a[k] = b[i]
            k += 1
            i += 1
        end

    end # if lo<hi...

    return a
end

function issorted(v::Vector)
    for i=1:(length(v)-1)
        if v[i] > v[i+1]; return false; end
    end
    return true
end

# Knuth shuffle
function shuffle(a::Vector)
    for i = length(a):-1:2
        j = randint(i)
        a[i], a[j] = a[j], a[i]
    end
    return a
end

function randperm(n::Int)
    a = Array(typeof(n), n)
    a[1] = 1
    for i = 2:n
        j = randint(i)
        a[i] = a[j]
        a[j] = i
    end
    return a
end

function randcycle(n::Int)
    a = Array(typeof(n), n)
    a[1] = 1
    for i = 2:n
        j = randint(i-1)
        a[i] = a[j]
        a[j] = i
    end
    return a
end

function nthperm(A, k::Int)
    fac = one(k)
    for i=2:length(A)
        fac *= (i-1)
        j = i - rem(div(k,fac),i)
        A[i], A[j] = A[j], A[i]
    end
    A
end
