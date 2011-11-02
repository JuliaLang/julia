function factorial(n::Int)
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
function factorial{T<:Int}(n::T, k::T)
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

function binomial{T<:Int}(n::T, k::T)
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
    if k > div(n,2)
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

## sorting ##

function issorted(v::AbstractVector)
    for i=1:(length(v)-1)
        if v[i] > v[i+1]; return false; end
    end
    return true
end

# sorting should be stable
# Thus, if a permutation is required, or records are being sorted
# a stable sort should be used.
# If only numbers are being sorted, a faster quicksort can be used.

function _jl_insertionsort(a::AbstractVector, lo::Int, hi::Int)
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

function _jl_quicksort(a::AbstractVector, lo::Int, hi::Int)
    while hi > lo
        if (hi-lo <= 20)
            return _jl_insertionsort(a, lo, hi)
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
        if lo < j; _jl_quicksort(a, lo, j); end
        lo = i
    end
    return a
end

function _jl_insertionsort(a::AbstractVector, p::AbstractVector{Size}, lo::Int, hi::Int)
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

function _jl_mergesort(a::AbstractVector, p::AbstractVector{Size}, lo::Int, hi::Int,
                    b::AbstractVector, pb::AbstractVector{Size})

    if lo < hi
        if (hi-lo <= 20)
            return _jl_insertionsort(a, p, lo, hi)
        end

        m = div ((lo + hi), 2)
        _jl_mergesort(a, p, lo, m, b, pb)
        _jl_mergesort(a, p, m+1, hi, b, pb)

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

function _jl_mergesort(a::AbstractVector, lo::Int, hi::Int, b::AbstractVector)
    if lo < hi
        if (hi-lo <= 20)
            return _jl_insertionsort(a, lo, hi)
        end

        m = div ((lo + hi), 2)
        _jl_mergesort(a, lo, m, b)
        _jl_mergesort(a, m+1, hi, b)

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

sort!{T <: Real}(a::AbstractVector{T}) = _jl_quicksort(a, 1, length(a))
sort!{T}(a::AbstractVector{T}) = _jl_mergesort(a, 1, length(a), Array(T, length(a)))

sortperm{T}(a::AbstractVector{T}) =
    _jl_mergesort(copy(a), linspace(1,length(a)), 1, length(a),
                  Array(T, length(a)), Array(Size, length(a)))

macro in_place_matrix_op(in_place, out_of_place)
    quote
        function ($in_place)(a::AbstractMatrix, dim::Index)
            m = size(a,1)
            if dim == 1
                for i=1:m:numel(a)
                    ($in_place)(sub(a, i:(i+m-1)))
                end
            elseif dim == 2
                for i=1:m
                    ($in_place)(sub(a, i:m:numel(a)))
                end
            end
            return a
        end
        # TODO: in-place generalized AbstractArray implementation
        ($in_place)(a::AbstractMatrix) = ($in_place)(a,1)

        ($out_of_place)(a::AbstractVector) = ($in_place)(copy(a))
        ($out_of_place)(a::AbstractArray, dim::Index) = ($in_place)(copy(a), dim)
        ($out_of_place)(a::AbstractMatrix) = ($out_of_place)(a,1)
    end
end

@in_place_matrix_op sort! sort

# TODO: implement generalized in-place, ditch this
function sort(a::AbstractArray, dim::Index)
    X = similar(a)
    n = size(a,dim)

    if dim == 1
        for i=1:n:numel(a)
            this_slice = i:(i+n-1)
            X[this_slice] = sort(sub(a, this_slice))
        end
    else
        p = [1:ndims(a)]
        p[dim], p[1] = p[1], p[dim]
        X = ipermute(sort(permute(a, p)), p)
    end

    return X
end

## other ordering related functions ##

function shuffle!(a::AbstractVector)
    for i = length(a):-1:2
        j = randi(i)
        a[i], a[j] = a[j], a[i]
    end
    return a
end

@in_place_matrix_op shuffle! shuffle

function randperm(n::Int)
    a = Array(typeof(n), n)
    a[1] = 1
    for i = 2:n
        j = randi(i)
        a[i] = a[j]
        a[j] = i
    end
    return a
end

function randcycle(n::Int)
    a = Array(typeof(n), n)
    a[1] = 1
    for i = 2:n
        j = randi(i-1)
        a[i] = a[j]
        a[j] = i
    end
    return a
end

function nthperm!(a::AbstractVector, k::Int)
    fac = one(k)
    for i=2:length(a)
        fac *= (i-1)
        j = i - rem(div(k,fac),i)
        a[i], a[j] = a[j], a[i]
    end
    a
end
nthperm(a::AbstractVector, k::Int) = nthperm!(copy(a))
