## standard sort comparisons ##

_jl_fp_pos_lt(x::Float32, y::Float32) = slt_int(unbox(Float32,x),unbox(Float32,y))
_jl_fp_pos_lt(x::Float64, y::Float64) = slt_int(unbox(Float64,x),unbox(Float64,y))
_jl_fp_pos_le(x::Float32, y::Float32) = sle_int(unbox(Float32,x),unbox(Float32,y))
_jl_fp_pos_le(x::Float64, y::Float64) = sle_int(unbox(Float64,x),unbox(Float64,y))

_jl_fp_neg_lt(x::Float32, y::Float32) = slt_int(unbox(Float32,y),unbox(Float32,x))
_jl_fp_neg_lt(x::Float64, y::Float64) = slt_int(unbox(Float64,y),unbox(Float64,x))
_jl_fp_neg_le(x::Float32, y::Float32) = sle_int(unbox(Float32,y),unbox(Float32,x))
_jl_fp_neg_le(x::Float64, y::Float64) = sle_int(unbox(Float64,y),unbox(Float64,x))

## internal sorting functionality ##

macro _jl_sort_functions(suffix, lt, args...)
insertionsort = esc(symbol("_jl_insertionsort$suffix"))
quicksort = esc(symbol("_jl_quicksort$suffix"))
mergesort = esc(symbol("_jl_mergesort$suffix"))
pivot_middle = esc(symbol("_jl_pivot_middle$suffix"))
lt = @eval (a,b)->$lt
quote

# sorting should be stable
# Thus, if a permutation is required, or records are being sorted
# a stable sort should be used.
# If only numbers are being sorted, a faster quicksort can be used.

# fast sort for small arrays
function ($insertionsort)($(args...), a::AbstractVector, lo::Int, hi::Int)
    for i = lo+1:hi
        j = i
        x = a[i]
        while j > lo
            if $(lt(:x, :(a[j-1])))
                a[j] = a[j-1]
                j -= 1
                continue
            end
            break
        end
        a[j] = x
    end
    return a
end

# permutes an auxilliary array mirroring the sort
function ($insertionsort)($(args...), a::AbstractVector, p::AbstractVector{Int}, lo::Int, hi::Int)
    for i = lo+1:hi
        j = i
        x = a[i]
        xp = p[i]
        while j > lo
            if $(lt(:x, :(a[j-1])))
                a[j] = a[j-1]
                p[j] = p[j-1]
                j -= 1
                continue
            end
            break
        end
        a[j] = x
        p[j] = xp
    end
    return a, p
end

($pivot_middle)(a,b,c) = $(lt(:a,:b)) ? ($(lt(:b,:c)) ? b : c) : ($(lt(:a,:c)) ? a : c)

# very fast but unstable
function ($quicksort)($(args...), a::AbstractVector, lo::Int, hi::Int)
    while hi > lo
        if hi-lo <= 20
            return $(expr(:call, insertionsort, args..., :a, :lo, :hi))
        end
        i, j = lo, hi
        # pivot = (a[lo]+a[hi])/2                                   # 1.14x
          pivot = a[(lo+hi)>>>1]                                    # 1.15x
        # pivot = (a[lo]+a[hi]+a[(lo+hi)>>>1])/3                    # 1.16x
        # pivot = _jl_pivot_middle(a[lo], a[hi], a[(lo+hi)>>>1])    # 1.23x
        # pivot = a[randival(lo,hi)]                                # 1.28x
        while i <= j
            while $(lt(:(a[i]), :pivot)); i += 1; end
            while $(lt(:pivot, :(a[j]))); j -= 1; end
            if i <= j
                a[i], a[j] = a[j], a[i]
                i += 1
                j -= 1
            end
        end
        if lo < j
            $(expr(:call, quicksort, args..., :a, :lo, :j))
        end
        lo = i
    end
    return a
end

# less fast & not in-place, but stable
function ($mergesort)($(args...), a::AbstractVector, lo::Int, hi::Int, b::AbstractVector)
    if lo < hi
        if hi-lo <= 20
            return ($insertionsort)($(args...), a, lo, hi)
        end

        m = (lo+hi)>>>1
        ($mergesort)($(args...), a, lo, m, b)
        ($mergesort)($(args...), a, m+1, hi, b)

        # merge(lo,m,hi)
        i = 1
        j = lo
        while j <= m
            b[i] = a[j]
            i += 1
            j += 1
        end

        i = 1
        k = lo
        while k < j <= hi
            if $(lt(:(a[j]), :(b[i])))
                a[k] = a[j]
                j += 1
            else
                a[k] = b[i]
                i += 1
            end
            k += 1
        end
        while k < j
            a[k] = b[i]
            k += 1
            i += 1
        end
    end
    return a
end

# permutes auxilliary arrays mirroring the sort
function ($mergesort)($(args...),
                      a::AbstractVector, p::AbstractVector{Int}, lo::Int, hi::Int,
                      b::AbstractVector, pb::AbstractVector{Int})
    if lo < hi
        if hi-lo <= 20
            return ($insertionsort)($(args...), a, p, lo, hi)
        end

        m = (lo+hi)>>>1
        ($mergesort)($(args...), a, p, lo, m, b, pb)
        ($mergesort)($(args...), a, p, m+1, hi, b, pb)

        # merge(lo,m,hi)
        i = 1
        j = lo
        while j <= m
            b[i] = a[j]
            pb[i] = p[j]
            i += 1
            j += 1
        end

        i = 1
        k = lo
        while k < j <= hi
            if $(lt(:(a[j]), :(b[i])))
                a[k] = a[j]
                p[k] = p[j]
                j += 1
            else
                a[k] = b[i]
                p[k] = pb[i]
                i += 1
            end
            k += 1
        end
        while k < j
            a[k] = b[i]
            p[k] = pb[i]
            k += 1
            i += 1
        end
    end
    return a, p
end

end; end # quote / macro

@_jl_sort_functions ""    :(isless($a,$b))
@_jl_sort_functions "_r"  :(isless($b,$a))
@_jl_sort_functions "_lt" :(lt($a,$b)) lt::Function
@_jl_sort_functions "_by" :(isless(by($a),by($b))) by::Function

## external sorting functions ##

sort!{T<:Real}(a::AbstractVector{T})  = _jl_quicksort(a, 1, length(a))
sortr!{T<:Real}(a::AbstractVector{T}) = _jl_quicksort_r(a, 1, length(a))
sort!{T}(a::AbstractVector{T})  = _jl_mergesort(a, 1, length(a), Array(T,length(a)))
sortr!{T}(a::AbstractVector{T}) = _jl_mergesort_r(a, 1, length(a), Array(T,length(a)))

sort!{T}(lt::Function, a::AbstractVector{T}) =
    _jl_mergesort_lt(lt, a, 1, length(a), Array(T,length(a)))
sort_by!{T}(by::Function, a::AbstractVector{T}) =
    _jl_mergesort_by(by, a, 1, length(a), Array(T,length(a)))

## special sorting for floating-point arrays ##

@_jl_sort_functions "_fp_pos" :(_jl_fp_pos_lt($a,$b))
@_jl_sort_functions "_fp_neg" :(_jl_fp_neg_lt($a,$b))

# push NaNs to the end of a, returning # of non-NaNs
function _jl_nans_to_end{T<:FloatingPoint}(a::AbstractVector{T})
    n = length(a)
    if n <= 1
        return n
    end
    i = 1
    while (i < n) & (a[i]==a[i])
        i += 1
    end
    nnan = 0
    while true
        if a[i]==a[i]
            i += 1
        else
            nnan += 1
        end
        if i+nnan > n
            break
        end
        if nnan > 0
            a[i], a[i+nnan] = a[i+nnan], a[i]
        end
    end
    return n-nnan
end

function sort!{T<:FloatingPoint}(a::AbstractVector{T})
    n = _jl_nans_to_end(a)
    i, j = 1, n
    while true
        # TODO: faster positive negative int check?
        while i <= j && _jl_fp_pos_lt(a[i],zero(T)); i += 1; end
        while i <= j && _jl_fp_pos_le(zero(T),a[j]); j -= 1; end
        if i <= j
            a[i], a[j] = a[j], a[i]
            i += 1
            j -= 1
        else
            break
        end
    end
    _jl_quicksort_fp_neg(a, 1, j)
    _jl_quicksort_fp_pos(a, i, n)
    return a
end

# TODO: something sensible should happen when each_col et. al. are used with a
# pure function argument
function each_col!(f::Function, a::AbstractMatrix)
    m = size(a,1)
    for i = 1:m:numel(a)
        f(sub(a, i:(i+m-1)))
    end
    return a
end

function each_row!(f::Function, a::AbstractMatrix)
    m = size(a,1)
    for i = 1:m
        f(sub(a, i:m:numel(a)))
    end
    return a
end

function each_vec!(f::Function, a::AbstractMatrix, dim::Integer)
    if dim == 1; return each_col!(f,a); end
    if dim == 2; return each_row!(f,a); end
    error("invalid matrix dimensions: $dim")
end

each_col(f::Function, a::AbstractMatrix) = each_col!(f,copy(a))
each_row(f::Function, a::AbstractMatrix) = each_row!(f,copy(a))
each_vec(f::Function, a::AbstractMatrix, d::Integer) = each_vec!(f,copy(a),d)

## other sorting functions defined in terms of sort! ##

macro in_place_matrix_op(out_of_place, args...)
    in_place = esc(symbol("$(out_of_place)!"))
    out_of_place = esc(out_of_place)
    quote
        function ($in_place)($(args...), a::AbstractMatrix, dim::Int)
            m = size(a,1)
            if dim == 1
                for i = 1:m:numel(a)
                    ($in_place)($(args...), sub(a, i:(i+m-1)))
                end
            elseif dim == 2
                for i = 1:m
                    ($in_place)($(args...), sub(a, i:m:numel(a)))
                end
            end
            return a
        end
        # TODO: in-place generalized AbstractArray implementation
        ($in_place)($(args...), a::AbstractArray) = ($in_place)($(args...), a,1)

        ($out_of_place)($(args...), a::AbstractVector) = ($in_place)($(args...), copy(a))
        ($out_of_place)($(args...), a::AbstractArray, d::Int) = ($in_place)($(args...), copy(a), d)
        ($out_of_place)($(args...), a::AbstractArray) = ($out_of_place)($(args...), a,1)
    end
end

@in_place_matrix_op sort
@in_place_matrix_op sort lt::Function
@in_place_matrix_op sortr
@in_place_matrix_op sort_by by::Function

# TODO: implement generalized in-place, ditch this
function sort(a::AbstractArray, dim::Int)
    X = similar(a)
    n = size(a,dim)
    if dim == 1
        for i = 1:n:numel(a)
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

sortperm{T}(a::AbstractVector{T}) =
    _jl_mergesort(copy(a), [1:length(a)], 1, length(a),
                  Array(T, length(a)), Array(Int, length(a)))

function issorted(v::AbstractVector)
  for i = 1:length(v)-1
      if isless(v[i+1], v[i])
          return false
      end
  end
  return true
end

function _jl_quickselect(a::AbstractArray, k::Int, lo::Int, hi::Int)
    if k < lo || k > hi; error("k is out of bounds"); end

    while true

        if lo == hi; return a[lo]; end

        i, j = lo, hi
        pivot = _jl_pivot_middle(a[lo], a[hi], a[(lo+hi)>>>1])
        while i < j
            while isless(a[i], pivot); i += 1; end
            while isless(pivot, a[j]); j -= 1; end
            if isequal(a[i], a[j])
                i += 1
            elseif i < j
                a[i], a[j] = a[j], a[i]
            end
        end
        pivot_ind = j

        length = pivot_ind - lo + 1
        if k == length
            return a[pivot_ind]
        elseif k <  length
            hi = pivot_ind - 1
        else
            lo = pivot_ind + 1
            k = k - length
        end

    end # while true...

end

select(a::AbstractArray, k::Int) = _jl_quickselect(copy(a), k, 1, length(a))
select!(a::AbstractArray, k::Int) = _jl_quickselect(a, k, 1, length(a))

function search_sorted(a, x)
    hi = length(a)
    if isless(a[hi], x)
        return hi+1
    end
    lo = 1
    while lo < hi-1
        i = (lo+hi)>>>1
        if isless(x,a[i])
            hi = i
        else
            lo = i
        end
    end
    return isless(a[lo],x) ? hi : lo
end

function search_sorted_last(a::Vector, x)
    ## Index of the last value of vector a that is less than or equal to x.
    ## Returns 0 if x is less than all values of a.
    ## 
    ## Good reference: http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary 
    lo = 0
    hi = length(a) + 1
    while lo < hi-1
        i = (lo+hi)>>>1
        if isless(x,a[i])
            hi = i
        else
            lo = i
        end
    end
    lo
end

function search_sorted_first(a::Vector, x)
    ## Index of the first value of vector a that is greater than or equal to x.
    ## Returns length(a) + 1 if x is greater than all values in a.
    ## 
    ## Good reference: http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary 
    lo = 0
    hi = length(a) + 1
    while lo < hi-1
        i = (lo+hi)>>>1
        if isless(a[i],x)
            lo = i
        else
            hi = i
        end
    end
    hi
end

order(a::AbstractVector) = sortperm(a)[2]
