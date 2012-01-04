## standard sort comparisons ##

sortlt(x,y) = x < y
sortle(x,y) = x <= y

sortlt(x::Float32, y::Float32) = fpsortlt32(unbox32(x),unbox32(y))
sortlt(x::Float64, y::Float64) = fpsortlt64(unbox64(x),unbox64(y))
sortle(x::Float32, y::Float32) = fpsortle32(unbox32(x),unbox32(y))
sortle(x::Float64, y::Float64) = fpsortle64(unbox64(x),unbox64(y))

_jl_fp_pos_lt(x::Float32, y::Float32) = slt_int(unbox32(x),unbox32(y))
_jl_fp_pos_lt(x::Float64, y::Float64) = slt_int(unbox64(x),unbox64(y))
_jl_fp_pos_le(x::Float32, y::Float32) = sle_int(unbox32(x),unbox32(y))
_jl_fp_pos_le(x::Float64, y::Float64) = sle_int(unbox64(x),unbox64(y))

_jl_fp_neg_lt(x::Float32, y::Float32) = sgt_int(unbox32(x),unbox32(y))
_jl_fp_neg_lt(x::Float64, y::Float64) = sgt_int(unbox64(x),unbox64(y))
_jl_fp_neg_le(x::Float32, y::Float32) = sge_int(unbox32(x),unbox32(y))
_jl_fp_neg_le(x::Float64, y::Float64) = sge_int(unbox64(x),unbox64(y))

## internal sorting functionality ##

macro _jl_sort_functions(suffix, lt, le, args...)
insertionsort = symbol("_jl_insertionsort$suffix")
quicksort = symbol("_jl_quicksort$suffix")
mergesort = symbol("_jl_mergesort$suffix")
lt = @eval (a,b)->$lt
le = @eval (a,b)->$le
quote

# sorting should be stable
# Thus, if a permutation is required, or records are being sorted
# a stable sort should be used.
# If only numbers are being sorted, a faster quicksort can be used.

# fast sort for small arrays
function ($insertionsort)($(args...), a::AbstractVector, lo::Integer, hi::Integer)
    for i = lo+1:hi
        j = i
        x = a[i]
        while j > lo
            if $le(:(a[j-1]), :x)
                break
            end
            a[j] = a[j-1]
            j -= 1
        end
        a[j] = x
    end
    return a
end

# permutes an auxilliary array mirroring the sort
function ($insertionsort)($(args...), a::AbstractVector, p::AbstractVector{Int}, lo::Integer, hi::Integer)
    for i = lo+1:hi
        j = i
        x = a[i]
        xp = p[i]
        while j > lo
            if $le(:(a[j-1]), :x)
                break
            end
            a[j] = a[j-1]
            p[j] = p[j-1]
            j -= 1
        end
        a[j] = x
        p[j] = xp
    end
    return a, p
end

_jl_pivot_middle(a,b,c) = a < b ? (b < c ? b : c) : (a < c ? a : c)

# very fast but unstable
function ($quicksort)($(args...), a::AbstractVector, lo::Integer, hi::Integer)
    while hi > lo
        if hi-lo <= 20
            return $expr(:call, insertionsort, args..., :a, :lo, :hi)
        end
        i, j = lo, hi
        # pivot = (a[lo]+a[hi])/2                                   # 1.14x
        # pivot = a[div(lo+hi,2)]                                   # 1.15x
          pivot = (a[lo]+a[hi]+a[div(lo+hi,2)])/3                   # 1.16x
        # pivot = _jl_pivot_middle(a[lo], a[hi], a[div(lo+hi,2)])   # 1.23x
        # pivot = a[randival(lo,hi)]                                # 1.28x
        while i <= j
            while $lt(:(a[i]), :pivot); i += 1; end
            while $lt(:pivot, :(a[j])); j -= 1; end
            if i <= j
                a[i], a[j] = a[j], a[i]
                i += 1
                j -= 1
            end
        end
        if lo < j
            $expr(:call, quicksort, args..., :a, :lo, :j)
        end
        lo = i
    end
    return a
end

# less fast but stable
function ($mergesort)($(args...), a::AbstractVector, lo::Integer, hi::Integer, b::AbstractVector)
    if lo < hi
        if hi-lo <= 20
            return ($insertionsort)($(args...), a, lo, hi)
        end

        m = div(lo+hi, 2)
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
            if $le(:(b[i]), :(a[j]))
                a[k] = b[i]
                i += 1
            else
                a[k] = a[j]
                j += 1
            end
            k += 1
        end

        while k < j
            a[k] = b[i]
            k += 1
            i += 1
        end

    end # if lo<hi...

    return a
end

# permutes auxilliary arrays mirroring the sort
function ($mergesort)($(args...),
                      a::AbstractVector, p::AbstractVector{Int}, lo::Integer, hi::Integer,
                      b::AbstractVector, pb::AbstractVector{Int})
    if lo < hi
        if hi-lo <= 20
            return ($insertionsort)($(args...), a, p, lo, hi)
        end

        m = div(lo+hi, 2)
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
            if $le(:(b[i]), :(a[j]))
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

@_jl_sort_functions ""    :(sortlt($a,$b)) :(sortle($a,$b))
@_jl_sort_functions "_r"  :(sortlt($b,$a)) :(sortle($b,$a))
@_jl_sort_functions "_lt" :(lt($a,$b))     :(!lt($b,$a))    lt::Function

## external sorting functions ##

sort!{T<:Real}(a::AbstractVector{T}) = _jl_quicksort(a, 1, length(a))
sortr!{T<:Real}(a::AbstractVector{T}) = _jl_quicksort_r(a, 1, length(a))
sort!{T}(a::AbstractVector{T}) = _jl_mergesort(a, 1, length(a), Array(T,length(a)))
sortr!{T}(a::AbstractVector{T}) = _jl_mergesort_r(a, 1, length(a), Array(T,length(a)))
sort!{T}(lt::Function, a::AbstractVector{T}) =
    _jl_mergesort_lt(lt, a, 1, length(a), Array(T,length(a)))

## special sorting for floating-point arrays ##

@_jl_sort_functions "_fp_pos" :(_jl_fp_pos_lt($a,$b)) :(_jl_fp_pos_le($a,$b))
@_jl_sort_functions "_fp_neg" :(_jl_fp_neg_lt($a,$b)) :(_jl_fp_neg_le($a,$b))

# push NaNs to the end of a, returning # of non-NaNs
function _jl_nans_to_end{T<:Float}(a::AbstractVector{T})
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

function sort!{T<:Float}(a::AbstractVector{T})
    i, j = 1, _jl_nans_to_end(a)
    while true
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
    _jl_quicksort_fp_pos(a, i, length(a))
    return a
end

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
    if dim == 1; return each_col(f,a); end
    if dim == 2; return each_row(f,a); end
    error("invalid matrix dimensions: $dim")
end

each_col(f::Function, a::AbstractMatrix) = each_col!(f,copy(a))
each_row(f::Function, a::AbstractMatrix) = each_row!(f,copy(a))
each_vec(f::Function, a::AbstractMatrix, d::Integer) = each_vec!(f,copy(a),d)

## other sorting functions defined in terms of sort! ##

macro in_place_matrix_op(out_of_place, args...)
    in_place = symbol("$(out_of_place)!")
    quote
        function ($in_place)(($args...), a::AbstractMatrix, dim::Int)
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
        ($in_place)(($args...), a::AbstractArray) = ($in_place)($(args...), a,1)

        ($out_of_place)(($args...), a::AbstractVector) = ($in_place)($(args...), copy(a))
        ($out_of_place)(($args...), a::AbstractArray, d::Int) = ($in_place)($(args...), copy(a), d)
        ($out_of_place)(($args...), a::AbstractArray) = ($out_of_place)($(args...), a,1)
    end
end

@in_place_matrix_op sort
@in_place_matrix_op sort lt::Function
@in_place_matrix_op sortr

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
    _jl_mergesort(copy(a), linspace(1,length(a)), 1, length(a),
                  Array(T, length(a)), Array(Int, length(a)))

function issorted(v::AbstractVector)
  for i = 1:length(v)-1
      if sortlt(v[i+1], v[i])
          return false
      end
  end
  return true
end

function _jl_quickselect(a::AbstractVector, k::Integer, lo::Integer, hi::Integer)
    if k < lo || k > hi; error("k is out of bounds"); end

    while true

        if lo == hi; return a[lo]; end

        # pivot_ind = partition(a, lo, hi)
        i, j = lo, hi
        pivot = a[randival(lo,hi)]
        while i < j
            while a[i] < pivot; i += 1; end
            while a[j] > pivot; j -= 1; end
            if a[i] == a[j]
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

select(a::AbstractVector, k::Integer) = _jl_quickselect(copy(a), k, 1, length(a))
select!(a::AbstractVector, k::Integer) = _jl_quickselect(a, k, 1, length(a))
