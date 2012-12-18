## standard sort comparisons ##

module Sort

export 
    @in_place_matrix_op,
    issorted,
    issorted_r,
    issorted_by,
    order,
    search_sorted,
    search_sorted_r,
    search_sorted_by,
    search_sorted_first,
    search_sorted_first_r,
    search_sorted_first_by,
    search_sorted_last,
    search_sorted_last_r,
    search_sorted_last_by,
    select,
    select!,
    select_r,
    select_r!,
    select_by,
    select_by!,
    sort,
    sort!,
    sort_by,
    sort_by!,
    sortr,
    sortr!,
    sortperm,
    sortperm!,
    sortperm_r,
    sortperm_r!,
    sortperm_by,
    sortperm_by!,

    insertionsort,
    insertionsort!,
    insertionsort_r,
    insertionsort_r!,
    insertionsort_by,
    insertionsort_by!,
    insertionsort_perm,
    insertionsort_perm!,
    insertionsort_perm_r,
    insertionsort_perm_r!,
    insertionsort_perm_by,
    insertionsort_perm_by!,
    mergesort,
    mergesort!,
    mergesort_r,
    mergesort_r!,
    mergesort_by,
    mergesort_by!,
    mergesort_perm,
    mergesort_perm!,
    mergesort_perm_r,
    mergesort_perm_r!,
    mergesort_perm_by,
    mergesort_perm_by!,
    quicksort,
    quicksort!,
    quicksort_r,
    quicksort_r!,
    quicksort_by,
    quicksort_by!,
    timsort,
    timsort!,
    timsort_r,
    timsort_r!,
    timsort_by,
    timsort_by!,
    timsort_perm,
    timsort_perm!,
    timsort_perm_r,
    timsort_perm_r!,
    timsort_perm_by,
    timsort_perm_by!

import Base.sort, Base.issorted, Base.sort, Base.sort!, Base.sortperm, Base.slt_int,
       Base.unbox, Base.sle_int, Base.length

_jl_fp_pos_lt(x::Float32, y::Float32) = slt_int(unbox(Float32,x),unbox(Float32,y))
_jl_fp_pos_lt(x::Float64, y::Float64) = slt_int(unbox(Float64,x),unbox(Float64,y))
_jl_fp_pos_le(x::Float32, y::Float32) = sle_int(unbox(Float32,x),unbox(Float32,y))
_jl_fp_pos_le(x::Float64, y::Float64) = sle_int(unbox(Float64,x),unbox(Float64,y))

_jl_fp_neg_lt(x::Float32, y::Float32) = slt_int(unbox(Float32,y),unbox(Float32,x))
_jl_fp_neg_lt(x::Float64, y::Float64) = slt_int(unbox(Float64,y),unbox(Float64,x))
_jl_fp_neg_le(x::Float32, y::Float32) = sle_int(unbox(Float32,y),unbox(Float32,x))
_jl_fp_neg_le(x::Float64, y::Float64) = sle_int(unbox(Float64,y),unbox(Float64,x))

## internal sorting functionality ##

include("timsort.jl")

for (suffix, lt, args) in (("",    (a,b)->:(isless($a,$b)), ()),
                           ("_r",  (a,b)->:(isless($b,$a)), ()),
                           ("",    (a,b)->:(lt($a,$b)), (:(lt::Function),)),
                           ("_by", (a,b)->:(isless(by($a),by($b))), (:(by::Function),)),
                           ## special sorting for floating-point arrays ##
                           ("_fp_pos", (a,b)->:(_jl_fp_pos_lt($a,$b)), ()),
                           ("_fp_neg", (a,b)->:(_jl_fp_neg_lt($a,$b)), ()))
    insertionsort = symbol("insertionsort$(suffix)")
    insertionsort! = symbol("insertionsort$(suffix)!")
    insertionsort_perm = symbol("insertionsort_perm$(suffix)")
    insertionsort_perm! = symbol("insertionsort_perm$(suffix)!")
    quicksort = symbol("quicksort$(suffix)")
    quicksort! = symbol("quicksort$(suffix)!")
    quicksort_perm = symbol("quicksort_perm$(suffix)")
    quicksort_perm! = symbol("quicksort_perm$(suffix)!")
    mergesort = symbol("mergesort$(suffix)")
    mergesort! = symbol("mergesort$(suffix)!")
    mergesort_perm = symbol("mergesort_perm$(suffix)")
    mergesort_perm! = symbol("mergesort_perm$(suffix)!")
    pivot_middle = symbol("_jl_pivot_middle$(suffix)")
    issorted = symbol("issorted$(suffix)")
    _jl_quickselect = symbol("_jl_quickselect$(suffix)")
    select = symbol("select$(suffix)")
    select! = symbol("select$(suffix)!")
    search_sorted = symbol("search_sorted$(suffix)")
    search_sorted_first = symbol("search_sorted_first$(suffix)")
    search_sorted_last = symbol("search_sorted_last$(suffix)")
    sortperm = symbol("sortperm$(suffix)")
    sortperm! = symbol("sortperm$(suffix)!")
@eval begin

# sorting should be stable
# Thus, if a permutation is required, or records are being sorted
# a stable sort should be used.
# If only numbers are being sorted, a faster quicksort can be used.

# fast sort for small arrays
function ($insertionsort!)($(args...), a::AbstractVector, lo::Int, hi::Int)
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

($insertionsort!)($(args...), a::AbstractVector) = ($insertionsort!)($(args...), a, 1, length(a))
($insertionsort)($(args...), a::AbstractVector, args2...) = ($insertionsort!)($(args...), copy(a), args2...)

# permutes an auxilliary array mirroring the sort
function ($insertionsort_perm!)($(args...), a::AbstractVector, p::AbstractVector{Int}, lo::Int, hi::Int)
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

($insertionsort_perm!){T}($(args...), a::AbstractVector{T}, p::AbstractVector{Int}) =
    ($insertionsort_perm!)($(args...), a, p, 1, length(a))
($insertionsort_perm!){T}($(args...), a::AbstractVector{T}) =
    ($insertionsort_perm!)($(args...), a, [1:length(a)])
($insertionsort_perm){T}($(args...), a::AbstractVector{T}, args2...) =
    ($insertionsort_perm!)($(args...), copy(a), [1:length(a)], args2...)


($pivot_middle)($(args...),a,b,c) = $(lt(:a,:b)) ? ($(lt(:b,:c)) ? b : c) : ($(lt(:a,:c)) ? a : c)

# very fast but unstable
function ($quicksort!)($(args...), a::AbstractVector, lo::Int, hi::Int)
    while hi > lo
        if hi-lo <= 20
            return $(expr(:call, insertionsort!, args..., :a, :lo, :hi))
        end
        i, j = lo, hi
        # pivot = (a[lo]+a[hi])/2                                               # 1.14x
          pivot = a[(lo+hi)>>>1]                                                # 1.15x
        # pivot = (a[lo]+a[hi]+a[(lo+hi)>>>1])/3                                # 1.16x
        # pivot = _jl_pivot_middle($(args...), a[lo], a[hi], a[(lo+hi)>>>1])    # 1.23x
        # pivot = a[randival(lo,hi)]                                            # 1.28x
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
            $(expr(:call, quicksort!, args..., :a, :lo, :j))
        end
        lo = i
    end
    return a
end

($quicksort!)($(args...), a::AbstractVector) = ($quicksort!)($(args...), a, 1, length(a))
($quicksort)($(args...), a::AbstractVector) = ($quicksort!)($(args...), copy(a), 1, length(a))

# less fast & not in-place, but stable
function ($mergesort!)($(args...), a::AbstractVector, lo::Int, hi::Int, b::AbstractVector)
    if lo < hi
        if hi-lo <= 20
            return ($insertionsort!)($(args...), a, lo, hi)
        end

        m = (lo+hi)>>>1
        ($mergesort!)($(args...), a, lo, m, b)
        ($mergesort!)($(args...), a, m+1, hi, b)

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

($mergesort!){T}($(args...), a::AbstractVector{T}) = ($mergesort!)($(args...), a, 1, length(a), Array(T,length(a)))
($mergesort){T}($(args...), a::AbstractVector{T}) = 
    ($mergesort!)($(args...), copy(a), 1, length(a), Array(T,length(a)))

# permutes auxilliary arrays mirroring the sort
function ($mergesort_perm!)($(args...),
                            a::AbstractVector, p::AbstractVector{Int}, lo::Int, hi::Int,
                            b::AbstractVector, pb::AbstractVector{Int})
    if lo < hi
        if hi-lo <= 20
            return ($insertionsort_perm!)($(args...), a, p, lo, hi)
        end

        m = (lo+hi)>>>1
        ($mergesort_perm!)($(args...), a, p, lo, m, b, pb)
        ($mergesort_perm!)($(args...), a, p, m+1, hi, b, pb)

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

($mergesort_perm!){T}($(args...), a::AbstractVector{T}, p::AbstractVector{Int}) = 
    ($mergesort_perm!)($(args...), a, p, 1, length(a), Array(T,length(a)), Array(Int,length(a)))
($mergesort_perm!){T}($(args...), a::AbstractVector{T}) = 
    ($mergesort_perm!)($(args...), a, [1:length(a)])
($mergesort_perm){T}($(args...), a::AbstractVector{T}, args2...) = 
    ($mergesort_perm!)($(args...), copy(a), [1:length(a)], args2...)


function ($issorted)($(args...), v::AbstractVector)
  for i = 1:length(v)-1
      if $(lt(:(v[i+1]), :(v[i])))
          return false
      end
  end
  return true
end

function ($_jl_quickselect)($(args...), a::AbstractArray, k::Int, lo::Int, hi::Int)
    if k < lo || k > hi; error("k is out of bounds"); end

    while true

        if lo == hi; return a[lo]; end

        i, j = lo, hi
        pivot = ($pivot_middle)($(args...), a[lo], a[hi], a[(lo+hi)>>>1])
        while i < j
            while $(lt(:(a[i]), :(pivot))); i += 1; end
            while $(lt(:(pivot), :(a[j]))); j -= 1; end
            #if isequal(a[i], a[j])
            if !$(lt(:(a[i]), :(a[j]))) && !$(lt(:(a[j]), :(a[i])))
                i += 1
            elseif i < j
                a[i], a[j] = a[j], a[i]
            end
        end
        pivot_ind = j

        len = pivot_ind - lo + 1
        if k == len
            return a[pivot_ind]
        elseif k <  len
            hi = pivot_ind - 1
        else
            lo = pivot_ind + 1
            k = k - len
        end

    end # while true...

end

($select!)($(args...), a::AbstractArray, k::Int) = ($_jl_quickselect)($(args...), a, k, 1, length(a))
($select)($(args...), a::AbstractArray, k::Int) = ($_jl_quickselect)($(args...), copy(a), k, 1, length(a))

($search_sorted)($(args...), a::Vector, x) = ($search_sorted_first)($(args...), a, x, 1, length(a))

($search_sorted_last)($(args...), a::Vector, x) = ($search_sorted_last)($(args...), a, x, 1, length(a))

function ($search_sorted_last)($(args...), a::Vector, x, lo::Int, hi::Int)
    ## Index of the last value of vector a that is less than or equal to x.
    ## Returns 0 if x is less than all values of a.
    ## 
    ## Good reference: http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary 
    lo = lo-1
    hi = hi+1
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

($search_sorted_first)($(args...), a::Vector, x) = ($search_sorted_first)($(args...), a, x, 1, length(a))

function ($search_sorted_first)($(args...), a::Vector, x, lo::Int, hi::Int)
    ## Index of the first value of vector a that is greater than or equal to x.
    ## Returns length(a) + 1 if x is greater than all values in a.
    ## 
    ## Good reference: http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary 
    lo = lo-1
    hi = hi+1
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

($sortperm){T}($(args...), a::AbstractVector{T}, args2...) = ($mergesort_perm)($(args...), a, args2...)
($sortperm!){T}($(args...), a::AbstractVector{T}, args2...) = ($mergesort_perm!)($(args...), a, args2...)

end; end # @eval / for

## external sorting functions ##

sort!{T<:Real}(a::AbstractVector{T})  = quicksort!(a, 1, length(a))
sortr!{T<:Real}(a::AbstractVector{T}) = quicksort_r!(a, 1, length(a))
sort!{T}(a::AbstractVector{T})  = mergesort!(a, 1, length(a), Array(T,length(a)))
sortr!{T}(a::AbstractVector{T}) = mergesort_r!(a, 1, length(a), Array(T,length(a)))

sort!{T}(lt::Function, a::AbstractVector{T}) =
    mergesort!(lt, a, 1, length(a), Array(T,length(a)))
sort_by!{T}(by::Function, a::AbstractVector{T}) =
    mergesort_by!(by, a, 1, length(a), Array(T,length(a)))

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
    quicksort_fp_neg!(a, 1, j)
    quicksort_fp_pos!(a, i, n)
    return a
end


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

@in_place_matrix_op insertionsort
@in_place_matrix_op insertionsort lt::Function
@in_place_matrix_op insertionsort_r
@in_place_matrix_op insertionsort_by by::Function

@in_place_matrix_op quicksort
@in_place_matrix_op quicksort lt::Function
@in_place_matrix_op quicksort_r
@in_place_matrix_op quicksort_by by::Function

@in_place_matrix_op mergesort
@in_place_matrix_op mergesort lt::Function
@in_place_matrix_op mergesortr
@in_place_matrix_op mergesort_by by::Function

@in_place_matrix_op timsort
@in_place_matrix_op timsort lt::Function
@in_place_matrix_op timsortr
@in_place_matrix_op timsort_by by::Function

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

order(a::AbstractVector) = sortperm(a)[2]

end # module Sort
