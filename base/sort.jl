## standard sort comparisons ##

module Sort

export 
    SortingAlgorithm,
    InsertionSort,
    QuickSort,
    MergeSort,
    #@in_place_matrix_op,
    issorted,
    issortedr,
    issortedby,
    order,
    searchsorted,
    searchsortedr,
    searchsortedby,
    searchsortedfirst,
    searchsortedfirstr,
    searchsortedfirstby,
    searchsortedlast,
    searchsortedlastr,
    searchsortedlastby,
    select,
    select!,
    selectr,
    selectr!,
    selectby,
    selectby!,
    sort,
    sort!,
    sortby,
    sortby!,
    sortr,
    sortr!,
    sortperm,
    sortperm!,
    sortpermr,
    sortpermr!,
    sortpermby,
    sortpermby!

import Base.sort, Base.issorted, Base.sort, Base.sort!, Base.sortperm, Base.slt_int,
       Base.unbox, Base.sle_int, Base.length

fp_pos_lt(x::Float32, y::Float32) = slt_int(unbox(Float32,x),unbox(Float32,y))
fp_pos_lt(x::Float64, y::Float64) = slt_int(unbox(Float64,x),unbox(Float64,y))
fp_pos_le(x::Float32, y::Float32) = sle_int(unbox(Float32,x),unbox(Float32,y))
fp_pos_le(x::Float64, y::Float64) = sle_int(unbox(Float64,x),unbox(Float64,y))

fp_neg_lt(x::Float32, y::Float32) = slt_int(unbox(Float32,y),unbox(Float32,x))
fp_neg_lt(x::Float64, y::Float64) = slt_int(unbox(Float64,y),unbox(Float64,x))
fp_neg_le(x::Float32, y::Float32) = sle_int(unbox(Float32,y),unbox(Float32,x))
fp_neg_le(x::Float64, y::Float64) = sle_int(unbox(Float64,y),unbox(Float64,x))

## internal sorting functionality ##

abstract SortingAlgorithm
type InsertionSort <: SortingAlgorithm end
type QuickSort <: SortingAlgorithm end
type MergeSort <: SortingAlgorithm end

for (suffix, lt, args) in (("",    (a,b)->:(isless($a,$b)), ()),
                           ("r",   (a,b)->:(isless($b,$a)), ()),
                           ("",    (a,b)->:(lt($a,$b)), (:(lt::Function),)),
                           ("by",  (a,b)->:(isless(by($a),by($b))), (:(by::Function),)),
                           ## special sorting for floating-point arrays ##
                           ("_fp_pos", (a,b)->:(fp_pos_lt($a,$b)), ()),
                           ("_fp_neg", (a,b)->:(fp_neg_lt($a,$b)), ()))
    sort = symbol("sort$(suffix)")
    sort! = symbol("sort$(suffix)!")
    sortperm = symbol("sortperm$(suffix)")
    sortperm! = symbol("sortperm$(suffix)!")
    insertionsort_args = tuple(expr(symbol("::"), expr(:curly, :Type, :InsertionSort)), args...)
    quicksort_args = tuple(expr(symbol("::"), expr(:curly, :Type, :QuickSort)), args...)
    mergesort_args = tuple(expr(symbol("::"), expr(:curly, :Type, :MergeSort)), args...)
    pivotmiddle = symbol("pivotmiddle$(suffix)")
    issorted = symbol("issorted$(suffix)")
    quickselect = symbol("quickselect$(suffix)")
    select = symbol("select$(suffix)")
    select! = symbol("select$(suffix)!")
    searchsorted = symbol("searchsorted$(suffix)")
    searchsortedfirst = symbol("searchsortedfirst$(suffix)")
    searchsortedlast = symbol("searchsortedlast$(suffix)")
@eval begin

# sorting should be stable
# Thus, if a permutation is required, or records are being sorted
# a stable sort should be used.
# If only numbers are being sorted, a faster quicksort can be used.

# fast sort for small arrays
function ($sort!)($(insertionsort_args...), a::AbstractVector, lo::Int, hi::Int)
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

($sort!)($(insertionsort_args...), a::AbstractVector) = ($sort!)(InsertionSort, $(args...), a, 1, length(a))
($sort)($(insertionsort_args...), a::AbstractVector, args2...) = ($sort!)(InsertionSort, $(args...), copy(a), args2...)

# permutes an auxilliary array mirroring the sort
function ($sortperm!)($(insertionsort_args...), a::AbstractVector, p::AbstractVector{Int}, lo::Int, hi::Int)
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

($sortperm!){T}($(insertionsort_args...), a::AbstractVector{T}, p::AbstractVector{Int}) =
    ($sortperm!)(InsertionSort, $(args...), a, p, 1, length(a))
($sortperm!){T}($(insertionsort_args...), a::AbstractVector{T}) =
    ($sortperm!)(InsertionSort, $(args...), a, [1:length(a)])
($sortperm){T}($(insertionsort_args...), a::AbstractVector{T}, args2...) =
    ($sortperm!)(InsertionSort, $(args...), copy(a), [1:length(a)], args2...)


($pivotmiddle)($(args...),a,b,c) = $(lt(:a,:b)) ? ($(lt(:b,:c)) ? b : c) : ($(lt(:a,:c)) ? a : c)

# very fast but unstable
function ($sort!)($(quicksort_args...), a::AbstractVector, lo::Int, hi::Int)
    while hi > lo
        if hi-lo <= 20
            return $(expr(:call, sort!, InsertionSort, args..., :a, :lo, :hi))
        end
        i, j = lo, hi
        # pivot = (a[lo]+a[hi])/2                                               # 1.14x
          pivot = a[(lo+hi)>>>1]                                                # 1.15x
        # pivot = (a[lo]+a[hi]+a[(lo+hi)>>>1])/3                                # 1.16x
        # pivot = pivot_middle($(args...), a[lo], a[hi], a[(lo+hi)>>>1])    # 1.23x
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
            $(expr(:call, sort!, QuickSort, args..., :a, :lo, :j))
        end
        lo = i
    end
    return a
end

($sort!)($(quicksort_args...), a::AbstractVector) = ($sort!)(QuickSort, $(args...), a, 1, length(a))
($sort)($(quicksort_args...), a::AbstractVector) = ($sort!)(QuickSort, $(args...), copy(a), 1, length(a))

# less fast & not in-place, but stable
function ($sort!)(MergeSort, $(args...), a::AbstractVector, lo::Int, hi::Int, b::AbstractVector)
    if lo < hi
        if hi-lo <= 20
            return ($sort!)(InsertionSort, $(args...), a, lo, hi)
        end

        m = (lo+hi)>>>1
        ($sort!)(MergeSort, $(args...), a, lo, m, b)
        ($sort!)(MergeSort, $(args...), a, m+1, hi, b)

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

($sort!){T}($(mergesort_args...), a::AbstractVector{T}) = ($sort!)(MergeSort, $(args...), a, 1, length(a), Array(T,length(a)))
($sort){T}($(mergesort_args...), a::AbstractVector{T}) = 
    ($sort!)(MergeSort, $(args...), copy(a), 1, length(a), Array(T,length(a)))

# permutes auxilliary arrays mirroring the sort
function ($sortperm!)($(mergesort_args...),
                      a::AbstractVector, p::AbstractVector{Int}, lo::Int, hi::Int,
                      b::AbstractVector, pb::AbstractVector{Int})
    if lo < hi
        if hi-lo <= 20
            return ($sortperm!)(InsertionSort, $(args...), a, p, lo, hi)
        end

        m = (lo+hi)>>>1
        ($sortperm!)($(args...), a, p, lo, m, b, pb)
        ($sortperm!)($(args...), a, p, m+1, hi, b, pb)

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

($sortperm!){T}($(mergesort_args...), a::AbstractVector{T}, p::AbstractVector{Int}) = 
    ($sortperm!)(MergeSort, $(args...), a, p, 1, length(a), Array(T,length(a)), Array(Int,length(a)))
($sortperm!){T}($(mergesort_args...), a::AbstractVector{T}) = 
    ($sortperm!)(MergeSort, $(args...), a, [1:length(a)])
($sortperm){T}($(mergesort_args...), a::AbstractVector{T}, args2...) = 
    ($sortperm!)(MergeSort, $(args...), copy(a), [1:length(a)], args2...)


function ($issorted)($(args...), v::AbstractVector)
  for i = 1:length(v)-1
      if $(lt(:(v[i+1]), :(v[i])))
          return false
      end
  end
  return true
end

function ($quickselect)($(args...), a::AbstractArray, k::Int, lo::Int, hi::Int)
    if k < lo || k > hi; error("k is out of bounds"); end

    while true

        if lo == hi; return a[lo]; end

        i, j = lo, hi
        pivot = ($pivotmiddle)($(args...), a[lo], a[hi], a[(lo+hi)>>>1])
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

($select!)($(args...), a::AbstractArray, k::Int) = ($quickselect)($(args...), a, k, 1, length(a))
($select)($(args...), a::AbstractArray, k::Int) = ($quickselect)($(args...), copy(a), k, 1, length(a))

($searchsorted)($(args...), a::Vector, x) = ($searchsortedfirst)($(args...), a, x, 1, length(a))

($searchsortedlast)($(args...), a::Vector, x) = ($searchsortedlast)($(args...), a, x, 1, length(a))

function ($searchsortedlast)($(args...), a::Vector, x, lo::Int, hi::Int)
    ## Index of the last value of vector a that is less than or equal to x.
    ## Returns 0 if x is less than all values of a.
    ## 
    ## Good reference: http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary 
    lo = lo-1
    hi = hi+1
    while lo < hi-1
        i = (lo+hi)>>>1
        if $(lt(:(x), :(a[i])))
            hi = i
        else
            lo = i
        end
    end
    lo
end

($searchsortedfirst)($(args...), a::Vector, x) = ($searchsortedfirst)($(args...), a, x, 1, length(a))

function ($searchsortedfirst)($(args...), a::Vector, x, lo::Int, hi::Int)
    ## Index of the first value of vector a that is greater than or equal to x.
    ## Returns length(a) + 1 if x is greater than all values in a.
    ## 
    ## Good reference: http://www.tbray.org/ongoing/When/200x/2003/03/22/Binary 
    lo = lo-1
    hi = hi+1
    while lo < hi-1
        i = (lo+hi)>>>1
        if $(lt(:(a[i]), :(x)))
            lo = i
        else
            hi = i
        end
    end
    hi
end

($sortperm){T}($(args...), a::AbstractVector{T}, args2...) = ($sortperm)(MergeSort, $(args...), a, args2...)
($sortperm!){T}($(args...), a::AbstractVector{T}, args2...) = ($sortperm!)(MergeSort, $(args...), a, args2...)

end; end # @eval / for

include("timsort.jl")

searchsortedlastr(a::Ranges, x::Real) = searchsortedlast(a, x)
searchsortedfirstr(a::Ranges, x::Real) = searchsortedfirst(a, x)
searchsortedr(a::Ranges, x::Real) = searchsorted(a, x)
searchsorted(a::Ranges, x::Real) = searchsortedfirst(a, x)

searchsortedlast(a::Ranges, x::Real) =
    max(min(int(fld(x - a[1], step(a))) + 1, length(a)), 0)

function searchsortedfirst(a::Ranges, x::Real)
    n = x - a[1]
    s = step(a)
    max(min(int(fld(n, s)) + (rem(n, s) != 0), length(a)), 0) + 1
end

## external sorting functions ##

sort!{T<:Real}(a::AbstractVector{T})  = sort!(QuickSort, a, 1, length(a))
sortr!{T<:Real}(a::AbstractVector{T}) = sortr!(QuickSort, a, 1, length(a))
sort!{T}(a::AbstractVector{T})  = sort!(MergeSort, a, 1, length(a), Array(T,length(a)))
sortr!{T}(a::AbstractVector{T}) = sortr!(MergeSort, a, 1, length(a), Array(T,length(a)))

sort!{T}(lt::Function, a::AbstractVector{T}) =
    sort!(MergeSort, lt, a, 1, length(a), Array(T,length(a)))
sort_by!{T}(by::Function, a::AbstractVector{T}) =
    sortby!(MergeSort, by, a, 1, length(a), Array(T,length(a)))

# push NaNs to the end of a, returning # of non-NaNs
function nans_to_end{T<:FloatingPoint}(a::AbstractVector{T})
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
    n = nans_to_end(a)
    i, j = 1, n
    while true
        # TODO: faster positive negative int check?
        while i <= j && fp_pos_lt(a[i],zero(T)); i += 1; end
        while i <= j && fp_pos_le(zero(T),a[j]); j -= 1; end
        if i <= j
            a[i], a[j] = a[j], a[i]
            i += 1
            j -= 1
        else
            break
        end
    end
    sort_fp_neg!(QuickSort, a, 1, j)
    sort_fp_pos!(QuickSort, a, i, n)
    return a
end

## other sorting functions defined in terms of sort! ##

# macro in_place_matrix_op(out_of_place, alg, args...)
#     in_place = esc(symbol("$(out_of_place)!"))
#     out_of_place = esc(out_of_place)
#     in_args = esc(args)
#     if alg != ()
#         in_args = tuple(expr(symbol("::"), expr(:curly, :Type, symbol("$alg"))), args...)
#         args = tuple(symbol("$alg"), args...)
#     end
#     quote
#         function ($in_place)($(in_args...), a::AbstractMatrix, dim::Int)
#             m = size(a,1)
#             if dim == 1
#                 for i = 1:m:length(a)
#                     ($in_place)($(args...), sub(a, i:(i+m-1)))
#                 end
#             elseif dim == 2
#                 for i = 1:m
#                     ($in_place)($(args...), sub(a, i:m:length(a)))
#                 end
#             end
#             return a
#         end
#         # TODO: in-place generalized AbstractArray implementation
#         ($in_place)($(in_args...), a::AbstractArray) = ($in_place)($(args...), a,1)

#         ($out_of_place)($(in_args...), a::AbstractVector) = ($in_place)($(args...), copy(a))
#         ($out_of_place)($(in_args...), a::AbstractArray, d::Int) = ($in_place)($(args...), copy(a), d)
#         ($out_of_place)($(in_args...), a::AbstractArray) = ($out_of_place)($(args...), a,1)
#     end
# end

# @in_place_matrix_op sort   () ()
# @in_place_matrix_op sort   () (:(lt::Function),)
# @in_place_matrix_op sortr  () ()
# @in_place_matrix_op sortby () (:(by::Function),)

# @in_place_matrix_op sort   :InsertionSort ()
# @in_place_matrix_op sort   :InsertionSort (:(lt::Function),)
# @in_place_matrix_op sortr  :InsertionSort ()
# @in_place_matrix_op sortby :InsertionSort (:(by::Function),)

# @in_place_matrix_op sort   :QuickSort ()
# @in_place_matrix_op sort   :QuickSort (:(lt::Function),)
# @in_place_matrix_op sortr  :QuickSort ()
# @in_place_matrix_op sortby :QuickSort (:(by::Function),)

# @in_place_matrix_op sort   :MergeSort ()
# @in_place_matrix_op sort   :MergeSort (:(lt::Function),)
# @in_place_matrix_op sortr  :MergeSort ()
# @in_place_matrix_op sortby :MergeSort (:(by::Function),)

# @in_place_matrix_op sort   :TimSort ()
# @in_place_matrix_op sort   :TimSort (:(lt::Function),)
# @in_place_matrix_op sortr  :TimSort ()
# @in_place_matrix_op sortby :TimSort (:(by::Function),)

for (out_of_place, alg, args) in ((:sort,   (), () ),
                                  (:sort,   (), (:(lt::Function),)),
                                  (:sortr,  (), () ),
                                  (:sortby, (), (:(by::Function),)),
                                  
                                  (:sort,   :InsertionSort, () ),
                                  (:sort,   :InsertionSort, (:(lt::Function),)),
                                  (:sortr,  :InsertionSort, () ),
                                  (:sortby, :InsertionSort, (:(by::Function),)),

                                  (:sort,   :QuickSort, () ),
                                  (:sort,   :QuickSort, (:(lt::Function),)),
                                  (:sortr,  :QuickSort, () ),
                                  (:sortby, :QuickSort, (:(by::Function),)),

                                  (:sort,   :MergeSort, () ),
                                  (:sort,   :MergeSort, (:(lt::Function),)),
                                  (:sortr,  :MergeSort, () ),
                                  (:sortby, :MergeSort, (:(by::Function),)),

                                  (:sort,   :TimSort, () ),
                                  (:sort,   :TimSort, (:(lt::Function),)),
                                  (:sortr,  :TimSort, () ),
                                  (:sortby, :TimSort, (:(by::Function),)))

    in_place = symbol("$(out_of_place)!")
    if alg == ()
        in_args = args
    else
        in_args = tuple(expr(symbol("::"), expr(:curly, :Type, alg)), args...)
        args = tuple(alg, args...)
    end
@eval begin
    function ($in_place)($(in_args...), a::AbstractMatrix, dim::Int)
        m = size(a,1)
        if dim == 1
            for i = 1:m:length(a)
                ($in_place)($(args...), sub(a, i:(i+m-1)))
            end
        elseif dim == 2
            for i = 1:m
                ($in_place)($(args...), sub(a, i:m:length(a)))
            end
        end
        return a
    end
    # TODO: in-place generalized AbstractArray implementation
    ($in_place)($(in_args...), a::AbstractArray) = ($in_place)($(args...), a,1)

    ($out_of_place)($(in_args...), a::AbstractVector) = ($in_place)($(args...), copy(a))
    ($out_of_place)($(in_args...), a::AbstractArray, d::Int) = ($in_place)($(args...), copy(a), d)
    ($out_of_place)($(in_args...), a::AbstractArray) = ($out_of_place)($(args...), a,1)
end; end

# TODO: implement generalized in-place, ditch this
function sort(a::AbstractArray, dim::Int)
    X = similar(a)
    n = size(a,dim)
    if dim == 1
        for i = 1:n:length(a)
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
