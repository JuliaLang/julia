# This file is a part of Julia. License is MIT: https://julialang.org/license

module StandardSortingAlgorithms

using Base.Sort, Base.Order
using Base.Sort: AdaptiveSort, BitSigned, QuickSortAlg, MergeSortAlg, midpoint, workspace
import Base.Sort.sort!, Base.Sort.defalg

defalg(::AbstractArray) = DEFAULT_STABLE
defalg(::AbstractArray{<:Union{Number, Missing}}) = DEFAULT_UNSTABLE
defalg(::AbstractArray{Missing}) = DEFAULT_UNSTABLE # for method disambiguation
defalg(::AbstractArray{Union{}}) = DEFAULT_UNSTABLE # for method disambiguation

defalg(::typeof(DEFAULT_UNSTABLE)) = DEFAULT_UNSTABLE

include("AdaptiveSort.jl")
include("QuickSort.jl")
include("MergeSort.jl")
include("RadixSort.jl")
include("uint_mappings.jl")
include("sort_int_range.jl")
include("Float.jl")

end
