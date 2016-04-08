# This file is a part of Julia. License is MIT: http://julialang.org/license

###### Function Objects ("Functors") ######

# More promote_op rules

promote_op{T<:Integer}(::typeof(^), ::Type{Bool}, ::Type{T}) = Bool
