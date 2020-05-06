#Methods some Some can broadcast
getindex(s::Some) = s.value
getindex(s::Some, ::CartesianIndex{0}) = s.value

iterate(s::Some) = (s.value, nothing)
iterate( ::Some, s) = nothing

ndims(::Some) = 0
ndims(::Type{<:Some}) = 0

length(::Some) = 1
size(::Some) = ()
axes(::Some) = ()

IteratorSize(::Type{<:Some}) = HasShape{0}()
Broadcast.broadcastable(s::Some) = s

eltype(::Some{T})       where {T} = T
eltype(::Type{Some{T}}) where {T} = T
