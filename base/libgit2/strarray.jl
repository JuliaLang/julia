# This file is a part of Julia. License is MIT: http://julialang.org/license

function Base.cconvert{T<:AbstractString}(::Type{Ptr{StrArrayStruct}}, strs::Vector{T})
    data = [Base.cconvert(Cstring, s) for s in strs]
    ptrs = [Base.unsafe_convert(Cstring, d) for d in data]
    sa = Ref(StrArrayStruct(pointer(ptrs), length(ptrs)))
    sa, data, ptrs
end
function Base.unsafe_convert(::Type{Ptr{StrArrayStruct}}, tup::Tuple)
    Base.unsafe_convert(Ptr{StrArrayStruct}, first(tup))
end

function Base.unsafe_convert(::Type{Vector{String}}, sa::StrArrayStruct)
    [unsafe_string(unsafe_load(sa.strings, i)) for i = 1:sa.count]
end
