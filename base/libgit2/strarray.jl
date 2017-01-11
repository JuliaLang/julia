# This file is a part of Julia. License is MIT: http://julialang.org/license


function Base.cconvert(::Type{Ptr{StrArrayStruct}}, x::Vector)
    str_ref = Base.cconvert(Ref{Cstring}, x)
    sa_ref = Ref(StrArrayStruct(Base.unsafe_convert(Ref{Cstring}, str_ref), length(x)))
    sa_ref, str_ref
end
function Base.unsafe_convert(::Type{Ptr{StrArrayStruct}}, rr::Tuple{Ref{StrArrayStruct}, Ref{Cstring}})
    Base.unsafe_convert(Ptr{StrArrayStruct}, first(rr))
end

function Base.convert(::Type{Vector{String}}, sa::StrArrayStruct)
    [unsafe_string(unsafe_load(sa.strings, i)) for i = 1:sa.count]
end
