# This file is a part of Julia. License is MIT: https://julialang.org/license

function Base.cconvert(::Type{Ptr{StrArrayStruct}}, x::Vector)
    str_ref = Base.cconvert(Ref{Cstring}, x)
    sa_ref = Ref(StrArrayStruct(Base.unsafe_convert(Ref{Cstring}, str_ref), length(x)))
    sa_ref, str_ref
end
function Base.unsafe_convert(::Type{Ptr{StrArrayStruct}}, rr::Tuple{Ref{StrArrayStruct}, Ref{Cstring}})
    Base.unsafe_convert(Ptr{StrArrayStruct}, first(rr))
end

Base.length(sa::StrArrayStruct) = sa.count
function Base.iterate(sa::StrArrayStruct, state=1)
    state > sa.count && return nothing
    (unsafe_string(unsafe_load(sa.strings, state)), state+1)
end
