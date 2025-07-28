module SimpleLib
# Test the logging of entrypoints and types in a C-callable Julia library.

struct CVector{T}
    length::Cint
    data::Ptr{T}
end

struct CVectorPair{T}
    from::CVector{T}
    to::CVector{T}
end

Base.@ccallable "copyto_and_sum" function badname(fromto::CVectorPair{Float32})::Float32
    from, to = unsafe_wrap(Array, fromto.from.data, fromto.from.length), unsafe_wrap(Array, fromto.to.data, fromto.to.length)
    copyto!(to, from)
    return sum(to)
end

# FIXME? varargs
# Base.@ccallable function printints(x::Cint...)::Nothing
#     for i in 1:length(x)
#         print(x[i], " ")
#     end
#     println()
# end

end
