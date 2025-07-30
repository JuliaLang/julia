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

struct MyTwoVec
    x::Int32
    y::Int32
end

Base.@ccallable "copyto_and_sum" function badname(fromto::CVectorPair{Float32})::Float32
    from, to = unsafe_wrap(Array, fromto.from.data, fromto.from.length), unsafe_wrap(Array, fromto.to.data, fromto.to.length)
    copyto!(to, from)
    return sum(to)
end

Base.@ccallable function countsame(list::Ptr{MyTwoVec}, n::Int32)::Int32
    list = unsafe_wrap(Array, list, n)
    count = 0
    for v in list
        count += v.x == v.y
    end
    return count
end

export countsame, copyto_and_sum

# FIXME? varargs
# Base.@ccallable function printints(x::Cint...)::Nothing
#     for i in 1:length(x)
#         print(x[i], " ")
#     end
#     println()
# end

end
