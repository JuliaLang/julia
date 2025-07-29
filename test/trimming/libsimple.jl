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

# Base.@ccallable function countsame(list::Ptr{MyTwoVec}, n::Int32)::Int32
Base.@ccallable function countsame(list::Vector{MyTwoVec})::Int32
    count = 0
    # list = unsafe_wrap(Array, list, n)
    for v in list
        if v.x == v.y
            count += 1
        end
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
