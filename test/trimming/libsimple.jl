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

struct CTree{T}
    # test that recursive datatypes work as expected
    children::CVector{CTree{T}}
end

Base.@ccallable "tree_size" function size(tree::CTree{Float64})::Int64
    children = unsafe_wrap(Array, tree.children.data, tree.children.length)
    # Return the size of this sub-tree
    return sum(Int64[
        size(child)
        for child in children
    ]; init=1)
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
