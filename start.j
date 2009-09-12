type Array(a)
    dims: buffer(size)
    strides: buffer(size)
    data: buffer(a)
end

function ref(b:buffer(a), i:int32)
    return refany(b, unbox(i))
end

function `+`(x:int32, y:int32)
    return box(typename(int32), add_int32(unbox(x), unbox(y)))
end
