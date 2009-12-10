ref(b::UnboxedBuffer, i::Index) = box(typeof(b).parameters[1], bufferref(b, unbox(i)))
ref(b::Buffer[`T], i::Index) = bufferref(b, unbox(i))

function set(b::UnboxedBuffer, i::Index, x::Unboxable)
    bufferset(b, unbox(i), unbox(convert(x,typeof(b).parameters[1])))
    return x
end

function set(b::Buffer[`T], i::Index, x)
    bufferset(b, unbox(i), convert(x,T))
    return x
end

length(b::Buffer) = b.length

function buffer(elts::`T...)
    b = new(Buffer[T],length(elts))
    for i = 1:length(elts)
        b[i] = elts[i]
    end
    return b
end

function apply_op(op::Function, b1::Buffer, b2::Buffer)
    n = length(b1) > length(b2) ? length(b1) : length(b2)
    T = (length(b1) > 0 && length(b2) > 0) ? typeof(op(b1[1],b2[1])) : (length(b1) > 0 ? typeof(b1[1]) : typeof(b2[1]))
    b = new(Buffer[T],n)
    for i = 1:n
        b[i] = (length(b1) >= i && length(b2) >= i) ? op(b1[i],b2[i]) : (length(b1) >= i ? b1[i] : b2[i])
    end
    return b
end

function ==(b1::Buffer, b2::Buffer)
    if length(b1) != length(b2)
        return false
    end
    for i = 1:length(b1)
        if b1[i] != b2[i]
            return false
        end
    end
    return true
end
