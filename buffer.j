ref[T](b::Buffer[T], i::Index) = bufferref(b, i)

function set[T](b::Buffer[T], x, i::Index)
    bufferset(b, i, convert(x,T))
    return x
end

length(b::Buffer) = bufferlen(b)

buffer() = Buffer[Bottom].new(0)

function buffer[T](elts::T...)
    b = Buffer[T].new(length(elts))
    for i = 1:length(elts)
        b[i] = elts[i]
    end
    return b
end

# iterating over buffers
start(b::Buffer) = 1
done(b::Buffer, i) = (i > length(b))
next(b::Buffer, i) = (b[i], i+1)

function apply_op(op::Function, b1::Buffer, b2::Buffer)
    n = length(b1) > length(b2) ? length(b1) : length(b2)
    T = (length(b1) > 0 && length(b2) > 0) ? typeof(op(b1[1],b2[1])) : 
        (length(b1) > 0 ? typeof(b1[1]) : typeof(b2[1]))
    b = Buffer[T].new(n)
    for i = 1:n
        b[i] = (length(b1) >= i && length(b2) >= i) ?
            op(b1[i],b2[i]) : (length(b1) >= i ? b1[i] : b2[i])
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
