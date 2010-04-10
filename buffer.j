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

function buffer_literal(t::Type, elts...)
    b = Buffer[t].new(length(elts))
    for i = 1:length(elts)
        b[i] = elts[i]
    end
    return b
end

function append[T](bs::Buffer[T]...)
    l = 0
    for b = bs
        l += length(b)
    end
    nb = Buffer[T].new(l)
    n = 1
    for b = bs
        for i = 1:length(b)
            nb[n] = b[i]
            n += 1
        end
    end
    return nb
end

expr(hd::Symbol, args...)  = Expr.new(hd, {args...})
exprl(hd::Symbol, arglist) = Expr.new(hd, arglist)

# iterating over buffers
start(b::Buffer) = 1
done(b::Buffer, i) = (i > length(b))
next(b::Buffer, i) = (b[i], i+1)

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
