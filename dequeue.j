type Dequeue[`T]
    maxsize:: Size
    size:: Size
    offset:: Size
    data:: Buffer[`T]
end

function dequeue(elts...)
    n = length(elts)
    if n < 4
        maxsize = 4
    else
        maxsize = n
    end
    data = new(Buffer[Any], maxsize)
    for i = 1:n
        data[i] = elts[i]
    end
    return new(Dequeue[Any], maxsize, n, 0, data)
end

function ref(l::Dequeue, i::Index)
    if i > l.size
        error("Out of bounds")
    end
    return l.data[i+l.offset]
end

function set(l::Dequeue, i::Index, elt)
    if i > l.size
        error("Out of bounds")
    end
    l.data[i+l.offset] = elt
end

length(l::Dequeue) = l.size

function print(l::Dequeue)
    print("dequeue(")
    for i=1:length(l)
        if i > 1
            print(", ")
        end
        print(l[i])
    end
    print(")")
end

function grow(a::Dequeue, inc::Size)
    if (inc == 0)
        return a
    end
    if (a.size + inc > a.maxsize - a.offset)
        newsize = a.maxsize*2
        while (a.size+inc > newsize-a.offset)
            newsize *= 2
        end
        newdeq = new(Buffer[Any], newsize)
        a.maxsize = newsize
        for i=1:a.size
            newdeq[i+a.offset] = a.data[i+a.offset]
        end
        a.data = newdeq
    end
    a.size += inc
    return a
end

function push(a::Dequeue, item)
    grow(a, 1)
    a[a.size] = item
    return a
end

function pop(a::Dequeue)
    if (a.size == 0)
        error("pop: dequeue is empty")
    end
    item = a[a.size]
    a[a.size] = ()
    a.size -= 1
    return item
end
