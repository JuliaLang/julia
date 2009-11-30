type List[`T]
    maxsize:: Size
    size:: Size
    offset:: Size
    data:: Buffer[`T]
end

function list(elts...)
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
    return new(List[Any], maxsize, n, 0, data)
end

function ref(l::List, i::Index)
    if i > l.size
        error("Out of bounds")
    end
    return l.data[i+l.offset]
end

function set(l::List, i::Index, elt)
    if i > l.size
        error("Out of bounds")
    end
    l.data[i+l.offset] = elt
end

length(l::List) = l.size

function print(l::List)
    print("{")
    for i=1:length(l)
        if i > 1
            print(", ")
        end
        print(l[i])
    end
    print("}")
end

function grow(a::List, inc::Size)
    if (inc == 0)
        return a
    end
    if (a.size + inc > a.maxsize - a.offset)
        newsize = a.maxsize*2
        while (a.size+inc > newsize-a.offset)
            newsize *= 2
        end
        newlist = new(Buffer[Any], newsize)
        a.maxsize = newsize
        for i=1:a.size
            newlist[i+a.offset] = a.data[i+a.offset]
        end
        a.data = newlist
    end
    a.size += inc
    return a
end

function push(a::List, item)
    grow(a, 1)
    a[a.size] = item
    return a
end

function pop(a::List)
    if (a.size == 0)
        error("pop: List is empty")
    end
    item = a[a.size]
    a[a.size] = ()
    a.size -= 1
    return item
end
