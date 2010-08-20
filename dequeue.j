struct Pair
    a
    b
end

struct Queue
    head::Union((),Pair)
    tail::Union((),Pair)

    Queue() = new((),())
end

isempty(q::Queue) = is(q.head,())

function enq(q::Queue, elt)
    if isempty(q)
        q.head = Pair(elt,())
        q.tail = q.head
    else
        q.tail.b = Pair(elt,())
        q.tail = q.tail.b
    end
    q
end

function pop(q::Queue)
    if isempty(q)
        error("pop: queue is empty")
    end
    elt = q.head.a
    q.head = q.head.b
    elt
end

start(q::Queue) = q.head
done(q::Queue, elt) = is(elt,())
next(q::Queue, elt) = (elt.a, elt.b)

struct Dequeue{T}
    maxsize:: Size
    size:: Size
    offset:: Size
    data:: Array{T,1}
end

function dequeue(elts...)
    n = length(elts)
    if n < 4
        maxsize = 4
    else
        maxsize = n
    end
    data = Array(Any,maxsize)
    for i = 1:n
        data[i] = elts[i]
    end
    return Dequeue(maxsize, n, 0, data)
end

function ref(l::Dequeue, i::Index)
    if i > l.size
        error("Out of bounds")
    end
    return l.data[i+l.offset]
end

function assign(l::Dequeue, elt, i::Index)
    if i > l.size
        error("Out of bounds")
    end
    l.data[i+l.offset] = elt
    l
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
    if inc == 0
        return a
    end
    if a.size + inc > a.maxsize - a.offset
        newsize = a.maxsize*2
        while (a.size+inc > newsize-a.offset)
            newsize *= 2
        end
        newdeq = Array(Any,newsize)
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
    if a.size == 0
        error("pop: dequeue is empty")
    end
    item = a[a.size]
    a[a.size] = ()
    a.size -= 1
    return item
end

function insert(a::Dequeue, i::Int, item)
    sz = a.size
    if i > sz
        grow(a, i-sz)
    else
        grow(a, 1)
        for k=(sz+1):-1:(i+1)
            a[k] = a[k-1]
        end
    end
    a[i] = item
end
