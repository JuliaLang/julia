type Pair
    a
    b
end

type Queue
    head::Union((),Pair)
    tail::Union((),Pair)

    Queue() = new((),())
end

isempty(q::Queue) = is(q.head,())

function enqueue(q::Queue, elt)
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
    if is(q.head,())
        q.tail = ()
    end
    elt
end

start(q::Queue) = q.head
done(q::Queue, elt) = is(elt,())
next(q::Queue, elt) = (elt.a, elt.b)
