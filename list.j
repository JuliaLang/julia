struct EmptyList
end

struct Cons
    head
    tail::Union(EmptyList, Cons)
end

typealias List Union(EmptyList, Cons)

nil = EmptyList()

head(x::Cons) = x.head
tail(x::Cons) = x.tail

function print(l::List)
    if isa(l,EmptyList)
        print("list()")
    else
        print("list(")
        while true
            print(head(l))
            l = tail(l)
            if isa(l,Cons)
                print(", ")
            else
                break
            end
        end
        print(")")
    end
end

list() = nil
list(first, rest...) = Cons(first, list(rest...))

length(l::EmptyList) = 0
length(l::Cons) = 1 + length(tail(l))

map(f, l::EmptyList) = nil
map(f, l::Cons) = Cons(f(head(l)), map(f, tail(l)))

copylist(l::EmptyList) = nil
copylist(l::Cons) = Cons(head(l), copylist(tail(l)))

function append2(a, b)
    if isa(a,EmptyList)
        b
    else
        Cons(head(a), append2(tail(a), b))
    end
end

function append(lsts...)
    n = length(lsts)
    l = nil
    for i = n:-1:1
        l = append2(lsts[i], l)
    end
    return l
end
