type EmptyList
end

type Cons
    head::Any
    tail::Union[EmptyList, Cons]
end

typealias List Union[EmptyList, Cons]

nil = new(EmptyList)

head(x::Cons) = x.head
tail(x::Cons) = x.tail
cons(x, y::List) = new(Cons,x,y)

function print(l::List)
    if istype(l,EmptyList)
        print("{}")
    else
        print("{")
        while true
            print(head(l))
            l = tail(l)
            if istype(l,Cons)
                print(", ")
            else
                break
            end
        end
        print("}")
    end
end

function list()
    return nil
end

function list(elts...)
    n = length(elts)
    first = cons(elts[1], nil)
    c = first
    for i = 2:n
        c.tail = cons(elts[i], nil)
        c = c.tail
    end
    return first
end

length(l::EmptyList) = 0
length(l::Cons) = 1 + length(tail(l))

map(f, l::EmptyList) = nil
map(f, l::Cons) = cons(f(head(l)), map(f, tail(l)))

copylist(l::EmptyList) = nil
copylist(l::Cons) = cons(head(l), copylist(tail(l)))

function append(lsts...)
    function append2(a, b)
        if istype(a,EmptyList)
            b
        else
            cons(head(a), append2(tail(a), b))
        end
    end
    n = length(lsts)
    l = nil
    for i = n:-1:1
        l = append2(lsts[i], l)
    end
    return l
end

type Expr
    head::Symbol
    args::List
end

expr(hd, args...)  = new(Expr, hd, list(args...))
exprl(hd, arglist) = new(Expr, hd, arglist)
