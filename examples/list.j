abstract List{T}

type Nil{T} <: List{T}
end

type Cons{T} <: List{T}
    head::T
    tail::List{T}
end

cons{T}(h::List{T}, t::List{List{T}}) = Cons{List{T}}(h, t)
cons(x,y) = Cons(x,y)

nil(T) = Nil{T}()
nil() = nil(Any)

head(x::Cons) = x.head
tail(x::Cons) = x.tail

function show{T}(l::List{T})
    if isa(l,Nil)
        if is(T,Any)
            print("nil()")
        else
            print("nil(")
            show(T)
            print(")")
        end
    else
        print("list(")
        while true
            show(head(l))
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

list() = nil()
list{T}(first::T) = cons(first, nil(T))
list(first, rest...) = cons(first, list(rest...))

length(l::Nil) = 0
length(l::Cons) = 1 + length(tail(l))

map(f, l::Nil) = l
map(f, l::Cons) = cons(f(head(l)), map(f, tail(l)))

copylist(l::Nil) = l
copylist(l::Cons) = cons(head(l), copylist(tail(l)))

function append2(a, b)
    if isa(a,Nil)
        b
    else
        cons(head(a), append2(tail(a), b))
    end
end

append(lst::List) = lst

function append{T}(lst::List{T}, lsts...)
    n = length(lsts)
    l = lsts[n]
    for i = (n-1):-1:1
        l = append2(lsts[i], l)
    end
    return append2(lst, l)
end
