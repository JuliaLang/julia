type List{T}

struct Nil{T} <: List{T}
end

struct Cons{T} <: List{T}
    head::T
    tail::List{T}
end

# make the type of Cons(Cons,List) be Cons{List}, not Cons{Cons}
Cons{T}(h::List{T}, t::List{List{T}}) = Cons{List{T}}.new(h, t)

nil(T) = Nil{T}.new()
nil() = nil(Any)

head(x::Cons) = x.head
tail(x::Cons) = x.tail

function print{T}(l::List{T})
    if isa(l,Nil)
        if is(T,Any)
            print("nil()")
        else
            print("nil(",T,")")
        end
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

list() = nil()
list{T}(first::T) = Cons(first, nil(T))
list(first, rest...) = Cons(first, list(rest...))

length(l::Nil) = 0
length(l::Cons) = 1 + length(tail(l))

map(f, l::Nil) = l
map(f, l::Cons) = Cons(f(head(l)), map(f, tail(l)))

copylist(l::Nil) = l
copylist(l::Cons) = Cons(head(l), copylist(tail(l)))

function append2(a, b)
    if isa(a,Nil)
        b
    else
        Cons(head(a), append2(tail(a), b))
    end
end

function append{T}(lst::List{T}, lsts...)
    n = length(lsts)
    l = nil(T)
    for i = n:-1:1
        l = append2(lsts[i], l)
    end
    return append2(lst, l)
end
