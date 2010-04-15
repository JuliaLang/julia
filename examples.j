function fib(n)
    if n < 2
        return n
    end
    return fib(n-1) + fib(n-2)
end

function fibnorecur(n)
    f = 1
    f1 = 0
    f2 = 1
    for i=2:n
        f = f1 + f2
        print(f)
        print(" ")
        tmp = f2
        f2 = f
        f1 = tmp
    end
    return f
end

function iterate(t::Tuple, body, it...)
    idx = length(t)-length(it)
    if (idx == 0)
        body(it)
    else
        for i = t[idx]
            iterate(t, body, i, it...)
        end
    end
end
