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
        _print(f)
        _print(" ")
        tmp = f2
        f2 = f
        f1 = tmp
    end
    return f
end
