function cmp_with_less(x::Vector)
    count::Int = 0
    for i = 1:length(x)-1
        if x[i] < x[i+1]
            count += 1
        end
    end
    return count
end

function cmp_with_func(x::Vector, f::Function)
    count::Int = 0
    for i = 1:length(x)-1
        if f(x[i], x[i+1])
            count += 1
        end
    end
    return count
end

function compare_inline(len::Int, niter::Int)
    x = randn(len)
    print("With inline less: ")
    @time begin
        for n in 1:niter
            count = cmp_with_less(x)
        end
    end
    print("With function less: ")
    @time begin
        for n in 1:niter
            count = cmp_with_func(x, isless)
        end
    end
end
compare_inline(1_000_000, 10)
