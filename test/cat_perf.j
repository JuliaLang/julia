macro timeit(ex,name)
    quote
        t = Inf
        for i=1:5
            t = min(t, @elapsed $ex)
        end
        println(rpad(strcat($name,":"), 20), t)
    end
end

function cat2d_perf(n, iter)
    a = rand(n,n)
    b = rand(n,n)
    for i=1:iter
        c = [a b; b a]
    end
end

function cat2d_perf2(n, iter)
    a = rand(n,n)
    b = rand(n,n)
    for i=1:iter
        c = Array(Float64, 2*n, 2*n)
        c[1:n,1:n] = a
        c[1:n,n+1:end] = b
        c[n+1:end,1:n] = b
        c[n+1:end,n+1:end] = a
        #c = [a b; b a]
    end
end

@timeit cat2d_perf(5, 20000) "small cat"
@timeit cat2d_perf(500, 2) "large cat"

@timeit cat2d_perf2(5, 20000) "small cat 2"
@timeit cat2d_perf2(500, 2) "large cat 2"

function hcat_perf(n, iter)
    a = rand(n,n)
    b = rand(n,n)
    for i=1:iter
        c = [a b b a]
    end
end

function hcat_perf2(n, iter)
    a = rand(n,n)
    b = rand(n,n)
    for i=1:iter
        c = Array(Float64, n, 4*n)
        c[:, 1:n] = a
        c[:, n+1:2*n] = b
        c[:, 2*n+1:3*n] = b
        c[:, 3*n+1:end] = a
    end
end

@timeit hcat_perf(5, 20000) "small hcat"
@timeit hcat_perf(500, 2) "large hcat"

@timeit hcat_perf2(5, 20000) "small hcat 2"
@timeit hcat_perf2(500, 2) "large hcat 2"
