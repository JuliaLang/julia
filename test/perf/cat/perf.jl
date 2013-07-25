include("../perfutil.jl")

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
        c = Array(Float64,2n,2n)
        c[1:n,1:n] = a
        c[1:n,n+1:end] = b
        c[n+1:end,1:n] = b
        c[n+1:end,n+1:end] = a
    end
end

@timeit cat2d_perf(5,20000) "small_hvcat" "Small horizontal/vertical matrix concatenaion"
@timeit cat2d_perf2(5,20000) "small_hvcat_setind" "Small horizontal/vertical matrix concatenation using setindex"

@timeit cat2d_perf(500,2) "large_hvcat" "Large horizontal/vertical matrix concatenation"
@timeit cat2d_perf2(500,2) "large_hvcat_setind" "Large horizontal/vertical matrix concatenation using setindex"

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
        c = Array(Float64, n, 4n)
        c[:, 1:n] = a
        c[:, n+1:2n] = b
        c[:, 2n+1:3n] = b
        c[:, 3n+1:end] = a
    end
end

@timeit hcat_perf(5,20000) "small_hcat" "Small horizontal matrix concatenation"
@timeit hcat_perf2(5,20000) "small_hcat_setind" "Small horizontal matrix concatenation using setindex"

@timeit hcat_perf(500,2) "large_hcat" "Large horizontal matrix concatenation"
@timeit hcat_perf2(500,2) "large_hcat_setind" "Large horizontal matrix concatenation using setindex"

function vcat_perf(n, iter)
    a = rand(n,n)
    b = rand(n,n)
    for i = 1:iter
        c = [a, b, b, a]
    end
end

function vcat_perf2(n, iter)
    a = rand(n,n)
    b = rand(n,n)
    for i=1:iter
        c = Array(Float64, 4n, n)
        c[1:n, :] = a
        c[n+1:2n, :] = b
        c[2n+1:3n, :] = b
        c[3n+1:4n, :] = a
    end
end

@timeit vcat_perf(5,20000) "small_vcat" "Small vertical matrix concatenaion"
@timeit vcat_perf2(5,20000) "small_vcat_setind" "Small vertical matrix concatenation using setindex"

@timeit vcat_perf(500,2) "large_vcat" "Large vertical matrix concatenaion"
@timeit vcat_perf2(500,2) "large_vcat_setind" "Large vertical matrix concatenation using setindex"

function catnd_perf(n, iter)
    a = rand(1,n,n,1)
    b = rand(1,n,n)
    for i = 1:iter
        c = cat(3, a, b, b, a)
    end
end

function catnd_perf2(n, iter)
    a = rand(1,n,n,1)
    b = rand(1,n,n)
    for i = 1:iter
        c = Array(Float64, 1, n, 4n, 1)
        c[1,:,1:n,1] = a
        c[1,:,n+1:2n,1] = b
        c[1,:,2n+1:3n,1] = b
        c[1,:,3n+1:4n,1] = a
    end
end

@timeit catnd_perf(5,20000) "small_catnd" "Small N-dimensional matrix concatenation"
@timeit catnd_perf2(5,20000) "small_catnd_setind" "Small N-dimensional matrix concatenation using setindex"

@timeit catnd_perf(500,2) "large_catnd" "Large N-dimensional matrix concatenation"
@timeit catnd_perf2(500,2) "large_catnd_setind" "Large N-dimensional matrix concatenation using setindex"
