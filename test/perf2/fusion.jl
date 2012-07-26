function arith_vectorized(b,c,d)
    a = b.*c + d + 1.0
end

function arith_loop(b,c,d)
    a = similar(b)
    for i = 1:length(b)
        a[i] = b[i]*c[i] + d[i] + 1.0
    end
end

function compare_arith(len::Int, niter::Int)
    b = randn(len)
    c = randn(len)
    d = randn(len)
    print("With vectorized: ")
    @time begin
        for n in 1:niter
            a = arith_vectorized(b,c,d)
        end
    end
    print("With loop: ")
    @time begin
        for n in 1:niter
            a = arith_loop(b,c,d)
        end
    end
end
compare_arith(1_000_000, 10)
