using Base.Random: Close1Open2, fill_array!



function randn0!(rng::MersenneTwister, A::Array{Float64})
    n = length(A)
    rand!(rng, A, n, Close1Open2)
    for i = 1:n
        @inbounds A[i] = randn(rng, reinterpret(UInt64, A[i]) & 0x000fffffffffffff)
    end
    A
end


function arrN(n, r=1)
    a = Array(Float64, n)
    m = MersenneTwister()
    tic()
    for i in 1:r
        randn!(m, a)
    end
    toq()
end

 function arrN0(n, r=1)
    a = Array(Float64, n)
    m = MersenneTwister()
    tic()
    for i in 1:r
        randn0!(m, a)
    end
    toq()
end


function scalN(n)
    m = MersenneTwister()
    tic()
    for i in 1:n
        randn(m)
    end
    toq()
end

warmup0() = (arrN(10); scalN(10))
warmup1() = (arrN(10); arrN0(10); scalN(10))
warmup2() = (arrN0(10); arrN(10); scalN(10))


# [arrN(10^i, 10^(8-i)) for i in 1:8]
# [arrN0(10^i, 10^(8-i)) for i in 1:8]
# scalN(10^8)
