# This file is a part of Julia. License is MIT: http://julialang.org/license

#
#  Test the pseudo-inverse
#

debug = false

using Base.Test

function hilb(T::Type, n::Integer)
    a=Array{T}(n,n)
    for i=1:n
        for j=1:n
            a[j,i]=one(T)/(i+j-one(T))
        end
    end
    return a
end
hilb(n::Integer) = hilb(Float64,n)

function hilb(T::Type, m::Integer, n::Integer)
    a=Array{T}(m,n)
    for i=1:n
        for j=1:m
            a[j,i]=one(T)/(i+j-one(T))
        end
    end
    return a
end
hilb(m::Integer, n::Integer) = hilb(Float64,m,n)

function onediag(T::Type, m::Integer, n::Integer)
    a=zeros(T,m,n)
    for i=1:min(n,m)
        a[i,i]=one(T)/(float(i)^5)
    end
    a[1,1] = 0
    a[min(m,n),min(m,n)] = 0
    return a
end
onediag(m::Integer, n::Integer) = onediag(Float64, m::Integer, n::Integer)

function onediag_sparse(T::Type, n::Integer)
    a=zeros(T,n)
    for i=1:n
        a[i]=one(T)/(float(i)^5)
    end
    a[1] = 0
    a[n] = 0
    return Diagonal(a)
end
onediag_sparse(n::Integer) = onediag_sparse(Float64, n::Integer)

function tridiag(T::Type, m::Integer, n::Integer)
    a=zeros(T,m,n)
    for i=1:min(n,m)
        a[i,i]=one(T)/(float(i)^5)
    end
    for i=1:min(n,m)-1
        a[i+1,i]=2*one(T)/(float(i)^5)
        a[1,i+1]=2*one(T)/(float(i)^5)
    end
    return a
end
tridiag(m::Integer, n::Integer) = tridiag(Float64, m::Integer, n::Integer)

function randn_float64(m::Integer, n::Integer)
    a=randn(m,n)
    b=Array{Float64}(m,n)
    for i=1:n
        for j=1:m
            b[j,i]=convert(Float64,a[j,i])
        end
    end
    return b
end

function randn_float32(m::Integer, n::Integer)
    a=randn(m,n)
    b=Array{Float32}(m,n)
    for i=1:n
        for j=1:m
            b[j,i]=convert(Float32,a[j,i])
        end
    end
    return b
end


function test_pinv(a,m,n,tol1,tol2,tol3)
    debug && println("=== julia/matlab pinv, default tol=eps(1.0)*max(size(a)) ===")
    apinv = @inferred pinv(a)

    @test vecnorm(a*apinv*a-a)/vecnorm(a) ≈ 0 atol=tol1
    x0 = randn(n); b = a*x0; x = apinv*b
    @test vecnorm(a*x-b)/vecnorm(b) ≈ 0 atol=tol1
    debug && println(vecnorm(a*apinv*a - a)/vecnorm(a))
    debug && println(vecnorm(a*x-b)/vecnorm(b))


    debug && println("=== julia pinv, tol=sqrt(eps(1.0)) ===")
    apinv = pinv(a,sqrt(eps(real(one(eltype(a))))))

    @test vecnorm(a*apinv*a-a)/vecnorm(a) ≈ 0 atol=tol2
    x0 = randn(n); b = a*x0; x = apinv*b
    @test vecnorm(a*x-b)/vecnorm(b) ≈ 0 atol=tol2
    debug && println(vecnorm(a*apinv*a - a)/vecnorm(a))
    debug && println(vecnorm(a*x-b)/vecnorm(b))
end


srand(12345)

let
    for eltya in (Float64, Complex128)

        debug && println("\n\n<<<<<", eltya, ">>>>>")

        m = 1000
        n = 100
        debug && println("\n\n n = ", n, ", m = ",m)

        default_tol = (real(one(eltya))) * max(m,n) * 10

        debug && println("\n--- dense/ill-conditioned matrix ---\n")
        ###    a = randn_float64(m,n) * hilb(eltya,n)
        a = hilb(eltya,m,n)
        test_pinv(a,m,n,1e-2,1e-5,1e-5)

        debug && println("\n--- dense/diagonal matrix ---\n")
        a = onediag(eltya,m,n)
        test_pinv(a,m,n,default_tol,default_tol,default_tol)

        debug && println("\n--- dense/tri-diagonal matrix ---\n")
        a = tridiag(eltya,m,n)
        test_pinv(a,m,n,default_tol,1e-5,default_tol)

        debug && println("\n--- Diagonal matrix ---\n")
        a = onediag_sparse(eltya,m)
        test_pinv(a,m,m,default_tol,default_tol,default_tol)

        m = 100
        n = 100
        debug && println("\n\n n = ", n, ", m = ",m)

        default_tol = (real(one(eltya))) * max(m,n) * 10

        debug && println("\n--- dense/ill-conditioned matrix ---\n")
        ###    a = randn_float64(m,n) * hilb(eltya,n)
        a = hilb(eltya,m,n)
        test_pinv(a,m,n,1e-2,1e-5,1e-5)

        debug && println("\n--- dense/diagonal matrix ---\n")
        a = onediag(eltya,m,n)
        test_pinv(a,m,n,default_tol,default_tol,default_tol)

        debug && println("\n--- dense/tri-diagonal matrix ---\n")
        a = tridiag(eltya,m,n)
        test_pinv(a,m,n,default_tol,1e-5,default_tol)

        debug && println("\n--- Diagonal matrix ---\n")
        a = onediag_sparse(eltya,m)
        test_pinv(a,m,m,default_tol,default_tol,default_tol)

        m = 100
        n = 1000
        debug && println("\n\n n = ", n, ", m = ",m)

        default_tol = (real(one(eltya))) * max(m,n) * 10

        debug && println("\n--- dense/ill-conditioned matrix ---\n")
    ###    a = randn_float64(m,n) * hilb(eltya,n)
        a = hilb(eltya,m,n)
        test_pinv(a,m,n,1e-2,1e-5,1e-5)

        debug && println("\n--- dense/diagonal matrix ---\n")
        a = onediag(eltya,m,n)
        test_pinv(a,m,n,default_tol,default_tol,default_tol)

        debug && println("\n--- dense/tri-diagonal matrix ---\n")
        a = tridiag(eltya,m,n)
        test_pinv(a,m,n,default_tol,1e-5,default_tol)

        debug && println("\n--- Diagonal matrix ---\n")
        a = onediag_sparse(eltya,m)
        test_pinv(a,m,m,default_tol,default_tol,default_tol)
    end
end


for eltya in (Float32, Complex64)

    debug && println("\n\n<<<<<", eltya, ">>>>>")

    m = 1000
    n = 100
    debug && println("\n\n n = ", n, ", m = ",m)

    default_tol = (real(one(eltya))) * max(m,n) * 10

    debug && println("\n--- dense/ill-conditioned matrix ---\n")
###    a = randn_float32(m,n) * hilb(eltya,n)
    a = hilb(eltya,m,n)
    test_pinv(a,m,n,1e0,1e-2,1e-2)

    debug && println("\n--- dense/diagonal matrix ---\n")
    a = onediag(eltya,m,n)
    test_pinv(a,m,n,default_tol,default_tol,default_tol)

    debug && println("\n--- dense/tri-diagonal matrix ---\n")
    a = tridiag(eltya,m,n)
    test_pinv(a,m,n,default_tol,1e-2,default_tol)

    debug && println("\n--- Diagonal matrix ---\n")
    a = onediag_sparse(eltya,m)
    test_pinv(a,m,m,default_tol,default_tol,default_tol)

    m = 100
    n = 100
    debug && println("\n\n n = ", n, ", m = ",m)

    default_tol = (real(one(eltya))) * max(m,n) * 10

    debug && println("\n--- dense/ill-conditioned matrix ---\n")
###    a = randn_float32(m,n) * hilb(eltya,n)
    a = hilb(eltya,m,n)
    test_pinv(a,m,n,1e0,1e-2,1e-2)

    debug && println("\n--- dense/diagonal matrix ---\n")
    a = onediag(eltya,m,n)
    test_pinv(a,m,n,default_tol,default_tol,default_tol)

    debug && println("\n--- dense/tri-diagonal matrix ---\n")
    a = tridiag(eltya,m,n)
    test_pinv(a,m,n,default_tol,1e-2,default_tol)

    debug && println("\n--- Diagonal matrix ---\n")
    a = onediag_sparse(eltya,m)
    test_pinv(a,m,m,default_tol,default_tol,default_tol)

    m = 100
    n = 1000
    debug && println("\n\n n = ", n, ", m = ",m)

    default_tol = (real(one(eltya))) * max(m,n) * 10

    debug && println("\n--- dense/ill-conditioned matrix ---\n")
###    a = randn_float32(m,n) * hilb(eltya,n)
    a = hilb(eltya,m,n)
    test_pinv(a,m,n,1e0,1e-2,1e-2)

    debug && println("\n--- dense/diagonal matrix ---\n")
    a = onediag(eltya,m,n)
    test_pinv(a,m,n,default_tol,default_tol,default_tol)

    debug && println("\n--- dense/tri-diagonal matrix ---\n")
    a = tridiag(eltya,m,n)
    test_pinv(a,m,n,default_tol,1e-2,default_tol)

    debug && println("\n--- Diagonal matrix ---\n")
    a = onediag_sparse(eltya,m)
    test_pinv(a,m,m,default_tol,default_tol,default_tol)

end

# test zero matrices
for eltya in (Float32, Float64, Complex64, Complex128)
    debug && println("\n\n<<<<<", eltya, ">>>>>")
    debug && println("\n--- zero constant ---")
    a = pinv(zero(eltya))
    @test a ≈ 0.0
end

for eltya in (Float32, Float64, Complex64, Complex128)
    debug && println("\n\n<<<<<", eltya, ">>>>>")
    debug && println("\n--- zero vector ---")
    a = pinv([zero(eltya); zero(eltya)])
    @test a[1] ≈ 0.0
    @test a[2] ≈ 0.0
end

for eltya in (Float32, Float64, Complex64, Complex128)
    debug && println("\n\n<<<<<", eltya, ">>>>>")
    debug && println("\n--- zero Diagonal matrix ---")
    a = pinv(Diagonal([zero(eltya); zero(eltya)]))
    @test a.diag[1] ≈ 0.0
    @test a.diag[2] ≈ 0.0
end

# test sub-normal matrices
for eltya in (Float32, Float64)
    debug && println("\n\n<<<<<", eltya, ">>>>>")
    debug && println("\n--- sub-normal constant ---")
    a = pinv(realmin(eltya)/100)
    @test a ≈ 0.0
    debug && println("\n\n<<<<<Complex{", eltya, "}>>>>>")
    debug && println("\n--- sub-normal constant ---")
    a = pinv(realmin(eltya)/100*(1+1im))
    @test a ≈ 0.0
end

for eltya in (Float32, Float64)
    debug && println("\n\n<<<<<", eltya, ">>>>>")
    debug && println("\n--- sub-normal vector ---")
    a = pinv([realmin(eltya); realmin(eltya)]/100)
    @test a[1] ≈ 0.0
    @test a[2] ≈ 0.0
    debug && println("\n\n<<<<<Complex{", eltya, "}>>>>>")
    debug && println("\n--- sub-normal vector ---")
    a = pinv([realmin(eltya); realmin(eltya)]/100*(1+1im))
    @test a[1] ≈ 0.0
    @test a[2] ≈ 0.0
end

for eltya in (Float32, Float64)
    debug && println("\n\n<<<<<", eltya, ">>>>>")
    debug && println("\n--- sub-normal Diagonal matrix ---")
    a = pinv(Diagonal([realmin(eltya); realmin(eltya)]/100))
    @test a.diag[1] ≈ 0.0
    @test a.diag[2] ≈ 0.0
    debug && println("\n\n<<<<<Complex{", eltya, "}>>>>>")
    debug && println("\n--- sub-normal Diagonal matrix ---")
    a = pinv(Diagonal([realmin(eltya); realmin(eltya)]/100*(1+1im)))
    @test a.diag[1] ≈ 0.0
    @test a.diag[2] ≈ 0.0
end
