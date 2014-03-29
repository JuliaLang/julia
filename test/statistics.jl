@test median([1.]) == 1.
@test median([1.,3]) == 2.
@test median([1.,3,2]) == 2.

@test median([1,3,2]) == 2.0
@test median([1,3,2,4]) == 2.5

@test median([0.0,Inf]) == Inf
@test median([0.0,-Inf]) == -Inf
@test median([0.,Inf,-Inf]) == 0.0
@test median([1.,-1.,Inf,-Inf]) == 0.0
@test isnan(median([-Inf,Inf]))

@test_throws median([])
@test_throws median([NaN])
@test_throws median([0.0,NaN])
@test_throws median([NaN,0.0])

@test mean([1,2,3]) == 2.
@test mean([0 1 2; 4 5 6], 1) == [2.  3.  4.]
@test mean([1 2 3; 4 5 6], 1) == [2.5 3.5 4.5]

# test var & std

@test var(1:8) == 6.

@test_approx_eq varm([1,2,3], 2) 1.
@test_approx_eq var([1,2,3]) 1.
@test_approx_eq var([1,2,3]; corrected=false) 2.0/3
@test_approx_eq var([1,2,3]; mean=0) 7.
@test_approx_eq var([1,2,3]; mean=0, corrected=false) 14.0/3

@test_approx_eq var([1 2 3 4 5; 6 7 8 9 10], 2) [2.5 2.5]'
@test_approx_eq var([1 2 3 4 5; 6 7 8 9 10], 2; corrected=false) [2.0 2.0]'

@test_approx_eq stdm([1,2,3], 2) 1.
@test_approx_eq std([1,2,3]) 1.
@test_approx_eq std([1,2,3]; corrected=false) sqrt(2.0/3)
@test_approx_eq std([1,2,3]; mean=0) sqrt(7.0)
@test_approx_eq std([1,2,3]; mean=0, corrected=false) sqrt(14.0/3)

@test_approx_eq std([1 2 3 4 5; 6 7 8 9 10], 2) sqrt([2.5 2.5]')
@test_approx_eq std([1 2 3 4 5; 6 7 8 9 10], 2; corrected=false) sqrt([2.0 2.0]')

A = Complex128[exp(i*im) for i in 1:10^4]
@test_approx_eq varm(A,0.) sum(map(abs2,A))/(length(A)-1)
@test_approx_eq varm(A,mean(A)) var(A)

# test covariance

function safe_cov(x, y, zm::Bool, cr::Bool)
    n = length(x)
    if !zm
        x = x .- mean(x)
        y = y .- mean(y)
    end
    dot(vec(x), vec(y)) / (n - int(cr))
end

X = [1. 2. 3. 4. 5.; 5. 4. 6. 2. 1.]'
Y = [6. 1. 5. 3. 2.; 2. 7. 8. 4. 3.]'

for vd in [1, 2], zm in [true, false], cr in [true, false] 
    # println("vd = $vd: zm = $zm, cr = $cr")
    if vd == 1
        k = size(X, 2)
        Cxx = zeros(k, k)
        Cxy = zeros(k, k)
        for i = 1:k, j = 1:k
            Cxx[i,j] = safe_cov(X[:,i], X[:,j], zm, cr)
            Cxy[i,j] = safe_cov(X[:,i], Y[:,j], zm, cr)
        end
        x1 = vec(X[:,1])
        y1 = vec(Y[:,1])
    else
        k = size(X, 1)
        Cxx = zeros(k, k)
        Cxy = zeros(k, k)
        for i = 1:k, j = 1:k
            Cxx[i,j] = safe_cov(X[i,:], X[j,:], zm, cr)
            Cxy[i,j] = safe_cov(X[i,:], Y[j,:], zm, cr)
        end
        x1 = vec(X[1,:])
        y1 = vec(Y[1,:])
    end

    c = zm ? cov(x1; mean=0, corrected=cr) :
             cov(x1; corrected=cr)
    @test isa(c, Float64)
    @test_approx_eq c Cxx[1,1]

    C = zm ? cov(X; vardim=vd, mean=0, corrected=cr) :
             cov(X; vardim=vd, corrected=cr)
    @test size(C) == (k, k)
    @test_approx_eq C Cxx

    c = zm ? cov(x1, y1; mean=0, corrected=cr) :
             cov(x1, y1; corrected=cr)
    @test isa(c, Float64)
    @test_approx_eq c Cxy[1,1]

    C = zm ? cov(x1, Y; vardim=vd, mean=0, corrected=cr) :
             cov(x1, Y; vardim=vd, corrected=cr)
    @test size(C) == (1, k)
    @test_approx_eq C Cxy[1,:]

    C = zm ? cov(X, y1; vardim=vd, mean=0, corrected=cr) :
             cov(X, y1; vardim=vd, corrected=cr) 
    @test size(C) == (k, 1)
    @test_approx_eq C Cxy[:,1]

    C = zm ? cov(X, Y; vardim=vd, mean=0, corrected=cr) :
             cov(X, Y; vardim=vd, corrected=cr)
    @test size(C) == (k, k)
    @test_approx_eq C Cxy
end

# test correlation

function safe_cor(x, y, zm::Bool)
    if !zm 
        x = x .- mean(x)
        y = y .- mean(y)
    end
    x = vec(x)
    y = vec(y)
    dot(x, y) / (sqrt(dot(x, x)) * sqrt(dot(y, y)))
end

for vd in [1, 2], zm in [true, false]
    # println("vd = $vd: zm = $zm")
    if vd == 1
        k = size(X, 2)
        Cxx = zeros(k, k)
        Cxy = zeros(k, k)
        for i = 1:k, j = 1:k
            Cxx[i,j] = safe_cor(X[:,i], X[:,j], zm)
            Cxy[i,j] = safe_cor(X[:,i], Y[:,j], zm)
        end
        x1 = vec(X[:,1])
        y1 = vec(Y[:,1])
    else
        k = size(X, 1)
        Cxx = zeros(k, k)
        Cxy = zeros(k, k)
        for i = 1:k, j = 1:k
            Cxx[i,j] = safe_cor(X[i,:], X[j,:], zm)
            Cxy[i,j] = safe_cor(X[i,:], Y[j,:], zm)
        end
        x1 = vec(X[1,:])
        y1 = vec(Y[1,:])
    end

    c = zm ? cor(x1; mean=0) : cor(x1)
    @test isa(c, Float64)
    @test_approx_eq c Cxx[1,1]

    C = zm ? cor(X; vardim=vd, mean=0) : cor(X; vardim=vd)
    @test size(C) == (k, k)
    @test_approx_eq C Cxx

    c = zm ? cor(x1, y1; mean=0) : cor(x1, y1)
    @test isa(c, Float64)
    @test_approx_eq c Cxy[1,1]

    C = zm ? cor(x1, Y; vardim=vd, mean=0) : cor(x1, Y; vardim=vd)
    @test size(C) == (1, k)
    @test_approx_eq C Cxy[1,:]

    C = zm ? cor(X, y1; vardim=vd, mean=0) : cor(X, y1; vardim=vd)
    @test size(C) == (k, 1)
    @test_approx_eq C Cxy[:,1]

    C = zm ? cor(X, Y; vardim=vd, mean=0) : cor(X, Y; vardim=vd)
    @test size(C) == (k, k)
    @test_approx_eq C Cxy
end



# test hist

@test sum(hist([1,2,3])[2]) == 3
@test hist([])[2] == []
@test hist([1])[2] == [1]
@test hist([1,2,3],[0,2,4]) == ([0,2,4],[2,1])
@test hist([1,2,3],0:2:4) == (0:2:4,[2,1])
@test all(hist([1:100]/100,0.0:0.01:1.0)[2] .==1)
@test hist([1,1,1,1,1])[2][1] == 5
@test sum(hist2d(rand(100, 2))[3]) == 100

@test midpoints(1.0:1.0:10.0) == 1.5:1.0:9.5
@test midpoints(1:10) == 1.5:9.5
@test midpoints(Float64[1.0:1.0:10.0]) == Float64[1.5:1.0:9.5]

@test quantile([1,2,3,4],0.5) == 2.5
@test quantile([1., 3],[.25,.5,.75])[2] == median([1., 3])
@test quantile([0.:100.],[.1,.2,.3,.4,.5,.6,.7,.8,.9])[1] == 10.0


