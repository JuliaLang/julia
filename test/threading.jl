import Base.Test.@test, Base.parapply

### perfect threading

function my_kernel(r)
    z = 0.0
    for l=1:40000*length(r)
        z += sin(2*pi*l/40000)
    end
end

let N=1000
    # warmup
    parapply(my_kernel, 1:N, numthreads=1)

    # run with 1 thread (serial)
    println("my_kernel - 1 thread")
    @time parapply(my_kernel, 1:N, numthreads=1)

    # run with 2 threads (parallel)
    println("my_kernel - 2 threads")
    @time parapply(my_kernel, 1:N, numthreads=2)
end

### bad threading

type MyType
    i::Int
end

function my_bad_kernel(r)
    z = 0.0
    for l=1:4000*length(r)
        tmp = MyType(l)
        for k=1:10
            z += sin(2*pi*l/40000)
        end
    end
end

let N=1000
    # warmup
    parapply(my_kernel, 1:N, numthreads=1)

    # run with 1 thread (serial)
    println("\nmy_bad_kernel - 1 thread")
    @time parapply(my_bad_kernel, 1:N, numthreads=1)

    # run with 2 threads (parallel)
    println("my_bad_kernel - 2 threads")
    @time parapply(my_bad_kernel, 1:N, numthreads=2)
end

### tanh

function tanh_core(r,x,y)
    for i in r
        @inbounds y[i] = tanh(x[i])
    end
end

function ptanh(x; numthreads=CPU_CORES)
    y = similar(x)
    N = length(x)
    parapply(tanh_core, 1:N, x, y, preapply=true, numthreads=numthreads)
    y
end

let
   x = rand(10000,200)
   
    # warmup
    tanh(x)
    println("\ntanh - serial")
    @time tanh(x)


    # run with 1 thread (serial)
    println("tanh - 1 thread")
    ptanh(x, numthreads=1)
    @time ptanh(x, numthreads=1)

    # run with 2 threads (parallel)
    println("tanh - 2 threads")
    @time ptanh(x, numthreads=2)

end

### matrix vector multiplication

function my_matmult(r,A,x,b)
    N = length(x)
    @inbounds begin
      for i in r
        b[i] = 0
        for l=1:N
            b[i] += A[l,i]*x[l]
        end
      end
    end
end

let N=9000
    dtype = Int128
    x = ones(dtype,N)
    A = ones(dtype,N,N)
    b1 = A'*x
    b2 = zeros(dtype,N)
    b3 = zeros(dtype,N)

    # warmup
    parapply(my_matmult,1:N, A,x,b2, numthreads=1)
  
    # run with 1 thread (serial)
    println("\nmy_matmult - 1 thread")
    @time parapply(my_matmult,1:N, A,x,b2, numthreads=1)

    # run with 2 threads (parallel)
    println("my_matmult - 2 threads")
    @time parapply(my_matmult,1:N, A,x,b3, numthreads=2)

    @test b1 == b2
    @test b1 == b3
end

### Simple thread test

function sqrt!(x) 
    for n=1:length(x)
        x[n] = sqrt(x[n])
    end
end

let N=1000
    x = rand(N)
    y = copy(x)

    t = Base.Thread(sqrt!,x)
    Base.run(t)
    sqrt!(y)
    Base.join(t)

    @test x == y
end

### Mutex test

function plus_one!(x,m) 
    for n=1:10000
        Base.lock(m)
        x[1] += 1
        Base.unlock(m)
    end
end

let
    x = zeros(1)
    m = Base.Mutex()

    t1 = Base.Thread(plus_one!,x,m)
    t2 = Base.Thread(plus_one!,x,m)
    Base.run(t1)
    Base.run(t2)
    Base.join(t1)
    Base.join(t2)

    @test x[1] == 20000
end

### Exception test

function i_will_throw() 
    error("An error in thread!")
end

let N=100

    nthrows = 0
    for l=1:N
        try
            t1 = Base.Thread(i_will_throw)
            t2 = Base.Thread(i_will_throw)
            Base.run(t1)
            Base.run(t2)
            Base.join(t1)
            Base.join(t2)
        catch
            nthrows += 1
        end
    end

    @test nthrows == N
end


### Median filter

function median_filter(im::Matrix, filterSize=3)
    N = size(im)
    out = similar(im)
    K = int(floor(filterSize/2))
    for x=1:N[1], y=1:N[2]
        x_min = max(1, x-K)
        x_max = min(N[1], x+K)
        y_min = max(1, y-K)
        y_max = min(N[2], y+K)

        s = im[x_min:x_max, y_min:y_max]
        out[x,y] = median(s[:])
    end
    out
end

function pmedian_filter_core(r, im, out, K)
    @inbounds begin
    for y in r
        N = size(im)
        y_min = max(1, y-K)
        y_max = min(N[2], y+K)
  
        for x=1:N[1]
            x_min = max(1, x-K)
            x_max = min(N[1], x+K)
  
            s = im[x_min:x_max, y_min:y_max]
            out[x,y] = median(s[:])
        end
     end
   end
end


function pmedian_filter(im::Matrix, filterSize=3; numthreads=2)
    N = size(im)
    out = similar(im)
    K = int(floor(filterSize/2))

    parapply(pmedian_filter_core,1:N[2], im,out,K, numthreads = numthreads)
    out
end


let N = 512
    A = rand(N,N)
    filterSize = 3

    println("\nmedian_filter - serial")
    B = median_filter(A,filterSize) 
    @time B = median_filter(A,filterSize) 
    println("median_filter - 1 thread")
    @time D = pmedian_filter(A, filterSize, numthreads=1) 
    println("median_filter - 2 threads")
    @time C = pmedian_filter(A, filterSize, numthreads=2) 

    @test B == C
end

