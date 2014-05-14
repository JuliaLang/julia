import Base.Test.@test

parapply = Base.parapply_jl

### perfect threading

function my_kernel(i)
  z = 0.0
  for l=1:40000
    z += sin(2*pi*l/40000)
  end
end

let N=1000
  # warmup
  parapply(my_kernel,(),1,1:N)

  # run with 1 thread (serial)
  println("my_kernel - 1 thread")
  @time parapply(my_kernel,(),1,1:N)

  # run with 2 threads (parallel)
  println("my_kernel - 2 threads")
  @time parapply(my_kernel,(),2,1:N)
end

### bad threading

type MyType
  i::Int
end

function my_bad_kernel(i)
  z = 0.0
  for l=1:4000
    tmp = MyType(i)
    for k=1:10
      z += sin(2*pi*l/40000)
    end
  end
end

let N=1000
  # warmup
  parapply(my_kernel,(),1,1:N)

  # run with 1 thread (serial)
  println("\nmy_bad_kernel - 1 thread")
  @time parapply(my_bad_kernel,(),1,1:N)

  # run with 2 threads (parallel)
  println("my_bad_kernel - 2 threads")
  @time parapply(my_bad_kernel,(),2,1:N)
end

### test parapply

function my_matmult(A,x,b,i)
  N = length(x)
  @inbounds begin
    b[i] = 0
    for l=1:N
      b[i] += A[l,i]*x[l]
    end
  end
end

let N=9000
  dtype = Int128
  x=ones(dtype,N)
  A=ones(dtype,N,N)
  b1 = A'*x
  b2=zeros(dtype,N)
  b3=zeros(dtype,N)

  # warmup
  parapply(my_matmult,(A,x,b2),1,1:N)
  
  # run with 1 thread (serial)
  println("\nmy_matmult - 1 thread")
  @time parapply(my_matmult,(A,x,b2),1,1:N)

  # run with 2 threads (parallel)
  println("my_matmult - 2 threads")
  @time parapply(my_matmult,(A,x,b3),2,1:N)

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
  x=rand(N)
  y=copy(x)

  t=Base.Thread(sqrt!,x)
  Base.run(t)
  sqrt!(y)
  Base.join(t)

  @test x == y
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

function pmedian_filter_core(im, out, K, y)
  @inbounds begin
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


function pmedian_filter(im::Matrix, filterSize=3; num_threads=2)
  N = size(im)
  out = similar(im)
  K = int(floor(filterSize/2))

  parapply(pmedian_filter_core, (im, out, K), num_threads, 1:N[2], preapply=false)
  out
end


let N = 512
  A=rand(N,N)
  filterSize = 3

  println("\nmedian_filter - serial")
  B = median_filter(A,filterSize) 
  @time B = median_filter(A,filterSize) 
  println("median_filter - 1 thread")
  @time D = pmedian_filter(A,filterSize,num_threads=1) 
  println("median_filter - 2 threads")
  @time C = pmedian_filter(A,filterSize,num_threads=2) 

  @test B == C

end


