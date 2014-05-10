import Base.Test.@test

parapply = Base.parapply_jl

### test parapply

function my_matmult(A,x,b,i)
  local N = length(x)
  @inbounds begin
    b[i] = 0
    local l=1
    while l<=N
      b[i] += A[l,i]*x[l]
      l += 1
    end
  end
  nothing
end

let N=9000
  dtype = Int128
  x=ones(dtype,N)
  A=ones(dtype,N,N)
  b1 = A'*x
  b2=zeros(dtype,N)
  b3=zeros(dtype,N)

  # warmup
  parapply(my_matmult,(A,x,b2),1,1,1,N)
  
  # run with 1 thread (serial)
  @time parapply(my_matmult,(A,x,b2),1,1,1,N)

  # run with 2 threads (parallel)
  @time parapply(my_matmult,(A,x,b3),2,1,1,N)

  @test b1 == b2
  if b1 != b3
    print(b1-b3)
  end 

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


### just another test

function my_func(x,i) 
  for l=1:length(x) 
    @inbounds x[i]=float(sin(i*l)) 
  end
end

let N=800
  x=zeros(N)
  y=copy(x)

   #warmup
   parapply(my_func,(x,),1,1,1,length(x))
   
   @time parapply(my_func,(x,),1,1,1,length(x))
   @time parapply(my_func,(y,),2,1,1,length(x))

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

function pmedian_filter_core(im::Matrix, out::Matrix, K, y)
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


function pmedian_filter(im::Matrix, filterSize=3; num_threads=2)
  N = size(im)
  out = similar(im)
  K = int(floor(filterSize/2))
  Base.parapply_jl(pmedian_filter_core, (im, out, K), num_threads, 1, 1, N[2])
  out
end


let N = 900
  A=rand(N,N)

  B = median_filter(A) 
  @time B = median_filter(A) 
  @time D = pmedian_filter(A,num_threads=1) 
  @time C = pmedian_filter(A,num_threads=2) 

  @test B == C

end

