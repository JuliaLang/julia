import Base.Test.@test

parapply = Base.parapply

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

