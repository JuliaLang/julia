import Base.Test.@test

parapply = Base.parapply_jl

### test parapply

function my_matmult(A,x,b,i)
  local N = length(x)
  @inbounds begin
    b[i] = sum( A[:,i].*x )
  end
  nothing
end

let N=2000
  dtype = Int128
  x=ones(dtype,N)
  A=ones(dtype,N,N)
  b3=zeros(dtype,N)

  for i=1:100
    println(i)
    parapply(my_matmult,(A,x,b3),2,1,1,N)
    gc()
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

