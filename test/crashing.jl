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
    parapply(my_matmult,(A,x,b3),2,1:N)
    gc()
  end
end

