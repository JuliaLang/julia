load ("extras/bigint.jl") #Assume running from julia base dir

#Large Fibonacci to exercise BigInt 
#from Bill Hart, https://groups.google.com/group/julia-dev/browse_frm/thread/798e2d1322daf633?hl=en
type Fibdata 
   data::Array{BigInt,1} 
end 
function *(a::Fibdata, b::Fibdata) 
   r = Fibdata(Array(BigInt,3)) 
   s = a.data[2]*b.data[2] 
   r.data[1] = a.data[1]*b.data[1] + s 
   r.data[2] = a.data[1]*b.data[2] + a.data[2]*b.data[3] 
   r.data[3] = s + a.data[3]*b.data[3] 
   r 
end 

function fib(n) 
   if n == 0 
      BiglntI(1) 
   elseif n == 1 
      BigInt(1) 
   else 
      r = Fibdata([BigInt(1), BigInt(1), BigInt(0)]) 
      s = Fibdata([BigInt(1), BigInt(0), BigInt(1)]) 
      while n != 0 
         if (n & 1) == 1 
            s = s*r 
         end 
         n >>= 1 
         if n != 0 
            r = r*r 
         end 
      end 
      s.data[1] 
   end 
end 

@time s = fib(100000000); 
print("The result contains $(length(string((s)))) decimal digits \n")
