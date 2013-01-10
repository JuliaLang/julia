require("extras/bigint")

module BigFib
export bigfib
const BigInt = Main.BigInt

# Large Fibonacci to exercise BigInt
# from Bill Hart, https://groups.google.com/group/julia-dev/browse_frm/thread/798e2d1322daf633

function mul(a::Vector{BigInt}, b::Vector{BigInt})
   x = a[2]*b[2]
   c = Array(BigInt,3)
   c[1] = a[1]*b[1] + x
   c[2] = a[1]*b[2] + a[2]*b[3]
   c[3] = x + a[3]*b[3]
   return c
end

function bigfib(n)
   if n == 0
      BigInt(1)
   elseif n == 1
      BigInt(1)
   else
      r = [BigInt(1), BigInt(1), BigInt(0)]
      s = [BigInt(1), BigInt(0), BigInt(1)]
      while n != 0
         if (n & 1) == 1
            s = mul(s,r)
         end
         n >>= 1
         if n != 0
            r = mul(r,r)
         end
      end
      s[1]
   end
end

function test()
    @time s = bigfib(100000000)
    print("The result contains $(length(string((s)))) decimal digits \n")
end

end # module
