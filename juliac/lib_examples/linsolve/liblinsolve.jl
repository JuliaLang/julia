module Lib

using LinearAlgebra

Base.@ccallable function linsolve10(A_ptr::Ptr{Float64}, b_ptr::Ptr{Float64}, N::Csize_t)::Ptr{Float64}
    A = unsafe_wrap(Array, A_ptr, (N,N)) 
    b = unsafe_wrap(Array, b_ptr, N)    
    F = lu!(A)
    ldiv!(F, b)
    return b_ptr
end

end
