using Libdl
lib = dlopen("./test.so")
linsolve = dlsym(lib, :linsolve10!)
N=10
A = rand(N, N); b = rand(N)
x = A\b
ccall(linsolve, Ptr{Float64}, (Ptr{Float64}, Ptr{Float64}),pointer(A),pointer(b))
