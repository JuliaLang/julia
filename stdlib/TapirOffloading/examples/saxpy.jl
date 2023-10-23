import Tapir, TapirOffloading, CUDA

function saxpy(Z, X, Y, a)
    # TODO: Move to TapirOffloading?
    CUDA.CUDAKernels.__pin!(Z)
    CUDA.CUDAKernels.__pin!(X)
    CUDA.CUDAKernels.__pin!(Y)

    Tapir.foreach(eachindex(Z, Y, X)) do I
        @inbounds Z[I] = a*X[I] + Y[I]
    end
    CUDA.synchronize()
    Z
end
using InteractiveUtils

# buf = CUDA.Mem.alloc(CUDA.Mem.UnifiedBuffer, 1024*sizeof(Float64))
# arr = Base.unsafe_wrap(Array{Float64}, reinterpret(Ptr{Cvoid}, pointer(buf)), 1024)

# @code_llvm raw=true dump_module=true optimize=false saxpy(zeros(3), ones(3), ones(3), 1.0)
# @code_llvm raw=true dump_module=true saxpy(zeros(4), ones(3), ones(3), 1.0)
N = 1024^2

@assert all(z->z==2.0, saxpy(zeros(N), ones(N), ones(N), 1.0))

