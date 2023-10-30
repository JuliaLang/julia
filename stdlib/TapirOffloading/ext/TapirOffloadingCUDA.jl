module TapirOffloadingCUDA
    using TapirOffloading
    using CUDA

    struct CUDABackend <: TapirOffloading.Backend end

    function __init__()
        TapirOffloading.register(:cuda, CUDABackend())
    end

    # Runtime
    module OffloadingRuntime
        import CUDA: blockIdx, blockDim, threadIdx, i32

        function __rts_get_iteration_64(base::Int64, grainsize::Int64)::Int64
            idx = (blockIdx().x-1i32) * blockDim().x + threadIdx().x
            base + (idx-1i32) * grainsize
        end
    end

    TapirOffloading.runtime(::CUDABackend) = OffloadingRuntime

    function link_kernel(image, name)
        cu_mod = CuModule(image)
        return CuFunction(cu_mod, name)
    end

    const _compiler_caches = Dict{CuContext, Dict{Any, CuFunction}}()
    function TapirOffloading.lookup_or_compile(backend::CUDABackend, mod, nbytes, name)
        cuda = CUDA.active_state()
        cfg = CUDA.compiler_config(cuda.device)

        key = (mod, name, cfg)
        cache = CUDA.compiler_cache(cuda.context)
        cuF = get!(cache, key) do
            image, name = TapirOffloading.codegen(backend, mod, nbytes, name, cfg)
            return link_kernel(image, name)
        end::CuFunction
        Base.unsafe_convert(Ptr{Cvoid}, cuF.handle)
    end

    function TapirOffloading.launch(::CUDABackend, func, args, args_sz, N)
        func = Base.unsafe_convert(CUDA.CUfunction, func)

        # config = launch_configuration(kernel.fun)
        # threads = min(N, config.threads)
        threads = min(N, 256)
        blocks = cld(N, threads)

        argBufferSize = Ref{Csize_t}(args_sz)
        GC.@preserve argBufferSize begin
            config = Vector{Ptr{Cvoid}}(undef, 5)
            config[1] = reinterpret(Ptr{Cvoid}, UInt(CUDA.CU_LAUNCH_PARAM_BUFFER_POINTER_AS_INT))
            config[2] = Base.unsafe_convert(Ptr{Cvoid}, args)
            config[3] = reinterpret(Ptr{Cvoid}, UInt(CUDA.CU_LAUNCH_PARAM_BUFFER_SIZE_AS_INT))
            config[4] = Base.unsafe_convert(Ptr{Cvoid}, argBufferSize)
            config[5] = reinterpret(Ptr{Cvoid}, UInt(CUDA.CU_LAUNCH_PARAM_END_AS_INT))

            CUDA.cuLaunchKernel(func,
                        blocks, 1, 1,
                        threads, 1, 1,
                        #=shmem=#0, CUDA.stream(), C_NULL, config)
        end
    end

    function TapirOffloading.sync(::CUDABackend)
        CUDA.synchronize()
    end

    function TapirOffloading.pin(::CUDABackend, x)
        CUDA.CUDAKernels.__pin!(x)
    end
end