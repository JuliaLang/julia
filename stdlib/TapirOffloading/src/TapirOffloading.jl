module TapirOffloading

using Tapir
using LLVM

##
# Offloading
##




# For now only support one backend

using LLVM
using CUDA
using GPUCompiler

function initialize()
    if !GPUCompiler.__llvm_initialized[]
        @info "Initializing LLVM" libLLVMExtra = LLVM.API.libLLVMExtra
        LLVM.InitializeAllTargets()
        LLVM.InitializeAllTargetInfos()
        LLVM.InitializeAllAsmPrinters()
        LLVM.InitializeAllAsmParsers()
        LLVM.InitializeAllTargetMCs()
        GPUCompiler.__llvm_initialized[] = true
    end
end

module TapirCUDARuntime
    import CUDA: blockIdx, blockDim, threadIdx, i32

    function __rts_get_iteration_64(base::Int64, grainsize::Int64)::Int64
        idx = (blockIdx().x-1i32) * blockDim().x + threadIdx().x
        base + (idx-1i32) * grainsize
    end
end

const rt_methods = [
    GPUCompiler.Runtime.RuntimeMethodInstance(
        TapirCUDARuntime.__rts_get_iteration_64,
        Int64, (Int64, Int64),
        :__rts_get_iteration_64,
        nothing, nothing,
        "__rts_get_iteration_64"
    ),

]

function build_runtime(config)
    mod = LLVM.Module("Tapir run-time library")

    # the compiler job passed into here is identifies the job that requires the runtime.
    # derive a job that represents the runtime itself (notably with kernel=false).
    config = CompilerConfig(config; kernel=false)

    for method in rt_methods
        @assert !isa(method.def, Symbol)
        def = method.def
        GPUCompiler.emit_function!(mod, config, typeof(def), method)
    end

    for f in functions(mod)
        push!(function_attributes(f), EnumAttribute("alwaysinline", 0))
    end

    return mod
end

const Key = Tuple{Ptr{Int8}, Ptr{Int8}}
# TODO: memoize on device
# See CUDA.jl -- src/compiler/compilation.jl

const KERNEL_CACHE = Dict{Key, CUDA.CuFunction}

function placeholder() end

function codegen(ir::LLVM.Module, entry_fn)
    initialize()

    cuda = CUDA.active_state()
    cfg = CUDA.compiler_config(cuda.device)::CUDA.CUDACompilerConfig
    # caps = CUDA.llvm_compat(LLVM.version()).cap
    # cap = last(caps)
    # cfg = CompilerConfig(PTXCompilerTarget(; cap), CUDA.CUDACompilerParams())

    @info "Compiling for" target=cfg.target

    job = CompilerJob(GPUCompiler.methodinstance(typeof(placeholder), Tuple{}), cfg, Base.get_world_counter())

    # 0. Fake it
    triple!(ir, GPUCompiler.llvm_triple(job.config.target))
    if GPUCompiler.julia_datalayout(job.config.target) !== nothing
        datalayout!(ir, GPUCompiler.julia_datalayout(job.config.target))
    end

    rt_mod = build_runtime(cfg)
    LLVM.link!(ir, rt_mod)

    # finalize the current module. this needs to happen before linking deferred modules,
    # since those modules have been finalized themselves, and we don't want to re-finalize.
    entry = functions(ir)[entry_fn]
    entry = GPUCompiler.finish_module!(job, ir, entry)

    # TODO: Mark everything internal?

    # mark the kernel entry-point functions
    # We don't want the kernel-state since we haven't accounted for that in
    # the ABI
    # push!(metadata(ir)["julia.kernel"], MDNode([entry]))

    # 1. Optimizations
    GPUCompiler.optimize!(job, ir)

    # optimization may have replaced functions
    entry = functions(ir)[entry_fn]
    LLVM.name!(entry, GPUCompiler.safe_name(entry_fn))
    entry_fn = LLVM.name(entry)

    # TODO: Cleanup?

    # finish the module
    entry = GPUCompiler.finish_ir!(job, ir, entry)

    GPUCompiler.check_ir(job, ir)
    verify(ir)

    @info "Backend" ir

    # 2. Emission
    asm, asm_meta = GPUCompiler.emit_asm(job, ir; strip=true, validate=true, format=LLVM.API.LLVMAssemblyFile)
    return asm, entry_fn
end


##
# Offloading runtime
##

import Base: @ccallable

const ROOTED_FUNCTIONS = Vector{CuFunction}()

@ccallable function __chi_lookup_or_compile(mod::Ptr{Int8}, nbytes::Csize_t, name::Ptr{Int8})::Ptr{Cvoid}
    name = Base.unsafe_string(name)
    mod = unsafe_wrap(Vector{Int8}, mod, nbytes)
    @info "Lookup or Compile called" name nbytes
    image, name = ThreadSafeContext() do ts_ctx
        LLVM.context!(LLVM.context(ts_ctx)) do
            mod = parse(LLVM.Module, mod)
            codegen(mod, name)
        end
    end
    @info "Emitted NVPTX"
    write(stdout, image)
    cu_mod = CuModule(image)
    cu_func = CuFunction(cu_mod, name)
    push!(ROOTED_FUNCTIONS, cu_func)
    return Base.unsafe_convert(Ptr{Cvoid}, cu_func.handle)
end

@ccallable function __chi_launch(func::Ptr{Int8}, args::Ptr{Int8}, args_sz::Csize_t, N::Csize_t)::Cvoid
    @assert func !== C_NULL

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

    return nothing
end

@ccallable function __chi_sync()::Cvoid
    CUDA.synchronize()
end

end # module TapirOffloading
