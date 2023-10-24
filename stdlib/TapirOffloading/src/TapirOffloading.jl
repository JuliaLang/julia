module TapirOffloading

abstract type Backend end

const BACKENDS = Dict{Symbol, Backend}()

function register(name, backend)
    if haskey(BACKENDS, name)
        error("$name is already a registered backend")
    end
    BACKENDS[name] = backend
end

function runtime end
function compiler_config end
function link_kernel end
function launch end
function sync end
function pin end

sync() = sync(current_backend())
pin(x) = pin(current_backend(), x)

const CURRENT_BACKEND = ScopedValue{Symbol}(:cuda)
current_backend() = BACKENDS[CURRENT_BACKEND[]]

using Tapir
using LLVM
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

const rt_methods = [
    GPUCompiler.Runtime.RuntimeMethodInstance(
        :__rts_get_iteration_64,
        Int64, (Int64, Int64),
        :__rts_get_iteration_64,
        nothing, nothing,
        "__rts_get_iteration_64"
    ),
]

function build_runtime(backend, config)
    mod = LLVM.Module("Tapir run-time library")

    # the compiler job passed into here is identifies the job that requires the runtime.
    # derive a job that represents the runtime itself (notably with kernel=false).
    config = CompilerConfig(config; kernel=false)

    for method in rt_methods
        def = if isa(method.def, Symbol)
            getglobal(runtime(backend), method.def)
        else
            method.def
        end
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

function placeholder() end

function codegen(backend, ir::LLVM.Module, entry_fn)
    initialize()
    cfg = compiler_config(backend)

    @info "Compiling for" target=cfg.target

    job = CompilerJob(GPUCompiler.methodinstance(typeof(placeholder), Tuple{}), cfg, Base.get_world_counter())

    # 0. Fake it
    triple!(ir, GPUCompiler.llvm_triple(job.config.target))
    if GPUCompiler.julia_datalayout(job.config.target) !== nothing
        datalayout!(ir, GPUCompiler.julia_datalayout(job.config.target))
    end

    rt_mod = build_runtime(backend, cfg)
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

@ccallable function __chi_lookup_or_compile(mod::Ptr{Int8}, nbytes::Csize_t, name::Ptr{Int8})::Ptr{Cvoid}
    backend = current_backend()
    name = Base.unsafe_string(name)
    mod = unsafe_wrap(Vector{Int8}, mod, nbytes)
    @info "Lookup or Compile called" name nbytes
    image, name = ThreadSafeContext() do ts_ctx
        LLVM.context!(LLVM.context(ts_ctx)) do
            mod = parse(LLVM.Module, mod)
            codegen(backend, mod, name)
        end
    end
    return link_kernel(backend, image, name)::Ptr{Cvoid}
end

@ccallable function __chi_launch(func::Ptr{Int8}, args::Ptr{Int8}, args_sz::Csize_t, N::Csize_t)::Cvoid
    @assert func !== C_NULL
    backend = current_backend()
    launch(backend, func, args, args_sz, N)
    return nothing
end

@ccallable function __chi_sync()::Cvoid
    backend = current_backend()
    sync(backend)
end

end # module TapirOffloading
