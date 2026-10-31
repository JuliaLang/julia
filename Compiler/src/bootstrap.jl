# This file is a part of Julia. License is MIT: https://julialang.org/license

# make sure that typeinf is executed before turning on typeinf_ext
# this ensures that typeinf_ext doesn't recurse before it can add the item to the workq
# especially try to make sure any recursive and leaf functions have concrete signatures,
# since we won't be able to specialize & infer them at runtime

function activate_codegen!()
    ccall(:jl_set_typeinf_func, Cvoid, (Any,), typeinf_ext_toplevel)
    # Register the new unified compile and emit function
    ccall(:jl_set_compile_and_emit_func, Cvoid, (Any,), compile_and_emit_native)
    Core.eval(Compiler, quote
        Core.OptimizedGenerics.CompilerPlugins.typeinf(::Nothing, mi::MethodInstance, source_mode::UInt8) =
            Base.invoke_in_world(unsafe_load(cglobal(:jl_typeinf_world, UInt)), typeinf_ext_toplevel, mi, Base.tls_world_age(), source_mode, Compiler.TRIM_NO)
    end)
end

# Run the compiler in the current world from now on. The system image build calls this once
# Base is complete: otherwise the compiler keeps running in the world it was bootstrapped in,
# and its code that later definitions invalidated is saved twice, once for that world.
function set_typeinf_world!()
    # Infer the compiler's invalidated code for the new world with the compiler of the old one
    # first. Otherwise the compiler infers itself on first use, where recursion makes it give up
    # and compile that code without inference, which slows down all inference after.
    args = Any[compile_invalidated!, tls_world_age()]
    ccall(:jl_call_in_typeinf_world, Any, (Ptr{Any}, Cint), args, length(args))
    ccall(:jl_set_typeinf_func, Cvoid, (Any,), typeinf_ext_toplevel)
    return nothing
end

# Infer and compile for `world` all code that has native code in the current world but is
# not valid in later ones
function compile_invalidated!(world::UInt)
    oldworld = tls_world_age()
    mis = MethodInstance[]
    visit(Core.methodtable) do method
        specs = isdefined(method, :specializations) ? method.specializations : nothing
        if specs isa SimpleVector
            for i = 1:length(specs)
                mi = specs[i]
                mi isa MethodInstance && is_compiled_invalidated(mi, oldworld) && push!(mis, mi)
            end
        elseif specs isa MethodInstance
            is_compiled_invalidated(specs, oldworld) && push!(mis, specs)
        end
        return true
    end
    for mi in mis
        typeinf_ext_toplevel(mi, world, SOURCE_MODE_ABI, TRIM_NO)
    end
    return nothing
end

function is_compiled_invalidated(mi::MethodInstance, world::UInt)
    isdefined(mi, :cache) || return false
    ci = mi.cache
    while true
        if ci.owner === nothing && ci.invoke != C_NULL && ci.min_world <= world <= ci.max_world
            return ci.max_world != typemax(UInt)
        end
        isdefined(ci, :next) || return false
        ci = ci.next
    end
end

global bootstrapping_compiler::Bool = false
function bootstrap!()
    global bootstrapping_compiler = true
    let time() = ccall(:jl_clock_now, Float64, ())
        println("Compiling the compiler. This may take several minutes ...")

        ssa_inlining_pass!_tt = Tuple{typeof(ssa_inlining_pass!), IRCode, InliningState{NativeInterpreter}, Bool}
        optimize_tt = Tuple{typeof(optimize), NativeInterpreter, OptimizationState{NativeInterpreter}, InferenceResult}
        typeinf_ext_tt = Tuple{typeof(typeinf_ext), NativeInterpreter, MethodInstance, UInt8}
        typeinf_tt = Tuple{typeof(typeinf), NativeInterpreter, InferenceState{NativeInterpreter}}
        typeinf_edge_tt = Tuple{
            typeof(typeinf_edge), NativeInterpreter, Method, Any, SimpleVector,
            InferenceState{NativeInterpreter}, Bool, Bool, Bool}
        fs = Any[
            # we first create caches for the optimizer, because they contain many loop constructions
            # and they're better to not run in interpreter even during bootstrapping
            compact!, ssa_inlining_pass!_tt, optimize_tt,
            # then we create caches for inference entries
            typeinf_ext_tt, typeinf_tt, typeinf_edge_tt,
        ]
        # tfuncs can't be inferred from the inference entries above, so here we infer them manually
        for x in T_FFUNC_VAL
            push!(fs, x[3])
        end
        for i = 1:length(T_IFUNC)
            if isassigned(T_IFUNC, i)
                x = T_IFUNC[i]
                push!(fs, x[3])
            else
                println(stderr, "WARNING: tfunc missing for ", reinterpret(IntrinsicFunction, Int32(i)))
            end
        end
        starttime = time()
        world = get_world_counter()
        for f in fs
            if isa(f, DataType) && f.name === typename(Tuple)
                tt = f
            else
                tt = Tuple{typeof(f), Vararg{Any}}
            end
            matches = _methods_by_ftype(tt, 10, world)::Vector
            if isempty(matches)
                println(stderr, "WARNING: no matching method found for `", tt, "`")
            else
                for m in matches
                    # remove any TypeVars from the intersection
                    m = m::MethodMatch
                    params = Any[m.spec_types.parameters...]
                    for i = 1:length(params)
                        params[i] = unwraptv(params[i])
                    end
                    mi = specialize_method(m.method, Tuple{params...}, m.sparams)
                    #isa_compileable_sig(mi) || println(stderr, "WARNING: inferring `", mi, "` which isn't expected to be called.")
                    typeinf_ext_toplevel(mi, world, isa_compileable_sig(mi) ? SOURCE_MODE_ABI : SOURCE_MODE_NOT_REQUIRED, TRIM_NO)
                end
            end
        end
        endtime = time()
        println("Base.Compiler ──── ", sub_float(endtime,starttime), " seconds")
    end
    activate_codegen!()
    global bootstrapping_compiler = false
    nothing
end

function activate!(; reflection=true, codegen=false)
    if reflection
        Base.REFLECTION_COMPILER[] = Compiler
    end
    if codegen
        bootstrap!()
    end
end
