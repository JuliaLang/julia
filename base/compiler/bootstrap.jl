# This file is a part of Julia. License is MIT: https://julialang.org/license

# make sure that typeinf is executed before turning on typeinf_ext
# this ensures that typeinf_ext doesn't recurse before it can add the item to the workq
# especially try to make sure any recursive and leaf functions have concrete signatures,
# since we won't be able to specialize & infer them at runtime

time() = ccall(:jl_clock_now, Float64, ())

let
    world = get_world_counter()
    interp = NativeInterpreter(world)

    fs = Any[
        # we first create caches for the optimizer, because they contain many loop constructions
        # and they're better to not run in interpreter even during bootstrapping
        run_passes,
        # then we create caches for inference entries
        typeinf_ext, typeinf, typeinf_edge,
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
    for f in fs
        for m in _methods_by_ftype(Tuple{typeof(f), Vararg{Any}}, 10, typemax(UInt))
            # remove any TypeVars from the intersection
            typ = Any[m.spec_types.parameters...]
            for i = 1:length(typ)
                if isa(typ[i], TypeVar)
                    typ[i] = typ[i].ub
                end
            end
            typeinf_type(interp, m.method, Tuple{typ...}, m.sparams)
        end
    end
    endtime = time()
    println("Core.Compiler ──── ", sub_float(endtime,starttime), " seconds")
end
