# This file is a part of Julia. License is MIT: https://julialang.org/license

# make sure that typeinf is executed before turning on typeinf_ext
# this ensures that typeinf_ext doesn't recurse before it can add the item to the workq
# especially try to make sure any recursive and leaf functions have concrete signatures,
# since we won't be able to specialize & infer them at runtime

module Main
    import Base: time
    import JuliaInterpreter: NativeInterpreter
    import Core.Compiler: typeinf_ext, typeinf, typeinf_edge, _methods_by_ftype, MethodMatch, typeinf_type

    # Define custom types here, if needed

    # Function signatures
    signatures = Any[
        Tuple{typeof(optimize), NativeInterpreter, OptimizationState{NativeInterpreter}, InferenceResult},
        # Add more function signatures as needed
    ]

    # Add function types manually
    for x in T_FFUNC_VAL
        push!(signatures, Tuple{typeof(x[3]), Vararg{Any}})
    end

    for i = 1:length(T_IFUNC)
        if isassigned(T_IFUNC, i)
            x = T_IFUNC[i]
            push!(signatures, Tuple{typeof(x[3]), Vararg{Any}})
        else
            println(stderr, "WARNING: tfunc missing for ", reinterpret(IntrinsicFunction, Int32(i)))
        end
    end

    # A method to perform type inference
    function perform_type_inference(interp::NativeInterpreter, signatures::Vector{T}) where T
        starttime = time()
        for sig in signatures
            for m in _methods_by_ftype(sig, 10, get_world_counter())::Vector{MethodMatch}
                # Remove TypeVars from the intersection
                typ = Any[m.spec_types.parameters...]
                for i = 1:length(typ)
                    typ[i] = unwraptv(typ[i])
                end
                typeinf_type(interp, m.method, Tuple{typ...}, m.sparams)
            end
        end
        endtime = time()
        println("Core.Compiler ──── ", sub_float(endtime, starttime), " seconds")
    end

    # Initialize the NativeInterpreter
    interp = NativeInterpreter()

    # Perform type inference
    perform_type_inference(interp, signatures)
end

