# This file is a part of Julia. License is MIT: https://julialang.org/license

# This file replaces the functionality from src/precompile_utils.c

compile_hint(@nospecialize(argt::Type)) = ccall(:jl_compile_hint, Int32, (Any,), argt) != 0

# Utility functions for type manipulation
function count_union_components(t::Union)
    count = 0
    while true
        count += 1
        t = t.b
        if !isa(t, Union)
            count += 1
            break
        end
    end
    return count
end

function nth_union_component(t::Union, n::Int)
    current = 1
    while current < n && isa(t, Union)
        current += 1
        t = t.b
    end
    if current == n
        return isa(t, Union) ? t.a : t
    else
        error("Index out of bounds for Union type")
    end
end

# Port of _compile_all_tvar_union
# f(...) where {T<:Union{...}} is a common pattern
function compile_all_tvar_union(methsig)
    tvarslen = unionall_depth(methsig)
    if tvarslen == 0
        return compile_hint(methsig)
    end

    sigbody = methsig
    env = Vector{Any}(undef, 2 * tvarslen)
    idx = Vector{Int}(undef, tvarslen)

    # Initialize environment
    for i in 1:tvarslen
        if !isa(sigbody, UnionAll)
            return false
        end
        idx[i] = 1
        var = sigbody.var
        env[2*i - 1] = var

        # Get upper bound
        tv = var
        while isa(tv, TypeVar)
            tv = tv.ub
        end

        if isa(tv, DataType) && isabstracttype(tv) && !isa(tv, Type)
            return false  # Any as TypeVar is common and not useful here
        end

        env[2*i] = tv
        sigbody = sigbody.body
    end

    all_success = true
    incr = false

    while !incr
        # Generate all combinations
        for i in 1:tvarslen
            incr = true
            tv = env[2*i - 1]
            while isa(tv, TypeVar)
                tv = tv.ub
            end

            if isa(tv, Union)
                l = count_union_components(tv)
                j = idx[i]
                env[2*i] = nth_union_component(tv, j)
                j += 1

                if incr
                    if j > l
                        idx[i] = 1
                    else
                        idx[i] = j
                        incr = false
                    end
                end
            end
        end

        # Try to instantiate and compile
        sig = try
                ccall(:jl_instantiate_type_with, Any, (Any, Ptr{Any}, Cint),
                        sigbody, env, tvarslen)
            catch
                nothing
            end
        if isa(sig, DataType) && ccall(:jl_has_concrete_subtype, Cint, (Any,), sig) != 0
            success = compile_hint(sig)
            all_success = all_success && success
        else
            all_success = false
        end
    end

    return all_success
end

# Port of _compile_all_union
function compile_all_union(sig)
    sigbody = unwrap_unionall(sig)::DataType

    if !isa(sigbody, Type) || !isa(sigbody, DataType)
        return compile_all_tvar_union(sig)
    end

    count_unions = 0
    union_size = 1
    params = sigbody.parameters

    for ty in params
        if isa(ty, Union)
            count_unions += 1
            union_size *= count_union_components(ty)
        elseif isa(ty, DataType) &&
               ((!isconcretetype(ty) || iskindtype(ty)) && !isType(ty))
            return false  # no amount of union splitting will help
        end
    end

    if union_size <= 1 || union_size > 8
        return compile_all_tvar_union(sig)
    end

    idx = zeros(Int, count_unions)
    all_success = true

    incr = false
    while !incr
        # Generate parameter combinations
        new_params = Vector{Any}(undef, length(params))
        idx_ctr = 1
        incr = true

        for (i, ty) in enumerate(params)
            if isa(ty, Union)
                l = count_union_components(ty)
                j = idx[idx_ctr]
                new_params[i] = nth_union_component(ty, j + 1)  # 1-based indexing
                j += 1

                if incr
                    if j >= l
                        idx[idx_ctr] = 0
                    else
                        idx[idx_ctr] = j
                        incr = false
                    end
                end
                idx_ctr += 1
            else
                new_params[i] = ty
            end
        end

        # Create new signature and try to compile
        # Reconstruct tuple type
        new_sigbody = Tuple{new_params...}
        # Rewrap in UnionAll if needed
        methsig = rewrap_unionall(new_sigbody, sig)
        success = compile_all_tvar_union(methsig)
        all_success = all_success && success
    end
    return all_success
end

# Complete method collection implementation
function collect_all_method_defs(newmodules, mod_array)
    allmeths = Any[]

    function method_visitor(method)
        method = method::Method
        if newmodules !== nothing
            method.module in newmodules || return true
        end
        if isdefined(method, :external_mt)
            return true  # Continue iteration
        end
        push!(allmeths, method)
        return true
    end

    # Always visit the global method table first
    visit(method_visitor, Core.methodtable)

    # If mod_array is provided, iterate through modules looking for MethodTable objects
    #if mod_array !== nothing
    #    function visit_methodtable(mt::Core.MethodTable)
    #        if mt !== Core.methodtable  # Skip global method table since we already visited it
    #            visit(method_visitor, mt)
    #        end
    #    end

    #    function foreach_mtable_in_module(mod::Module)
    #        # Get all bindings in the module and look for MethodTable objects
    #        for name in names(mod, all=true, imported=true)
    #            if isdefined(mod, name)
    #                val = getglobal(mod, name)
    #                if isa(val, Module) && val !== mod && parentmodule(val) === mod
    #                    # Recursively visit submodules
    #                    foreach_mtable_in_module(val)
    #                elseif isa(val, Core.MethodTable)
    #                    # Visit this method table
    #                    visit_methodtable(val)
    #                end
    #            end
    #        end
    #    end

    #    # Iterate through provided modules
    #    for mod in mod_array
    #        if isa(mod, Module)
    #            # Only visit toplevel modules (where parent == mod)
    #            if parentmodule(mod) === mod
    #                foreach_mtable_in_module(mod)
    #            end
    #        end
    #    end
    #end
    return allmeths
end

function infer_all_method_defs!(all::Bool, allmeths, world::UInt, worklist)
    # Process collected methods and create method instances
    for m in allmeths
        m = m::Method

        # Skip macro methods unless specifically requested
        if !all && !iszero(ccall(:jl_method_is_macro, Cint, (Any,), m))
            continue
        end

        if !isdefined(m, :source)
            continue
        end

        # Check if this method has a single compilable specialization
        if isa(m.sig, DataType) && isa_compileable_sig(m.sig, Core.svec(), m)
            # Method has a single compilable specialization, e.g. its definition
            # signature is concrete. in this case we can just hint it.
            ccall(:jl_compile_method_sig, Cvoid, (Any, Any, Any, Csize_t),
                  m, m.sig, Core.svec(), world)
        else
            # Try to create leaf signatures using union expansion from the signature declaration and compile those
            compile_all_union(m.sig)

            if all
                # Also compile fully generic fallback if requested
                unspec = ccall(:jl_get_unspecialized, Any, (Any,), m)
                if unspec !== nothing
                    push!(worklist, unspec)
                end

            end
        end
    end
end

# This corresponds to precompile_enq_all_specializations_
function enqueue_specializations!(all::Bool, newmethods, worklist)
    for method in newmethods
        method = method::Method

        # Check for special methods that should always be compiled
        if (method.name === :__init__ || isdefined(method, :ccallable)) && isdispatchtuple(method.sig)
            # Get method instance for __init__ methods and @ccallable functions
            mi = specialize_method(method, method.sig, Core.svec())::MethodInstance
            push!(worklist, mi)
        else
            # Process existing specializations
            specializations = method.specializations
            if isa(specializations, Core.SimpleVector)
                for i = 1:length(specializations)
                    mi = specializations[i]
                    if mi !== nothing
                        enqueue_specialization!(all, worklist, mi::Core.MethodInstance)
                    end
                end
            elseif isa(specializations, Core.MethodInstance)
                enqueue_specialization!(all, worklist, specializations)
            end
        end

        # Handle ccallable methods
        if isdefined(method, :ccallable)
            push!(worklist, method.ccallable)
        end
    end
end

function enqueue_specialization!(all::Bool, worklist, mi::Core.MethodInstance)
    # Translation of precompile_enq_specialization_ from C
    codeinst = isdefined(mi, :cache) ? mi.cache : nothing
    while codeinst !== nothing
        do_compile = false
        if codeinst.owner !== nothing
            # TODO(vchuravy) native code caching for foreign interpreters
            # Skip foreign code instances
        elseif use_const_api(codeinst) # Check if invoke is jl_fptr_const_return
            do_compile = true
        elseif codeinst.invoke != C_NULL || codeinst.precompile
            do_compile = true
        elseif !do_compile && isdefined(codeinst, :inferred)
            inferred = codeinst.inferred
            # Check compilation options and inlining cost
            if (all || inferred === nothing ||
                ((isa(inferred, String) || isa(inferred, CodeInfo) || isa(inferred, UInt8)) &&
                 ccall(:jl_ir_inlining_cost, UInt16, (Any,), inferred) == typemax(UInt16)))
                do_compile = true
            end
        end
        if do_compile
            push!(worklist, mi)
            return true
        end
        # Move to the next code instance in the chain
        codeinst = isdefined(codeinst, :next) ? codeinst.next : nothing
    end
    return true
end

# Main unified compilation and emission function
# This replaces the functionality from jl_precompile
function compile_and_emit_native(worlds::Vector{UInt},
                                 trim_mode::UInt8,
                                 external_linkage::Bool,
                                 newmodules, # Vector{Module} or Nothing
                                 mod_array, # Vector{Module} or Nothing
                                 all::Bool,
                                 module_init_order::Vector{Any}) # Vector{Module}
    latestworld = worlds[end]

    # Step 1: Precompile all __init__ methods that will be required
    for mod in module_init_order
        if Core.invoke_in_world(latestworld, isdefined, mod, :__init__)
            f = Core.invoke_in_world(latestworld, getglobal, mod, :__init__)
            # Get module compile setting
            setting = ccall(:jl_get_module_compile, Cint, (Any,), mod)
            if setting != 0 && setting != 1  # JL_OPTIONS_COMPILE_OFF=0, JL_OPTIONS_COMPILE_MIN=1
                tt = Tuple{Core.Typeof(f)}
                compile_hint(tt)
                trim_mode == 0x00 || add_entrypoint(tt)
            end
        end
    end

    # Step 2: Collect all method definitions, filtered by worklist if provided
    newmethods = collect_all_method_defs(newmodules, mod_array)

    # Step 3: Collect set of method instances that seem worth compiling
    specialization_worklist = []
    if trim_mode == 0x00
        if newmodules === nothing
            infer_all_method_defs!(all, newmethods, latestworld, specialization_worklist)
        else
            # Compute new_ext_cis using queue_external_cis with global newly_inferred
            new_ext_cis = ccall(:jl_compute_new_ext_cis, Any, ())
            if new_ext_cis !== nothing
                for i in 1:length(new_ext_cis::Vector{Any})
                    ci = new_ext_cis[i]::CodeInstance
                    enqueue_specialization!(all, specialization_worklist, get_ci_mi(ci))
                end
            end
        end
        enqueue_specializations!(all, newmethods, specialization_worklist)

        # Process the specialization worklist and prepare final tocompile worklist
        tocompile = []
        for item in specialization_worklist
            if isa(item, Core.MethodInstance)
                processed_mi = process_method_instance_for_compilation(item, latestworld)
                if processed_mi !== nothing
                    push!(tocompile, processed_mi)
                end
            else
                # Handle SimpleVector (ccallable entries)
                push!(tocompile, item::Core.SimpleVector)
            end
        end
    else # trimming mode
        # array of MethodInstances and ccallable aliases to include in the output
        tocompile = []

        # Process entrypoint method instances
        for mi in _entrypoint_mis
            # Add the method instance to compile list
            push!(tocompile, mi)

            # Check if this method has a ccallable annotation
            if isdefined(mi.def, :ccallable) && mi.def.ccallable !== nothing
                push!(tocompile, mi.def.ccallable)
            end
        end
    end

    # Step 4: Perform type inference on tocompile to create codeinfos
    codeinfos = try
        typeinf_ext_toplevel(tocompile, worlds, trim_mode)
    catch exc
        # Handle trimming failures
        isa(exc, Core.TrimFailure) || rethrow()
        # The verification check failed. The error message should already have
        # been printed, so give up here and exit (w/o a stack trace).
        invokelatest(invokelatest(getglobal, Base, :exit), 1)
    end

    return codeinfos

end

# Helper function to process method instances for compilation
# This corresponds to the logic in jl_precompile_
function process_method_instance_for_compilation(mi::Core.MethodInstance, world::UInt)
    method = mi.def::Method
    if !(isdefined(method, :unspecialized) && mi === method.unspecialized)
        if !isa_compileable_sig(mi.specTypes, mi.sparam_vals, method)
            # Try to get a compileable specialization
            mi = ccall(:jl_get_specialization1, Any, (Any, Csize_t, Cint),
                         mi.specTypes, world, #= mt_cache =# 0)::Union{Nothing,MethodInstance}
        end
    end
    return mi
end

const _entrypoint_mis = Vector{Core.MethodInstance}()

# Add a method signature as an entrypoint for compilation.
function add_entrypoint(types::Type)
    world = get_world_counter()
    # Get the method instance for this signature
    mi = ccall(:jl_get_compile_hint_specialization, Any,
                   (Any, Csize_t, Cint),
                   types, world, 1)
    if mi === nothing
        return false
    end
    push!(_entrypoint_mis, mi::Core.MethodInstance)
    return true
end

# This corresponds to jl_add_ccallable_entrypoints
function add_ccallable_entrypoints!()
    # Collect all methods with ccallable annotations
    ccallable_methods = Any[]
    visit(Core.methodtable) do method
        method = method::Method
        if isdefined(method, :ccallable)
            # Add the ccallable tuple signature
            ccallable_sig = method.ccallable[2]  # Second element is the signature
            add_entrypoint(ccallable_sig)
        end
        return true
    end
end
