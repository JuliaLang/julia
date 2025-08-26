# This file is a part of Julia. License is MIT: https://julialang.org/license

struct GlobalRefIterator
    mod::Module
end
IteratorSize(::Type{GlobalRefIterator}) = SizeUnknown()
globalrefs(mod::Module) = GlobalRefIterator(mod)

function iterate(gri::GlobalRefIterator, i = 1)
    m = gri.mod
    table = ccall(:jl_module_get_bindings, Ref{SimpleVector}, (Any,), m)
    i > length(table) && return nothing
    b = table[i]
    b === nothing && return iterate(gri, i+1)
    return ((b::Core.Binding).globalref, i+1)
end

function foreachgr(visit, src::CodeInfo)
    stmts = src.code
    for i = 1:length(stmts)
        stmt = stmts[i]
        isa(stmt, GlobalRef) && visit(stmt)
        for ur in Compiler.userefs(stmt)
            arg = ur[]
            isa(arg, GlobalRef) && visit(arg)
        end
    end
end

function anygr(visit, src::CodeInfo)
    stmts = src.code
    for i = 1:length(stmts)
        stmt = stmts[i]
        if isa(stmt, GlobalRef)
            visit(stmt) && return true
            continue
        end
        for ur in Compiler.userefs(stmt)
            arg = ur[]
            isa(arg, GlobalRef) && visit(arg) && return true
        end
    end
    return false
end

function should_invalidate_code_for_globalref(gr::GlobalRef, src::CodeInfo)
    isgr(g::GlobalRef) = gr.mod == g.mod && gr.name === g.name
    isgr(g) = false
    return anygr(isgr, src)
end

function scan_edge_list(ci::Core.CodeInstance, binding::Core.Binding)
    isdefined(ci, :edges) || return false
    edges = ci.edges
    i = 1
    while i <= length(edges)
        if isassigned(edges, i) && edges[i] === binding
            return true
        end
        i += 1
    end
    return false
end

function invalidate_method_for_globalref!(gr::GlobalRef, method::Method, invalidated_bpart::Core.BindingPartition, new_max_world::UInt)
    invalidate_all = false
    binding = convert(Core.Binding, gr)
    if isdefined(method, :source)
        src = _uncompressed_ir(method)
        invalidate_all = should_invalidate_code_for_globalref(gr, src)
    end
    if invalidate_all && !Base.generating_output()
        @atomic method.did_scan_source |= 0x4
    end
    invalidated_any = false
    for mi in specializations(method)
        isdefined(mi, :cache) || continue
        ci = mi.cache
        invalidated = false
        while true
            if ci.max_world > new_max_world && (invalidate_all || scan_edge_list(ci, binding))
                ccall(:jl_invalidate_code_instance, Cvoid, (Any, UInt), ci, new_max_world)
                invalidated = true
            end
            isdefined(ci, :next) || break
            ci = ci.next
        end
        invalidated && ccall(:jl_maybe_log_binding_invalidation, Cvoid, (Any,), mi)
        invalidated_any |= invalidated
    end
    return invalidated_any
end

export_affecting_partition_flags(bpart::Core.BindingPartition) =
    ((bpart.kind & PARTITION_MASK_KIND) == PARTITION_KIND_GUARD,
     (bpart.kind & PARTITION_FLAG_EXPORTED) != 0,
     (bpart.kind & PARTITION_FLAG_DEPRECATED) != 0)

function invalidate_code_for_globalref!(b::Core.Binding, invalidated_bpart::Core.BindingPartition, new_bpart::Core.BindingPartition, new_max_world::UInt)
    gr = b.globalref

    (_, (ib, ibpart)) = Compiler.walk_binding_partition(b, invalidated_bpart, new_max_world)
    (_, (nb, nbpart)) = Compiler.walk_binding_partition(b, new_bpart, new_max_world+1)

    # `abstract_eval_partition_load` is the maximum amount of information that inference
    # reads from a binding partition. If this information does not change - we do not need to
    # invalidate any code that inference created, because we know that the result will not change.
    need_to_invalidate_code =
        Compiler.abstract_eval_partition_load(nothing, ib, ibpart) !==
        Compiler.abstract_eval_partition_load(nothing, nb, nbpart)

    need_to_invalidate_export = export_affecting_partition_flags(invalidated_bpart) !==
                                export_affecting_partition_flags(new_bpart)

    invalidated_any = false
    queued_bindings = Tuple{Core.Binding, Core.BindingPartition, Core.BindingPartition}[]    # defer handling these to keep the logging coherent
    if need_to_invalidate_code
        if (b.flags & BINDING_FLAG_ANY_IMPLICIT_EDGES) != 0
            nmethods = ccall(:jl_module_scanned_methods_length, Csize_t, (Any,), gr.mod)
            for i = 1:nmethods
                method = ccall(:jl_module_scanned_methods_getindex, Any, (Any, Csize_t), gr.mod, i)::Method
                invalidated_any |= invalidate_method_for_globalref!(gr, method, invalidated_bpart, new_max_world)
            end
        end
        nbackedges = ccall(:jl_binding_backedges_length, Csize_t, (Any,), b)
        for i = 1:nbackedges
            edge = ccall(:jl_binding_backedges_getindex, Any, (Any, Csize_t), b, i)
            if isa(edge, CodeInstance)
                ccall(:jl_invalidate_code_instance, Cvoid, (Any, UInt), edge, new_max_world)
                invalidated_any = true
            elseif isa(edge, Core.Binding)
                isdefined(edge, :partitions) || continue
                latest_bpart = edge.partitions
                latest_bpart.max_world == typemax(UInt) || continue
                is_some_imported(binding_kind(latest_bpart)) || continue
                if is_some_binding_imported(binding_kind(latest_bpart))
                    partition_restriction(latest_bpart) === b || continue
                end
                push!(queued_bindings, (edge, latest_bpart, latest_bpart))
            else
                invalidated_any |= invalidate_method_for_globalref!(gr, edge::Method, invalidated_bpart, new_max_world)
            end
        end
    end

    if need_to_invalidate_code || need_to_invalidate_export
        # This binding was exported - we need to check all modules that `using` us to see if they
        # have a binding that is affected by this change.
        usings_backedges = ccall(:jl_get_module_usings_backedges, Any, (Any,), gr.mod)
        if usings_backedges !== nothing
            for user::Module in usings_backedges::Vector{Any}
                user_binding = ccall(:jl_get_module_binding_or_nothing, Any, (Any, Any), user, gr.name)::Union{Core.Binding, Nothing}
                user_binding === nothing && continue
                isdefined(user_binding, :partitions) || continue
                latest_bpart = user_binding.partitions
                latest_bpart.max_world == typemax(UInt) || continue
                is_some_implicit(binding_kind(latest_bpart)) || continue
                new_bpart = ccall(:jl_maybe_reresolve_implicit, Any, (Any, Csize_t), user_binding, new_max_world)
                if need_to_invalidate_code || new_bpart !== latest_bpart
                    push!(queued_bindings, (convert(Core.Binding, user_binding), latest_bpart, new_bpart))
                end
            end
        end
    end
    invalidated_any && ccall(:jl_maybe_log_binding_invalidation, Cvoid, (Any,), invalidated_bpart)
    for (edge, invalidated_bpart, new_bpart) in queued_bindings
        invalidated_any |= invalidate_code_for_globalref!(edge, invalidated_bpart, new_bpart, new_max_world)
    end
    return invalidated_any
end
invalidate_code_for_globalref!(gr::GlobalRef, invalidated_bpart::Core.BindingPartition, new_bpart::Core.BindingPartition, new_max_world::UInt) =
    invalidate_code_for_globalref!(convert(Core.Binding, gr), invalidated_bpart, new_bpart, new_max_world)

function maybe_add_binding_backedge!(b::Core.Binding, edge::Union{Method, CodeInstance})
    meth = isa(edge, Method) ? edge : Compiler.get_ci_mi(edge).def
    ccall(:jl_maybe_add_binding_backedge, Cint, (Any, Any, Any), b, edge, meth)
    return nothing
end

function binding_was_invalidated(b::Core.Binding)
    # At least one partition is required for invalidation
    !isdefined(b, :partitions) && return false
    b.partitions.min_world > unsafe_load(cglobal(:jl_require_world, UInt))
end

function scan_new_method!(method::Method, image_backedges_only::Bool)
    isdefined(method, :source) || return
    if image_backedges_only && !has_image_globalref(method)
        return
    end
    src = _uncompressed_ir(method)
    mod = method.module
    foreachgr(src) do gr::GlobalRef
        b = convert(Core.Binding, gr)
        if binding_was_invalidated(b)
            # TODO: We could turn this into an addition if condition. For now, use it as a reasonably cheap
            # additional consistency chekc
            @assert !image_backedges_only
            @atomic method.did_scan_source |= 0x4
        end
        maybe_add_binding_backedge!(b, method)
    end
    @atomic method.did_scan_source |= 0x1
end

function scan_new_methods!(extext_methods::Vector{Any}, internal_methods::Vector{Any}, image_backedges_only::Bool)
    for method in internal_methods
        if isa(method, Method)
           scan_new_method!(method, image_backedges_only)
        end
    end
    for tme::Core.TypeMapEntry in extext_methods
        scan_new_method!(tme.func::Method, image_backedges_only)
    end
end
