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

const TYPE_TYPE_MT = Type.body.name.mt
const NONFUNCTION_MT = Core.MethodTable.name.mt
function foreach_module_mtable(visit, m::Module, world::UInt)
    for gb in globalrefs(m)
        binding = gb.binding
        bpart = lookup_binding_partition(world, binding)
        if is_defined_const_binding(binding_kind(bpart))
            v = partition_restriction(bpart)
            uw = unwrap_unionall(v)
            name = gb.name
            if isa(uw, DataType)
                tn = uw.name
                if tn.module === m && tn.name === name && tn.wrapper === v && isdefined(tn, :mt)
                    # this is the original/primary binding for the type (name/wrapper)
                    mt = tn.mt
                    if mt !== nothing && mt !== TYPE_TYPE_MT && mt !== NONFUNCTION_MT
                        @assert mt.module === m
                        visit(mt) || return false
                    end
                end
            elseif isa(v, Core.MethodTable) && v.module === m && v.name === name
                # this is probably an external method table here, so let's
                # assume so as there is no way to precisely distinguish them
                visit(v) || return false
            end
        end
    end
    return true
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
        old_stmts = src.code
        invalidate_all = should_invalidate_code_for_globalref(gr, src)
    end
    for mi in specializations(method)
        isdefined(mi, :cache) || continue
        ci = mi.cache
        while true
            if ci.max_world > new_max_world && (invalidate_all || scan_edge_list(ci, binding))
                ccall(:jl_invalidate_code_instance, Cvoid, (Any, UInt), ci, new_max_world)
            end
            isdefined(ci, :next) || break
            ci = ci.next
        end
    end
end

export_affecting_partition_flags(bpart::Core.BindingPartition) =
    ((bpart.kind & PARTITION_MASK_KIND) == PARTITION_KIND_GUARD,
     (bpart.kind & PARTITION_FLAG_EXPORTED) != 0,
     (bpart.kind & PARTITION_FLAG_DEPRECATED) != 0)

function invalidate_code_for_globalref!(b::Core.Binding, invalidated_bpart::Core.BindingPartition, new_bpart::Core.BindingPartition, new_max_world::UInt)
    gr = b.globalref

    (_, (ib, ibpart)) = Compiler.walk_binding_partition(b, invalidated_bpart, new_max_world)
    (_, (nb, nbpart)) = Compiler.walk_binding_partition(b, new_bpart, new_max_world+1)

    # abstract_eval_globalref_partition is the maximum amount of information that inference
    # reads from a binding partition. If this information does not change - we do not need to
    # invalidate any code that inference created, because we know that the result will not change.
    need_to_invalidate_code =
        Compiler.abstract_eval_globalref_partition(nothing, ib, ibpart) !==
        Compiler.abstract_eval_globalref_partition(nothing, nb, nbpart)

    need_to_invalidate_export = export_affecting_partition_flags(invalidated_bpart) !==
                                export_affecting_partition_flags(new_bpart)

    if need_to_invalidate_code
        if (b.flags & BINDING_FLAG_ANY_IMPLICIT_EDGES) != 0
            nmethods = ccall(:jl_module_scanned_methods_length, Csize_t, (Any,), gr.mod)
            for i = 1:nmethods
                method = ccall(:jl_module_scanned_methods_getindex, Any, (Any, Csize_t), gr.mod, i)::Method
                invalidate_method_for_globalref!(gr, method, invalidated_bpart, new_max_world)
            end
        end
        if isdefined(b, :backedges)
            for edge in b.backedges
                if isa(edge, CodeInstance)
                    ccall(:jl_invalidate_code_instance, Cvoid, (Any, UInt), edge, new_max_world)
                elseif isa(edge, Core.Binding)
                    isdefined(edge, :partitions) || continue
                    latest_bpart = edge.partitions
                    latest_bpart.max_world == typemax(UInt) || continue
                    is_some_imported(binding_kind(latest_bpart)) || continue
                    partition_restriction(latest_bpart) === b || continue
                    invalidate_code_for_globalref!(edge, latest_bpart, latest_bpart, new_max_world)
                else
                    invalidate_method_for_globalref!(gr, edge::Method, invalidated_bpart, new_max_world)
                end
            end
        end
    end

    if need_to_invalidate_code || need_to_invalidate_export
        # This binding was exported - we need to check all modules that `using` us to see if they
        # have a binding that is affected by this change.
        usings_backedges = ccall(:jl_get_module_usings_backedges, Any, (Any,), gr.mod)
        if usings_backedges !== nothing
            for user::Module in usings_backedges::Vector{Any}
                user_binding = ccall(:jl_get_module_binding_or_nothing, Any, (Any, Any), user, gr.name)
                user_binding === nothing && continue
                isdefined(user_binding, :partitions) || continue
                latest_bpart = user_binding.partitions
                latest_bpart.max_world == typemax(UInt) || continue
                binding_kind(latest_bpart) in (PARTITION_KIND_IMPLICIT, PARTITION_KIND_FAILED, PARTITION_KIND_GUARD) || continue
                new_bpart = need_to_invalidate_export ?
                    ccall(:jl_maybe_reresolve_implicit, Any, (Any, Any, Csize_t), user_binding, latest_bpart, new_max_world) :
                    latest_bpart
                if need_to_invalidate_code || new_bpart !== latest_bpart
                    invalidate_code_for_globalref!(convert(Core.Binding, user_binding), latest_bpart, new_bpart, new_max_world)
                end
            end
        end
    end
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

function scan_new_method!(methods_with_invalidated_source::IdSet{Method}, method::Method, image_backedges_only::Bool)
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
            push!(methods_with_invalidated_source, method)
        end
        maybe_add_binding_backedge!(b, method)
    end
end

function scan_new_methods(extext_methods::Vector{Any}, internal_methods::Vector{Any}, image_backedges_only::Bool)
    methods_with_invalidated_source = IdSet{Method}()
    for method in internal_methods
        if isa(method, Method)
           scan_new_method!(methods_with_invalidated_source, method, image_backedges_only)
        end
    end
    for tme::Core.TypeMapEntry in extext_methods
        scan_new_method!(methods_with_invalidated_source, tme.func::Method, image_backedges_only)
    end
    return methods_with_invalidated_source
end
