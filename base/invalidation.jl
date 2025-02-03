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

function invalidate_code_for_globalref!(gr::GlobalRef, invalidated_bpart::Core.BindingPartition, new_max_world::UInt)
    b = convert(Core.Binding, gr)
    try
        valid_in_valuepos = false
        foreach_module_mtable(gr.mod, new_max_world) do mt::Core.MethodTable
            for method in MethodList(mt)
                invalidate_method_for_globalref!(gr, method, invalidated_bpart, new_max_world)
            end
            return true
        end
        b = convert(Core.Binding, gr)
        if isdefined(b, :backedges)
            for edge in b.backedges
                if isa(edge, CodeInstance)
                    ccall(:jl_invalidate_code_instance, Cvoid, (Any, UInt), edge, new_max_world)
                else
                    invalidate_method_for_globalref!(gr, edge::Method, invalidated_bpart, new_max_world)
                end
            end
        end
    catch err
        bt = catch_backtrace()
        invokelatest(Base.println, "Internal Error during invalidation:")
        invokelatest(Base.display_error, err, bt)
    end
end

gr_needs_backedge_in_module(gr::GlobalRef, mod::Module) = gr.mod !== mod

# N.B.: This needs to match jl_maybe_add_binding_backedge
function maybe_add_binding_backedge!(b::Core.Binding, edge::Union{Method, CodeInstance})
    method = isa(edge, Method) ? edge : edge.def.def::Method
    gr_needs_backedge_in_module(b.globalref, method.module) || return
    if !isdefined(b, :backedges)
        b.backedges = Any[]
    end
    !isempty(b.backedges) && b.backedges[end] === edge && return
    push!(b.backedges, edge)
end

function binding_was_invalidated(b::Core.Binding)
    # At least one partition is required for invalidation
    !isdefined(b, :partitions) && return false
    b.partitions.min_world > unsafe_load(cglobal(:jl_require_world, UInt))
end

function scan_new_method!(methods_with_invalidated_source::IdSet{Method}, method::Method)
    isdefined(method, :source) || return
    src = _uncompressed_ir(method)
    mod = method.module
    foreachgr(src) do gr::GlobalRef
        b = convert(Core.Binding, gr)
        binding_was_invalidated(b) && push!(methods_with_invalidated_source, method)
        maybe_add_binding_backedge!(b, method)
    end
end

function scan_new_methods(extext_methods::Vector{Any}, internal_methods::Vector{Any})
    methods_with_invalidated_source = IdSet{Method}()
    for method in internal_methods
        if isa(method, Method)
           scan_new_method!(methods_with_invalidated_source, method)
        end
    end
    for tme::Core.TypeMapEntry in extext_methods
        scan_new_method!(methods_with_invalidated_source, tme.func::Method)
    end
    return methods_with_invalidated_source
end
