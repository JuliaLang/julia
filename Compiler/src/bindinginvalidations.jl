# This file is a part of Julia. License is MIT: https://julialang.org/license

using ..Compiler: _uncompressed_ir, specializations, get_ci_mi, convert, generating_output, has_image_globalref,
    PARTITION_MASK_KIND, PARTITION_KIND_GUARD, PARTITION_FLAG_EXPORTED, PARTITION_FLAG_DEPRECATED,
    BINDING_FLAG_ANY_IMPLICIT_EDGES, binding_kind, partition_restriction, is_some_imported,
    is_some_binding_imported, is_some_implicit, SizeUnknown, maybe_add_binding_backedge!, walk_binding_partition, binding_access_key, userefs,
    MaybeCompressed
using .Core: SimpleVector, CodeInfo

function foreachgr(visit, src::CodeInfo)
    stmts = src.code
    for i = 1:length(stmts)
        stmt = stmts[i]
        isa(stmt, GlobalRef) && visit(stmt)
        for ur in userefs(stmt)
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
        for ur in userefs(stmt)
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
    if isdefined(method, :source) && isa(method.source, MaybeCompressed)
        src = _uncompressed_ir(method)
        invalidate_all = should_invalidate_code_for_globalref(gr, src)
    end
    if invalidate_all && !generating_output()
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

    (_, ileaf) = walk_binding_partition(b, invalidated_bpart, new_max_world, false)
    (_, nleaf) = walk_binding_partition(b, new_bpart, new_max_world+1, false)

    # `binding_access_key` captures everything inference/codegen accesses about a binding partition.
    # If this information does not change, we do not need to invalidate any code that inference created
    # because we know that the result will not change.
    need_to_invalidate_code = binding_access_key(ileaf) !== binding_access_key(nleaf)

    need_to_invalidate_export = export_affecting_partition_flags(invalidated_bpart) !==
                                export_affecting_partition_flags(new_bpart)

    invalidated_any = false
    queued_bindings = Tuple{Core.Binding, Core.BindingPartition, Core.BindingPartition}[]    # defer handling these to keep the logging coherent
    if need_to_invalidate_code
        scan_pending_images!(b)
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
                # A partition starting past `new_max_world` was created during this invalidation:
                # either this binding was already re-resolved (it reaches `b` through more than one
                # `using`) and queued then with its partitions on both sides of the change, or a
                # resolution materialized it just now and nothing compiled depends on it yet. Either
                # way there is nothing left to do here, and `latest_bpart` is not the old partition.
                latest_bpart.min_world > new_max_world && continue
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

# Reconstruct, for a method whose source this process has not scanned yet, the invalidation
# that a binding change would have triggered had this code been present when it happened.
# Not every access records an edge (one inference left on the runtime path does not), so this
# source scan is what covers them, and it must ask the same question the edge check asks:
# `binding_changed_since_require_world`, not just "was this binding repartitioned".
function scan_new_method!(method::Method, world::UInt, image_backedges_only::Bool)
    isdefined(method, :source) || return
    isa(method.source, MaybeCompressed) || return
    if image_backedges_only && !has_image_globalref(method)
        return
    end
    _scan_method_source!(method, world, image_backedges_only)
end

# Methods whose image binding backedges are not installed yet, collected while loading
# `image`. Their source can only name bindings of the images `image` was built against, and
# an image is only deferred while no binding of those has been replaced
# (`image_deps_unreplaced`), so nothing reads these backedges until one is:
# `scan_pending_images!` installs them then.
mutable struct PendingImageScan
    const image::UInt # linkage blob
    const world::UInt
    const methods::Vector{Method}
    next::Union{PendingImageScan,Nothing}
end
mutable struct PendingImageScans
    @atomic head::Union{PendingImageScan,Nothing}
end
const PENDING_IMAGE_SCANS = PendingImageScans(nothing)

image_depends_on(image::UInt, blob::UInt) =
    ccall(:jl_linkage_blob_dep_world, Csize_t, (Csize_t, Csize_t), image, blob) != 0

# Every image was built against `Main`, whose bindings change all the time, so source that
# names it (as its own module or through a root) is scanned at load.
names_main(method::Method) = method.module === Core.Main || !isa(method.source, String) ||
    ccall(:jl_method_has_root, Cint, (Any, Any), method, Core.Main) != 0

# Whether the image binding backedges of `method` can wait for `scan_pending_images!`.
# Those of a method that names `Main` are installed here.
function defers_image_scan(method::Method, world::UInt)
    isdefined(method, :source) || return false
    isa(method.source, MaybeCompressed) || return false
    has_image_globalref(method) || return false
    names_main(method) || return true
    _scan_method_source!(method, world, true)
    return false
end

# Leave the image binding backedges of what `image` loaded to `scan_pending_images!`: those
# of its methods, and of the methods of earlier images that it has code for. They are
# published before any of that code is validated and before the methods are marked as
# scanned, so that a replacement from here on finds them. If one came before that, they are
# scanned in full instead.
function defer_image_scans!(internal_methods::Vector{Any}, image::UInt, world::UInt)
    deferred = Method[]
    for item in internal_methods
        if item isa Method && defers_image_scan(item, world)
            push!(deferred, item)
        end
    end
    earlier = nothing
    for item in internal_methods
        item isa CodeInstance || continue
        method = get_ci_mi(item).def
        method isa Method || continue
        (method.did_scan_source & 0x1) == 0x0 || continue
        external_blob_index(method) == image && continue
        defers_image_scan(method, world) || continue
        earlier === nothing && (earlier = IdSet{Method}())
        method in earlier && continue
        push!(earlier, method)
        push!(deferred, method)
    end
    isempty(deferred) && return
    push_pending_image_scan!(PendingImageScan(image, world, deferred, nothing))
    for method in deferred
        @atomic method.did_scan_source |= 0x1
    end
    ccall(:jl_linkage_blob_deps_replaced_locked, Cint, (Csize_t,), image) == 0 && return
    world = get_world_counter()
    for method in deferred
        _scan_method_source!(method, world, false)
    end
    scan_new_methods!(internal_methods, world, false)
    nothing
end

function push_pending_image_scan!(pending::PendingImageScan)
    old = @atomic PENDING_IMAGE_SCANS.head
    while true
        pending.next = old
        old, ok = @atomicreplace PENDING_IMAGE_SCANS.head old => pending
        ok && break
    end
    nothing
end

# `b` is changing: install the deferred backedges of the images that can name it before they
# are read. Only `invalidate_code_for_globalref!` calls this, under the `world_counter_lock`,
# so calls never overlap. A load racing with the change may push afterwards; the next change
# picks it up.
function scan_pending_images!(b::Core.Binding)
    (@atomic PENDING_IMAGE_SCANS.head) === nothing && return
    mod = b.globalref.mod
    mod === Core.Main && return
    blob = external_blob_index(mod)
    blob == typemax(UInt) && return # no image was built against this module
    pending = @atomicswap PENDING_IMAGE_SCANS.head = nothing
    unrelated = nothing
    while pending !== nothing
        next = pending.next
        if image_depends_on(pending.image, blob)
            for method in pending.methods
                _scan_method_source!(method, pending.world, true)
            end
        else
            pending.next = unrelated
            unrelated = pending
        end
        pending = next
    end
    while unrelated !== nothing
        next = unrelated.next
        push_pending_image_scan!(unrelated)
        unrelated = next
    end
    nothing
end

function _scan_method_source!(method::Method, world::UInt, image_backedges_only::Bool)
    src = _uncompressed_ir(method)
    foreachgr(src) do gr::GlobalRef
        b = convert(Core.Binding, gr)
        if binding_changed_since_require_world(b, world)
            # TODO: We could turn this into an additional if condition. For now, use it as a reasonably cheap
            # additional consistency check
            @assert !image_backedges_only
            @atomic method.did_scan_source |= 0x4
        end
        maybe_add_binding_backedge!(b, method)
    end
    @atomic method.did_scan_source |= 0x1
end

function scan_new_methods!(internal_methods::Vector{Any}, world::UInt, image_backedges_only::Bool)
    if image_backedges_only && generating_output(true)
        # Replacing image bindings is forbidden during incremental precompilation - skip backedge insertion
        return
    end
    for method in internal_methods
        if isa(method, Method)
            scan_new_method!(method, world, image_backedges_only)
        end
    end
end
