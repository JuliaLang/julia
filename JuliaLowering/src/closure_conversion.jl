struct ClosureInfo{Attrs}
    # Global name of the type of the closure
    type_name::SyntaxTree{Attrs}
    # Names of fields for use with getfield, in order
    field_names::SyntaxList{Attrs, Vector{NodeId}}
    # Map from the original BindingId of closed-over vars to the index of the
    # associated field in the closure type.
    field_inds::Dict{IdTag,Int}
end

struct ClosureConversionCtx{Attrs} <: AbstractLoweringContext
    graph::SyntaxGraph{Attrs}
    bindings::Bindings
    mod::Module
    closure_bindings::Dict{IdTag,ClosureBindings}
    capture_rewriting::Union{Nothing,ClosureInfo{Attrs},
                             SyntaxList{Attrs, Vector{NodeId}}}
    lambda_bindings::LambdaBindings
    # True if we're in a section of code which preserves top-level sequencing
    # such that closure types can be emitted inline with other code.
    is_toplevel_seq_point::Bool
    # True if this expression should not have toplevel effects, namely, it
    # should not declare the globals it references.  This allows generated
    # functions to refer to globals that have already been declared, without
    # triggering the "function body AST not pure" error.
    toplevel_pure::Bool
    toplevel_stmts::SyntaxList{Attrs, Vector{NodeId}}
    closure_infos::Dict{IdTag,ClosureInfo{Attrs}}
    # Occurrence nodes of merged-capture variables classified POST-creation
    # by analyze_merged_captures! (home accesses that compile to field
    # operations on the closure instance; all other home occurrences use the
    # local slot).
    merged_post::Set{NodeId}
end

function current_lambda_bindings(ctx::ClosureConversionCtx)
    ctx.lambda_bindings
end

# Access captured variable from inside a closure
function captured_var_access(ctx, ex)
    cap_rewrite = ctx.capture_rewriting
    if cap_rewrite isa ClosureInfo
        field_sym = cap_rewrite.field_names[cap_rewrite.field_inds[ex.var_id]]
        @ast ctx ex [K"call"
            "getfield"::K"core"
            binding_ex(ctx, current_lambda_bindings(ctx).self)
            field_sym
        ]
    else
        interpolations = cap_rewrite
        @jl_assert !isnothing(cap_rewrite) ex
        if isempty(interpolations) || !is_same_identifier_like(interpolations[end], ex)
            push!(interpolations, ex)
        end
        @ast ctx ex [K"captured_local" length(interpolations)::K"Integer"]
    end
end

# Guarded field read: `getfield(container, fname)` with the #20016 undef
# guard. Used for `Core.Box` `.contents` reads and for merged mutable fields
# of the closure itself (where `fname` is the variable's field).
function get_field_contents(ctx::ClosureConversionCtx, var, container_ex, fname_ex)
    binfo = get_binding(ctx, var.var_id)
    undef_var = new_local_binding(ctx, var, binfo.name;
                                  is_used_undef=true)
    @ast ctx var [K"block"
        container := container_ex
        # Lower in an UndefVar check to a similarly named variable
        # (ref #20016) so that closure lowering container introduction
        # doesn't impact the error message and the compiler is expected
        # to fold away the extraneous null check
        #
        # TODO: Ideally the runtime would rely on provenance info for
        # this error and we can remove the isdefined check.
        [K"if" [K"call"
                "isdefined"::K"core"
                container
                fname_ex
            ]
            ::K"TOMBSTONE"
            [K"block"
                 [K"newvar" undef_var]
                 undef_var
            ]
        ]
        [K"call"
            "getfield"::K"core"
            container
            fname_ex
        ]
    ]
end

function get_box_contents(ctx::ClosureConversionCtx, var, box_ex)
    get_field_contents(ctx, var, box_ex,
                       @ast ctx var "contents"::K"Symbol")
end

# Convert `ex` to `type` by calling `convert(type, ex)` when necessary.
#
# Used for converting the right hand side of an assignment to a typed local or
# global and for converting the return value of a function call to the declared
# return type.
function convert_for_type_decl(ctx, srcref, ex, type, do_typeassert)
    # Use a slot to permit union-splitting this in inference
    tmp = new_local_binding(ctx, srcref, "tmp", is_always_defined=true)

    @ast ctx srcref [K"block"
        type_tmp := type
        # [K"=" type_ssa renumber_assigned_ssavalues(type)]
        [K"=" tmp ex]
        [K"if"
            [K"call" "isa"::K"core" tmp type_tmp]
            (::K"nothing")
            [K"="
                tmp
                if do_typeassert
                    [K"call"
                        "typeassert"::K"core"
                        [K"call" "convert"::K"top" type_tmp tmp]
                        type_tmp
                    ]
                else
                    [K"call" "convert"::K"top" type_tmp tmp]
                end
            ]
        ]
        tmp
    ]
end

# TODO: Avoid producing redundant calls to declare_global
function make_globaldecl(ctx, src_ex, mod, name, strong=false, type=nothing)
    decl = @ast ctx src_ex [K"block"
        [K"call"
            "declare_global"::K"core"
            mod::K"Value" name::K"Symbol" strong::K"Bool"
            type
        ]
        (::K"latestworld")
        (::K"nothing")
    ]
    ctx.toplevel_pure && return newleaf(ctx, decl, K"TOMBSTONE")
    if !ctx.is_toplevel_seq_point
        push!(ctx.toplevel_stmts, decl)
        newleaf(ctx, decl, K"TOMBSTONE")
    else
        return decl
    end
end

function convert_global_assignment(ctx, ex, var, rhs0)
    binfo = get_binding(ctx, var)
    @jl_assert binfo.kind == :global ex var
    stmts = SyntaxList(ctx)
    decl = make_globaldecl(ctx, ex, binfo.mod, binfo.name, true)
    if kind(decl) !== K"TOMBSTONE"
        push!(stmts, decl)
    end
    rhs1 = if is_simple_atom(ctx, rhs0)
        rhs0
    else
        tmp = ssavar(ctx, rhs0)
        push!(stmts, @ast ctx rhs0 [K"=" tmp rhs0])
        tmp
    end
    rhs = if binfo.is_const && isnothing(binfo.type)
        # const global assignments without a type declaration don't need us to
        # deal with the binding type at all.
        rhs1
    else
        type_var = ssavar(ctx, ex, "binding_type")
        push!(stmts, @ast ctx ex [K"="
            type_var
            [K"call"
                "get_binding_type"::K"core"
                binfo.mod::K"Value"
                binfo.name::K"Symbol"
            ]
        ])
        do_typeassert = false # Global assignment type checking is done by the runtime
        convert_for_type_decl(ctx, ex, rhs1, type_var, do_typeassert)
    end
    push!(stmts, @ast ctx ex [K"=" var rhs])
    @ast ctx ex [K"block"
        stmts...
        rhs1
    ]
end

# Convert assignment to a closed variable to a `setfield!` call and generate
# `convert` calls for variables with declared types.
#
# When doing this, the original value needs to be preserved, to ensure the
# expression `a=b` always returns exactly `b`.
function convert_assignment(ctx, ex)
    var = ex[1]
    rhs0 = _convert_closures(ctx, ex[2])
    if kind(var) == K"Placeholder"
        return @ast ctx ex [K"=" var rhs0]
    end
    @jl_assert kind(var) == K"BindingId" ex
    binfo = get_binding(ctx, var)
    if binfo.kind == :global
        convert_global_assignment(ctx, ex, var, rhs0)
    else
        @jl_assert binfo.kind == :local || binfo.kind == :argument ex
        boxed = is_boxed(binfo)
        mparts = is_merged(binfo) ? merged_field_parts(ctx, var, binfo) : nothing
        if isnothing(binfo.type) && !boxed && mparts === nothing
            @ast ctx ex [K"=" var rhs0]
        else
            # Typed local and/or a store into the shared container
            tmp_rhs0 = ssavar(ctx, rhs0)
            rhs = isnothing(binfo.type) ? tmp_rhs0 :
                convert_for_type_decl(
                    ctx, ex, tmp_rhs0,
                    _convert_closures(ctx, binding_type_ex(ctx, binfo)),
                    true)
            assignment = if boxed
                @ast ctx ex [K"call"
                    "setfield!"::K"core"
                    is_self_captured(ctx, var) ? captured_var_access(ctx, var) : var
                    "contents"::K"Symbol"
                    rhs
                ]
            elseif mparts !== nothing
                container, fsym = mparts
                @ast ctx ex [K"call"
                    "setfield!"::K"core"
                    container
                    fsym
                    rhs
                ]
            else
                @ast ctx ex [K"=" var rhs]
            end
            @ast ctx ex [K"block"
                [K"=" tmp_rhs0 rhs0]
                assignment
                tmp_rhs0
            ]
        end
    end
end

# Compute fields for a closure type, one field for each captured variable.
function closure_type_fields(ctx, srcref, closure_id, closure_binds, is_opaque)
    capture_ids = Set{IdTag}()
    for lambda_bindings in closure_binds.lambdas
        for (id, is_capt) in lambda_bindings.locals_capt
            is_capt && push!(capture_ids, id)
        end
    end
    # sort here to avoid depending on undefined Set iteration order
    capture_ids = sort!(collect(capture_ids))

    field_syms = SyntaxList(ctx)
    if is_opaque
        field_orig_bindings = capture_ids
        # For opaque closures we don't try to generate sensible names for the
        # fields as there's no closure type to generate.
        for i in eachindex(field_orig_bindings)
            push!(field_syms, @ast ctx srcref i::K"Integer")
        end
    else
        field_names = Dict{String,IdTag}()
        for id in capture_ids
            binfo = get_binding(ctx, id)
            # We name each field of the closure after the variable which was closed
            # over, for clarity. Adding a suffix can be necessary when collisions
            # occur due to macro expansion and generated bindings
            name0 = binfo.name
            name = name0
            i = 1
            while haskey(field_names, name)
                name = "$name0#$i"
                i += 1
            end
            field_names[name] = id
        end
        field_orig_bindings = Vector{IdTag}()
        for (name,id) in sort!(collect(field_names))
            push!(field_syms, @ast ctx srcref name::K"Symbol")
            push!(field_orig_bindings, id)
        end
    end
    # entries: CAPTURE_VALUE (value capture, type-parameterized const field),
    # CAPTURE_BOX (`Core.Box` shared capture, const field), CAPTURE_MUT /
    # CAPTURE_MUT_UNDEF (capture merged into this closure: untyped mutable
    # field)
    field_kinds = Vector{Int}()
    for id in field_orig_bindings
        binfo = get_binding(ctx, id)
        fk = if is_merged(binfo)
            # merging requires a unique capturing closure: it is this one
            @jl_assert binfo.merged_into == closure_id srcref
            binfo.merged_undef ? CAPTURE_MUT_UNDEF : CAPTURE_MUT
        elseif is_boxed(binfo)
            CAPTURE_BOX
        else
            CAPTURE_VALUE
        end
        push!(field_kinds, fk)
    end
    # Maybe-undef merged fields must be last: `new` initializes a field
    # prefix, and these are the only fields creation may leave uninitialized.
    if CAPTURE_MUT_UNDEF in field_kinds
        perm = sortperm(collect(zip(field_kinds .== CAPTURE_MUT_UNDEF,
                                    eachindex(field_kinds))))
        reordered_syms = SyntaxList(ctx)
        for i in perm
            push!(reordered_syms, field_syms[i])
        end
        field_syms = reordered_syms
        field_orig_bindings = field_orig_bindings[perm]
        field_kinds = field_kinds[perm]
    end
    field_inds = Dict{IdTag,Int}()
    for (i,id) in enumerate(field_orig_bindings)
        field_inds[id] = i
    end

    return field_syms, field_orig_bindings, field_inds, field_kinds
end

# Return a thunk which creates a new type for a closure with `field_syms` named
# fields. The new type will be named `name_str` which must be an unassigned
# name in the module.
function type_for_closure(ctx::ClosureConversionCtx, srcref, name_str, field_syms, field_kinds)
    # New closure types always belong to the module we're expanding into - they
    # need to be serialized there during precompile.
    mod = ctx.mod
    type_binding = new_global_binding(ctx, srcref, name_str, mod)
    kindflags = SyntaxList(ctx)
    for f in field_kinds
        push!(kindflags, @ast ctx srcref f::K"Integer")
    end
    type_ex = @ast ctx srcref [K"call"
        #"_call_latest"::K"core"
        eval_closure_type::K"Value"
        ctx.mod::K"Value"
        name_str::K"Symbol"
        [K"call" "svec"::K"core" field_syms...]
        [K"call" "svec"::K"core" kindflags...]
    ]
    type_ex, type_binding
end

# No box needed for:
# - non-captured vars
# - static params (can't be reassigned)
# - any local our optimizations have determined to be unboxed
# - merged captures (the shared location is a mutable FIELD of the unique
#   capturing closure, not a separate Core.Box)
function is_boxed(binfo::BindingInfo)
    binfo.kind === :static_parameter && return false
    binfo.unboxed && return false
    binfo.merged_into != 0 && return false
    binfo.kind === :argument && !binfo.is_assigned && return false
    return binfo.is_captured
end

function is_boxed(ctx, x)
    is_boxed(get_binding(ctx, x))
end

# Is a mutably-captured variable whose container is merged into its unique
# capturing closure as an untyped mutable field (analyze_merged_captures!).
is_merged(binfo::BindingInfo) = binfo.merged_into != 0
is_merged(ctx, x) = is_merged(get_binding(ctx, x))

"""
Locate the container of a merged-capture variable at the current use site:
returns `(container_ex, field_sym_ex)` when the access must be a field
operation on the closure instance — inside the capturing closure itself
(through `#self#`) or in the home frame after the creation (through the
closure's binding; the creation statement structurally dominates every such
access) — or `nothing` when the access precedes the creation and uses the
plain local slot.
"""
function merged_field_parts(ctx::ClosureConversionCtx, var, binfo::BindingInfo)
    if is_self_captured(ctx, var)
        cap_rewrite = ctx.capture_rewriting
        @jl_assert cap_rewrite isa ClosureInfo var
        fsym = cap_rewrite.field_names[cap_rewrite.field_inds[var.var_id]]
        (binding_ex(ctx, current_lambda_bindings(ctx).self), fsym)
    elseif getfield(var, :_id) in ctx.merged_post
        ci = get(ctx.closure_infos, binfo.merged_into, nothing)
        @jl_assert(ci !== nothing,
                   (var, "post-creation access converted before its closure's creation"))
        (binding_ex(ctx, binfo.merged_into), ci.field_names[ci.field_inds[var.var_id]])
    else
        nothing
    end
end

# Is captured in the closure's `self` argument
function is_self_captured(ctx, x)
    get(ctx.lambda_bindings.locals_capt, _binding_id(x), false)
end

# Map the children of `ex` through _convert_closures, lifting any toplevel
# closure definition statements to occur before the other content of `ex`.
function map_cl_convert(ctx::ClosureConversionCtx, ex, toplevel_preserving)
    if ctx.is_toplevel_seq_point && !toplevel_preserving
        toplevel_stmts = SyntaxList(ctx)
        ctx2 = ClosureConversionCtx(ctx.graph, ctx.bindings, ctx.mod,
                                    ctx.closure_bindings, ctx.capture_rewriting, ctx.lambda_bindings,
                                    false, ctx.toplevel_pure, toplevel_stmts, ctx.closure_infos,
                                    ctx.merged_post)
        res = mapchildren(e->_convert_closures(ctx2, e), ctx2, ex)
        if isempty(toplevel_stmts)
            res
        else
            @ast ctx ex [K"block"
                toplevel_stmts...
                res
            ]
        end
    else
        mapchildren(e->_convert_closures(ctx, e), ctx, ex)
    end
end

function _convert_closures(ctx::ClosureConversionCtx, ex)
    k = kind(ex)
    if k == K"BindingId"
        binfo = get_binding(ctx, ex)
        if is_merged(binfo)
            parts = merged_field_parts(ctx, ex, binfo)
            if parts === nothing
                ex     # pre-creation home access: the plain local slot
            else
                container, fsym = parts
                if binfo.is_always_defined
                    @ast ctx ex [K"call" "getfield"::K"core" container fsym]
                else
                    get_field_contents(ctx, ex, container, fsym)
                end
            end
        else
            access = is_self_captured(ctx, ex) ? captured_var_access(ctx, ex) : ex
            if is_boxed(ctx, ex)
                get_box_contents(ctx, ex, access)
            else
                access
            end
        end
    elseif is_leaf(ex) || k == K"inert" || k == K"inert_syntaxtree" || k == K"static_eval"
        ex
    elseif k == K"="
        convert_assignment(ctx, ex)
    elseif k == K"isdefined"
        # Convert isdefined expr to function for closure converted variables
        var = ex[1]
        binfo = get_binding(ctx, var)
        if is_merged(binfo)
            parts = merged_field_parts(ctx, var, binfo)
            if parts !== nothing
                container, fsym = parts
                @ast ctx ex [K"call"
                    "isdefined"::K"core"
                    container
                    fsym
                ]
            elseif binfo.is_always_defined
                @ast ctx ex true::K"Bool"
            else
                ex   # pre-creation: the local slot carries definedness
            end
        elseif is_boxed(binfo)
            access = is_self_captured(ctx, var) ? captured_var_access(ctx, var) : var
            @ast ctx ex [K"call"
                "isdefined"::K"core"
                access
                "contents"::K"Symbol"
            ]
        elseif binfo.is_always_defined || is_self_captured(ctx, var)
            # Captured but unboxed vars are always defined
            @ast ctx ex true::K"Bool"
        elseif binfo.kind == :global
            # Normal isdefined won't work for globals (#56985)
            @ast ctx ex [K"call"
                "isdefinedglobal"::K"core"
                ctx.mod::K"Value"
                binfo.name::K"Symbol"
                false::K"Bool"]
        else
            ex
        end
    elseif k == K"decl"
        @jl_assert kind(ex[1]) == K"BindingId" ex
        binfo = get_binding(ctx, ex[1])
        if binfo.kind == :global
            # flisp has this, but our K"assert" handling is in a previous pass
            # [K"assert" "toplevel_only"::K"Symbol" [K"inert_syntaxtree" ex]]
            make_globaldecl(ctx, ex, binfo.mod, binfo.name, true, _convert_closures(ctx, ex[2]))
        else
            newleaf(ctx, ex, K"TOMBSTONE")
        end
    elseif k == K"global"
        # Leftover `global` forms become weak globals.
        mod, name = if kind(ex[1]) == K"BindingId"
            binfo = get_binding(ctx, ex[1])
            @jl_assert binfo.kind == :global ex
            binfo.mod, binfo.name
        else
            # See note about using eval on Expr(:global/:const, GlobalRef(...))
            @jl_assert ex[1].value isa GlobalRef ex[1]
            ex[1].value.mod, String(ex[1].value.name)
        end
        @ast ctx ex [K"unused_only" make_globaldecl(ctx, ex, mod, name, false)]
    elseif k == K"local"
        var = ex[1]
        binfo = get_binding(ctx, var)
        if is_boxed(binfo)
            @ast ctx ex [K"=" var [K"call" "Box"::K"core"]]
        elseif !binfo.is_always_defined
            @ast ctx ex [K"newvar" var]
        else
            newleaf(ctx, ex, K"TOMBSTONE")
        end
    elseif k == K"lambda"
        closure_convert_lambda(ctx, ex)
    elseif k == K"function_decl"
        func_name = ex[1]
        @jl_assert kind(func_name) == K"BindingId" ex
        func_name_id = func_name.var_id
        if haskey(ctx.closure_bindings, func_name_id)
            closure_info = get(ctx.closure_infos, func_name_id, nothing)
            needs_def = isnothing(closure_info)
            if needs_def
                closure_binds = ctx.closure_bindings[func_name_id]
                field_syms, field_orig_bindings, field_inds, field_kinds =
                    closure_type_fields(ctx, ex, func_name_id, closure_binds, false)
                name_str = reserve_module_binding_i(
                    ctx.mod,
                    string("#", join(closure_binds.name_stack, "#"), "##"))
                closure_type_def, closure_type_ =
                    type_for_closure(ctx, ex, name_str, field_syms, field_kinds)
                if !ctx.is_toplevel_seq_point
                    push!(ctx.toplevel_stmts, closure_type_def)
                    push!(ctx.toplevel_stmts, @ast ctx ex (::K"latestworld_if_toplevel"))
                    closure_type_def = nothing
                end
                closure_info = ClosureInfo(closure_type_, field_syms, field_inds)
                ctx.closure_infos[func_name_id] = closure_info
                type_params = SyntaxList(ctx)
                init_closure_args = SyntaxList(ctx)
                undef_init_ids = Vector{IdTag}()
                for (id, fkind) in zip(field_orig_bindings, field_kinds)
                    field_val = binding_ex(ctx, id)
                    if is_self_captured(ctx, field_val)
                        # Access from outer closure if necessary but do not
                        # unbox to feed into the inner nested closure.
                        field_val = captured_var_access(ctx, field_val)
                    end
                    if fkind == CAPTURE_MUT_UNDEF
                        # maybe-undef merged field (ordered last): left
                        # uninitialized by `new`; conditionally initialized
                        # from the local below
                        push!(undef_init_ids, id)
                        continue
                    end
                    push!(init_closure_args, field_val)
                    if fkind == CAPTURE_VALUE
                        push!(type_params, @ast ctx ex [K"call"
                              "_typeof_captured_variable"::K"core"
                              field_val])
                    end
                    # CAPTURE_BOX passes the box; CAPTURE_MUT passes the
                    # current value of the local (the creation initializes
                    # the mutable field from it)
                end
                # maybe-undef merged fields: conditionally initialize each
                # from its (possibly undefined) local after `new`
                closure_val = ssavar(ctx, ex, "closure_val")
                undef_inits = SyntaxList(ctx)
                for id in undef_init_ids
                    fv = binding_ex(ctx, id)
                    push!(undef_inits, @ast ctx ex [K"if"
                        [K"isdefined" fv]
                        [K"call"
                            "setfield!"::K"core"
                            closure_val
                            field_syms[field_inds[id]]
                            fv
                        ]
                        ::K"TOMBSTONE"
                    ])
                end
                @ast ctx ex [K"block"
                    closure_type_def
                    (::K"latestworld_if_toplevel")
                    closure_type := if isempty(type_params)
                        closure_type_
                    else
                        [K"call" "apply_type"::K"core" closure_type_ type_params...]
                    end
                    [K"=" closure_val [K"new"
                        closure_type
                        init_closure_args...
                    ]]
                    undef_inits...
                    convert_assignment(ctx, [K"=" func_name closure_val])
                    ::K"TOMBSTONE"
                ]
            else
                @ast ctx ex (::K"TOMBSTONE")
            end
        else
            # Single-arg K"method" has the side effect of creating a global
            # binding for `func_name` if it doesn't exist.
            @ast ctx ex [K"block"
                [K"method" func_name]
                ::K"TOMBSTONE" # <- function_decl should not be used in value position
            ]
        end
    elseif k == K"method" && kind(ex[1]) === K"BindingId" &&
            haskey(ctx.closure_bindings, ex[1].var_id)
        # rm method table argument if it's a closure id, since it's unnecessary
        # and requires the `(= id (new ...))` call to be lifted above the
        # method.  flisp might be messing up overlays when it does this, since
        # it removes all locals, not just closure ids.
        @ast ctx ex [K"method"
            (::K"nothing"(ex[1]))
            _convert_closures(ctx, ex[2])
            _convert_closures(ctx, ex[3])]
    elseif k == K"function_type"
        func_name = ex[1]
        if kind(func_name) == K"BindingId" && get_binding(ctx, func_name).kind === :local
            @jl_assert(haskey(ctx.closure_infos, func_name.var_id),
                       (ex, "function_type of local without known closure type"))
            ctx.closure_infos[func_name.var_id].type_name
        else
            @ast ctx ex [K"call" "TypeEqOf"::K"core" _convert_closures(ctx, func_name)]
        end
    elseif k == K"method_defs"
        name = ex[1]
        is_closure = kind(name) == K"BindingId" && get_binding(ctx, name).kind === :local
        cap_rewrite = is_closure ? ctx.closure_infos[name.var_id] : nothing
        ctx2 = ClosureConversionCtx(ctx.graph, ctx.bindings, ctx.mod,
                                    ctx.closure_bindings, cap_rewrite, ex.lambda_bindings,
                                    ctx.is_toplevel_seq_point, ctx.toplevel_pure, ctx.toplevel_stmts,
                                    ctx.closure_infos, ctx.merged_post)
        body = map_cl_convert(ctx2, ex[2], false)
        if is_closure
            if ctx.is_toplevel_seq_point
                body
            else
                # Move methods out to a top-level sequence point.
                push!(ctx.toplevel_stmts, body)
                @ast ctx ex (::K"TOMBSTONE")
            end
        else
            @ast ctx ex [K"block"
                body
                ::K"TOMBSTONE"
            ]
        end
    elseif k == K"_opaque_closure"
        closure_binds = ctx.closure_bindings[ex[1].var_id]
        field_syms, field_orig_bindings, field_inds, _field_kinds =
            closure_type_fields(ctx, ex, ex[1].var_id, closure_binds, true)

        capture_rewrites = ClosureInfo(ex #=unused=#, field_syms, field_inds)

        ctx2 = ClosureConversionCtx(ctx.graph, ctx.bindings, ctx.mod,
                                    ctx.closure_bindings, capture_rewrites, ctx.lambda_bindings,
                                    false, ctx.toplevel_pure, ctx.toplevel_stmts, ctx.closure_infos,
                                    ctx.merged_post)

        argt = _convert_closures(ctx, ex[2])
        rt_lb = _convert_closures(ctx, ex[3])
        rt_ub = _convert_closures(ctx, ex[4])

        init_closure_args = SyntaxList(ctx)
        for id in field_orig_bindings
            init_arg = binding_ex(ctx, id)
            if is_self_captured(ctx, init_arg)
                init_arg = captured_var_access(ctx, init_arg)
            end
            push!(init_closure_args, init_arg)
        end
        @ast ctx ex [K"new_opaque_closure"
            argt # arg type tuple
            rt_lb # return_lower_bound
            rt_ub # return_upper_bound
            ex[5] # allow_partial
            [K"opaque_closure_method"
                (::K"nothing")
                ex[6] # nargs
                ex[7] # is_va
                ex[8] # functionloc
                closure_convert_lambda(ctx2, ex[9])
            ]
            init_closure_args...
        ]
    else
        # A small number of kinds are toplevel-preserving in terms of closure
        # closure definitions will be lifted out into `toplevel_stmts` if they
        # occur inside `ex`.
        toplevel_seq_preserving = k == K"if" || k == K"elseif" || k == K"block" ||
                              k == K"tryfinally" || k == K"trycatchelse"
        map_cl_convert(ctx, ex, toplevel_seq_preserving)
    end
end

function closure_convert_lambda(ctx, ex)
    @jl_assert kind(ex) == K"lambda" ex
    lambda_bindings = ex.lambda_bindings
    interpolations = nothing
    if isnothing(ctx.capture_rewriting)
        # Global method which may capture locals
        interpolations = SyntaxList(ctx)
        cap_rewrite = interpolations
    else
        cap_rewrite = ctx.capture_rewriting
    end
    ctx2 = ClosureConversionCtx(ctx.graph, ctx.bindings, ctx.mod,
                                ctx.closure_bindings, cap_rewrite, lambda_bindings,
                                ex.is_toplevel_thunk, ctx.toplevel_pure && ex.toplevel_pure,
                                ctx.toplevel_stmts, ctx.closure_infos, ctx.merged_post)
    lambda_children = SyntaxList(ctx)
    args = ex[1]
    push!(lambda_children, args)
    push!(lambda_children, ex[2])

    # Add box initializations for arguments which are captured by an inner lambda
    body_stmts = SyntaxList(ctx)
    for arg in children(args)
        kind(arg) != K"Placeholder" || continue
        if is_boxed(ctx, arg)
            push!(body_stmts, @ast ctx arg [K"="
                arg
                [K"call" "Box"::K"core" arg]
            ])
        end
    end
    # Convert body.
    input_body_stmts = kind(ex[3]) != K"block" ? ex[3:3] : ex[3][1:end]
    for e in input_body_stmts
        push!(body_stmts, _convert_closures(ctx2, e))
    end
    push!(lambda_children, @ast ctx2 ex[3] [K"block" body_stmts...])

    if numchildren(ex) > 3
        # Convert return type
        @jl_assert numchildren(ex) == 4 ex
        push!(lambda_children, _convert_closures(ctx2, ex[4]))
    end

    lam = setattr!(mknode(ex, lambda_children), :lambda_bindings, lambda_bindings)
    if !isnothing(interpolations) && !isempty(interpolations)
        @ast ctx ex [K"call"
            replace_captured_locals!::K"Value"
            lam
            [K"call"
                "svec"::K"core"
                interpolations...
            ]
        ]
    else
        lam
    end
end


"""
Closure conversion and lowering of bindings

This pass does a few things:
* Deal with typed variables (K"decl") and their assignments
* Deal with const and non-const global assignments
* Convert closures into types
* Lower variables captured by closures into boxes, etc, as necessary

Invariants:
* This pass must not introduce new K"Identifier" - only K"BindingId".
* Any new binding IDs must be added to the enclosing lambda locals
"""
@fzone "JL: closures" function convert_closures(
    ctx::VariableAnalysisContext, ex::SyntaxTree{Attrs}
) where Attrs
    ctx_out = ClosureConversionCtx(ctx.graph, ctx.bindings, ctx.mod,
                                   ctx.closure_bindings, nothing,
                                   ex.lambda_bindings,
                                   false, true, SyntaxList(ctx.graph),
                                   Dict{IdTag,ClosureInfo{Attrs}}(),
                                   ctx.merged_post)
    ex_out = closure_convert_lambda(ctx_out, ex)
    if !isempty(ctx_out.toplevel_stmts)
        throw(LoweringError(first(ctx_out.toplevel_stmts), "Top level code was found outside any top level context. `@generated` functions may not contain closures, including `do` syntax and generators/comprehension"))
    end
    ctx_out, flatten_blocks(ex_out)
end

#-------------------------------------------------------------------------------
# Closure-definition sinking
#
# The one code-motion optimization lowering is allowed ("sinking closure
# definitions, as long as they haven't been used yet" — the envelope rule,
# UnifiedIR/docs/closures.md): closure CREATION is pure, so moving a whole
# creation statement from its declaration position down to just before the
# first statement that can observe it is unobservable — and it turns
# store-after-creation-but-before-first-use into a legal VALUE capture,
# because the capture criterion is evaluated at the creation position.
#
# This runs on the scoped tree BEFORE `analyze_captures_precise!` and before
# EITHER lowering path reads it, which is the soundness story: the capture
# DECISION and the EMITTED creation position cannot disagree, because every
# consumer (the capture-analysis IR, `convert_closures`, and the
# UnifiedBackend region emitter) consumes the same already-sunk statement
# order. There is exactly one implementation of the position rule.
#
# v1 rule (purely structural, conservative):
#   * The unit of motion is a whole statement S of a block B inside a method
#     body. S must be a PURE CREATION STATEMENT (whitelist `_sink_match`):
#     nothing but closure-creation machinery for local bindings —
#     function_decl + method_defs + the instance assignment — whose only
#     runtime effects are the pure creation and stores to the bindings it
#     creates, W(S). (Signature svecs may read globals: the eager path lifts
#     them to definition toplevel, so their in-body position is already
#     meaningless; anything else non-trivial disqualifies S.)
#   * Confinement: every occurrence of every w ∈ W(S) outside S must sit in
#     a LATER statement of the same block B (declaration markers `local w`
#     do not count). Occurrences in earlier statements, enclosing blocks,
#     handlers, or other frames could observe w's definedness or identity on
#     paths the motion changes — refuse.
#   * S sinks past following statements that mention no w ∈ W(S) anywhere in
#     their subtree (nested lambdas included — a capture is a use;
#     `@isdefined w` is a use) and contain no `@label`/`@goto` (a label
#     could let control enter between the old and new position, reaching
#     the use region without the sunk creation). It stops just before the
#     first statement that mentions W(S): the first use. A `function_decl z`
#     for a closure z counts as mentioning every variable z CAPTURES, not
#     just z: instance materialization happens at the decl (`%new(T,
#     captures...)` above) and reads each captured binding there — an
#     implicit read the subtree scan cannot see when z's methods live in
#     separate `method_defs` statements (kwargs closures split this way:
#     sinking the kw-body's decl past the sorter's decl would make the
#     sorter capture an undefined variable).
#   * Same block only, never to the block's last position (block value).
#     Statements that may throw or exit early are fine to sink past: on such
#     a path control leaves B, and by confinement nothing that can observe w
#     is reachable afterwards; a loop re-enters B from the top and re-runs
#     the sunk creation before any use, preserving per-iteration lifetimes.
#   * Method bodies only, never toplevel thunks (a toplevel creation pins
#     closure-type definition and world-age effects to its position).

function sink_closure_definitions!(ctx::VariableAnalysisContext, ex)
    isempty(ctx.closure_bindings) && return nothing
    census = Dict{IdTag,Int}()
    _sink_count!(census, ex)
    _sink_walk!(ctx, ex, census, false)
    return nothing
end

# Occurrence census: every K"BindingId" under `ex`, in the motion sense —
# declaration markers do not count; quoted subtrees cannot reference
# bindings; nested lambda bodies DO count (a capture is a use).
function _sink_count!(census::Dict{IdTag,Int}, ex)
    k = kind(ex)
    if k == K"BindingId"
        census[ex.var_id] = get(census, ex.var_id, 0) + 1
    elseif k == K"local" || k == K"global"
        return nothing
    elseif is_leaf(ex) || k == K"inert" || k == K"inert_syntaxtree" || k == K"quote"
        return nothing
    else
        for c in children(ex)
            _sink_count!(census, c)
        end
    end
    return nothing
end

function _sink_walk!(ctx, ex, census, in_method::Bool)
    k = kind(ex)
    if k == K"lambda"
        inm = !ex.is_toplevel_thunk
        for c in children(ex)
            _sink_walk!(ctx, c, census, inm)
        end
        return nothing
    elseif is_leaf(ex) || k == K"inert" || k == K"inert_syntaxtree" || k == K"quote"
        return nothing
    end
    in_method && k == K"block" && _sink_block!(ctx, ex, census)
    for c in children(ex)
        _sink_walk!(ctx, c, census, in_method)
    end
    return nothing
end

function _sink_block!(ctx, bex, census)
    n = numchildren(bex)
    moved = Set{NodeId}()   # each statement moves at most once: two movable
                            # creations before a common first use would
                            # otherwise leapfrog each other forever
    i = 1
    while i < n
        if getfield(bex[i], :_id) in moved
            i += 1
            continue
        end
        W = _sink_creation_bindings(ctx, bex[i])
        if W === nothing || isempty(W)
            i += 1
            continue
        end
        inS = Dict{IdTag,Int}()
        _sink_count!(inS, bex[i])
        rest = Dict{IdTag,Int}()
        for j in (i+1):n
            _sink_count!(rest, bex[j])
        end
        confined = true
        hasuse = false
        for w in W
            if get(census, w, 0) != get(inS, w, 0) + get(rest, w, 0)
                confined = false
                break
            end
            get(rest, w, 0) > 0 && (hasuse = true)
        end
        if !(confined && hasuse)
            i += 1
            continue
        end
        j = i
        while j + 1 <= n && _sink_past_ok(ctx, bex[j+1], W)
            j += 1
        end
        # hasuse ⇒ the scan stopped at a mentioning statement ⇒ j < n
        if j > i
            push!(moved, getfield(bex[i], :_id))
            _sink_move!(bex, i, j)
            # do not advance: the statement previously at i+1 is now at i
        else
            i += 1
        end
    end
    return nothing
end

"""
Sinkable creation statement? Returns the set of bindings it assigns, or
nothing. The whitelist must have matched at least one actual
`function_decl` — a statement of otherwise-pure material (e.g. a literal
assignment `r = 0`) is not a closure creation and is not this
optimization's to move.
"""
function _sink_creation_bindings(ctx, ex)
    W = Set{IdTag}()
    saw_decl = Ref(false)
    (_sink_match(ctx, ex, W, false, saw_decl) && saw_decl[]) ? W : nothing
end

function _sink_match(ctx, ex, W::Set{IdTag}, insig::Bool, saw_decl::Ref{Bool})::Bool
    k = kind(ex)
    if k == K"block"
        for c in children(ex)
            _sink_match(ctx, c, W, insig, saw_decl) || return false
        end
        return numchildren(ex) > 0
    elseif k == K"nothing" || k == K"TOMBSTONE"
        return true
    elseif k == K"=" && numchildren(ex) == 2
        lhs = ex[1]
        kind(lhs) == K"BindingId" || return false
        b = get_binding(ctx, lhs)
        (b.kind === :local || b.kind === :argument) || return false
        push!(W, lhs.var_id)
        return _sink_match(ctx, ex[2], W, insig, saw_decl)
    elseif k == K"function_decl"
        f = ex[1]
        kind(f) == K"BindingId" || return false
        b = get_binding(ctx, f)
        (b.kind === :local && haskey(ctx.closure_bindings, f.var_id)) || return false
        saw_decl[] = true
        push!(W, f.var_id)
        for i in 2:numchildren(ex)
            _sink_match(ctx, ex[i], W, insig, saw_decl) || return false
        end
        return true
    elseif k == K"method_defs"
        numchildren(ex) >= 2 || return false
        nm = ex[1]
        (kind(nm) == K"BindingId" && get_binding(ctx, nm).kind === :local) || return false
        for i in 2:numchildren(ex)
            _sink_match(ctx, ex[i], W, true, saw_decl) || return false
        end
        return true
    elseif k == K"method"
        # closure method entry [name sig lambda]: the lambda body is deferred
        # code, not part of the statement's runtime effects
        (insig && numchildren(ex) == 3) || return false
        kind(ex[1]) == K"BindingId" || return false
        _sink_match(ctx, ex[2], W, true, saw_decl) || return false
        return kind(ex[3]) == K"lambda"
    elseif k == K"removable" || k == K"unused_only"
        for c in children(ex)
            _sink_match(ctx, c, W, insig, saw_decl) || return false
        end
        return true
    elseif k == K"BindingId"
        ex.var_id in W && return true
        b = get_binding(ctx, ex.var_id)
        return insig && (b.kind === :global || b.kind === :static_parameter)
    elseif k == K"function_type"
        return numchildren(ex) == 1 && kind(ex[1]) == K"BindingId"
    elseif k == K"call"
        insig || return false
        for c in children(ex)
            _sink_match(ctx, c, W, true, saw_decl) || return false
        end
        return true
    elseif k == K"core" || k == K"top" || k == K"globalref" || k == K"Value" ||
           k == K"Symbol" || k == K"SourceLocation" || k == K"inert" ||
           k == K"inert_syntaxtree" || k == K"quote" || is_literal(k)
        return true
    else
        return false
    end
end

# May the creation sink past `ex`? No mention of any w ∈ W anywhere in the
# subtree (nested lambdas included; `local w` markers count as mentions here,
# conservatively), no symbolic label/goto, and no `function_decl` of a
# closure that CAPTURES a w — the decl materializes the instance and reads
# every captured binding right there (see the conversion above), a mention
# that lives outside the decl's subtree when the closure's methods sit in
# separate `method_defs` statements.
function _sink_past_ok(ctx, ex, W::Set{IdTag})
    k = kind(ex)
    if k == K"BindingId"
        return !(ex.var_id in W)
    elseif k == K"symboliclabel" || k == K"symbolicgoto" || k == K"oldsymbolicgoto"
        return false
    elseif k == K"function_decl"
        f = ex[1]
        kind(f) == K"BindingId" || return false
        f.var_id in W && return false
        cb = get(ctx.closure_bindings, f.var_id, nothing)
        if cb !== nothing
            for lam in cb.lambdas, (id, capt) in lam.locals_capt
                capt && id in W && return false
            end
        end
        return true
    elseif is_leaf(ex) || k == K"inert" || k == K"inert_syntaxtree" || k == K"quote"
        return true
    else
        for c in children(ex)
            _sink_past_ok(ctx, c, W) || return false
        end
        return true
    end
end

# Move B[i] to position j (i < j < numchildren): an in-place permutation of
# the block node's child edge list — same children, new order, no new nodes.
function _sink_move!(bex, i::Int, j::Int)
    graph = syntax_graph(bex)
    r = graph.edge_ranges[getfield(bex, :_id)]
    ids = collect(graph.edges[r])
    moved = ids[i]
    deleteat!(ids, i)
    insert!(ids, j, moved)
    for (m, e) in enumerate(r)
        graph.edges[e] = ids[m]
    end
    return nothing
end

#-------------------------------------------------------------------------------
# Part 3: merged captures — closures as mutable structs
#
# The authorized representation ("it is fine to lower closures to mutable
# struct where mutably captured values become mutable fields and everything
# else `const`" — see UnifiedIR/docs/closures.md): a mutably-captured
# variable owned SOLELY by one closure merges its shared container INTO that
# closure as an untyped mutable field. One allocation and one indirection
# instead of two (`Core.Box` + closure). The field is NEVER type-annotated:
# any later assignment in the closure body could read an updated world we
# know nothing about, so the value type is unknowable to lowering.
#
# This is a pure REPRESENTATION change of what remains shared: the capture
# DECISIONS (which variables are shared at all) belong to
# `analyze_captures_precise!` and the sinking pass, both of which run first.
#
# v1 applicability, purely structural (ANY failure ⇒ that variable keeps the
# `Core.Box` container, exactly as before):
#
#   * exactly ONE closure binding g captures x — checked both on the
#     closure-bindings table and on the tree (no occurrence of x under any
#     nested lambda that is not one of g's method lambdas, which also
#     excludes opaque closures and global methods capturing x). Cross-closure
#     sharing keeps Box in v1 (a shared frame struct is future work).
#   * x is not itself a closure binding (recursion / self-capture keeps the
#     Box: the container must exist before the instance).
#   * g is a plain local holding exactly the instance: no declared type, not
#     boxed/merged itself, assigned only by its unique home-level
#     `function_decl` — the creation statement CS. This makes home reads of
#     x through g (`getfield(g, :x)`) reads of THE instance CS created.
#   * every home occurrence of x classifies as PRE or POST against CS by
#     ancestor-path divergence (`_amc_classify`):
#       - PRE (an earlier sibling subtree at the divergence node): executes
#         before the creation on any path that reaches both; keeps using the
#         plain local slot. The capture at creation initializes the field
#         from that slot.
#       - POST (a later sibling subtree at a divergence BLOCK, with every
#         node between it and the `function_decl` "completion-transparent":
#         K"block" (any child), K"=" (the rhs), K"removable"/K"unused_only"):
#         reaching such a position means the creation executed — blocks run
#         children in order, completing any transparent node implies its
#         chained child ran, and there are no labels/gotos (refused below) to
#         enter a block midway. POST accesses compile to field operations on
#         g's instance.
#       - anything else refuses: divergence at or inside the creation group
#         (method_defs signatures), exclusive if-arms on the later side,
#         catch/finally handlers, occurrences under K"decl" type expressions
#         or K"meta", a node classified both ways (lowering may share
#         BindingId nodes).
#   * one activation of x sees at most one execution of CS, and each
#     execution of CS sees a fresh x: the nearest enclosing loop
#     (K"_while"/K"_do_while") of x's K"local" declaration marker must be
#     THE SAME NODE as the nearest enclosing loop of CS (scope resolution
#     emits a marker at scope entry for every local; arguments have none and
#     require CS outside any loop). Without this, a pre-creation read in a
#     later iteration would read the stale local while the previous
#     instance's field holds the current value (or two live instances would
#     need one location).
#   * no symboliclabel/symbolicgoto in the home frame (jumps break the
#     structural order arguments). The symbolicblock/break structured forms
#     are fine.
#
# maybe-undef x (not provably defined at CS) still merges in this path:
# the field is ordered last, `new` leaves it uninitialized, creation runs
# `isdefined(x) && setfield!(cl, :x, x)`, reads keep the #20016 named-
# variable guard so UndefVarError still names x at use time, and
# `@isdefined x` maps to `isdefined(container, :x)`.

# One home-frame occurrence of a candidate variable: the walk path from the
# lambda body root (pairs of (parent node id, child index taken)), the
# occurrence node itself, and whether it is an assignment target.
struct AMCOccurrence
    path::Vector{Tuple{NodeId,Int}}
    node::NodeId
    is_assign::Bool
end

mutable struct AMCScan{Ctx}
    ctx::Ctx
    cand::Set{IdTag}                    # candidate variable ids (this frame)
    clos::Set{IdTag}                    # owner closure ids of the candidates
    occs::Dict{IdTag,Vector{AMCOccurrence}}
    sites::Dict{IdTag,Vector{Vector{Tuple{NodeId,Int}}}}  # closure id -> function_decl paths
    decls::Dict{IdTag,Vector{Vector{Tuple{NodeId,Int}}}}  # var id -> K"local" marker paths
    gassign::Dict{IdTag,Int}            # closure id -> count of `=` assignments
    lambda_binds::Vector{LambdaBindings} # every nested lambda's bindings
    refuse::Set{IdTag}                  # vars refused during the walk
    bad::Bool                           # labels/gotos: refuse everything
end

function analyze_merged_captures!(ctx::VariableAnalysisContext, ex)
    isempty(ctx.closure_bindings) && return nothing
    # how many closure bindings capture each variable, and which one
    ncap = Dict{IdTag,Int}()
    owner = Dict{IdTag,IdTag}()
    for (cid, cb) in ctx.closure_bindings
        seen = Set{IdTag}()
        for lb in cb.lambdas, (id, capt) in lb.locals_capt
            capt && push!(seen, id)
        end
        for id in seen
            ncap[id] = get(ncap, id, 0) + 1
            owner[id] = cid
        end
    end
    isempty(ncap) && return nothing
    _amc_lambda_walk!(ctx, ex, ncap, owner)
    return nothing
end

function _amc_lambda_walk!(ctx, ex, ncap, owner)
    k = kind(ex)
    if k == K"lambda"
        _amc_lambda_walk!(ctx, ex[3], ncap, owner)   # nested frames first
        ex.is_toplevel_thunk || _amc_frame!(ctx, ex, ncap, owner)
    elseif !is_leaf(ex) && !is_quoted(ex)
        for c in children(ex)
            _amc_lambda_walk!(ctx, c, ncap, owner)
        end
    end
    return nothing
end

function _amc_frame!(ctx, lam, ncap, owner)
    lb = lam.lambda_bindings
    cand = Set{IdTag}()
    for (id, capt) in lb.locals_capt
        capt === false || continue                    # native to this frame
        get(ncap, id, 0) == 1 || continue             # exactly one capturer
        haskey(ctx.closure_bindings, id) && continue  # not itself a closure
        binfo = get_binding(ctx, id)
        binfo.kind in (:local, :argument) || continue
        binfo.merged_into == 0 || continue
        is_boxed(binfo) || continue                   # only still-shared vars
        push!(cand, id)
    end
    isempty(cand) && return nothing
    st = AMCScan(ctx, cand, Set{IdTag}(owner[id] for id in cand),
                 Dict{IdTag,Vector{AMCOccurrence}}(),
                 Dict{IdTag,Vector{Vector{Tuple{NodeId,Int}}}}(),
                 Dict{IdTag,Vector{Vector{Tuple{NodeId,Int}}}}(),
                 Dict{IdTag,Int}(), Vector{LambdaBindings}(),
                 Set{IdTag}(), false)
    path = Vector{Tuple{NodeId,Int}}()
    _amc_scan!(st, lam[3], path)
    st.bad && return nothing

    for x in sort!(collect(cand))
        x in st.refuse && continue
        binfo = get_binding(ctx, x)
        g = owner[x]
        gb = get_binding(ctx, g)
        # g must be a plain local holding exactly the instance. Note g is
        # necessarily a candidate of THIS SAME frame walk when boxed/merged
        # (its decl is home here), so no cross-frame ordering hazard exists:
        # a boxed g refuses x right here.
        (gb.kind === :local && !is_boxed(gb) && gb.merged_into == 0 &&
         isnothing(gb.type)) || continue
        get(st.gassign, g, 0) == 0 || continue
        gsites = get(st.sites, g, nothing)
        (gsites !== nothing && length(gsites) == 1) || continue
        cs_path = gsites[1]
        # the tree-level unique-capturer check: every nested lambda that
        # captures x must be one of g's method lambdas
        gscopes = Set{ScopeId}(l.scope_id for l in ctx.closure_bindings[g].lambdas)
        capbad = false
        for lmb in st.lambda_binds
            if get(lmb.locals_capt, x, false) === true && !(lmb.scope_id in gscopes)
                capbad = true
                break
            end
        end
        capbad && continue
        # declaration marker: at most one; must classify PRE; its nearest
        # enclosing loop must be CS's nearest enclosing loop
        dpaths = get(st.decls, x, nothing)
        (dpaths === nothing || length(dpaths) <= 1) || continue
        dpath = (dpaths === nothing || isempty(dpaths)) ? nothing : dpaths[1]
        loop_cs = _amc_nearest_loop(ctx, cs_path)
        loop_decl = dpath === nothing ? 0 : _amc_nearest_loop(ctx, dpath)
        loop_cs == loop_decl || continue
        if dpath !== nothing
            _amc_classify(ctx, dpath, cs_path) === :pre || continue
        end
        # classify every home occurrence
        ok = true
        dom_assign = false
        cls_by_node = Dict{NodeId,Symbol}()
        post_nodes = Vector{NodeId}()
        for occ in get(st.occs, x, AMCOccurrence[])
            cls = _amc_classify(ctx, occ.path, cs_path)
            if cls === :refuse
                ok = false
                break
            end
            prev = get(cls_by_node, occ.node, cls)
            if prev !== cls
                ok = false     # one (shared) node in both regimes
                break
            end
            cls_by_node[occ.node] = cls
            if cls === :post
                push!(post_nodes, occ.node)
            elseif occ.is_assign && _amc_assign_dominates(ctx, occ.path, cs_path)
                dom_assign = true
            end
        end
        ok || continue
        # verdict: arguments and variables with a pre-assignment that
        # dominates the creation are provably defined there (the field is
        # initialized unconditionally by `new`); everything else takes the
        # maybe-undef flavor (uninitialized trailing field + conditional
        # initialization)
        binfo.merged_into = g
        binfo.merged_undef = !(binfo.kind === :argument || dom_assign)
        for n in post_nodes
            push!(ctx.merged_post, n)
        end
    end
    return nothing
end

# Deepest ancestor on `path` that is a loop node (its node id), or 0.
function _amc_nearest_loop(ctx, path::Vector{Tuple{NodeId,Int}})
    for i in length(path):-1:1
        k = kind(SyntaxTree(syntax_graph(ctx), path[i][1]))
        (k == K"_while" || k == K"_do_while") && return Int(path[i][1])
    end
    return 0
end

"""
Classify one occurrence path against the creation (`function_decl`) path:
`:pre`, `:post`, or `:refuse`. See the block comment above for the rules.
"""
function _amc_classify(ctx, opath::Vector{Tuple{NodeId,Int}},
                       cs_path::Vector{Tuple{NodeId,Int}})
    n = length(cs_path)
    d = 0
    for i in 1:min(length(opath), n)
        if opath[i] != cs_path[i]
            d = i
            break
        end
    end
    # d == 0: opath extends (or equals) cs_path — inside the function_decl
    d == 0 && return :refuse
    # divergence at the creation group (the function_decl's parent):
    # signature/removable material of the creation statement
    d == n && return :refuse
    graph = syntax_graph(ctx)
    i_occ = opath[d][2]
    i_cs = cs_path[d][2]
    if i_occ < i_cs
        return :pre
    end
    # POST: divergence node must be a block, and completing each node on the
    # chain below it must imply the creation executed
    kind(SyntaxTree(graph, opath[d][1])) == K"block" || return :refuse
    for l in (d+1):n
        kl = kind(SyntaxTree(graph, cs_path[l][1]))
        if kl == K"block" || kl == K"removable" || kl == K"unused_only"
            # completing these implies every child ran
        elseif kl == K"="
            cs_path[l][2] == 2 || return :refuse   # creation on the rhs
        else
            return :refuse
        end
    end
    return :post
end

"""
Does a PRE-classified assignment occurrence dominate the creation? True when
control reaching the creation implies the assignment executed: the two paths
diverge at a block (an earlier statement of it holds the assignment), and
every node between that block and the assignment's `=` node is
completion-transparent — so completing the earlier statement implies the
store happened. Used to prove the variable defined at creation (the merged
field can then be initialized unconditionally by `new`).
"""
function _amc_assign_dominates(ctx, opath::Vector{Tuple{NodeId,Int}},
                               cs_path::Vector{Tuple{NodeId,Int}})
    # (assignment occurrence paths end with the (=-node, 1) step to the LHS)
    n = length(cs_path)
    d = 0
    for i in 1:min(length(opath), n)
        if opath[i] != cs_path[i]
            d = i
            break
        end
    end
    (d == 0 || d >= n || opath[d][2] >= cs_path[d][2]) && return false
    graph = syntax_graph(ctx)
    kind(SyntaxTree(graph, opath[d][1])) == K"block" || return false
    for l in (d+1):(length(opath)-1)
        kl = kind(SyntaxTree(graph, opath[l][1]))
        if kl == K"block" || kl == K"removable" || kl == K"unused_only"
            # completing these implies every child ran
        elseif kl == K"="
            opath[l][2] == 2 || return false
        else
            return false
        end
    end
    return kind(SyntaxTree(graph, opath[end][1])) == K"="
end

# The tree walk: record candidate occurrences (with paths), closure
# `function_decl` sites, K"local" declaration markers, `=`-assignment counts
# for owner closures, every nested lambda's bindings, and the presence of
# label/goto forms. Descends the same positions `_convert_closures` treats
# as runtime accesses; positions it cannot classify refuse the variable.
function _amc_scan!(st::AMCScan, ex, path::Vector{Tuple{NodeId,Int}})
    st.bad && return nothing
    k = kind(ex)
    if k == K"symboliclabel" || k == K"symbolicgoto" || k == K"oldsymbolicgoto"
        st.bad = true   # NB: these are leaves — test before the leaf case
        return nothing
    elseif k == K"BindingId"
        id = ex.var_id
        if id in st.cand
            push!(get!(() -> AMCOccurrence[], st.occs, id),
                  AMCOccurrence(copy(path), getfield(ex, :_id), false))
        end
        return nothing
    elseif is_leaf(ex) || k == K"inert" || k == K"inert_syntaxtree" ||
           k == K"quote" || k == K"static_eval"
        return nothing
    elseif k == K"=" && numchildren(ex) == 2
        lhs = ex[1]
        if kind(lhs) == K"BindingId"
            id = lhs.var_id
            if id in st.clos
                st.gassign[id] = get(st.gassign, id, 0) + 1
            end
            if id in st.cand
                push!(path, (getfield(ex, :_id), 1))
                push!(get!(() -> AMCOccurrence[], st.occs, id),
                      AMCOccurrence(copy(path), getfield(lhs, :_id), true))
                pop!(path)
            end
        else
            _amc_scan_child!(st, ex, 1, path)
        end
        _amc_scan_child!(st, ex, 2, path)
        return nothing
    elseif k == K"local"
        # `path` currently ends at (parent, index-of-this-K"local"): the
        # marker's statement position
        c = ex[1]
        if kind(c) == K"BindingId" && c.var_id in st.cand
            push!(get!(() -> Vector{Vector{Tuple{NodeId,Int}}}(), st.decls, c.var_id),
                  copy(path))
        end
        return nothing
    elseif k == K"global" || k == K"always_defined" || k == K"newvar"
        return nothing                   # declaration markers, not accesses
    elseif k == K"decl"
        # the declared type expression is re-evaluated at every assignment of
        # the declared variable: its position is not this node's position
        numchildren(ex) >= 2 && _amc_refuse_scan!(st, ex[2])
        return nothing
    elseif k == K"isdefined"
        c = ex[1]
        if kind(c) == K"BindingId" && c.var_id in st.cand
            push!(path, (getfield(ex, :_id), 1))
            push!(get!(() -> AMCOccurrence[], st.occs, c.var_id),
                  AMCOccurrence(copy(path), getfield(c, :_id), false))
            pop!(path)
        end
        return nothing
    elseif k == K"throw_undef_if_not"
        numchildren(ex) >= 1 && kind(ex[1]) == K"BindingId" &&
            ex[1].var_id in st.cand && _amc_refuse_scan!(st, ex[1])
        numchildren(ex) >= 2 && _amc_scan_child!(st, ex, 2, path)
        return nothing
    elseif k == K"symbolicblock"
        _amc_scan_child!(st, ex, 2, path)       # child 1 is the label
        return nothing
    elseif k == K"break"
        for i in 2:numchildren(ex)              # child 1 is the label
            _amc_scan_child!(st, ex, i, path)
        end
        return nothing
    elseif k == K"meta" || k == K"loopinfo"
        # may mention bindings without being runtime accesses
        _amc_refuse_scan!(st, ex)
        return nothing
    elseif k == K"lambda"
        push!(st.lambda_binds, ex.lambda_bindings)
        return nothing                          # captures counted via bindings
    elseif k == K"function_decl"
        # `path` currently ends at (parent, index-of-this-function_decl): the
        # creation position — its last node is the creation group block
        f = ex[1]
        if kind(f) == K"BindingId"
            if f.var_id in st.clos
                push!(get!(() -> Vector{Vector{Tuple{NodeId,Int}}}(), st.sites, f.var_id),
                      copy(path))
            end
            f.var_id in st.cand && push!(st.refuse, f.var_id)
        end
        for i in 2:numchildren(ex)
            _amc_refuse_scan!(st, ex[i])
        end
        return nothing
    elseif k == K"method_defs"
        # signature material is lifted to definition toplevel by the
        # conversion — position-based classification cannot see it move;
        # method lambda bodies are the closure's own frames
        for i in 2:numchildren(ex)
            _amc_defs_scan!(st, ex[i])
        end
        return nothing
    elseif k == K"function_type"
        return nothing                          # names the TYPE, no access
    elseif k == K"method"
        # 1-arg: global binding effect. 3-arg (global method): the signature
        # evaluates in place; the lambda is a nested frame.
        for i in 2:numchildren(ex)
            _amc_scan_child!(st, ex, i, path)
        end
        return nothing
    elseif k == K"_opaque_closure"
        for i in 2:numchildren(ex)
            _amc_scan_child!(st, ex, i, path)
        end
        return nothing
    else
        for i in 1:numchildren(ex)
            _amc_scan_child!(st, ex, i, path)
        end
        return nothing
    end
end

function _amc_scan_child!(st::AMCScan, ex, i::Int, path::Vector{Tuple{NodeId,Int}})
    push!(path, (getfield(ex, :_id), i))
    _amc_scan!(st, ex[i], path)
    pop!(path)
    return nothing
end

# Refuse every candidate mentioned in a subtree the analysis cannot position
# (declared-type expressions, meta, creation-statement signature material).
function _amc_refuse_scan!(st::AMCScan, ex)
    k = kind(ex)
    if k == K"BindingId"
        ex.var_id in st.cand && push!(st.refuse, ex.var_id)
    elseif k == K"lambda"
        push!(st.lambda_binds, ex.lambda_bindings)
    elseif !is_leaf(ex) && !is_quoted(ex)
        for c in children(ex)
            _amc_refuse_scan!(st, c)
        end
    end
    return nothing
end

# method_defs subtrees: record method lambdas; any candidate occurrence in
# the signature material (outside the lambdas) refuses the variable.
function _amc_defs_scan!(st::AMCScan, ex)
    k = kind(ex)
    if k == K"lambda"
        push!(st.lambda_binds, ex.lambda_bindings)
    elseif k == K"BindingId"
        ex.var_id in st.cand && push!(st.refuse, ex.var_id)
    elseif !is_leaf(ex) && !is_quoted(ex)
        for c in children(ex)
            _amc_defs_scan!(st, c)
        end
    end
    return nothing
end
