struct ClosureInfo{Attrs}
    # Global name of the type of the closure
    type_name::SyntaxTree{Attrs}
    # Names of fields for use with getfield, in order
    field_names::SyntaxList{Attrs, Vector{NodeId}}
    # Map from the original BindingId of closed-over vars to the index of the
    # associated field in the closure type.
    field_inds::Dict{IdTag,Int}
    capt_sp::SyntaxList{Attrs, Vector{NodeId}}
end

struct ClosureConversionCtx{Attrs} <: AbstractLoweringContext
    graph::SyntaxGraph{Attrs}
    bindings::Bindings
    mod::Module
    closure_bindings::Dict{IdTag,ClosureBindings}
    capture_rewriting::Union{Nothing,ClosureInfo{Attrs},
                             SyntaxList{Attrs, Vector{NodeId}}}
    lambda_bindings::LambdaBindings
    sp_typevars::Dict{IdTag, IdTag}
    # True if we're in a section of code which preserves top-level sequencing
    # such that closure types can be emitted inline with other code.
    toplevel::Bool
    # toplevel, or contained by method_defs and no lambda within it
    lifted::Bool
    # True if this expression should not have toplevel effects, namely, it
    # should not declare the globals it references.  This allows generated
    # functions to refer to globals that have already been declared, without
    # triggering the "function body AST not pure" error.
    toplevel_pure::Bool
    toplevel_stmts::SyntaxList{Attrs, Vector{NodeId}}
    closure_infos::Dict{IdTag,ClosureInfo{Attrs}}
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

function get_box_contents(ctx::ClosureConversionCtx, var, box_ex)
    undef_var = new_local_binding(ctx, var, get_binding(ctx, var.var_id).name;
                                  is_used_undef=true)
    @ast ctx var [K"block"
        box := box_ex
        # Lower in an UndefVar check to a similarly named variable
        # (ref #20016) so that closure lowering Box introduction
        # doesn't impact the error message and the compiler is expected
        # to fold away the extraneous null check
        #
        # TODO: Ideally the runtime would rely on provenance info for
        # this error and we can remove the isdefined check.
        [K"if" [K"call"
                "isdefined"::K"core"
                box
                "contents"::K"Symbol"
            ]
            (::K"TOMBSTONE")
            [K"block"
                 [K"newvar" undef_var]
                 undef_var
            ]
        ]
        [K"call"
            "getfield"::K"core"
            box
            "contents"::K"Symbol"
        ]
    ]
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
    if !ctx.toplevel
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
        @jl_assert binfo.kind in (:local, :argument, :typevar) ex
        boxed = is_boxed(binfo)
        if isnothing(binfo.type) && !boxed
            @ast ctx ex [K"=" var rhs0]
        else
            # Typed local
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
function closure_type_fields(ctx, srcref, closure_binds, is_opaque)
    capt_locals = Set{IdTag}()
    capt_sp = Set{IdTag}()
    add_capt(id) = push!(
        get_binding(ctx, id).kind !== :static_parameter || is_opaque ?
            capt_locals : capt_sp, id)
    for lambda_bindings in closure_binds.lambdas
        for (id, is_capt) in lambda_bindings.locals_capt
            is_capt && add_capt(id)
        end
    end
    foreach(add_capt, closure_binds.capt_sp)

    field_syms = SyntaxList(ctx)
    if is_opaque
        field_orig_bindings = sort!(collect(capt_locals))
        # For opaque closures we don't try to generate sensible names for the
        # fields as there's no closure type to generate.
        for i in eachindex(field_orig_bindings)
            push!(field_syms, @ast ctx srcref i::K"Integer")
        end
    else
        field_names = Dict{String,IdTag}()
        for id in sort!(collect(capt_locals))
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
    field_inds = Dict{IdTag,Int}()
    field_is_box = Vector{Bool}()
    for (i,id) in enumerate(field_orig_bindings)
        push!(field_is_box, is_boxed(ctx, id))
        field_inds[id] = i
    end
    capt_sp2 = SyntaxList(ctx)
    for sp in sort!(collect(capt_sp))
        push!(capt_sp2, binding_ex(ctx, sp))
    end

    return field_syms, field_orig_bindings, field_inds, field_is_box, capt_sp2
end

# No box needed for:
# - non-captured vars
# - static params (can't be reassigned)
# - any local our optimizations have determined to be unboxed
function is_boxed(binfo::BindingInfo)
    binfo.kind === :static_parameter && return false
    binfo.kind === :typevar && return false
    binfo.unboxed && return false
    binfo.kind === :argument && !binfo.is_assigned && return false
    return binfo.is_captured
end

function is_boxed(ctx, x)
    is_boxed(get_binding(ctx, x))
end

# Is a field in the closure argument `self`.  Exception: non-OC sparams are type
# params to the `self` type, and are rewritten later in linearization.
function is_self_captured(ctx, x)
    b = get_binding(ctx, x)
    out = get(ctx.lambda_bindings.locals_capt, b.id, false)
    if out && (b.kind === :static_parameter || b.kind === :typevar)
        ctx.capture_rewriting isa ClosureInfo &&
            haskey(ctx.capture_rewriting.field_inds, b.id)
    else
        out
    end
end

function convert_local_function_decl(ctx, ex)
    fid = ex[1].var_id::IdTag
    haskey(ctx.closure_infos, fid) && return @ast ctx ex (::K"TOMBSTONE")

    closure_binds = ctx.closure_bindings[fid]
    field_syms, field_orig_bindings, field_inds, field_is_box, capt_sp =
        closure_type_fields(ctx, ex, closure_binds, false)
    name_str = reserve_module_binding_i(
        ctx.mod,
        string("#", join(closure_binds.name_stack, "#"), "##"))
    global_clstruct = new_global_binding(ctx, ex, name_str, ctx.mod)
    sp_syms = mapsyntax(sp->newleaf(ctx, sp, K"Symbol",
                                    get_binding(ctx, sp.var_id::IdTag).name),
                        capt_sp)
    define_clstruct = type_ex = @ast ctx ex [K"call"
        eval_closure_type::K"Value"
        ctx.mod::K"Value"
        name_str::K"Symbol"
        [K"call" "svec"::K"core" sp_syms...]
        [K"call" "svec"::K"core" field_syms...]
        [K"call" "svec"::K"core" [f::K"Bool" for f in field_is_box]...]
    ]
    if !ctx.toplevel
        push!(ctx.toplevel_stmts, define_clstruct)
        push!(ctx.toplevel_stmts, @ast ctx ex (::K"latestworld_if_toplevel"))
        define_clstruct = nothing
    end
    closure_info = ClosureInfo(global_clstruct, field_syms, field_inds, capt_sp)
    ctx.closure_infos[fid] = closure_info
    type_params = mapsyntax(capt_sp) do sp
        is_self_captured(ctx, sp) ? captured_var_access(ctx, sp) : sp
    end
    init_closure_args = SyntaxList(ctx)
    for (id, boxed) in zip(field_orig_bindings, field_is_box)
        field_val = binding_ex(ctx, id)
        if is_self_captured(ctx, field_val)
            # Access from outer closure if necessary but do not
            # unbox to feed into the inner nested closure.
            field_val = captured_var_access(ctx, field_val)
        end
        push!(init_closure_args, field_val)
        if !boxed
            push!(type_params, @ast ctx ex [K"call"
                  "_typeof_captured_variable"::K"core"
                  field_val])
        end
    end
    @ast ctx ex [K"block"
        define_clstruct
        (::K"latestworld_if_toplevel")
        closure_type := if isempty(type_params)
            global_clstruct
        else
            [K"call" "apply_type"::K"core" global_clstruct type_params...]
        end
        closure_val := [K"new" closure_type init_closure_args...]
        convert_assignment(ctx, [K"=" ex[1] closure_val])
        (::K"TOMBSTONE")
    ]
end

# Map the children of `ex` through _convert_closures, lifting any toplevel
# closure definition statements to occur before the other content of `ex`.
function map_cl_convert(ctx::ClosureConversionCtx, ex)
    if ctx.toplevel
        toplevel_stmts = SyntaxList(ctx)
        ctx2 = ClosureConversionCtx(
            ctx.graph, ctx.bindings, ctx.mod,
            ctx.closure_bindings, ctx.capture_rewriting, ctx.lambda_bindings,
            ctx.sp_typevars, true, ctx.lifted,
            ctx.toplevel_pure, toplevel_stmts, ctx.closure_infos)
        res = mapchildren(e->_convert_closures(ctx2, e), ctx2, ex)
        if isempty(toplevel_stmts)
            res
        else
            @ast ctx ex [K"block" toplevel_stmts... res]
        end
    else
        mapchildren(e->_convert_closures(ctx, e), ctx, ex)
    end
end

function _convert_closures(ctx::ClosureConversionCtx, ex)
    k = kind(ex)
    if k == K"BindingId"
        b = get_binding(ctx, ex)
        if ctx.lifted && haskey(ctx.sp_typevars, b.id)
            binding_ex(ctx, ctx.sp_typevars[b.id])
        else
            access = is_self_captured(ctx, ex) ? captured_var_access(ctx, ex) : ex
            is_boxed(ctx, ex) ? get_box_contents(ctx, ex, access) : access
        end
    elseif is_leaf(ex) || k == K"inert" || k == K"syntaxinert" || k == K"static_eval"
        ex
    elseif k == K"="
        convert_assignment(ctx, ex)
    elseif k == K"isdefined"
        # Convert isdefined expr to function for closure converted variables
        var = ex[1]
        binfo = get_binding(ctx, var)
        if is_boxed(binfo)
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
            # [K"assert" "toplevel_only"::K"Symbol" [K"syntaxinert" ex]]
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
        @jl_assert false (ex, "lambda should be at top level or in `method`")
    elseif k == K"function_decl"
        func_name = ex[1]
        @jl_assert kind(func_name) == K"BindingId" ex
        if haskey(ctx.closure_bindings, func_name.var_id::IdTag)
            convert_local_function_decl(ctx, ex)
        else
            # Single-arg K"method" has the side effect of creating a global
            # binding for `func_name` if it doesn't exist.
            @ast ctx ex [K"block"
                [K"method" func_name]
                (::K"TOMBSTONE") # <- function_decl should not be used in value position
            ]
        end
    elseif k == K"method"
        @jl_assert ctx.lifted ex
        # The method sp svec needs every sp the body and sig capture
        cr = ctx.capture_rewriting
        sp_ids = IdTag[c.var_id::IdTag for c in children(ex[3][2])]
        if cr isa ClosureInfo
            append!(sp_ids, sp.var_id::IdTag for sp in cr.capt_sp)
        end
        sort!(sp_ids)
        sps = SyntaxList(ctx)
        for id in sp_ids
            push!(sps, binding_ex(ctx, id))
        end
        tvs = mapsyntax(c->binding_ex(ctx, ctx.sp_typevars[c.var_id::IdTag]), sps)

        # rm method table argument if it's a closure id, since it's unnecessary
        # and requires the `(= id (new ...))` call to be lifted above the
        # method.  flisp might be messing up overlays when it does this, since
        # it removes all locals, not just closure ids.
        mtable = kind(ex[1]) === K"BindingId" &&
            haskey(ctx.closure_bindings, ex[1].var_id) ?
            @ast(ctx, ex[1], (::K"nothing")) : _convert_closures(ctx, ex[1])
        @ast ctx ex [K"method"
            mtable
            [K"call" "svec"::K"core"
                _convert_closures(ctx, ex[2])
                [K"call" "svec"::K"core" tvs...]
                (::K"SourceLocation")]
            closure_convert_lambda(ctx, ex[3], sps)
        ]
    elseif k == K"function_type"
        func_name = ex[1]
        if kind(func_name) == K"BindingId" && get_binding(ctx, func_name).kind === :local
            @jl_assert(haskey(ctx.closure_infos, func_name.var_id),
                       (ex, "function_type of local without known closure type"))
            ci = ctx.closure_infos[func_name.var_id]
            if isempty(ci.capt_sp) || ci !== ctx.capture_rewriting
                ci.type_name
            else
                # flisp: fix-function-arg-type
                tvs = mapsyntax(
                    sp->binding_ex(ctx, ctx.sp_typevars[sp.var_id::IdTag]),
                    ci.capt_sp)
                @ast ctx ex [K"call" "apply_type"::K"core" ci.type_name tvs...]
            end
        else
            @ast ctx ex [K"call" TypeEqOf::K"core" _convert_closures(ctx, func_name)]
        end
    elseif k == K"method_defs"
        name = ex[1]
        is_closure = kind(name) == K"BindingId" && get_binding(ctx, name).kind === :local
        cap_rewrite = is_closure ? ctx.closure_infos[name.var_id] : nothing
        ctx2 = ClosureConversionCtx(
            ctx.graph, ctx.bindings, ctx.mod,
            ctx.closure_bindings, cap_rewrite, ex.lambda_bindings, ctx.sp_typevars,
            ctx.toplevel, true, ctx.toplevel_pure, ctx.toplevel_stmts,
            ctx.closure_infos)
        tvs = map_cl_convert(ctx2, ex[2])
        if !ctx.toplevel
            push!(ctx2.toplevel_stmts, tvs)
            tvs = @ast ctx ex[2] (::K"TOMBSTONE")
        end
        body = map_cl_convert(ctx2, ex[3])
        if is_closure
            if ctx.toplevel
                @ast ctx ex [K"block" tvs body]
            else
                push!(ctx2.toplevel_stmts, body)
                @ast ctx ex (::K"TOMBSTONE")
            end
        else
            @ast ctx ex [K"block" tvs body (::K"TOMBSTONE")]
        end
    elseif k == K"_opaque_closure"
        closure_binds = ctx.closure_bindings[ex[1].var_id]
        field_syms, field_orig_bindings, field_inds, _field_is_box, capt_sp =
            closure_type_fields(ctx, ex, closure_binds, true)

        capture_rewrites = ClosureInfo(ex #=unused=#, field_syms, field_inds, capt_sp)

        ctx2 = ClosureConversionCtx(
            ctx.graph, ctx.bindings, ctx.mod,
            ctx.closure_bindings, capture_rewrites, ctx.lambda_bindings,
            ctx.sp_typevars, false, false,
            ctx.toplevel_pure, ctx.toplevel_stmts, ctx.closure_infos)

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
                closure_convert_lambda(ctx2, ex[9], SyntaxList(ctx))
            ]
            init_closure_args...
        ]
    else
        map_cl_convert(ctx, ex)
    end
end

function closure_convert_lambda(ctx, ex, sps)
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
    ctx2 = ClosureConversionCtx(
        ctx.graph, ctx.bindings, ctx.mod,
        ctx.closure_bindings, cap_rewrite, lambda_bindings, ctx.sp_typevars,
        ex.is_toplevel_thunk, ex.is_toplevel_thunk,
        ctx.toplevel_pure && ex.toplevel_pure,
        ctx.toplevel_stmts, ctx.closure_infos)
    lambda_children = SyntaxList(ctx)
    args = ex[1]
    push!(lambda_children, args)
    push!(lambda_children, @ast ctx ex[2] [K"block" sps...])

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
            replace_captured_locals::K"Value"
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
    # TODO: ctx.mod is used instead of syntax_module(ex) beyond this point,
    # which is dubious
    ctx_out = ClosureConversionCtx(ctx.graph, ctx.bindings,
                                   ctx.layer.mod,
                                   ctx.closure_bindings, nothing,
                                   ex.lambda_bindings, ctx.sp_typevars,
                                   false, true, true, SyntaxList(ctx.graph),
                                   Dict{IdTag,ClosureInfo{Attrs}}())
    ex_out = closure_convert_lambda(ctx_out, ex, children(ex[2]))
    if !isempty(ctx_out.toplevel_stmts)
        throw(LoweringError(first(ctx_out.toplevel_stmts), "Top level code was found outside any top level context. `@generated` functions may not contain closures, including `do` syntax and generators/comprehension"))
    end
    ctx_out, flatten_blocks(ex_out)
end
