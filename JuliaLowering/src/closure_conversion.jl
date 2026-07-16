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
    binfo = get_binding(ctx, var.var_id)
    undef_var = new_local_binding(ctx, var, binfo.name;
                                  is_used_undef=true)
    fname = "contents"
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
                fname::K"Symbol"
            ]
            ::K"TOMBSTONE"
            [K"block"
                 [K"newvar" undef_var]
                 undef_var
            ]
        ]
        [K"call"
            "getfield"::K"core"
            box
            fname::K"Symbol"
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
    field_inds = Dict{IdTag,Int}()
    # entries: `false` (value capture, type-parameterized field), `true`
    # (`Core.Box` shared capture)
    field_is_box = Vector{Bool}()
    for (i,id) in enumerate(field_orig_bindings)
        push!(field_is_box, is_boxed(ctx, id))
        field_inds[id] = i
    end

    return field_syms, field_orig_bindings, field_inds, field_is_box
end

# Return a thunk which creates a new type for a closure with `field_syms` named
# fields. The new type will be named `name_str` which must be an unassigned
# name in the module.
function type_for_closure(ctx::ClosureConversionCtx, srcref, name_str, field_syms, field_is_box)
    # New closure types always belong to the module we're expanding into - they
    # need to be serialized there during precompile.
    mod = ctx.mod
    type_binding = new_global_binding(ctx, srcref, name_str, mod)
    boxflags = SyntaxList(ctx)
    for f in field_is_box
        push!(boxflags, @ast ctx srcref f::K"Bool")
    end
    type_ex = @ast ctx srcref [K"call"
        #"_call_latest"::K"core"
        eval_closure_type::K"Value"
        ctx.mod::K"Value"
        name_str::K"Symbol"
        [K"call" "svec"::K"core" field_syms...]
        [K"call" "svec"::K"core" boxflags...]
    ]
    type_ex, type_binding
end

# No box needed for:
# - non-captured vars
# - static params (can't be reassigned)
# - any local our optimizations have determined to be unboxed
function is_boxed(binfo::BindingInfo)
    binfo.kind === :static_parameter && return false
    binfo.unboxed && return false
    binfo.kind === :argument && !binfo.is_assigned && return false
    return binfo.is_captured
end

function is_boxed(ctx, x)
    is_boxed(get_binding(ctx, x))
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
                                    false, ctx.toplevel_pure, toplevel_stmts, ctx.closure_infos)
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
        access = is_self_captured(ctx, ex) ? captured_var_access(ctx, ex) : ex
        if is_boxed(ctx, ex)
            get_box_contents(ctx, ex, access)
        else
            access
        end
    elseif is_leaf(ex) || k == K"inert" || k == K"inert_syntaxtree" || k == K"static_eval"
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
                field_syms, field_orig_bindings, field_inds, field_is_box =
                    closure_type_fields(ctx, ex, closure_binds, false)
                name_str = reserve_module_binding_i(
                    ctx.mod,
                    string("#", join(closure_binds.name_stack, "#"), "##"))
                closure_type_def, closure_type_ =
                    type_for_closure(ctx, ex, name_str, field_syms, field_is_box)
                if !ctx.is_toplevel_seq_point
                    push!(ctx.toplevel_stmts, closure_type_def)
                    push!(ctx.toplevel_stmts, @ast ctx ex (::K"latestworld_if_toplevel"))
                    closure_type_def = nothing
                end
                closure_info = ClosureInfo(closure_type_, field_syms, field_inds)
                ctx.closure_infos[func_name_id] = closure_info
                type_params = SyntaxList(ctx)
                init_closure_args = SyntaxList(ctx)
                for (id, boxed) in zip(field_orig_bindings, field_is_box)
                    field_val = binding_ex(ctx, id)
                    if is_self_captured(ctx, field_val)
                        # Access from outer closure if necessary but do not
                        # unbox to feed into the inner nested closure.
                        field_val = captured_var_access(ctx, field_val)
                    end
                    push!(init_closure_args, field_val)
                    if boxed === false
                        push!(type_params, @ast ctx ex [K"call"
                              "_typeof_captured_variable"::K"core"
                              field_val])
                    end
                end
                @ast ctx ex [K"block"
                    closure_type_def
                    (::K"latestworld_if_toplevel")
                    closure_type := if isempty(type_params)
                        closure_type_
                    else
                        [K"call" "apply_type"::K"core" closure_type_ type_params...]
                    end
                    closure_val := [K"new"
                        closure_type
                        init_closure_args...
                    ]
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
                                    ctx.closure_infos)
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
        field_syms, field_orig_bindings, field_inds, _field_is_box =
            closure_type_fields(ctx, ex, closure_binds, true)

        capture_rewrites = ClosureInfo(ex #=unused=#, field_syms, field_inds)

        ctx2 = ClosureConversionCtx(ctx.graph, ctx.bindings, ctx.mod,
                                    ctx.closure_bindings, capture_rewrites, ctx.lambda_bindings,
                                    false, ctx.toplevel_pure, ctx.toplevel_stmts, ctx.closure_infos)

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
                                ctx.toplevel_stmts, ctx.closure_infos)
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
                                   Dict{IdTag,ClosureInfo{Attrs}}())
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
