struct ClosureInfo{GraphType}
    # Global name of the type of the closure
    type_name::SyntaxTree{GraphType}
    # Names of fields for use with getfield, in order
    field_names::SyntaxList{GraphType}
    # Map from the original BindingId of closed-over vars to the index of the
    # associated field in the closure type.
    field_inds::Dict{IdTag,Int}
end

struct ClosureConversionCtx{GraphType} <: AbstractLoweringContext
    graph::GraphType
    bindings::Bindings
    mod::Module
    closure_bindings::Dict{IdTag,ClosureBindings}
    capture_rewriting::Union{Nothing,ClosureInfo{GraphType},SyntaxList{GraphType}}
    lambda_bindings::LambdaBindings
    # True if we're in a section of code which preserves top-level sequencing
    # such that closure types can be emitted inline with other code.
    is_toplevel_seq_point::Bool
    # True if this expression should not have toplevel effects, namely, it
    # should not declare the globals it references.  This allows generated
    # functions to refer to globals that have already been declared, without
    # triggering the "function body AST not pure" error.
    toplevel_pure::Bool
    toplevel_stmts::SyntaxList{GraphType}
    closure_infos::Dict{IdTag,ClosureInfo{GraphType}}
end

function ClosureConversionCtx(graph::GraphType, bindings::Bindings,
                              mod::Module, closure_bindings::Dict{IdTag,ClosureBindings},
                              lambda_bindings::LambdaBindings) where {GraphType}
    ClosureConversionCtx{GraphType}(
        graph, bindings, mod, closure_bindings, nothing,
        lambda_bindings, false, true, SyntaxList(graph),
        Dict{IdTag,ClosureInfo{GraphType}}())
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
        @assert !isnothing(cap_rewrite)
        if isempty(interpolations) || !is_same_identifier_like(interpolations[end], ex)
            push!(interpolations, ex)
        end
        @ast ctx ex [K"captured_local" length(interpolations)::K"Integer"]
    end
end

function get_box_contents(ctx::ClosureConversionCtx, var, box_ex)
    undef_var = new_local_binding(ctx, var, lookup_binding(ctx, var.var_id).name)
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
            ::K"TOMBSTONE"
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
            "nothing"::K"core"
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
function make_globaldecl(ctx, src_ex, mod, name, strong=false, type=nothing; ret_nothing=false)
    if !ctx.toplevel_pure
        decl = @ast ctx src_ex [K"block"
            [K"call"
                "declare_global"::K"core"
                mod::K"Value" name::K"Symbol" strong::K"Bool"
                if type !== nothing
                    type
                end
            ]
            [K"latestworld"]
            @ast ctx src_ex [K"removable" "nothing"::K"core"]
        ]
        if ctx.is_toplevel_seq_point
            return decl
        else
            push!(ctx.toplevel_stmts, decl)
        end
    end
    if ret_nothing
        nothing
    else
        @ast ctx src_ex [K"removable" "nothing"::K"core"]
    end
end

function convert_global_assignment(ctx, ex, var, rhs0)
    binfo = lookup_binding(ctx, var)
    @assert binfo.kind == :global
    stmts = SyntaxList(ctx)
    decl = make_globaldecl(ctx, ex, binfo.mod, binfo.name, true; ret_nothing=true)
    decl !== nothing && push!(stmts, decl)
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
    @chk kind(var) == K"BindingId"
    binfo = lookup_binding(ctx, var)
    if binfo.kind == :global
        convert_global_assignment(ctx, ex, var, rhs0)
    else
        @assert binfo.kind == :local || binfo.kind == :argument
        boxed = is_boxed(binfo)
        if isnothing(binfo.type) && !boxed
            @ast ctx ex [K"=" var rhs0]
        else
            # Typed local
            tmp_rhs0 = ssavar(ctx, rhs0)
            rhs = isnothing(binfo.type) ? tmp_rhs0 :
                  convert_for_type_decl(ctx, ex, tmp_rhs0, _convert_closures(ctx, binfo.type), true)
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
    capture_ids = Vector{IdTag}()
    for lambda_bindings in closure_binds.lambdas
        for (id, lbinfo) in lambda_bindings.bindings
            if lbinfo.is_captured
                push!(capture_ids, id)
            end
        end
    end
    # sort here to avoid depending on undefined Dict iteration order.
    capture_ids = sort!(unique(capture_ids))

    field_syms = SyntaxList(ctx)
    if is_opaque
        field_orig_bindings = capture_ids
        # For opaque closures we don't try to generate sensible names for the
        # fields as there's no closure type to generate.
        for (i,id) in enumerate(field_orig_bindings)
            push!(field_syms, @ast ctx srcref i::K"Integer")
        end
    else
        field_names = Dict{String,IdTag}()
        for id in capture_ids
            binfo = lookup_binding(ctx, id)
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
    type_ex = @ast ctx srcref [K"call"
        #"_call_latest"::K"core"
        eval_closure_type::K"Value"
        ctx.mod::K"Value"
        name_str::K"Symbol"
        [K"call" "svec"::K"core" field_syms...]
        [K"call" "svec"::K"core" [f::K"Bool" for f in field_is_box]...]
    ]
    type_ex, type_binding
end

function is_boxed(binfo::BindingInfo)
    # True for
    # * :argument when it's not reassigned
    # * :static_parameter (these can't be reassigned)
    defined_but_not_assigned = binfo.is_always_defined && binfo.n_assigned == 0
    # For now, we box almost everything but later we'll want to do dominance
    # analysis on the untyped IR.
    return binfo.is_captured && !defined_but_not_assigned
end

function is_boxed(ctx, x)
    is_boxed(lookup_binding(ctx, x))
end

# Is captured in the closure's `self` argument
function is_self_captured(ctx, x)
    lbinfo = lookup_lambda_binding(ctx, x)
    !isnothing(lbinfo) && lbinfo.is_captured
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
    elseif is_leaf(ex) || k == K"inert" || k == K"static_eval"
        ex
    elseif k == K"="
        convert_assignment(ctx, ex)
    elseif k == K"isdefined"
        # Convert isdefined expr to function for closure converted variables
        var = ex[1]
        binfo = lookup_binding(ctx, var)
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
        @assert kind(ex[1]) == K"BindingId"
        binfo = lookup_binding(ctx, ex[1])
        if binfo.kind == :global
            # flisp has this, but our K"assert" handling is in a previous pass
            # [K"assert" "toplevel_only"::K"Symbol" [K"inert" ex]]
            make_globaldecl(ctx, ex, binfo.mod, binfo.name, true, _convert_closures(ctx, ex[2]))
        else
            makeleaf(ctx, ex, K"TOMBSTONE")
        end
    elseif k == K"global"
        # Leftover `global` forms become weak globals.
        mod, name = if kind(ex[1]) == K"BindingId"
            binfo = lookup_binding(ctx, ex[1])
            @assert binfo.kind == :global
            binfo.mod, binfo.name
        else
            # See note about using eval on Expr(:global/:const, GlobalRef(...))
            @assert ex[1].value isa GlobalRef
            ex[1].value.mod, String(ex[1].value.name)
        end
        @ast ctx ex [K"unused_only" make_globaldecl(ctx, ex, mod, name, false)]
    elseif k == K"local"
        var = ex[1]
        binfo = lookup_binding(ctx, var)
        if binfo.is_captured
            @ast ctx ex [K"=" var [K"call" "Box"::K"core"]]
        elseif !binfo.is_always_defined
            @ast ctx ex [K"newvar" var]
        else
            makeleaf(ctx, ex, K"TOMBSTONE")
        end
    elseif k == K"lambda"
        closure_convert_lambda(ctx, ex)
    elseif k == K"function_decl"
        func_name = ex[1]
        @assert kind(func_name) == K"BindingId"
        func_name_id = func_name.var_id
        if haskey(ctx.closure_bindings, func_name_id)
            closure_info = get(ctx.closure_infos, func_name_id, nothing)
            needs_def = isnothing(closure_info)
            if needs_def
                closure_binds = ctx.closure_bindings[func_name_id]
                field_syms, field_orig_bindings, field_inds, field_is_box =
                    closure_type_fields(ctx, ex, closure_binds, false)
                name_str = reserve_module_binding_i(ctx.mod,
                    "#$(join(closure_binds.name_stack, "#"))##")
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
                    if !boxed
                        push!(type_params, @ast ctx ex [K"call"
                              # TODO: Update to use _typeof_captured_variable (#40985)
                              #"_typeof_captured_variable"::K"core"
                              "typeof"::K"core"
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
    elseif k == K"function_type"
        func_name = ex[1]
        if kind(func_name) == K"BindingId" && lookup_binding(ctx, func_name).kind === :local
            ctx.closure_infos[func_name.var_id].type_name
        else
            @ast ctx ex [K"call" "Typeof"::K"core" func_name]
        end
    elseif k == K"method_defs"
        name = ex[1]
        is_closure = kind(name) == K"BindingId" && lookup_binding(ctx, name).kind === :local
        cap_rewrite = is_closure ? ctx.closure_infos[name.var_id] : nothing
        ctx2 = ClosureConversionCtx(ctx.graph, ctx.bindings, ctx.mod,
                                    ctx.closure_bindings, cap_rewrite, ctx.lambda_bindings,
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
        field_syms, field_orig_bindings, field_inds, field_is_box =
            closure_type_fields(ctx, ex, closure_binds, true)

        capture_rewrites = ClosureInfo(ex #=unused=#, field_syms, field_inds)

        ctx2 = ClosureConversionCtx(ctx.graph, ctx.bindings, ctx.mod,
                                    ctx.closure_bindings, capture_rewrites, ctx.lambda_bindings,
                                    false, ctx.toplevel_pure, ctx.toplevel_stmts, ctx.closure_infos)

        init_closure_args = SyntaxList(ctx)
        for id in field_orig_bindings
            push!(init_closure_args, binding_ex(ctx, id))
        end
        @ast ctx ex [K"new_opaque_closure"
            ex[2] # arg type tuple
            ex[3] # return_lower_bound
            ex[4] # return_upper_bound
            ex[5] # allow_partial
            [K"opaque_closure_method"
                "nothing"::K"core"
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
    @assert kind(ex) == K"lambda"
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
        @assert numchildren(ex) == 4
        push!(lambda_children, _convert_closures(ctx2, ex[4]))
    end

    lam = makenode(ctx, ex, ex, lambda_children; lambda_bindings=lambda_bindings)
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

This pass does a few things things:
* Deal with typed variables (K"decl") and their assignments
* Deal with const and non-const global assignments
* Convert closures into types
* Lower variables captured by closures into boxes, etc, as necessary

Invariants:
* This pass must not introduce new K"Identifier" - only K"BindingId".
* Any new binding IDs must be added to the enclosing lambda locals
"""
@fzone "JL: closures" function convert_closures(ctx::VariableAnalysisContext, ex)
    ctx = ClosureConversionCtx(ctx.graph, ctx.bindings, ctx.mod,
                               ctx.closure_bindings, ex.lambda_bindings)
    ex1 = closure_convert_lambda(ctx, ex)
    if !isempty(ctx.toplevel_stmts)
        throw(LoweringError(first(ctx.toplevel_stmts), "Top level code was found outside any top level context. `@generated` functions may not contain closures, including `do` syntax and generators/comprehension"))
    end
    ctx, ex1
end
