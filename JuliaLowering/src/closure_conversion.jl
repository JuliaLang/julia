struct ClosureInfo{GraphType}
    # Global name of the type of the closure
    type_name::SyntaxTree{GraphType}
    # Names of fields as K"Symbol" nodes, in order
    field_syms::SyntaxList{GraphType}
    # Map from the original BindingId of closed-over vars to the index of the
    # associated field in the closure type.
    field_name_inds::Dict{IdTag,Int}
end

struct ClosureConversionCtx{GraphType} <: AbstractLoweringContext
    graph::GraphType
    bindings::Bindings
    mod::Module
    closure_bindings::Dict{IdTag,ClosureBindings}
    closure_info::Union{Nothing,ClosureInfo{GraphType}}
    lambda_bindings::LambdaBindings
    toplevel_stmts::SyntaxList{GraphType}
    closure_infos::Dict{IdTag,ClosureInfo{GraphType}}
end

function ClosureConversionCtx(graph::GraphType, bindings::Bindings,
                              mod::Module, closure_bindings::Dict{IdTag,ClosureBindings},
                              lambda_bindings::LambdaBindings) where {GraphType}
    ClosureConversionCtx{GraphType}(
        graph, bindings, mod, closure_bindings, nothing, lambda_bindings, SyntaxList(graph),
        Dict{IdTag,ClosureInfo{GraphType}}())
end

function current_lambda_bindings(ctx::ClosureConversionCtx)
    ctx.lambda_bindings
end

# Access captured variable from inside a closure
function captured_var_access(ctx, ex)
    cinfo = ctx.closure_info
    field_sym = cinfo.field_syms[cinfo.field_name_inds[ex.var_id]]
    @ast ctx ex [K"call"
        "getfield"::K"core"
        # FIXME: attributing the self binding to srcref=ex gives misleading printing.
        # We should carry provenance with each binding to fix this.
        binding_ex(ctx, current_lambda_bindings(ctx).self)
        field_sym
    ]
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
    # Require that the caller make `type` "simple", for now (can generalize
    # later if necessary)
    kt = kind(type)
    @assert (kt == K"Identifier" || kt == K"BindingId" || is_literal(kt))
    # Use a slot to permit union-splitting this in inference
    tmp = new_local_binding(ctx, srcref, "tmp", is_always_defined=true)

    @ast ctx srcref [K"block"
        # [K"=" type_ssa renumber_assigned_ssavalues(type)]
        [K"=" tmp ex]
        [K"if"
            [K"call" "isa"::K"core" tmp type]
            "nothing"::K"core"
            [K"="
                tmp
                if do_typeassert
                    [K"call"
                        "typeassert"::K"core"
                        [K"call" "convert"::K"top" type tmp]
                        type
                    ]
                else
                    [K"call" "convert"::K"top" type tmp]
                end
            ]
        ]
        tmp
    ]
end

function convert_global_assignment(ctx, ex, var, rhs0)
    binfo = lookup_binding(ctx, var)
    @assert binfo.kind == :global
    stmts = SyntaxList(ctx)
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
        lbinfo = lookup_lambda_binding(ctx, var)
        self_captured = !isnothing(lbinfo) && lbinfo.is_captured
        captured      = binfo.is_captured
        if isnothing(binfo.type) && !self_captured && !captured
            @ast ctx ex [K"=" var rhs0]
        else
            # Typed local
            tmp_rhs0 = ssavar(ctx, rhs0)
            rhs = isnothing(binfo.type) ? tmp_rhs0 :
                  convert_for_type_decl(ctx, ex, tmp_rhs0, _convert_closures(ctx, binfo.type), true)
            assignment = if self_captured || captured
                @ast ctx ex [K"call"
                    "setfield!"::K"core"
                    self_captured ? captured_var_access(ctx, var) : var
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
function closure_type_fields(ctx, srcref, closure_binds)
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
    field_names = Dict{String,IdTag}()
    for (i, id) in enumerate(capture_ids)
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
    field_syms = SyntaxList(ctx)
    field_orig_bindings = Vector{IdTag}()
    field_name_inds = Dict{IdTag,Int}()
    for (name,id) in sort!(collect(field_names))
        push!(field_syms, @ast ctx srcref name::K"Symbol")
        push!(field_orig_bindings, id)
        field_name_inds[id] = lastindex(field_syms)
    end

    return field_syms, field_orig_bindings, field_name_inds
end

function closure_name(mod, name_stack)
    basename = "#$(join(name_stack, "#"))##"
    i = 0
    while true
        name = "$basename$i"
        if reserve_module_binding(mod, Symbol(name))
            return name
        end
        i += 1
    end
end

# Return a thunk which creates a new type for a closure with `field_syms` named
# fields. The new type will be named `name_str` which must be an unassigned
# name in the module.
function type_for_closure(ctx::ClosureConversionCtx, srcref, name_str, field_syms)
    # New closure types always belong to the module we're expanding into - they
    # need to be serialized there during precompile.
    mod = ctx.mod
    type_binding = new_global_binding(ctx, srcref, name_str, mod)
    type_ex = @ast ctx srcref [K"lambda"(is_toplevel_thunk=true, lambda_bindings=LambdaBindings())
        [K"block"]
        [K"block"]
        [K"block"
            [K"global" type_binding]
            closure_type := [K"call"
                "_structtype"::K"core"
                mod::K"Value"
                name_str::K"Symbol"
                [K"call" "svec"::K"core"]
                [K"call"
                    "svec"::K"core"
                    field_syms...
                ]
                [K"call" "svec"::K"core"]
                false::K"Bool"
                length(field_syms)::K"Integer"
            ]
            [K"call" "_setsuper!"::K"core" closure_type "Function"::K"core"]
            # TODO: Need K"const_decl" or whatever when we upgrade to the latest Julia.
            [K"const" type_binding]
            [K"=" type_binding closure_type]
            [K"call"
                "_typebody!"::K"core"
                closure_type
                [K"call" "svec"::K"core" ["Box"::K"core" for _ in field_syms]...]
            ]
            "nothing"::K"core"
        ]
    ]
    type_ex, type_binding
end

function _convert_closures(ctx::ClosureConversionCtx, ex)
    k = kind(ex)
    if k == K"BindingId"
        id = ex.var_id
        lbinfo = lookup_lambda_binding(ctx, id)
        if !isnothing(lbinfo) && lbinfo.is_captured # TODO: && vinfo:asgn cv ??
            get_box_contents(ctx, ex, captured_var_access(ctx, ex))
        elseif lookup_binding(ctx, id).is_captured # TODO: && vinfo:asgn vi
            get_box_contents(ctx, ex, ex)
        else
            ex
        end
    elseif is_leaf(ex) || k == K"inert"
        ex
    elseif k == K"="
        convert_assignment(ctx, ex)
    elseif k == K"decl"
        if kind(ex[1]) != K"BindingId"
            # TODO: This case might be better dealt with in an earlier pass,
            # emitting `K"::"`??
            TODO(ex, "assertions for decls with non-bindings")
        end
        binfo = lookup_binding(ctx, ex[1])
        if binfo.kind == :local
            makeleaf(ctx, ex, K"TOMBSTONE")
        else
            @ast ctx ex [K"call"
                "set_binding_type!"::K"core"
                binfo.mod::K"Value"
                binfo.name::K"Symbol"
                _convert_closures(ctx, ex[2])
            ]
        end
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
                field_syms, field_orig_bindings, field_name_inds =
                    closure_type_fields(ctx, ex, closure_binds)
                name_str = closure_name(ctx.mod, closure_binds.name_stack)
                closure_type_def, closure_type =
                    type_for_closure(ctx, ex, name_str, field_syms)
                push!(ctx.toplevel_stmts, closure_type_def)
                closure_info = ClosureInfo(closure_type, field_syms, field_name_inds)
                ctx.closure_infos[func_name_id] = closure_info
                init_closure_args = SyntaxList(ctx)
                for id in field_orig_bindings
                    push!(init_closure_args, binding_ex(ctx, id))
                end
                @ast ctx ex [K"block"
                    [K"=" func_name
                        [K"new"
                            closure_type
                            init_closure_args...
                        ]
                    ]
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
                ::K"TOMBSTONE"
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
        cinfo = is_closure ? ctx.closure_infos[name.var_id] : nothing
        ctx2 = ClosureConversionCtx(ctx.graph, ctx.bindings, ctx.mod,
                                    ctx.closure_bindings, cinfo, ctx.lambda_bindings,
                                    ctx.toplevel_stmts, ctx.closure_infos)
        body = _convert_closures(ctx2, ex[2])
        if is_closure
            # Move methods to top level
            # FIXME: Probably lots more work to do to make this correct
            # Especially
            # * Renumbering SSA vars
            # * Ensuring that moved locals become slots in the top level thunk
            push!(ctx.toplevel_stmts, body)
            @ast ctx ex (::K"TOMBSTONE")
        else
            @ast ctx ex [K"block"
                body
                ::K"TOMBSTONE"
            ]
        end
    else
        mapchildren(e->_convert_closures(ctx, e), ctx, ex)
    end
end

function closure_convert_lambda(ctx, ex)
    @assert kind(ex) == K"lambda"
    body_stmts = SyntaxList(ctx)
    toplevel_stmts = ex.is_toplevel_thunk ? body_stmts : ctx.toplevel_stmts
    lambda_bindings = ex.lambda_bindings
    ctx2 = ClosureConversionCtx(ctx.graph, ctx.bindings, ctx.mod,
                                ctx.closure_bindings, ctx.closure_info, lambda_bindings,
                                toplevel_stmts, ctx.closure_infos)
    lambda_children = SyntaxList(ctx)
    args = ex[1]
    push!(lambda_children, args)
    push!(lambda_children, ex[2])

    # Add box initializations for arguments which are captured by an inner lambda
    for arg in children(args)
        kind(arg) != K"Placeholder" || continue
        binfo = lookup_binding(ctx, arg)
        if binfo.is_captured # TODO: && binfo.is_assigned
            push!(body_stmts, @ast ctx arg [K"="
                arg
                [K"call" "Box"::K"core" arg]
            ])
        end
    end
    # Convert body. Note that _convert_closures may call `push!(body_stmts, e)`
    # internally for any expressions `e` which need to be moved to top level.
    input_body_stmts = kind(ex[3]) != K"block" ? ex[3:3] : ex[3][1:end]
    for e in input_body_stmts
        push!(body_stmts, _convert_closures(ctx2, e))
    end
    push!(lambda_children, @ast ctx2 ex[3] [K"block" body_stmts...])

    if numchildren(ex) > 3
        @assert numchildren(ex) == 4
        push!(lambda_children, _convert_closures(ctx2, ex[4]))
    end

    makenode(ctx, ex, ex, lambda_children; lambda_bindings=lambda_bindings)
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
function convert_closures(ctx::VariableAnalysisContext, ex)
    ctx = ClosureConversionCtx(ctx.graph, ctx.bindings, ctx.mod,
                               ctx.closure_bindings, ex.lambda_bindings)
    ex1 = closure_convert_lambda(ctx, ex)
    ctx, ex1
end
