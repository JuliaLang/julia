struct ClosureConversionCtx{GraphType} <: AbstractLoweringContext
    graph::GraphType
    bindings::Bindings
    mod::Module
    lambda_bindings::LambdaBindings
end

function add_lambda_local!(ctx::ClosureConversionCtx, id)
    init_lambda_binding(ctx.lambda_bindings, id)
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
    tmp = new_mutable_var(ctx, srcref, "tmp", is_always_defined=true)

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
        closed   = false # TODO
        captured = false # TODO
        @assert binfo.kind == :local
        if isnothing(binfo.type) && !closed && !captured
            @ast ctx ex [K"=" var rhs0]
        else
            @assert binfo.kind == :local
            # Typed local
            tmp_rhs0 = is_simple_atom(ctx, rhs0) ? nothing : ssavar(ctx, rhs0)
            rhs1 = isnothing(tmp_rhs0) ? rhs0 : tmp_rhs0
            rhs = isnothing(binfo.type) ? rhs1 :
                  convert_for_type_decl(ctx, ex, rhs1, _convert_closures(ctx, binfo.type), true)
            assgn = if closed
                @assert false # TODO
            elseif captured
                @assert false # TODO
            else
                @ast ctx ex [K"=" var rhs]
            end
            if isnothing(tmp_rhs0)
                @ast ctx ex [K"block"
                    assgn
                    rhs0
                ]
            else
                @ast ctx ex [K"block"
                    [K"=" tmp_rhs0 rhs0]
                    assgn
                    tmp_rhs0
                ]
            end
        end
    end
end

function _convert_closures(ctx::ClosureConversionCtx, ex)
    k = kind(ex)
    if k == K"BindingId"
        # TODO: Captures etc
        ex
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
    elseif k == K"::"
        _convert_closures(ctx,
            @ast ctx ex [K"call"
                "typeassert"::K"core"
                children(ex)...
        ])
    elseif k == K"lambda"
        ctx2 = ClosureConversionCtx(ctx.graph, ctx.bindings, ctx.mod, ex.lambda_bindings)
        mapchildren(e->_convert_closures(ctx2, e), ctx2, ex)
    else
        mapchildren(e->_convert_closures(ctx, e), ctx, ex)
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
function convert_closures(ctx::ScopeResolutionContext, ex)
    @assert kind(ex) == K"lambda"
    ctx = ClosureConversionCtx(ctx.graph, ctx.bindings, ctx.mod, ex.lambda_bindings)
    ex1 = _convert_closures(ctx, ex)
    ctx, ex1
end
