struct ClosureConversionCtx{GraphType} <: AbstractLoweringContext
    graph::GraphType
    bindings::Bindings
    mod::Module
    lambda_locals::Set{IdTag}
end

function add_lambda_local!(ctx::ClosureConversionCtx, id)
    push!(ctx.lambda_locals, id)
end

function convert_for_type_decl(ctx, srcref, ex, type, do_typeassert)
    # Require that the caller make `type` "simple", for now (can generalize
    # later if necessary)
    kt = kind(type)
    @assert (kt == K"Identifier" || kt == K"BindingId" || is_literal(kt))
    # Use a slot to permit union-splitting this in inference
    tmp = new_mutable_var(ctx, srcref, "tmp")

    @ast ctx srcref [K"block"
        # [K"local_def" tmp]
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
        # TODO: convert-global-assignment
        if !isnothing(binfo.type)
            TODO(ex, "Typed global assignment??")
        end
        @ast ctx ex [K"=" var rhs0]
    else
        closed   = false # TODO
        captured = false # TODO
        @assert binfo.kind == :local
        if isnothing(binfo.type) && !closed && !captured
            @ast ctx ex [K"=" var rhs0]
        else
            @assert binfo.kind == :local
            # Typed local
            tmp_rhs0 = is_simple_atom(ctx, rhs0) || kind(rhs0) == K"the_exception" ?
                nothing : ssavar(ctx, rhs0)
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
        binfo = lookup_binding(ctx, ex[1])
        if binfo.kind == :local
            makeleaf(ctx, ex, K"TOMBSTONE")
        else
            # Remaining `decl` expressions are type assertions if the argument is global
            # (TODO: Maybe we should remove the useless ones in
            #  analyze_variables() pass, or convert to `::`??)
            TODO(ex, "global variables with type assertions")
        end
    elseif k == K"lambda"
        ctx2 = ClosureConversionCtx(ctx.graph, ctx.bindings, ctx.mod, ex.lambda_locals)
        mapchildren(e->_convert_closures(ctx2, e), ctx2, ex)
    else
        mapchildren(e->_convert_closures(ctx, e), ctx, ex)
    end
end


"""
Closure conversion and lowering of bindings

This pass does a few things things:
* Deal with typed variables (K"decl") and their assignments
* Deal with global assignments
* Convert closures into types

Invariants:
* This pass must not introduce new K"Identifier" - only K"BindingId".
* Any new binding IDs must be added to the enclosing lambda locals
"""
function convert_closures(ctx::ScopeResolutionContext, ex)
    @assert kind(ex) == K"lambda"
    ctx = ClosureConversionCtx(ctx.graph, ctx.bindings, ctx.mod, ex.lambda_locals)
    ex1 = _convert_closures(ctx, ex)
    ctx, ex1
end
