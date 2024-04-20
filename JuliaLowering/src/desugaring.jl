"""
Unique symbolic identity for a variable within a `DesugaringContext`
"""
const VarId = Int

struct SSAVar
    id::VarId
end

struct LambdaInfo
    # TODO: Make SyntaxList concretely typed?
    args::SyntaxList
    static_parameters::SyntaxList
    ret_var::Union{Nothing,SyntaxTree}
    is_toplevel_thunk::Bool
end

struct DesugaringContext{GraphType} <: AbstractLoweringContext
    graph::GraphType
    next_var_id::Ref{VarId}
end

function DesugaringContext(ctx)
    graph = syntax_graph(ctx)
    ensure_attributes!(graph,
                       kind=Kind, syntax_flags=UInt16, green_tree=GreenNode,
                       source_pos=Int, source=Union{SourceRef,NodeId},
                       value=Any, name_val=String,
                       scope_type=Symbol, # :hard or :soft
                       var_id=VarId,
                       lambda_info=LambdaInfo)
    DesugaringContext(freeze_attrs(graph), Ref{VarId}(1))
end

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Predicates and accessors working on expression trees

function is_quoted(ex)
    kind(ex) in KSet"quote top core globalref outerref break inert
                     meta inbounds inline noinline loopinfo"
end

function is_sym_decl(x)
    k = kind(x)
    k == K"Identifier" || k == K"::"
end

# Identifier made of underscores
function is_placeholder(ex)
    kind(ex) == K"Identifier" && all(==('_'), ex.name_val)
end

function is_identifier(x)
    k = kind(x)
    k == K"Identifier" || k == K"var" || is_operator(k) || is_macro_name(k)
end

function is_eventually_call(ex::SyntaxTree)
    k = kind(ex)
    return k == K"call" || ((k == K"where" || k == K"::") && is_eventually_call(ex[1]))
end

function is_function_def(ex)
    k = kind(ex)
    return k == K"function" || k == K"->" ||
        (k == K"=" && numchildren(ex) == 2 && is_eventually_call(ex[1]))
end

function identifier_name(ex)
    kind(ex) == K"var" ? ex[1] : ex
end

function is_valid_name(ex)
    n = identifier_name(ex).name_val
    n !== "ccall" && n !== "cglobal"
end

function decl_var(ex)
    kind(ex) == K"::" ? ex[1] : ex
end

# given a complex assignment LHS, return the symbol that will ultimately be assigned to
function assigned_name(ex)
    k = kind(ex)
    if (k == K"call" || k == K"curly" || k == K"where") || (k == K"::" && is_eventually_call(ex))
        assigned_name(ex[1])
    else
        ex
    end
end

#-------------------------------------------------------------------------------
# TODO: Lowering pass 1.1:
# Aim of this pass is to do some super simple normalizations to make
# desugaring-proper easier to write. The kinds of things like identifier
# normalization which would require extra logic to pervade the remaining
# desugaring.
#
# * Identifier normalization
#   - Strip var""
#   - Operator -> Identifier if necessary
# * Strip "container" nodes
#   - K"char"
#   - K"parens" nodes
# * Quasiquote expansion

#-------------------------------------------------------------------------------
# Lowering Pass 1.2 - desugaring
function expand_assignment(ctx, ex)
end

function expand_condition(ctx, ex)
    if head(ex) == K"block" || head(ex) == K"||" || head(ex) == K"&&"
        # || and && get special lowering so that they compile directly to jumps
        # rather than first computing a bool and then jumping.
        error("TODO expand_condition")
    end
    expand_forms(ctx, ex)
end

function expand_let(ctx, ex)
    scope_type = get(ex, :scope_type, :hard)
    blk = ex[2]
    if numchildren(ex[1]) == 0 # TODO: Want to use !haschildren(ex[1]) but this doesn't work...
        return @ast ctx ex [K"scope_block"(scope_type=scope_type) blk]
    end
    for binding in Iterators.reverse(children(ex[1]))
        kb = kind(binding)
        if is_sym_decl(kb)
            blk = @ast ctx ex [
                K"scope_block"(scope_type=scope_type)
                [K"local" binding]
                blk
            ]
        elseif kb == K"=" && numchildren(binding) == 2
            lhs = binding[1]
            rhs = binding[2]
            if is_sym_decl(lhs)
                blk = @ast ctx binding [
                    K"block"
                    tmp=rhs
                    [K"scope_block"(ex, scope_type=scope_type)
                        [K"local_def"(lhs) lhs] # TODO: Use K"local" with attr?
                        [K"="(rhs)
                            decl_var(lhs)
                            tmp
                        ]
                        blk
                    ]
                ]
            else
                TODO("Functions and multiple assignment")
            end
        else
            throw(LoweringError(binding, "Invalid binding in let"))
            continue
        end
    end
    return blk
end

function expand_call(ctx, ex)
    cs = expand_forms(ctx, children(ex))
    if is_infix_op_call(ex) || is_postfix_op_call(ex)
        cs[1], cs[2] = cs[2], cs[1]
    end
    # TODO: keywords
    @ast ctx ex [K"call" cs...]
end

# Strip variable type declarations from within a `local` or `global`, returning
# the stripped expression. Works recursively with complex left hand side
# assignments containing tuple destructuring. Eg, given
#   (x::T, (y::U, z))
#   strip out stmts = (local x) (decl x T) (local x) (decl y U) (local z)
#   and return (x, (y, z))
function strip_decls!(ctx, stmts, declkind, ex)
    k = kind(ex)
    if k == K"Identifier"
        push!(stmts, makenode(ctx, ex, declkind, ex))
        ex
    elseif k == K"::"
        @chk numchildren(ex) == 2
        name = ex[1]
        @chk kind(name) == K"Identifier"
        push!(stmts, makenode(ctx, ex, declkind, name))
        push!(stmts, makenode(ctx, ex, K"decl", name, ex[2]))
        name
    elseif k == K"tuple" || k == K"parameters"
        cs = SyntaxList(ctx)
        for e in children(ex)
            push!(cs, strip_decls!(ctx, stmts, declkind, e))
        end
        makenode(ctx, ex, k, cs)
    end
end

# local x, (y=2), z => local x; local y; y = 2; local z
function expand_decls(ctx, ex)
    declkind = kind(ex)
    stmts = SyntaxList(ctx)
    for binding in children(ex)
        kb = kind(binding)
        if is_function_def(binding)
            push!(stmts, makenode(ctx, binding, declkind, assigned_name(binding)))
            push!(stmts, binding)
        elseif is_prec_assignment(kb)
            lhs = strip_decls!(ctx, stmts, declkind, binding[1])
            push!(stmts, makenode(ctx, binding, kb, lhs, binding[2]))
        elseif is_sym_decl(binding)
            strip_decls!(ctx, stmts, declkind, binding)
        else
            throw(LoweringError("invalid syntax in variable declaration"))
        end
    end
    makenode(ctx, ex, K"block", stmts)
end

function analyze_function_arg(full_ex)
    name = nothing
    type = nothing
    default = nothing
    is_slurp = false
    is_nospecialize = false
    ex = full_ex
    while true
        k = kind(ex)
        if k == K"Identifier" || k == K"tuple"
            name = ex
            break
        elseif k == K"::"
            @chk numchildren(ex) in (1,2)
            if numchildren(ex) == 1
                type = ex[1]
            else
                name = ex[1]
                type = ex[2]
            end
            break
        elseif k == K"..."
            @chk full_ex !is_slurp
            @chk numchildren(ex) == 1
            is_slurp = true
            ex = ex[1]
        elseif k == K"meta"
            @chk ex[1].name_val == "nospecialize"
            is_nospecialize = true
            ex = ex[2]
        elseif k == K"="
            @chk full_ex isnothing(default) && !is_slurp
            default = ex[2]
            ex = ex[1]
        else
            throw(LoweringError(ex, "Invalid function argument"))
        end
    end
    return (name=name,
            type=type,
            default=default,
            is_slurp=is_slurp,
            is_nospecialize=is_nospecialize)
end

function expand_function_def(ctx, ex)
    @chk numchildren(ex) in (1,2)
    name = ex[1]
    if kind(name) == K"where"
        TODO("where handling")
    end
    return_type = nothing
    if kind(name) == K"::"
        @chk numchildren(name) == 2
        return_type = name[2]
        name = name[1]
    end
    if numchildren(ex) == 1 && is_identifier(name) # TODO: Or name as globalref
        if !is_valid_name(name)
            throw(LoweringError(name, "Invalid function name"))
        end
        return @ast ctx ex [K"method" name]
    elseif kind(name) == K"call"
        callex = name
        body = ex[2]
        # TODO
        # static params
        # nospecialize
        # argument destructuring
        # dotop names
        # overlays
        static_parameters = SyntaxList(ctx)

        # Add self argument where necessary
        args = name[2:end]
        name = name[1]
        if kind(name) == K"::"
            if numchildren(name) == 1
                farg = @ast ctx name [K"::"
                    "#self#"::K"Identifier"
                    name[1]
                ]
            else
                TODO("Fixme type")
                farg = name
            end
            function_name = nothing_(ctx, ex)
        else
            if !is_valid_name(name)
                throw(LoweringError(name, "Invalid function name"))
            end
            farg = @ast ctx name [K"::"
                "#self#"::K"Identifier"
                [K"call"
                    "Typeof"::K"core"
                    name
                ]
            ]
            function_name = name
        end
        args = pushfirst!(collect(args), farg)

        # preamble is arbitrary code which computes
        # svec(types, sparms, location)

        arg_names = SyntaxList(ctx)
        arg_types = SyntaxList(ctx)
        for (i,arg) in enumerate(args)
            info = analyze_function_arg(arg)
            aname = (isnothing(info.name) || is_placeholder(info.name)) ?
                    unused(ctx, arg) : info.name
            push!(arg_names, aname)
            atype = !isnothing(info.type) ? info.type : Any_type(ctx, arg)
            @assert !info.is_nospecialize # TODO
            @assert !isnothing(info.name) && kind(info.name) == K"Identifier" # TODO
            if info.is_slurp
                if i != length(args)
                    throw(LoweringError(arg, "`...` may only be used for the last function argument"))
                end
                atype = @ast ctx arg [K"curly" "Vararg"::K"core" arg]
            end
            push!(arg_types, atype)
        end

        preamble = @ast ctx callex [
            K"call"
            "svec"              ::K"core"
            [K"call"
                "svec"          ::K"core"
                arg_types...
            ]
            [K"call"
                "svec"          ::K"core"
                # FIXME sparams
            ]
            QuoteNode(source_location(LineNumberNode, callex))::K"Value"
        ]
        if !isnothing(return_type)
            body = @ast ctx body [
                K"block"
                ret_var=return_type
                body
            ]
        else
            ret_var = nothing
        end
        @ast ctx ex [
            K"block"
            [K"method" function_name]
            [K"method"
                function_name
                preamble
                [K"lambda"(body, lambda_info=LambdaInfo(arg_names, static_parameters, ret_var, false))
                    body
                ]
            ]
            [K"unnecessary" function_name]
        ]
    elseif kind(name) == K"tuple"
        TODO(name, "Anon function lowering")
    else
        throw(LoweringError(name, "Bad function definition"))
    end
end

function expand_forms(ctx::DesugaringContext, ex::SyntaxTree)
    k = kind(ex)
    if k == K"call"
        expand_call(ctx, ex)
    elseif k == K"function"
        expand_forms(ctx, expand_function_def(ctx, ex))
    elseif k == K"let"
        expand_forms(ctx, expand_let(ctx, ex))
    elseif k == K"local" || k == K"global"
        if numchildren(ex) == 1 && kind(ex[1]) == K"Identifier"
            # Don't recurse when already simplified - `local x`, etc
            ex
        else
            expand_forms(ctx, expand_decls(ctx, ex)) # FIXME
        end
    elseif is_operator(k) && !haschildren(ex)
        makeleaf(ctx, ex, K"Identifier", ex.name_val)
    elseif k == K"char" || k == K"var"
        @chk numchildren(ex) == 1
        ex[1]
    elseif k == K"string"
        if numchildren(ex) == 1 && kind(ex[1]) == K"String"
            ex[1]
        else
            @ast ctx ex [K"call" 
                "string"::K"top"
                expand_forms(ctx, children(ex))...
            ]
        end
    elseif k == K"tuple"
        # TODO: named tuples
        @ast ctx ex [K"call" 
            "tuple"::K"core"
            expand_forms(ctx, children(ex))...
        ]
    elseif !haschildren(ex)
        ex
    else
        if k == K"="
            @chk numchildren(ex) == 2
            if kind(ex[1]) ∉ (K"Identifier", K"SSAValue")
                TODO(ex, "destructuring assignment")
            end
        end
        mapchildren(e->expand_forms(ctx,e), ctx, ex)
    end
end

function expand_forms(ctx::DesugaringContext, exs::Union{Tuple,AbstractVector})
    res = SyntaxList(ctx)
    for e in exs
        push!(res, expand_forms(ctx, e))
    end
    res
end

function expand_forms(ex::SyntaxTree)
    ctx = DesugaringContext(ex)
    res = expand_forms(ctx, reparent(ctx, ex))
    ctx, res
end

