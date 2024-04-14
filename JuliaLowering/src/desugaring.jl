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
# AST creation utilities
_node_id(ex::NodeId) = ex
_node_id(ex::SyntaxTree) = ex.id

_node_ids() = ()
_node_ids(c, cs...) = (_node_id(c), _node_ids(cs...)...)

function _makenode(graph::SyntaxGraph, srcref, head, children; attrs...)
    id = newnode!(graph)
    # TODO: Having this list of kinds seeems hacky?
    if kind(head) in (K"Identifier", K"core", K"top", K"SSAValue", K"Value", K"slot") || is_literal(head)
        @assert length(children) == 0
    else
        setchildren!(graph, id, children)
    end
    setattr!(graph, id; source=srcref.id, attrs...)
    sethead!(graph, id, head)
    return SyntaxTree(graph, id)
end

function makenode(graph::SyntaxGraph, srcref, head, children...; attrs...)
    _makenode(graph, srcref, head, children; attrs...)
end

function makenode(ctx::Union{AbstractLoweringContext,SyntaxTree},
                  srcref, head, children::SyntaxTree...; attrs...)
    _makenode(ctx.graph, srcref, head, _node_ids(children...); attrs...)
end

function makenode(ctx::Union{AbstractLoweringContext,SyntaxTree},
                  srcref, head, children::SyntaxList; attrs...)
    ctx.graph === children.graph || error("Mismatching graphs")
    _makenode(ctx.graph, srcref, head, children.ids; attrs...)
end

function mapchildren(f, ctx, ex)
    cs = SyntaxList(ctx)
    for e in children(ex)
        push!(cs, f(e))
    end
    ex2 = makenode(ctx, ex, head(ex), cs)
    # Copy all attributes.
    # TODO: Make this type stable and efficient
    for v in values(ex.graph.attributes)
        if haskey(v, ex.id)
            v[ex2.id] = v[ex.id]
        end
    end
    return ex2
end

function syntax_graph(ctx::AbstractLoweringContext)
    ctx.graph
end

function new_var_id(ctx::AbstractLoweringContext)
    id = ctx.next_var_id[]
    ctx.next_var_id[] += 1
    return id
end

# Create a new SSA variable
function ssavar(ctx::AbstractLoweringContext, srcref)
    id = makenode(ctx, srcref, K"SSAValue", var_id=new_var_id(ctx))
    return id
end

# Assign `ex` to an SSA variable.
# Return (variable, assignment_node)
function assign_tmp(ctx::AbstractLoweringContext, ex)
    var = ssavar(ctx, ex)
    assign_var = makenode(ctx, ex, K"=", var, ex)
    var, assign_var
end

# Convenience functions to create leaf nodes referring to identifiers within
# the Core and Top modules.
core_ref(ctx, ex, name) = makenode(ctx, ex, K"core", name_val=name)
Any_type(ctx, ex) = core_ref(ctx, ex, "Any")
svec_type(ctx, ex) = core_ref(ctx, ex, "svec")
nothing_(ctx, ex) = core_ref(ctx, ex, "nothing")
unused(ctx, ex) = core_ref(ctx, ex, "UNUSED")

top_ref(ctx, ex, name) = makenode(ctx, ex, K"top", name_val=name)

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
# Lowering Pass 1 - basic desugaring
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
        return makenode(ctx, ex, K"block", blk;
                        scope_type=scope_type)
    end
    for binding in Iterators.reverse(children(ex[1]))
        kb = kind(binding)
        if is_sym_decl(kb)
            blk = makenode(ctx, ex, K"block",
                makenode(ctx, ex, K"local", binding),
                blk;
                scope_type=scope_type
            )
        elseif kb == K"=" && numchildren(binding) == 2
            lhs = binding[1]
            rhs = binding[2]
            if is_sym_decl(lhs)
                tmp, tmpdef = assign_tmp(ctx, rhs)
                blk = makenode(ctx, binding, K"block",
                    tmpdef,
                    makenode(ctx, ex, K"block",
                        makenode(ctx, lhs, K"local_def", lhs), # TODO: Use K"local" with attr?
                        makenode(ctx, rhs, K"=", decl_var(lhs), tmp),
                        blk;
                        scope_type=scope_type
                    )
                )
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
    makenode(ctx, ex, K"call", cs...)
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
        return makenode(ctx, ex, K"method", identifier_name(name))
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
                farg = makenode(ctx, name, K"::",
                                makenode(ctx, name, K"Identifier", name_val="#self#"),
                                name[1])
            else
                TODO("Fixme type")
                farg = name
            end
            function_name = nothing_(ctx, ex)
        else
            if !is_valid_name(name)
                throw(LoweringError(name, "Invalid function name"))
            end
            farg = makenode(ctx, name, K"::",
                            makenode(ctx, name, K"Identifier", name_val="#self#"),
                            makenode(ctx, name, K"call", core_ref(ctx, name, "Typeof"), name))
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
                atype = makenode(K"curly", core_ref(ctx, arg, "Vararg"), arg)
            end
            push!(arg_types, atype)
        end

        preamble = makenode(ctx, ex, K"call",
                            svec_type(ctx, callex),
                            makenode(ctx, callex, K"call",
                                     svec_type(ctx, name),
                                     arg_types...),
                            makenode(ctx, callex, K"call",
                                     svec_type(ctx, name)), # FIXME sparams
                            makenode(ctx, callex, K"Value", value=QuoteNode(source_location(LineNumberNode, callex)))
                           )
        if !isnothing(return_type)
            ret_var, ret_assign = assign_tmp(ctx, return_type)
            body = makenode(ctx, body, K"block",
                            ret_assign,
                            body)
        else
            ret_var = nothing
        end
        lambda = makenode(ctx, body, K"lambda", body,
                          lambda_info=LambdaInfo(arg_names, static_parameters, ret_var, false))
        makenode(ctx, ex, K"block",
                 makenode(ctx, ex, K"method", function_name),
                 makenode(ctx, ex, K"method",
                          function_name,
                          preamble,
                          lambda),
                 makenode(ctx, ex, K"unnecessary", function_name))
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
        makenode(ctx, ex, K"Identifier", name_val=ex.name_val)
    elseif k == K"char" || k == K"var"
        @chk numchildren(ex) == 1
        ex[1]
    elseif k == K"string"
        if numchildren(ex) == 1 && kind(ex[1]) == K"String"
            ex[1]
        else
            makenode(ctx, ex, K"call", top_ref(ctx, ex, "string"), expand_forms(ctx, children(ex))...)
        end
    elseif k == K"tuple"
        # TODO: named tuples
        makenode(ctx, ex, K"call", core_ref(ctx, ex, "tuple"), expand_forms(ctx, children(ex))...)
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

