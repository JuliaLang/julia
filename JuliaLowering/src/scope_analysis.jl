# Lowering pass 2: analyze scopes (passes 2/3 in flisp code)
#
# This pass analyzes the names (variables/constants etc) used in scopes
#
# This pass records information about variables used by closure conversion.
# finds which variables are assigned or captured, and records variable
# type declarations.
#
# This info is recorded by setting the second argument of `lambda` expressions
# in-place to
#   (var-info-lst captured-var-infos ssavalues static_params)
# where var-info-lst is a list of var-info records

#-------------------------------------------------------------------------------
# AST traversal functions - useful for performing non-recursive AST traversals
# function _schedule_traverse(stack, e)
#     push!(stack, e)
#     return nothing
# end
# function _schedule_traverse(stack, es::Union{Tuple,AbstractVector,Base.Generator})
#     append!(stack, es)
#     return nothing
# end
#
# function traverse_ast(f, exs)
#     todo = SyntaxList(first(exs).graph)
#     append!(todo, exs)
#     while !isempty(todo)
#         f(pop!(todo), e->_schedule_traverse(todo, e))
#     end
# end
#
# function traverse_ast(f, ex::SyntaxTree)
#     traverse_ast(f, (ex,))
# end
#
# function find_in_ast(f, ex::SyntaxTree)
#     todo = SyntaxList(ex.graph)
#     push!(todo, ex)
#     while !isempty(todo)
#         e1 = pop!(todo)
#         res = f(e1, e->_schedule_traverse(todo, e))
#         if !isnothing(res)
#             return res
#         end
#     end
#     return nothing
# end


#-------------------------------------------------------------------------------
function _find_scope_vars!(assignments, locals, globals, used_names, ex)
    k = kind(ex)
    if k == K"Identifier"
        push!(used_names, ex.name_val)
    elseif !haschildren(ex) || is_quoted(k) ||
            k in KSet"scope_block lambda module toplevel"
        return
    elseif k == K"local" || k == K"local_def"
        name = ex[1].name_val
        get!(locals, name, ex)
    elseif k == K"global"
        name = ex[1].name_val
        get!(globals, name, ex)
    # elseif k == K"method" TODO static parameters
    elseif k == K"="
        v = decl_var(ex[1])
        if !(kind(v) in KSet"SSAValue globalref outerref" || is_placeholder(v))
            get!(assignments, v.name_val, v)
        end
        _find_scope_vars!(assignments, locals, globals, used_names, ex[2])
    else
        for e in children(ex)
            _find_scope_vars!(assignments, locals, globals, used_names, e)
        end
    end
end

# Find names of all identifiers used in the given expression, grouping them
# into sets by type.
#
# NB: This only works propery after expand_forms has already processed assignments
function find_scope_vars(ex)
    ExT = typeof(ex)
    assignments = Dict{String,ExT}()
    locals = Dict{String,ExT}()
    globals = Dict{String,ExT}()
    used_names = Set{String}()
    for e in children(ex)
        _find_scope_vars!(assignments, locals, globals, used_names, e)
    end
    return assignments, locals, globals, used_names
end

"""
Metadata about a variable name - whether it's a local, etc
"""
struct VarInfo
    name::String              # Variable name
    kind::Symbol              # :local :global :argument :static_parameter
    is_single_assign::Bool    # Single assignment
    is_ambiguous_local::Bool  # Local, but would be global in soft scope (ie, the REPL)
end

struct ScopeInfo
    # True if scope is part of top level code, or a non-lambda scope nested
    # inside top level code. Thus requiring special scope resolution rules.
    in_toplevel_thunk::Bool
    # Soft/hard scope. For top level thunks only
    is_soft::Bool
    is_hard::Bool
    # Map from variable names to IDs which appear in this scope but not in the
    # parent scope
    var_ids::Dict{String,VarId}
    # Variables used by the enclosing lambda
    lambda_locals::Set{VarId}
end

struct ScopeResolutionContext{GraphType} <: AbstractLoweringContext
    graph::GraphType
    next_var_id::Ref{VarId}
    mod::Module
    # name=>id mappings for all discovered global vars
    global_vars::Dict{String,VarId}
    # Stack of name=>id mappings for each scope, innermost scope last.
    scope_stack::Vector{ScopeInfo}
    # Metadata about variables. There's only one map for this, as var_id is is
    # unique across the context, even for same-named vars in unrelated local
    # scopes.
    var_info::Dict{VarId,VarInfo}
    # Variables which were implicitly global due to being assigned to in top
    # level code
    implicit_toplevel_globals::Set{String}
end

function ScopeResolutionContext(ctx)
    graph = ensure_attributes(ctx.graph, lambda_locals=Set{VarId})
    ScopeResolutionContext(graph,
                           ctx.next_var_id,
                           ctx.mod,
                           Dict{String,VarId}(),
                           Vector{ScopeInfo}(),
                           Dict{VarId,VarInfo}(),
                           Set{String}())
end

function lookup_var(ctx, name::String, exclude_toplevel_globals=false)
    for i in lastindex(ctx.scope_stack):-1:1
        ids = ctx.scope_stack[i].var_ids
        id = get(ids, name, nothing)
        if !isnothing(id) && (!exclude_toplevel_globals ||
                              i > 1 || ctx.var_info[id].kind != :global)
            return id
        end
    end
    return exclude_toplevel_globals ? nothing : get(ctx.global_vars, name, nothing)
end

function current_scope(ctx)
    last(ctx.scope_stack)
end

function var_kind(ctx, id::VarId)
    ctx.var_info[id].kind
end

function var_kind(ctx, name::String, exclude_toplevel_globals=false)
    id = lookup_var(ctx, name, exclude_toplevel_globals)
    isnothing(id) ? nothing : ctx.var_info[id].kind
end

function new_var(ctx, name, kind, is_ambiguous_local=false)
    id = kind === :global ? get(ctx.global_vars, name, nothing) : nothing
    if isnothing(id)
        id = new_var_id(ctx)
        ctx.var_info[id] = VarInfo(name, kind, false, is_ambiguous_local)
    end
    if kind === :global
        ctx.global_vars[name] = id
    end
    id
end

# Analyze identifier usage within a scope, adding all newly discovered
# identifiers to ctx.var_info and constructing a lookup table from identifier
# names to their variable IDs
function make_scope(ctx, ex, scope_type, lambda_info)
    parentscope = isempty(ctx.scope_stack) ? nothing : current_scope(ctx)
    is_outer_lambda_scope = kind(ex) == K"lambda"
    is_toplevel = !isnothing(lambda_info) && lambda_info.is_toplevel_thunk
    in_toplevel_thunk = is_toplevel || (!is_outer_lambda_scope && parentscope.in_toplevel_thunk)

    assignments, locals, globals, used = find_scope_vars(ex)

    # Create new lookup table for variables in this scope which differ from the
    # parent scope.
    var_ids = Dict{String,VarId}()

    # Add lambda arguments
    if !isnothing(lambda_info)
        for a in lambda_info.args
            var_ids[a.name_val] = new_var(ctx, a.name_val, :argument)
        end
        for a in lambda_info.static_parameters
            var_ids[a.name_val] = new_var(ctx, a.name_val, :static_parameter)
        end
    end

    # Add explicit locals
    for (name,e) in pairs(locals)
        if haskey(globals, name)
            throw(LoweringError(e, "Variable `$name` declared both local and global"))
        elseif haskey(var_ids, name)
            vk = ctx.var_info[var_ids[name]].kind
            if vk === :argument && is_outer_lambda_scope
                throw(LoweringError(e, "local variable name `$name` conflicts with an argument"))
            elseif vk === :static_parameter
                throw(LoweringError(e, "local variable name `$name` conflicts with a static parameter"))
            end
        elseif var_kind(ctx, name) === :static_parameter
            throw(LoweringError(e, "local variable name `$name` conflicts with a static parameter"))
        end
        var_ids[name] = new_var(ctx, name, :local)
    end

    # Add explicit globals
    for (name,e) in pairs(globals)
        if haskey(var_ids, name)
            vk = ctx.var_info[var_ids[name]].kind
            if vk === :argument && is_outer_lambda_scope
                throw(LoweringError(e, "global variable name `$name` conflicts with an argument"))
            elseif vk === :static_parameter
                throw(LoweringError(e, "global variable name `$name` conflicts with a static parameter"))
            end
        elseif var_kind(ctx, name) === :static_parameter
            throw(LoweringError(e, "global variable name `$name` conflicts with a static parameter"))
        end
        var_ids[name] = new_var(ctx, name, :global)
    end

    # Compute implicit locals and globals
    if is_toplevel
        is_hard_scope = false
        is_soft_scope = false

        # All non-local assignments are implicitly global at top level
        for (name,e) in assignments
            if !haskey(locals, name)
                new_var(ctx, name, :global)
                push!(ctx.implicit_toplevel_globals, name)
            end
        end
    else
        is_hard_scope = in_toplevel_thunk && (parentscope.is_hard || scope_type === :hard)
        is_soft_scope = in_toplevel_thunk && !is_hard_scope &&
                        (scope_type === :neutral ? parentscope.is_soft : scope_type === :soft)

        # Outside top level code, most assignments create local variables implicitly
        for (name,e) in assignments
            vk = haskey(var_ids, name) ?
                 ctx.var_info[var_ids[name]].kind :
                 var_kind(ctx, name, true)
            if vk === :static_parameter
                throw(LoweringError(e, "local variable name `$name` conflicts with a static parameter"))
            elseif vk !== nothing
                continue
            end
            # Assignment is to a newly discovered variable name
            is_ambiguous_local = false
            if in_toplevel_thunk && !is_hard_scope
                # In a top level thunk but *inside* a nontrivial scope
                if (name in ctx.implicit_toplevel_globals || isdefined(ctx.mod, Symbol(name)))
                    # Special scope rules to make assignments to globals work
                    # like assignments to locals do inside a function.
                    if is_soft_scope
                        # Soft scope (eg, for loop in REPL) => treat as a global
                        new_var(ctx, name, :global)
                        continue
                    else
                        # Ambiguous case (eg, nontrivial scopes in package top level code)
                        # => Treat as local but generate warning when assigned to
                        is_ambiguous_local = true
                    end
                end
            end
            var_ids[name] = new_var(ctx, name, :local, is_ambiguous_local)
        end
    end

    for name in used
        if lookup_var(ctx, name) === nothing
            # Add other newly discovered identifiers as globals
            new_var(ctx, name, :global)
        end
    end

    lambda_locals = is_outer_lambda_scope ? Set{VarId}() : parentscope.lambda_locals
    for id in values(var_ids)
        vk = var_kind(ctx, id)
        if vk === :local
            push!(lambda_locals, id)
        end
    end

    return ScopeInfo(in_toplevel_thunk, is_soft_scope, is_hard_scope, var_ids, lambda_locals)
end

function _resolve_scopes!(ctx, ex)
    k = kind(ex)
    if k == K"Identifier"
        if is_placeholder(ex)
            return # FIXME - make these K"placeholder"?
        end
        # TODO: Maybe we shouldn't do this in place??
        id = lookup_var(ctx, ex.name_val)
        setattr!(ctx.graph, ex.id, var_id=id)
    elseif !haschildren(ex) || is_quoted(ex) || k == K"toplevel"
        return
    # TODO
    # elseif k == K"global"
    # elseif k == K"local"
    # elseif require_existing_local
    # elseif locals # return Dict of locals
    # elseif islocal
    elseif k == K"lambda"
        lambda_info = ex.lambda_info
        scope = make_scope(ctx, ex, nothing, lambda_info)
        push!(ctx.scope_stack, scope)
        # Resolve args and static parameters so that variable IDs get pushed
        # back into the original tree (not required for downstream processing)
        for a in lambda_info.args
            resolve_scopes!(ctx, a)
        end
        for a in lambda_info.static_parameters
            resolve_scopes!(ctx, a)
        end
        for e in children(ex)
            _resolve_scopes!(ctx, e)
        end
        pop!(ctx.scope_stack)
        setattr!(ctx.graph, ex.id, lambda_locals=scope.lambda_locals)
    elseif k == K"scope_block"
        scope = make_scope(ctx, ex, ex.scope_type, nothing)
        push!(ctx.scope_stack, scope)
        for e in children(ex)
            _resolve_scopes!(ctx, e)
        end
        pop!(ctx.scope_stack)
    else
        for e in children(ex)
            _resolve_scopes!(ctx, e)
        end
    end
    ex
end

function resolve_scopes!(ctx::ScopeResolutionContext, ex)
    thunk = makenode(ctx, ex, K"lambda", ex;
                     lambda_info=LambdaInfo(SyntaxList(ctx), SyntaxList(ctx), nothing, true))
    _resolve_scopes!(ctx, thunk)
    return thunk
end

function resolve_scopes!(ctx::DesugaringContext, ex)
    ctx2 = ScopeResolutionContext(ctx)
    res = resolve_scopes!(ctx2, reparent(ctx2, ex))
    ctx2, res
end

