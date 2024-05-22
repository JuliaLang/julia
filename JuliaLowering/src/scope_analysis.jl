# Lowering pass 3: analyze scopes (passes 2/3 in flisp code)
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
        push!(used_names, VarKey(ex))
    elseif !haschildren(ex) || is_quoted(k) ||
            k in KSet"scope_block lambda module toplevel"
        return
    elseif k == K"local" || k == K"local_def"
        get!(locals, VarKey(ex[1]), ex)
    elseif k == K"global"
        get!(globals, VarKey(ex[1]), ex)
    # elseif k == K"method" TODO static parameters
    elseif k == K"="
        v = decl_var(ex[1])
        if !(kind(v) in KSet"SSAValue globalref outerref" || is_placeholder(v))
            get!(assignments, VarKey(v), v)
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
# NB: This only works propery after desugaring has already processed assignments
function find_scope_vars(ex)
    ExT = typeof(ex)
    assignments = Dict{VarKey,ExT}()
    locals = Dict{VarKey,ExT}()
    globals = Dict{VarKey,ExT}()
    used_names = Set{VarKey}()
    for e in children(ex)
        _find_scope_vars!(assignments, locals, globals, used_names, e)
    end
    return assignments, locals, globals, used_names
end

"""
Key to use when looking up variables, composed of the name and scope layer.
"""
struct VarKey
    name::String
    layer::LayerId
end

# Identifiers produced by lowering will have the following layer by default.
#
# To make new mutable variables without colliding names, lowering can
# - generate new var_id's directly (like the gensyms used by the old system)
# - create additional layers, though this may be unnecessary
const _lowering_internal_layer = -1

function VarKey(ex::SyntaxTree)
    @chk kind(ex) == K"Identifier"
    VarKey(ex.name_val, get(ex, :scope_layer, _lowering_internal_layer))
end

"""
Metadata about a variable name - whether it's a local, etc
"""
struct VarInfo
    name::String
    mod::Union{Nothing,Module}
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
    var_ids::Dict{VarKey,VarId}
    # Variables used by the enclosing lambda
    lambda_locals::Set{VarId}
end

struct ScopeResolutionContext{GraphType} <: AbstractLoweringContext
    graph::GraphType
    next_var_id::Ref{VarId}
    mod::Module
    scope_layers::Vector{ScopeLayer}
    # name=>id mappings for all discovered global vars
    global_vars::Dict{VarKey,VarId}
    # Stack of name=>id mappings for each scope, innermost scope last.
    scope_stack::Vector{ScopeInfo}
    # Metadata about variables. There's only one map for this, as var_id is is
    # unique across the context, even for same-named vars in unrelated local
    # scopes.
    var_info::Dict{VarId,VarInfo}
    # Variables which were implicitly global due to being assigned to in top
    # level code
    implicit_toplevel_globals::Set{VarKey}
end

function ScopeResolutionContext(ctx)
    graph = ensure_attributes(ctx.graph, lambda_locals=Set{VarId})
    ScopeResolutionContext(graph,
                           ctx.next_var_id,
                           ctx.mod,
                           ctx.scope_layers,
                           Dict{VarKey,VarId}(),
                           Vector{ScopeInfo}(),
                           Dict{VarId,VarInfo}(),
                           Set{VarKey}())
end

function lookup_var(ctx, varkey::VarKey, exclude_toplevel_globals=false)
    for i in lastindex(ctx.scope_stack):-1:1
        ids = ctx.scope_stack[i].var_ids
        id = get(ids, varkey, nothing)
        if !isnothing(id) && (!exclude_toplevel_globals ||
                              i > 1 || ctx.var_info[id].kind != :global)
            return id
        end
    end
    return exclude_toplevel_globals ? nothing : get(ctx.global_vars, varkey, nothing)
end

function var_kind(ctx, id::VarId)
    ctx.var_info[id].kind
end

function var_kind(ctx, varkey::VarKey, exclude_toplevel_globals=false)
    id = lookup_var(ctx, varkey, exclude_toplevel_globals)
    isnothing(id) ? nothing : ctx.var_info[id].kind
end

# FIXME: This name is a misnomer now. It's more like "maybe_new_var" ...
function new_var(ctx, varkey::VarKey, kind::Symbol, is_ambiguous_local=false)
    id = kind === :global ? get(ctx.global_vars, varkey, nothing) : nothing
    if isnothing(id)
        id = new_var_id(ctx)
        mod = kind === :global ? ctx.scope_layers[varkey.layer].mod : nothing
        ctx.var_info[id] = VarInfo(varkey.name, mod, kind, false, is_ambiguous_local)
    end
    if kind === :global
        ctx.global_vars[varkey] = id
    end
    id
end

# Analyze identifier usage within a scope, adding all newly discovered
# identifiers to ctx.var_info and constructing a lookup table from identifier
# names to their variable IDs
function analyze_scope(ctx, ex, scope_type, lambda_info)
    parentscope = isempty(ctx.scope_stack) ? nothing : ctx.scope_stack[end]
    is_outer_lambda_scope = kind(ex) == K"lambda"
    is_toplevel = !isnothing(lambda_info) && lambda_info.is_toplevel_thunk
    in_toplevel_thunk = is_toplevel || (!is_outer_lambda_scope && parentscope.in_toplevel_thunk)

    assignments, locals, globals, used = find_scope_vars(ex)

    # Create new lookup table for variables in this scope which differ from the
    # parent scope.
    var_ids = Dict{VarKey,VarId}()

    # Add lambda arguments
    if !isnothing(lambda_info)
        for a in lambda_info.args
            varkey = VarKey(a)
            var_ids[varkey] = new_var(ctx, varkey, :argument)
        end
        for a in lambda_info.static_parameters
            varkey = VarKey(a)
            var_ids[varkey] = new_var(ctx, varkey, :static_parameter)
        end
    end

    # Add explicit locals
    for (varkey,e) in pairs(locals)
        if haskey(globals, varkey)
            throw(LoweringError(e, "Variable `$(varkey.name)` declared both local and global"))
        elseif haskey(var_ids, varkey)
            vk = ctx.var_info[var_ids[varkey]].kind
            if vk === :argument && is_outer_lambda_scope
                throw(LoweringError(e, "local variable name `$(varkey.name)` conflicts with an argument"))
            elseif vk === :static_parameter
                throw(LoweringError(e, "local variable name `$(varkey.name)` conflicts with a static parameter"))
            end
        elseif var_kind(ctx, varkey) === :static_parameter
            throw(LoweringError(e, "local variable name `$(varkey.name)` conflicts with a static parameter"))
        end
        var_ids[varkey] = new_var(ctx, varkey, :local)
    end

    # Add explicit globals
    for (varkey,e) in pairs(globals)
        if haskey(var_ids, varkey)
            vk = ctx.var_info[var_ids[varkey]].kind
            if vk === :argument && is_outer_lambda_scope
                throw(LoweringError(e, "global variable name `$(varkey.name)` conflicts with an argument"))
            elseif vk === :static_parameter
                throw(LoweringError(e, "global variable name `$(varkey.name)` conflicts with a static parameter"))
            end
        elseif var_kind(ctx, varkey) === :static_parameter
            throw(LoweringError(e, "global variable name `$(varkey.name)` conflicts with a static parameter"))
        end
        var_ids[varkey] = new_var(ctx, varkey, :global)
    end

    # Compute implicit locals and globals
    if is_toplevel
        is_hard_scope = false
        is_soft_scope = false

        # Assignments are implicitly global at top level, unless they come from
        # a macro expansion
        for (varkey,e) in assignments
            vk = haskey(var_ids, varkey) ?
                 ctx.var_info[var_ids[varkey]].kind :
                 var_kind(ctx, varkey, true)
            if vk === nothing
                if ctx.scope_layers[varkey.layer].is_macro_expansion
                    var_ids[varkey] = new_var(ctx, varkey, :local)
                else
                    new_var(ctx, varkey, :global)
                    push!(ctx.implicit_toplevel_globals, varkey)
                end
            end
        end
    else
        is_hard_scope = in_toplevel_thunk && (parentscope.is_hard || scope_type === :hard)
        is_soft_scope = in_toplevel_thunk && !is_hard_scope &&
                        (scope_type === :neutral ? parentscope.is_soft : scope_type === :soft)

        # Outside top level code, most assignments create local variables implicitly
        for (varkey,e) in assignments
            vk = haskey(var_ids, varkey) ?
                 ctx.var_info[var_ids[varkey]].kind :
                 var_kind(ctx, varkey, true)
            if vk === :static_parameter
                throw(LoweringError(e, "local variable name `$(varkey.name)` conflicts with a static parameter"))
            elseif vk !== nothing
                continue
            end
            # Assignment is to a newly discovered variable name
            is_ambiguous_local = false
            if in_toplevel_thunk && !is_hard_scope
                # In a top level thunk but *inside* a nontrivial scope
                layer = ctx.scope_layers[varkey.layer]
                if !layer.is_macro_expansion && (varkey in ctx.implicit_toplevel_globals ||
                        isdefined(layer.mod, Symbol(varkey.name)))
                    # Special scope rules to make assignments to globals work
                    # like assignments to locals do inside a function.
                    if is_soft_scope
                        # Soft scope (eg, for loop in REPL) => treat as a global
                        new_var(ctx, varkey, :global)
                        continue
                    else
                        # Ambiguous case (eg, nontrivial scopes in package top level code)
                        # => Treat as local but generate warning when assigned to
                        is_ambiguous_local = true
                    end
                end
            end
            var_ids[varkey] = new_var(ctx, varkey, :local, is_ambiguous_local)
        end
    end

    for varkey in used
        if lookup_var(ctx, varkey) === nothing
            # Add other newly discovered identifiers as globals
            new_var(ctx, varkey, :global)
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
        id = lookup_var(ctx, VarKey(ex))
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
        scope = analyze_scope(ctx, ex, nothing, lambda_info)
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
        scope = analyze_scope(ctx, ex, ex.scope_type, nothing)
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

