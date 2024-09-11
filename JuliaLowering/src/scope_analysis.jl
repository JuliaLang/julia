# Lowering pass 3: analyze scopes (passes 2+3 in flisp code)

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
#     todo = SyntaxList(ex._graph)
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

"""
Key to use when transforming names into bindings
"""
struct NameKey
    name::String
    layer::LayerId
end

#-------------------------------------------------------------------------------
function _find_scope_vars!(assignments, locals, globals, used_names, used_bindings, ex)
    k = kind(ex)
    if k == K"Identifier"
        push!(used_names, NameKey(ex))
    elseif k == K"BindingId"
        push!(used_bindings, ex.var_id)
    elseif is_leaf(ex) || is_quoted(k) ||
            k in KSet"scope_block lambda module toplevel"
        return
    elseif k == K"local" || k == K"local_def"
        get!(locals, NameKey(ex[1]), ex)
    elseif k == K"global"
        get!(globals, NameKey(ex[1]), ex)
    # elseif k == K"method" TODO static parameters
    elseif k == K"="
        v = decl_var(ex[1])
        if !(kind(v) in KSet"BindingId globalref outerref Placeholder")
            get!(assignments, NameKey(v), v)
        end
        _find_scope_vars!(assignments, locals, globals, used_names, used_bindings, ex[2])
    else
        for e in children(ex)
            _find_scope_vars!(assignments, locals, globals, used_names, used_bindings, e)
        end
    end
end

# Find names of all identifiers used in the given expression, grouping them
# into sets by type.
#
# NB: This only works propery after desugaring has already processed assignments
function find_scope_vars(ex)
    ExT = typeof(ex)
    assignments = Dict{NameKey,ExT}()
    locals = Dict{NameKey,ExT}()
    globals = Dict{NameKey,ExT}()
    used_names = Set{NameKey}()
    used_bindings = Set{IdTag}()
    for e in children(ex)
        _find_scope_vars!(assignments, locals, globals, used_names, used_bindings, e)
    end

    # Sort by key so that id generation is deterministic
    assignments = sort(collect(pairs(assignments)), by=first)
    locals = sort(collect(pairs(locals)), by=first)
    globals = sort(collect(pairs(globals)), by=first)
    used_names = sort(collect(used_names))
    used_bindings = sort(collect(used_bindings))

    return assignments, locals, globals, used_names, used_bindings
end

function Base.isless(a::NameKey, b::NameKey)
    (a.name, a.layer) < (b.name, b.layer)
end

# Identifiers produced by lowering will have the following layer by default.
#
# To make new mutable variables without colliding names, lowering can
# - generate new var_id's directly (like the gensyms used by the old system)
# - create additional layers, though this may be unnecessary
const _lowering_internal_layer = -1

function NameKey(ex::SyntaxTree)
    @chk kind(ex) == K"Identifier"
    NameKey(ex.name_val, get(ex, :scope_layer, _lowering_internal_layer))
end

struct ScopeInfo
    # True if scope is the global top level scope
    is_toplevel_global_scope::Bool
    # True if scope is part of top level code, or a non-lambda scope nested
    # inside top level code. Thus requiring special scope resolution rules.
    in_toplevel_thunk::Bool
    # Soft/hard scope. For top level thunks only
    is_soft::Bool
    is_hard::Bool
    # Map from variable names to IDs which appear in this scope but not in the
    # parent scope
    # TODO: Rename to `locals` or local_bindings?
    var_ids::Dict{NameKey,IdTag}
    # Variables used by the enclosing lambda
    lambda_locals::Set{IdTag}
end

struct ScopeResolutionContext{GraphType} <: AbstractLoweringContext
    graph::GraphType
    bindings::Bindings
    mod::Module
    scope_layers::Vector{ScopeLayer}
    # name=>id mappings for all discovered global vars
    global_vars::Dict{NameKey,IdTag}
    # Stack of name=>id mappings for each scope, innermost scope last.
    scope_stack::Vector{ScopeInfo}
    # Variables which were implicitly global due to being assigned to in top
    # level code
    implicit_toplevel_globals::Set{NameKey}
end

function ScopeResolutionContext(ctx)
    graph = ensure_attributes(ctx.graph, lambda_locals=Set{IdTag})
    ScopeResolutionContext(graph,
                           ctx.bindings,
                           ctx.mod,
                           ctx.scope_layers,
                           Dict{NameKey,IdTag}(),
                           Vector{ScopeInfo}(),
                           Set{NameKey}())
end

function lookup_var(ctx, varkey::NameKey, exclude_toplevel_globals=false)
    for i in lastindex(ctx.scope_stack):-1:1
        ids = ctx.scope_stack[i].var_ids
        id = get(ids, varkey, nothing)
        if !isnothing(id) && (!exclude_toplevel_globals ||
                              i > 1 || lookup_binding(ctx, id).kind != :global)
            return id
        end
    end
    return exclude_toplevel_globals ? nothing : get(ctx.global_vars, varkey, nothing)
end

function var_kind(ctx, id::IdTag)
    lookup_binding(ctx, id).kind
end

function var_kind(ctx, varkey::NameKey, exclude_toplevel_globals=false)
    id = lookup_var(ctx, varkey, exclude_toplevel_globals)
    isnothing(id) ? nothing : lookup_binding(ctx, id).kind
end

function init_binding(ctx, varkey::NameKey, kind::Symbol, is_ambiguous_local=false)
    id = kind === :global ? get(ctx.global_vars, varkey, nothing) : nothing
    if isnothing(id)
        mod = kind === :global ? ctx.scope_layers[varkey.layer].mod : nothing
        id = new_binding(ctx.bindings,
                         BindingInfo(varkey.name, kind;
                                     mod=mod, is_ambiguous_local=is_ambiguous_local))
    end
    if kind === :global
        ctx.global_vars[varkey] = id
    end
    id
end

# Analyze identifier usage within a scope, adding all newly discovered
# identifiers to ctx.bindings and returning a lookup table from identifier
# names to their variable IDs
function analyze_scope(ctx, ex, scope_type, lambda_info)
    parentscope = isempty(ctx.scope_stack) ? nothing : ctx.scope_stack[end]
    is_outer_lambda_scope = kind(ex) == K"lambda"
    is_toplevel_global_scope = !isnothing(lambda_info) && lambda_info.is_toplevel_thunk
    in_toplevel_thunk = is_toplevel_global_scope ||
        (!is_outer_lambda_scope && parentscope.in_toplevel_thunk)

    assignments, locals, globals, used, used_bindings = find_scope_vars(ex)

    # Create new lookup table for variables in this scope which differ from the
    # parent scope.
    var_ids = Dict{NameKey,IdTag}()

    # Add lambda arguments
    if !isnothing(lambda_info)
        for a in lambda_info.args
            varkey = NameKey(a)
            var_ids[varkey] = init_binding(ctx, varkey, :argument)
        end
        for a in lambda_info.static_parameters
            varkey = NameKey(a)
            var_ids[varkey] = init_binding(ctx, varkey, :static_parameter)
        end
    end

    global_keys = Set(first(g) for g in globals)
    # Add explicit locals
    for (varkey,e) in locals
        if varkey in global_keys
            throw(LoweringError(e, "Variable `$(varkey.name)` declared both local and global"))
        elseif haskey(var_ids, varkey)
            vk = lookup_binding(ctx, var_ids[varkey]).kind
            if vk === :argument && is_outer_lambda_scope
                throw(LoweringError(e, "local variable name `$(varkey.name)` conflicts with an argument"))
            elseif vk === :static_parameter
                throw(LoweringError(e, "local variable name `$(varkey.name)` conflicts with a static parameter"))
            end
        elseif var_kind(ctx, varkey) === :static_parameter
            throw(LoweringError(e, "local variable name `$(varkey.name)` conflicts with a static parameter"))
        end
        var_ids[varkey] = init_binding(ctx, varkey, :local)
    end

    # Add explicit globals
    for (varkey,e) in globals
        if haskey(var_ids, varkey)
            vk = lookup_binding(ctx, var_ids[varkey]).kind
            if vk === :argument && is_outer_lambda_scope
                throw(LoweringError(e, "global variable name `$(varkey.name)` conflicts with an argument"))
            elseif vk === :static_parameter
                throw(LoweringError(e, "global variable name `$(varkey.name)` conflicts with a static parameter"))
            end
        elseif var_kind(ctx, varkey) === :static_parameter
            throw(LoweringError(e, "global variable name `$(varkey.name)` conflicts with a static parameter"))
        end
        var_ids[varkey] = init_binding(ctx, varkey, :global)
    end

    # Compute implicit locals and globals
    if is_toplevel_global_scope
        is_hard_scope = false
        is_soft_scope = false

        # Assignments are implicitly global at top level, unless they come from
        # a macro expansion
        for (varkey,e) in assignments
            vk = haskey(var_ids, varkey) ?
                lookup_binding(ctx, var_ids[varkey]).kind :
                 var_kind(ctx, varkey, true)
            if vk === nothing
                if ctx.scope_layers[varkey.layer].is_macro_expansion
                    var_ids[varkey] = init_binding(ctx, varkey, :local)
                else
                    init_binding(ctx, varkey, :global)
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
                 lookup_binding(ctx, var_ids[varkey]).kind :
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
                        is_defined_and_owned_global(layer.mod, Symbol(varkey.name)))
                    # Special scope rules to make assignments to globals work
                    # like assignments to locals do inside a function.
                    if is_soft_scope
                        # Soft scope (eg, for loop in REPL) => treat as a global
                        init_binding(ctx, varkey, :global)
                        continue
                    else
                        # Ambiguous case (eg, nontrivial scopes in package top level code)
                        # => Treat as local but generate warning when assigned to
                        is_ambiguous_local = true
                    end
                end
            end
            var_ids[varkey] = init_binding(ctx, varkey, :local, is_ambiguous_local)
        end
    end

    for varkey in used
        if lookup_var(ctx, varkey) === nothing
            # Add other newly discovered identifiers as globals
            init_binding(ctx, varkey, :global)
        end
    end

    lambda_locals = is_outer_lambda_scope ? Set{IdTag}() : parentscope.lambda_locals
    for id in values(var_ids)
        vk = var_kind(ctx, id)
        if vk === :local
            push!(lambda_locals, id)
        end
    end
    for id in used_bindings
        info = lookup_binding(ctx, id)
        if !info.is_ssa && info.kind == :local
            push!(lambda_locals, id)
        end
    end

    return ScopeInfo(is_toplevel_global_scope, in_toplevel_thunk, is_soft_scope,
                     is_hard_scope, var_ids, lambda_locals)
end

# Do some things which are better done after converting to BindingId.
function maybe_update_bindings!(ctx, ex)
    k = kind(ex)
    if k == K"decl"
        @chk numchildren(ex) == 2
        id = ex[1]
        if kind(id) != K"Placeholder"
            binfo = lookup_binding(ctx, id)
            if !isnothing(binfo.type)
                throw(LoweringError(ex, "multiple type declarations found for `$(binfo.name)`"))
            end
            if binfo.kind == :global && !ctx.scope_stack[end].in_toplevel_thunk
                throw(LoweringError(ex, "type declarations for global variables must be at top level, not inside a function"))
                # set_binding_type!
            end
            update_binding!(ctx, id; type=ex[2])
        end
    elseif k == K"const"
        id = ex[1]
        if lookup_binding(ctx, id).kind == :local
            throw(LoweringError(ex, "unsupported `const` declaration on local variable"))
        end
        update_binding!(ctx, id; is_const=true)
    end
    nothing
end

function _resolve_scopes(ctx, ex::SyntaxTree)
    k = kind(ex)
    if k == K"Identifier"
        id = lookup_var(ctx, NameKey(ex))
        @ast ctx ex id::K"BindingId"
    elseif is_leaf(ex) || is_quoted(ex) || k == K"toplevel"
        ex
    # elseif k == K"global"
    #     ex
    elseif k == K"local"
        makeleaf(ctx, ex, K"TOMBSTONE")
    elseif k == K"lambda"
        lambda_info = ex.lambda_info
        scope = analyze_scope(ctx, ex, nothing, lambda_info)
        push!(ctx.scope_stack, scope)
        arg_bindings = _resolve_scopes(ctx, lambda_info.args)
        sparm_bindings = _resolve_scopes(ctx, lambda_info.static_parameters)
        body = _resolve_scopes(ctx, only(children(ex)))
        pop!(ctx.scope_stack)
        # TODO: add a lambda locals field to lambda_info or make a new struct
        # containing the additional info ??
        new_info = LambdaInfo(arg_bindings, sparm_bindings,
                              lambda_info.ret_var, lambda_info.is_toplevel_thunk)
        makenode(ctx, ex, K"lambda", body; lambda_info=new_info, lambda_locals=scope.lambda_locals)
    elseif k == K"scope_block"
        scope = analyze_scope(ctx, ex, ex.scope_type, nothing)
        push!(ctx.scope_stack, scope)
        body = SyntaxList(ctx)
        for e in children(ex)
            push!(body, _resolve_scopes(ctx, e))
        end
        body
        pop!(ctx.scope_stack)
        @ast ctx ex [K"block" body...]
    elseif k == K"extension"
        etype = extension_type(ex)
        if etype == "islocal"
            id = lookup_var(ctx, NameKey(ex[2]))
            islocal = !isnothing(id) && var_kind(ctx, id) != :global
            @ast ctx ex islocal::K"Bool"
        elseif etype == "locals"
            # return Dict of locals
            TODO(ex, "@locals")
        else
            throw(LoweringError(ex, "Unknown syntax extension"))
        end
    elseif k == K"assert"
        etype = extension_type(ex)
        if etype == "require_existing_locals"
            for v in ex[2:end]
                vk = var_kind(ctx, NameKey(v))
                if vk !== :local
                    throw(LoweringError(v, "`outer` annotations must match with a local variable in an outer scope but no such variable was found"))
                end
            end
        elseif etype == "global_toplevel_only"
            if !ctx.scope_stack[end].is_toplevel_global_scope
                e = ex[2][1]
                throw(LoweringError(e, "$(kind(e)) is only allowed in global scope"))
            end
        else
            throw(LoweringError(ex, "Unknown syntax assertion"))
        end
        makeleaf(ctx, ex, K"TOMBSTONE")
    elseif k == K"const_if_global"
        id = _resolve_scopes(ctx, ex[1])
        if lookup_binding(ctx, id).kind == :global
            @ast ctx ex [K"const" ex[1]]
        else
            makeleaf(ctx, ex, K"TOMBSTONE")
        end
    else
        ex_mapped = mapchildren(e->_resolve_scopes(ctx, e), ctx, ex)
        maybe_update_bindings!(ctx, ex_mapped)
        ex_mapped
    end
end

function _resolve_scopes(ctx, exs::AbstractVector)
    out = SyntaxList(ctx)
    for e in exs
        push!(out, _resolve_scopes(ctx, e))
    end
    out
end

function resolve_scopes(ctx::ScopeResolutionContext, ex)
    thunk = makenode(ctx, ex, K"lambda", ex;
                     lambda_info=LambdaInfo(SyntaxList(ctx), SyntaxList(ctx), nothing, true))
    return _resolve_scopes(ctx, thunk)
end

"""
This pass analyzes scopes and the names (locals/globals etc) used within them.

Names of kind `K"Identifier"` are transformed into binding identifiers of
kind `K"BindingId"`. The associated `Bindings` table in the context records
metadata about each binding.

This pass also records the set of binding IDs are locals within the enclosing
lambda form.

TODO: This pass should also record information about variables used by closure
conversion, find which variables are assigned or captured, and record variable
type declarations.
"""
function resolve_scopes(ctx::DesugaringContext, ex)
    ctx2 = ScopeResolutionContext(ctx)
    ex2 = resolve_scopes(ctx2, reparent(ctx2, ex))
    ctx2, ex2
end
