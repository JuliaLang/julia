# Lowering pass 3: scope and variable analysis

"""
Key to use when transforming names into bindings
"""
struct NameKey
    name::String
    layer::LayerId
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

#-------------------------------------------------------------------------------
_insert_if_not_present!(dict, key, val) = get!(dict, key, val)

function _find_scope_vars!(ctx, assignments, locals, destructured_args, globals, used_names, used_bindings, alias_bindings, ex)
    k = kind(ex)
    if k == K"Identifier"
        push!(used_names, NameKey(ex))
    elseif k == K"BindingId"
        push!(used_bindings, ex.var_id)
    elseif k == K"alias_binding"
        push!(alias_bindings, NameKey(ex[2])=>ex[1].var_id)
    elseif is_leaf(ex) || is_quoted(k) ||
            k in KSet"scope_block lambda module toplevel"
        return
    elseif k == K"local" || k == K"local_def"
        if getmeta(ex, :is_destructured_arg, false)
            push!(destructured_args, ex[1])
        else
            _insert_if_not_present!(locals, NameKey(ex[1]), ex)
        end
    elseif k == K"global"
        _insert_if_not_present!(globals, NameKey(ex[1]), ex)
    elseif k == K"="
        v = decl_var(ex[1])
        if !(kind(v) in KSet"BindingId globalref Placeholder")
            _insert_if_not_present!(assignments, NameKey(v), v)
        end
        _find_scope_vars!(ctx, assignments, locals, destructured_args, globals, used_names, used_bindings, alias_bindings, ex[2])
    elseif k == K"function_decl"
        v = ex[1]
        kv = kind(v)
        if kv == K"Identifier"
            _insert_if_not_present!(assignments, NameKey(v), v)
        elseif kv == K"BindingId"
            if !lookup_binding(ctx, v).is_ssa
                TODO(v, "BindingId as function name")
            end
        else
            @assert false
        end
    else
        for e in children(ex)
            _find_scope_vars!(ctx, assignments, locals, destructured_args, globals, used_names, used_bindings, alias_bindings, e)
        end
    end
end

# Find names of all identifiers used in the given expression, grouping them
# into sets by type of usage.
#
# NB: This only works propery after desugaring
function find_scope_vars(ctx, ex)
    ExT = typeof(ex)
    assignments = Dict{NameKey,ExT}()
    locals = Dict{NameKey,ExT}()
    destructured_args = Vector{ExT}()
    globals = Dict{NameKey,ExT}()
    used_names = Set{NameKey}()
    used_bindings = Set{IdTag}()
    alias_bindings = Vector{Pair{NameKey,IdTag}}()
    for e in children(ex)
        _find_scope_vars!(ctx, assignments, locals, destructured_args, globals, used_names, used_bindings, alias_bindings, e)
    end

    # Sort by key so that id generation is deterministic
    assignments = sort(collect(pairs(assignments)), by=first)
    locals = sort(collect(pairs(locals)), by=first)
    globals = sort(collect(pairs(globals)), by=first)
    used_names = sort(collect(used_names))
    used_bindings = sort(collect(used_bindings))

    return assignments, locals, destructured_args, globals, used_names, used_bindings, alias_bindings
end

# Metadata about how a binding is used within some enclosing lambda
struct LambdaBindingInfo
    is_captured::Bool
    is_read::Bool
    is_assigned::Bool
    is_called::Bool
end

LambdaBindingInfo() = LambdaBindingInfo(false, false, false, false)

function LambdaBindingInfo(parent::LambdaBindingInfo;
                           is_captured = nothing,
                           is_read     = nothing,
                           is_assigned = nothing,
                           is_called   = nothing)
    LambdaBindingInfo(
        isnothing(is_captured) ? parent.is_captured : is_captured,
        isnothing(is_read)     ? parent.is_read     : is_read,
        isnothing(is_assigned) ? parent.is_assigned : is_assigned,
        isnothing(is_called)   ? parent.is_called   : is_called,
    )
end

struct LambdaBindings
    # Bindings used within the lambda
    self::IdTag
    bindings::Dict{IdTag,LambdaBindingInfo}
end

LambdaBindings(self::IdTag = 0) = LambdaBindings(self, Dict{IdTag,LambdaBindings}())

function init_lambda_binding(binds::LambdaBindings, id; kws...)
    @assert !haskey(binds.bindings, id)
    binds.bindings[id] = LambdaBindingInfo(LambdaBindingInfo(); kws...)
end

function update_lambda_binding!(binds::LambdaBindings, id; kws...)
    binfo = binds.bindings[id]
    binds.bindings[id] = LambdaBindingInfo(binfo; kws...)
end

function update_lambda_binding!(ctx::AbstractLoweringContext, id; kws...)
    update_lambda_binding!(last(ctx.scope_stack).lambda_bindings, id; kws...)
end

struct ClosureBindings
    name_stack::Vector{String}      # Names of functions the closure is nested within
    lambdas::Vector{LambdaBindings} # Bindings for each method of the closure
end

ClosureBindings(name_stack) = ClosureBindings(name_stack, Vector{LambdaBindings}())

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
    # Bindings used by the enclosing lambda
    lambda_bindings::LambdaBindings
end

struct ScopeResolutionContext{GraphType} <: AbstractLoweringContext
    graph::GraphType
    bindings::Bindings
    mod::Module
    scope_layers::Vector{ScopeLayer}
    # name=>id mappings for all discovered global vars
    global_vars::Dict{NameKey,IdTag}
    # Map for rewriting binding aliases
    alias_map::Dict{IdTag,IdTag}
    # Stack of name=>id mappings for each scope, innermost scope last.
    scope_stack::Vector{ScopeInfo}
    method_def_stack::SyntaxList{GraphType}
    # Variables which were implicitly global due to being assigned to in top
    # level code
    implicit_toplevel_globals::Set{NameKey}
    # Collection of information about each closure, principally which methods
    # are part of the closure (and hence captures).
    closure_bindings::Dict{IdTag,ClosureBindings}
end

function ScopeResolutionContext(ctx)
    graph = ensure_attributes(ctx.graph, lambda_bindings=LambdaBindings)
    ScopeResolutionContext(graph,
                           ctx.bindings,
                           ctx.mod,
                           ctx.scope_layers,
                           Dict{NameKey,IdTag}(),
                           Dict{IdTag,IdTag}(),
                           Vector{ScopeInfo}(),
                           SyntaxList(graph),
                           Set{NameKey}(),
                           Dict{IdTag,ClosureBindings}())
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

function init_binding(ctx, varkey::NameKey, kind::Symbol; kws...)
    id = kind === :global ? get(ctx.global_vars, varkey, nothing) : nothing
    if isnothing(id)
        mod = kind === :global ? ctx.scope_layers[varkey.layer].mod : nothing
        id = new_binding(ctx.bindings, BindingInfo(varkey.name, kind; mod=mod, kws...))
    end
    if kind === :global
        ctx.global_vars[varkey] = id
    end
    id
end

# Add lambda arguments and static parameters
function add_lambda_args(ctx, var_ids, args, args_kind)
    for arg in args
        ka = kind(arg)
        if ka == K"Identifier"
            varkey = NameKey(arg)
            if haskey(var_ids, varkey)
                vk = lookup_binding(ctx, var_ids[varkey]).kind
                _is_arg(k) = k == :argument || k == :local
                msg = _is_arg(vk) && _is_arg(args_kind) ? "function argument name not unique"         :
                      vk == :static_parameter && args_kind == :static_parameter ? "function static parameter name not unique" :
                      "static parameter name not distinct from function argument"
                throw(LoweringError(arg, msg))
            end
            id = init_binding(ctx, varkey, args_kind;
                              is_nospecialize=getmeta(arg, :nospecialize, false))
            var_ids[varkey] = id
        elseif ka != K"BindingId" && ka != K"Placeholder"
            throw(LoweringError(arg, "Unexpected lambda arg kind"))
        end
    end
end

# Analyze identifier usage within a scope
# * Allocate a new binding for each identifier which the scope introduces.
# * Record the identifier=>binding mapping in a lookup table
# * Return a `ScopeInfo` with the mapping plus additional scope metadata
function analyze_scope(ctx, ex, scope_type, is_toplevel_global_scope=false,
                       lambda_args=nothing, lambda_static_parameters=nothing)
    parentscope = isempty(ctx.scope_stack) ? nothing : ctx.scope_stack[end]
    is_outer_lambda_scope = kind(ex) == K"lambda"
    in_toplevel_thunk = is_toplevel_global_scope ||
        (!is_outer_lambda_scope && parentscope.in_toplevel_thunk)

    assignments, locals, destructured_args, globals,
        used, used_bindings, alias_bindings = find_scope_vars(ctx, ex)

    # Construct a mapping from identifiers to bindings
    #
    # This will contain a binding ID for each variable which is introduced by
    # the scope, including
    # * Explicit locals
    # * Explicit globals
    # * Implicit locals created by assignment
    var_ids = Dict{NameKey,IdTag}()

    if !isnothing(lambda_args)
        add_lambda_args(ctx, var_ids, lambda_args, :argument)
        add_lambda_args(ctx, var_ids, lambda_static_parameters, :static_parameter)
        add_lambda_args(ctx, var_ids, destructured_args, :local)
    end

    # Add explicit locals
    for (varkey,e) in locals
        if haskey(var_ids, varkey)
            vk = lookup_binding(ctx, var_ids[varkey]).kind
            if vk === :argument && is_outer_lambda_scope
                throw(LoweringError(e, "local variable name `$(varkey.name)` conflicts with an argument"))
            elseif vk === :static_parameter
                throw(LoweringError(e, "local variable name `$(varkey.name)` conflicts with a static parameter"))
            end
        elseif var_kind(ctx, varkey) === :static_parameter
            throw(LoweringError(e, "local variable name `$(varkey.name)` conflicts with a static parameter"))
        else
            var_ids[varkey] = init_binding(ctx, varkey, :local)
        end
    end

    # Add explicit globals
    for (varkey,e) in globals
        if haskey(var_ids, varkey)
            vk = lookup_binding(ctx, var_ids[varkey]).kind
            if vk === :local
                throw(LoweringError(e, "Variable `$(varkey.name)` declared both local and global"))
            elseif vk === :argument && is_outer_lambda_scope
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
            var_ids[varkey] = init_binding(ctx, varkey, :local;
                                           is_ambiguous_local=is_ambiguous_local)
        end
    end

    #--------------------------------------------------
    # At this point we've discovered all the bindings defined in this scope and
    # added them to `var_ids`.
    #
    # Next we record information about how the new bindings relate to the
    # enclosing lambda
    # * All non-globals are recorded (kind :local and :argument will later be turned into slots)
    # * Captured variables are detected and recorded
    lambda_bindings = if is_outer_lambda_scope
        if isempty(lambda_args)
            LambdaBindings()
        else
            selfarg = first(lambda_args)
            selfid = kind(selfarg) == K"BindingId" ?
                     selfarg.var_id : var_ids[NameKey(selfarg)]
            LambdaBindings(selfid)
        end
    else
        parentscope.lambda_bindings
    end

    for id in values(var_ids)
        binfo = lookup_binding(ctx, id)
        if !binfo.is_ssa && binfo.kind !== :global
            init_lambda_binding(lambda_bindings, id)
        end
    end

    # FIXME: This assumes used bindings are internal to the lambda and cannot
    # be from the environment, and also assumes they are assigned. That's
    # correct for now but in general we should go by the same code path that
    # identifiers do.
    for id in used_bindings
        binfo = lookup_binding(ctx, id)
        if (binfo.kind === :local && !binfo.is_ssa) || binfo.kind === :argument ||
                binfo.kind === :static_parameter
            if !haskey(lambda_bindings.bindings, id)
                init_lambda_binding(lambda_bindings, id, is_read=true, is_assigned=true)
            end
        end
    end

    for varkey in used
        id = haskey(var_ids, varkey) ? var_ids[varkey] : lookup_var(ctx, varkey)
        if id === nothing
            # Identifiers which are used but not defined in some scope are
            # newly discovered global bindings
            init_binding(ctx, varkey, :global)
        elseif !in_toplevel_thunk
            binfo = lookup_binding(ctx, id)
            if binfo.kind !== :global
                if !haskey(lambda_bindings.bindings, id)
                    # Used vars from a scope *outside* the current lambda are captured
                    init_lambda_binding(lambda_bindings, id, is_captured=true, is_read=true)
                    update_binding!(ctx, id; is_captured=true)
                else
                    update_lambda_binding!(lambda_bindings, id, is_read=true)
                end
            end
        end
    end

    if !in_toplevel_thunk
        for (varkey,_) in assignments
            id = haskey(var_ids, varkey) ? var_ids[varkey] : lookup_var(ctx, varkey)
            binfo = lookup_binding(ctx, id)
            if binfo.kind !== :global
                if !haskey(lambda_bindings.bindings, id)
                    # Assigned vars from a scope *outside* the current lambda are captured
                    init_lambda_binding(lambda_bindings, id, is_captured=true, is_assigned=true)
                    update_binding!(ctx, id; is_captured=true)
                else
                    update_lambda_binding!(lambda_bindings, id, is_assigned=true)
                end
            end
        end
    end

    # TODO: Remove alias bindings? Dynamically generated scope layers are
    # simpler and probably sufficient?
    for (varkey, id) in alias_bindings
        @assert !haskey(ctx.alias_map, id)
        ctx.alias_map[id] = get(var_ids, varkey) do
            lookup_var(ctx, varkey)
        end
    end

    return ScopeInfo(is_toplevel_global_scope, in_toplevel_thunk, is_soft_scope,
                     is_hard_scope, var_ids, lambda_bindings)
end

function add_local_decls!(ctx, stmts, srcref, scope)
    # Add local decls to start of block so that closure conversion can
    # initialize if necessary.
    for id in sort!(collect(values(scope.var_ids)))
        binfo = lookup_binding(ctx, id)
        if binfo.kind == :local
            push!(stmts, @ast ctx srcref [K"local" id::K"BindingId"])
        end
    end
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
    elseif k == K"call"
        name = ex[1]
        if kind(name) == K"BindingId"
            id = name.var_id
            if haskey(last(ctx.scope_stack).lambda_bindings.bindings, id)
                update_lambda_binding!(ctx, id, is_called=true)
            end
        end
    end
    nothing
end

function _resolve_scopes(ctx, ex::SyntaxTree)
    k = kind(ex)
    if k == K"Identifier"
        id = lookup_var(ctx, NameKey(ex))
        @ast ctx ex id::K"BindingId"
    elseif k == K"BindingId"
        mapped_id = get(ctx.alias_map, ex.var_id, nothing)
        if isnothing(mapped_id)
            ex
        else
            @ast ctx ex mapped_id::K"BindingId"
        end
    elseif is_leaf(ex) || is_quoted(ex) || k == K"toplevel"
        ex
    # elseif k == K"global"
    #     ex
    elseif k == K"local" || k == K"alias_binding"
        makeleaf(ctx, ex, K"TOMBSTONE")
    elseif k == K"local_def"
        id = lookup_var(ctx, NameKey(ex[1]))
        update_binding!(ctx, id; is_always_defined=true)
        makeleaf(ctx, ex, K"TOMBSTONE")
    elseif k == K"lambda"
        is_toplevel_thunk = ex.is_toplevel_thunk
        scope = analyze_scope(ctx, ex, nothing, is_toplevel_thunk,
                              children(ex[1]), children(ex[2]))

        push!(ctx.scope_stack, scope)
        arg_bindings = _resolve_scopes(ctx, ex[1])
        sparm_bindings = _resolve_scopes(ctx, ex[2])
        body_stmts = SyntaxList(ctx)
        add_local_decls!(ctx, body_stmts, ex, scope)
        body = _resolve_scopes(ctx, ex[3])
        if kind(body) == K"block"
            append!(body_stmts, children(body))
        else
            push!(body_stmts, body)
        end
        ret_var = numchildren(ex) == 4 ? _resolve_scopes(ctx, ex[4]) : nothing
        pop!(ctx.scope_stack)

        lambda_bindings = scope.lambda_bindings
        if !is_toplevel_thunk
            # Record all lambdas for the same closure type in one place
            func_name = last(ctx.method_def_stack)
            if kind(func_name) == K"BindingId"
                func_name_id = func_name.var_id
                if lookup_binding(ctx, func_name_id).kind === :local
                    cbinds = get!(ctx.closure_bindings, func_name_id) do
                        name_stack = Vector{String}()
                        for fname in ctx.method_def_stack
                            if kind(fname) == K"BindingId"
                                push!(name_stack, lookup_binding(ctx, fname).name)
                            end
                        end
                        ClosureBindings(name_stack)
                    end
                    push!(cbinds.lambdas, lambda_bindings)
                end
            end
        end

        @ast ctx ex [K"lambda"(lambda_bindings=lambda_bindings,
                               is_toplevel_thunk=is_toplevel_thunk)
            arg_bindings
            sparm_bindings
            [K"block"
                body_stmts...
            ]
            ret_var
        ]
    elseif k == K"scope_block"
        scope = analyze_scope(ctx, ex, ex.scope_type)
        push!(ctx.scope_stack, scope)
        stmts = SyntaxList(ctx)
        add_local_decls!(ctx, stmts, ex, scope)
        for e in children(ex)
            push!(stmts, _resolve_scopes(ctx, e))
        end
        pop!(ctx.scope_stack)
        @ast ctx ex [K"block" stmts...]
    elseif k == K"extension"
        etype = extension_type(ex)
        if etype == "islocal"
            id = lookup_var(ctx, NameKey(ex[2]))
            islocal = !isnothing(id) && var_kind(ctx, id) != :global
            @ast ctx ex islocal::K"Bool"
        elseif etype == "locals"
            stmts = SyntaxList(ctx)
            locals_dict = ssavar(ctx, ex, "locals_dict")
            push!(stmts, @ast ctx ex [K"="
                locals_dict
                [K"call"
                    [K"call"
                        "apply_type"::K"core"
                        "Dict"::K"top"
                        "Symbol"::K"core" 
                        "Any"::K"core" 
                    ]
                ]
            ])
            for scope in ctx.scope_stack
                for id in values(scope.var_ids)
                    binfo = lookup_binding(ctx, id)
                    if binfo.kind == :global || binfo.is_internal
                        continue
                    end
                    binding = @ast ctx (@ast ctx ex binfo.name::K"Identifier") id::K"BindingId"
                    push!(stmts, @ast ctx ex [K"if"
                        [K"isdefined" binding]
                        [K"call"
                            "setindex!"::K"top"
                            locals_dict
                            binding
                            binfo.name::K"Symbol"
                        ]
                    ])
                end
            end
            push!(stmts, locals_dict)
            makenode(ctx, ex, K"block", stmts)
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
        elseif etype == "toplevel_only"
            if !ctx.scope_stack[end].in_toplevel_thunk
                e = ex[2][1]
                throw(LoweringError(e, "this syntax is only allowed in top level code"))
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
    elseif k == K"method_defs"
        push!(ctx.method_def_stack, _resolve_scopes(ctx, ex[1]))
        ex_mapped = mapchildren(e->_resolve_scopes(ctx, e), ctx, ex)
        pop!(ctx.method_def_stack)
        ex_mapped
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
    thunk = @ast ctx ex [K"lambda"(is_toplevel_thunk=true)
        [K"block"]
        [K"block"]
        ex
    ]
    return _resolve_scopes(ctx, thunk)
end

"""
This pass analyzes scopes and the names (locals/globals etc) used within them.

Names of kind `K"Identifier"` are transformed into binding identifiers of
kind `K"BindingId"`. The associated `Bindings` table in the context records
metadata about each binding.

This pass also records the set of binding IDs used locally within the
enclosing lambda form and information about variables captured by closures.
"""
function resolve_scopes(ctx::DesugaringContext, ex)
    ctx2 = ScopeResolutionContext(ctx)
    ex2 = resolve_scopes(ctx2, reparent(ctx2, ex))
    ctx2, ex2
end
