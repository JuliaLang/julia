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

function _find_scope_vars!(ctx, assignments, locals, destructured_args, globals, used_names, used_bindings, ex)
    k = kind(ex)
    if k == K"Identifier"
        _insert_if_not_present!(used_names, NameKey(ex), ex)
    elseif k == K"BindingId"
        push!(used_bindings, ex.var_id)
    elseif is_leaf(ex) || is_quoted(k) ||
            k in KSet"scope_block lambda module toplevel"
        return
    elseif k == K"local"
        if getmeta(ex, :is_destructured_arg, false)
            push!(destructured_args, ex[1])
        else
            _insert_if_not_present!(locals, NameKey(ex[1]), ex)
        end
    elseif k == K"global"
        _insert_if_not_present!(globals, NameKey(ex[1]), ex)
    elseif k == K"assign_or_constdecl_if_global"
        # like v = val, except that if `v` turns out global(either implicitly or
        # by explicit `global`), it gains an implicit `const`
        _insert_if_not_present!(assignments, NameKey(ex[1]), ex)
    elseif k == K"=" || k == K"constdecl"
        v = decl_var(ex[1])
        if !(kind(v) in KSet"BindingId globalref Placeholder")
            _insert_if_not_present!(assignments, NameKey(v), v)
        end
        _find_scope_vars!(ctx, assignments, locals, destructured_args, globals, used_names, used_bindings, ex[2])
    elseif k == K"function_decl"
        v = ex[1]
        kv = kind(v)
        if kv == K"Identifier"
            _insert_if_not_present!(assignments, NameKey(v), v)
        elseif kv == K"BindingId"
            binfo = lookup_binding(ctx, v)
            if !binfo.is_ssa && binfo.kind != :global
                @assert false "allow local BindingId as function name?"
            end
        else
            @assert false
        end
    else
        for e in children(ex)
            _find_scope_vars!(ctx, assignments, locals, destructured_args, globals, used_names, used_bindings, e)
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
    used_names = Dict{NameKey,ExT}()
    used_bindings = Set{IdTag}()
    for e in children(ex)
        _find_scope_vars!(ctx, assignments, locals, destructured_args, globals, used_names, used_bindings, e)
    end

    # Sort by key so that id generation is deterministic
    assignments = sort!(collect(pairs(assignments)), by=first)
    locals      = sort!(collect(pairs(locals)),      by=first)
    globals     = sort!(collect(pairs(globals)),     by=first)
    used_names  = sort!(collect(pairs(used_names)),  by=first)
    used_bindings = sort!(collect(used_bindings))

    return assignments, locals, destructured_args, globals, used_names, used_bindings
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
    # Stack of name=>id mappings for each scope, innermost scope last.
    scope_stack::Vector{ScopeInfo}
    # Variables which were implicitly global due to being assigned to in top
    # level code
    implicit_toplevel_globals::Set{NameKey}
end

function ScopeResolutionContext(ctx)
    graph = ensure_attributes(ctx.graph, lambda_bindings=LambdaBindings)
    ScopeResolutionContext(graph,
                           ctx.bindings,
                           ctx.mod,
                           ctx.scope_layers,
                           Dict{NameKey,IdTag}(),
                           Vector{ScopeInfo}(),
                           Set{NameKey}())
end

function current_lambda_bindings(ctx::ScopeResolutionContext)
    last(ctx.scope_stack).lambda_bindings
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

function init_binding(ctx, srcref, varkey::NameKey, kind::Symbol; kws...)
    id = kind === :global ? get(ctx.global_vars, varkey, nothing) : nothing
    if isnothing(id)
        mod = kind === :global ? ctx.scope_layers[varkey.layer].mod : nothing
        ex = new_binding(ctx, srcref, varkey.name, kind; mod=mod, kws...)
        id = ex.var_id
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
            is_always_defined = args_kind == :argument
            id = init_binding(ctx, arg, varkey, args_kind;
                              is_nospecialize=getmeta(arg, :nospecialize, false),
                              is_always_defined=is_always_defined)
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
        used_names, used_bindings = find_scope_vars(ctx, ex)

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
            var_ids[varkey] = init_binding(ctx, e[1], varkey, :local)
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
        var_ids[varkey] = init_binding(ctx, e[1], varkey, :global)
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
                    var_ids[varkey] = init_binding(ctx, e, varkey, :local)
                else
                    init_binding(ctx, e, varkey, :global)
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
                        init_binding(ctx, e, varkey, :global)
                        continue
                    else
                        # Ambiguous case (eg, nontrivial scopes in package top level code)
                        # => Treat as local but generate warning when assigned to
                        is_ambiguous_local = true
                    end
                end
            end
            var_ids[varkey] = init_binding(ctx, e, varkey, :local;
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
    #
    # TODO: Move most or-all of this to the VariableAnalysis sub-pass
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
            if !has_lambda_binding(lambda_bindings, id)
                init_lambda_binding(lambda_bindings, id)
            end
        end
    end

    for (varkey, e) in used_names
        id = haskey(var_ids, varkey) ? var_ids[varkey] : lookup_var(ctx, varkey)
        if id === nothing
            # Identifiers which are used but not defined in some scope are
            # newly discovered global bindings
            init_binding(ctx, e, varkey, :global)
        elseif !in_toplevel_thunk
            binfo = lookup_binding(ctx, id)
            if binfo.kind !== :global
                if !has_lambda_binding(lambda_bindings, id)
                    # Used vars from a scope *outside* the current lambda are captured
                    init_lambda_binding(lambda_bindings, id, is_captured=true)
                    update_binding!(ctx, id; is_captured=true)
                end
            end
        end
    end

    if !in_toplevel_thunk
        for (varkey,_) in assignments
            id = haskey(var_ids, varkey) ? var_ids[varkey] : lookup_var(ctx, varkey)
            binfo = lookup_binding(ctx, id)
            if binfo.kind !== :global
                if !has_lambda_binding(lambda_bindings, id)
                    # Assigned vars from a scope *outside* the current lambda are captured
                    init_lambda_binding(lambda_bindings, id, is_captured=true)
                    update_binding!(ctx, id; is_captured=true)
                end
            end
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
            push!(stmts, @ast ctx srcref [K"local" binding_ex(ctx, id)])
        end
    end
end

function _resolve_scopes(ctx, ex::SyntaxTree)
    k = kind(ex)
    if k == K"Identifier"
        @ast ctx ex lookup_var(ctx, NameKey(ex))::K"BindingId"
    elseif is_leaf(ex) || is_quoted(ex) || k == K"toplevel"
        ex
    # elseif k == K"global"
    #     ex
    elseif k == K"local"
        makeleaf(ctx, ex, K"TOMBSTONE")
    elseif k == K"decl"
        ex_out = mapchildren(e->_resolve_scopes(ctx, e), ctx, ex)
        name = ex_out[1]
        if kind(name) != K"Placeholder"
            binfo = lookup_binding(ctx, name)
            if binfo.kind == :global && !ctx.scope_stack[end].in_toplevel_thunk
                throw(LoweringError(ex, "type declarations for global variables must be at top level, not inside a function"))
            end
        end
        id = ex_out[1]
        if kind(id) != K"Placeholder"
            binfo = lookup_binding(ctx, id)
            if !isnothing(binfo.type)
                throw(LoweringError(ex, "multiple type declarations found for `$(binfo.name)`"))
            end
            update_binding!(ctx, id; type=ex_out[2])
        end
        ex_out
    elseif k == K"always_defined"
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

        @ast ctx ex [K"lambda"(lambda_bindings=scope.lambda_bindings,
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
        elseif etype == "isglobal"
            e2 = ex[2]
            @chk kind(e2) in KSet"Identifier Placeholder"
            isglobal = if kind(e2) == K"Identifier"
                id = lookup_var(ctx, NameKey(e2))
                isnothing(id) || var_kind(ctx, id) == :global
            else
                false
            end
            @ast ctx ex isglobal::K"Bool"
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
                    binding = binding_ex(ctx, id)
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
    elseif k == K"function_decl"
        resolved = mapchildren(e->_resolve_scopes(ctx, e), ctx, ex)
        name = resolved[1]
        if kind(name) == K"BindingId"
            bk = lookup_binding(ctx, name).kind
            if bk == :argument
                throw(LoweringError(name, "Cannot add method to a function argument"))
            elseif bk == :global && !ctx.scope_stack[end].in_toplevel_thunk
                throw(LoweringError(name, 
                    "Global method definition needs to be placed at the top level, or use `eval()`"))
            end
        end
        resolved
    elseif k == K"assign_or_constdecl_if_global"
        id = _resolve_scopes(ctx, ex[1])
        bk = lookup_binding(ctx, id).kind
        @assert numchildren(ex) === 2
        assignment_kind = bk == :global ? K"constdecl" : K"="
        @ast ctx ex _resolve_scopes(ctx, [assignment_kind ex[1] ex[2]])
    else
        mapchildren(e->_resolve_scopes(ctx, e), ctx, ex)
    end
end

function _resolve_scopes(ctx, exs::AbstractVector)
    out = SyntaxList(ctx)
    for e in exs
        push!(out, _resolve_scopes(ctx, e))
    end
    out
end

#-------------------------------------------------------------------------------
# Sub-pass to compute additional information about variable usage as required
# by closure conversion, etc
struct ClosureBindings
    name_stack::Vector{String}      # Names of functions the closure is nested within
    lambdas::Vector{LambdaBindings} # Bindings for each method of the closure
end

ClosureBindings(name_stack) = ClosureBindings(name_stack, Vector{LambdaBindings}())

struct VariableAnalysisContext{GraphType} <: AbstractLoweringContext
    graph::GraphType
    bindings::Bindings
    mod::Module
    lambda_bindings::LambdaBindings
    # Stack of method definitions for closure naming
    method_def_stack::SyntaxList{GraphType}
    # Collection of information about each closure, principally which methods
    # are part of the closure (and hence captures).
    closure_bindings::Dict{IdTag,ClosureBindings}
end

function VariableAnalysisContext(graph, bindings, mod, lambda_bindings)
    VariableAnalysisContext(graph, bindings, mod, lambda_bindings,
                            SyntaxList(graph), Dict{IdTag,ClosureBindings}())
end

function current_lambda_bindings(ctx::VariableAnalysisContext)
    ctx.lambda_bindings
end

function init_closure_bindings!(ctx, fname)
    func_name_id = fname.var_id
    @assert lookup_binding(ctx, func_name_id).kind === :local
    get!(ctx.closure_bindings, func_name_id) do
        name_stack = Vector{String}()
        for parentname in ctx.method_def_stack
            if kind(parentname) == K"BindingId"
                push!(name_stack, lookup_binding(ctx, parentname).name)
            end
        end
        push!(name_stack, lookup_binding(ctx, func_name_id).name)
        ClosureBindings(name_stack)
    end
end

function find_any_local_binding(ctx, ex)
    k = kind(ex)
    if k == K"BindingId"
        bkind = lookup_binding(ctx, ex.var_id).kind
        if bkind != :global && bkind != :static_parameter
            return ex
        end
    elseif !is_leaf(ex) && !is_quoted(ex)
        for e in children(ex)
            r = find_any_local_binding(ctx, e)
            if !isnothing(r)
                return r
            end
        end
    end
    return nothing
end

# Update ctx.bindings and ctx.lambda_bindings metadata based on binding usage
function analyze_variables!(ctx, ex)
    k = kind(ex)
    if k == K"BindingId"
        if has_lambda_binding(ctx, ex)
            # TODO: Move this after closure conversion so that we don't need
            # to model the closure conversion transformations here.
            update_lambda_binding!(ctx, ex, is_read=true)
        else
            binfo = lookup_binding(ctx, ex.var_id)
            if !binfo.is_ssa && binfo.kind != :global
                # The type of typed locals is invisible in the previous pass,
                # but is filled in here.
                init_lambda_binding(ctx.lambda_bindings, ex.var_id, is_captured=true, is_read=true)
                update_binding!(ctx, ex, is_captured=true)
            end
        end
    elseif is_leaf(ex) || is_quoted(ex)
        return
    elseif k == K"static_eval"
        badvar = find_any_local_binding(ctx, ex[1])
        if !isnothing(badvar)
            name_hint = getmeta(ex, :name_hint, "syntax")
            throw(LoweringError(badvar, "$(name_hint) cannot reference local variable"))
        end
        return
    elseif k == K"local" || k == K"global"
        # Presence of BindingId within local/global is ignored.
        return
    elseif k == K"="
        lhs = ex[1]
        if kind(lhs) != K"Placeholder"
            update_binding!(ctx, lhs, add_assigned=1)
            if has_lambda_binding(ctx, lhs)
                update_lambda_binding!(ctx, lhs, is_assigned=true)
            end
            lhs_binfo = lookup_binding(ctx, lhs)
            if !isnothing(lhs_binfo.type)
                # Assignments introduce a variable's type later during closure
                # conversion, but we must model that explicitly here.
                analyze_variables!(ctx, lhs_binfo.type)
            end
        end
        analyze_variables!(ctx, ex[2])
    elseif k == K"function_decl"
        name = ex[1]
        if lookup_binding(ctx, name.var_id).kind === :local
            init_closure_bindings!(ctx, name)
        end
        update_binding!(ctx, name, add_assigned=1)
        if has_lambda_binding(ctx, name)
            update_lambda_binding!(ctx, name, is_assigned=true)
        end
    elseif k == K"function_type"
        if kind(ex[1]) != K"BindingId" || lookup_binding(ctx, ex[1]).kind !== :local
            analyze_variables!(ctx, ex[1])
        end
    elseif k == K"constdecl"
        id = ex[1]
        if lookup_binding(ctx, id).kind == :local
            throw(LoweringError(ex, "unsupported `const` declaration on local variable"))
        end
        update_binding!(ctx, id; is_const=true)
    elseif k == K"call"
        name = ex[1]
        if kind(name) == K"BindingId"
            id = name.var_id
            if has_lambda_binding(ctx, id)
                # TODO: Move this after closure conversion so that we don't need
                # to model the closure conversion transformations.
                update_lambda_binding!(ctx, id, is_called=true)
            end
        end
        foreach(e->analyze_variables!(ctx, e), children(ex))
    elseif k == K"method_defs"
        push!(ctx.method_def_stack, ex[1])
        analyze_variables!(ctx, ex[2])
        pop!(ctx.method_def_stack)
    elseif k == K"_opaque_closure"
        name = ex[1]
        init_closure_bindings!(ctx, name)
        push!(ctx.method_def_stack, name)
        analyze_variables!(ctx, ex[2])
        analyze_variables!(ctx, ex[3])
        analyze_variables!(ctx, ex[4])
        analyze_variables!(ctx, ex[9])
        pop!(ctx.method_def_stack)
    elseif k == K"lambda"
        lambda_bindings = ex.lambda_bindings
        if !ex.is_toplevel_thunk && !isempty(ctx.method_def_stack)
            # Record all lambdas for the same closure type in one place
            func_name = last(ctx.method_def_stack)
            if kind(func_name) == K"BindingId"
                func_name_id = func_name.var_id
                if lookup_binding(ctx, func_name_id).kind === :local
                    push!(ctx.closure_bindings[func_name_id].lambdas, lambda_bindings)
                end
            end
        end
        ctx2 = VariableAnalysisContext(ctx.graph, ctx.bindings, ctx.mod, lambda_bindings,
                                       ctx.method_def_stack, ctx.closure_bindings)
        foreach(e->analyze_variables!(ctx2, e), ex[3:end]) # body & return type
        for (id,lbinfo) in pairs(lambda_bindings.bindings)
            if lbinfo.is_captured
                # Add any captured bindings to the enclosing lambda, if necessary.
                outer_lbinfo = lookup_lambda_binding(ctx.lambda_bindings, id)
                if isnothing(outer_lbinfo)
                    # Inner lambda captures a variable. If it's not yet present
                    # in the outer lambda, the outer lambda must capture it as
                    # well so that the closure associated to the inner lambda
                    # can be initialized when `function_decl` is hit.
                    init_lambda_binding(ctx.lambda_bindings, id, is_captured=true, is_read=true)
                end
            end
        end
    else
        foreach(e->analyze_variables!(ctx, e), children(ex))
    end
    nothing
end

function resolve_scopes(ctx::ScopeResolutionContext, ex)
    if kind(ex) != K"lambda"
        # Wrap in a top level thunk if we're not already expanding a lambda.
        # (Maybe this should be done elsewhere?)
        ex = @ast ctx ex [K"lambda"(is_toplevel_thunk=true)
            [K"block"]
            [K"block"]
            ex
        ]
    end
    _resolve_scopes(ctx, ex)
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
    ctx3 = VariableAnalysisContext(ctx2.graph, ctx2.bindings, ctx2.mod, ex2.lambda_bindings)
    analyze_variables!(ctx3, ex2)
    ctx3, ex2
end
