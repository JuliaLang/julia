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

function NameKey(ex::SyntaxTree)
    @chk kind(ex) == K"Identifier"
    NameKey(ex.name_val, ex.scope_layer)
end

struct ScopeInfo
    # index into ctx.scopes
    id::ScopeId
    # 0 if top-level thunk
    parent_id::ScopeId
    # Own ID if lambda, else some parent ID
    lambda_id::ScopeId
    # Tree introducing this scope
    node_id::NodeId
    # True in the top-level scope, and any neutral scope nested within it not
    # protected by a hard scope.  Becomes soft if `ctx.enable_soft_scopes`.
    is_permeable::Bool
    binding_assignments::Dict{IdTag, NodeId}
    assignments::Dict{NameKey, NodeId}
    # Map from variable names to binding IDs for resolution.  Includes all
    # locals, args, sparams, and explicit globals belonging to this scope.
    # Variables captured from an outer scope are not included.  The top-level
    # scope also contains all globals for resolution to fall back to.
    vars::Dict{NameKey,IdTag}
    # See `LambdaBindings`. Nothing if not a lambda scope.
    locals_capt::Union{Nothing, Dict{IdTag,Bool}}
end

function ScopeInfo(ctx, parent_id, ex::SyntaxTree)
    id = length(ctx.scopes) + 1
    lambda_id = kind(ex) === K"lambda" ? id : ctx.scopes[parent_id].lambda_id
    is_toplevel_thunk = kind(ex) === K"lambda" && ex.is_toplevel_thunk
    is_permeable = is_toplevel_thunk ||
        (kind(ex) === K"scope_block" && ex.scope_type === :neutral &&
        parent_id !== 0 && ctx.scopes[parent_id].is_permeable)

    s = ScopeInfo(
        id, parent_id, lambda_id, ex._id, is_permeable,
        Dict{IdTag, NodeId}(), Dict{NameKey, NodeId}(), Dict{NameKey,IdTag}(),
        kind(ex) === K"lambda" ? Dict{IdTag,Bool}() : nothing)
    push!(ctx.scopes, s)
    return s
end

struct ScopeResolutionContext{Attrs} <: AbstractLoweringContext
    graph::SyntaxGraph{Attrs}
    bindings::Bindings
    mod::Module
    # Every lexical scope, indexed by ScopeId
    scopes::Vector{ScopeInfo}
    # Current stack of scopes to look for names in, innermost scope last
    scope_stack::Vector{ScopeId}
    # Macro hygienic scopes (confusing name here)
    scope_layers::Vector{ScopeLayer}
    # Usually, globals in the top scope are ignored.  This is a subset that may
    # be assigned to without the `global` keyword in soft scopes due to being
    # assigned to at top level, or passing the defined-and-owned-global check.
    soft_assignable_globals::Set{NameKey}
    enable_soft_scopes::Bool
    expr_compat_mode::Bool
end

function ScopeResolutionContext(ctx, ex)
    graph = ensure_attributes(ctx.graph, lambda_bindings=LambdaBindings)
    ScopeResolutionContext(
        graph,
        ctx.bindings,
        ctx.mod,
        Vector{ScopeInfo}(),
        Vector{ScopeId}(),
        ctx.scope_layers,
        Set{NameKey}(),
        contains_softscope_marker(ex),
        ctx.expr_compat_mode)
end

function contains_softscope_marker(ex)
    kind(ex) == K"softscope" ||
        needs_resolution(ex) && any(contains_softscope_marker, children(ex))
end

top_scope(ctx) = ctx.scopes[1]
is_top_scope(scope::ScopeInfo) = scope.parent_id === 0
enclosing_lambda(ctx, scope::ScopeInfo) = ctx.scopes[scope.lambda_id]
parent(ctx, scope::ScopeInfo) = is_top_scope(scope) ? nothing :
    ctx.scopes[scope.parent_id]

_var_str(v) = v === :local ? "local variable" :
    v === :global ? "global variable" :
    v === :argument ? "argument" :
    v === :destructured_arg ? "destructured argument" :
    v === :static_parameter ? "static parameter" : "unknown"

# Declare `ex` in `scope`, unless a binding already exists with the same name in
# scope, or id anywhere.  Throw an error if a name conflict occurs.  The rules
# for conflict: declaring a local (or global) twice with the same name is a
# no-op, but doing so with an argument or static parameter is an error.  A
# variable usually can't be two things in one scope, but flisp has quirks.
function maybe_declare_in_scope!(ctx, scope::ScopeInfo, ex, new_k::Symbol)
    if kind(ex) === K"BindingId"
        bid = ex.var_id
        @assert get_binding(ctx, bid).kind === new_k
        record_lambda_var!(ctx, scope, get_binding(ctx, bid), capt=false)
        return bid
    elseif kind(ex) === K"Placeholder"
        return nothing
    end
    bid = get(scope.vars, NameKey(ex), nothing)
    old_k = isnothing(bid) ? nothing : get_binding(ctx, bid).kind
    if isnothing(old_k)
        if new_k === :argument
            declare_in_scope!(ctx, scope, ex, :argument;
                              is_nospecialize=getmeta(ex, :nospecialize, false))
        else
            real_k = new_k === :destructured_arg ? :local : new_k
            declare_in_scope!(ctx, scope, ex, real_k)
        end
    elseif old_k === new_k
        (new_k === :global || new_k === :local) && return bid
        throw(LoweringError(ex, "function $(_var_str(new_k)) name not unique"))
    # See note in test/scopes.jl: "globals may overlap args or sparams"
    # elseif new_k === :global && old_k in (:argument, :static_parameter)
    #     declare_in_scope!(ctx, scope, ex, :global)
    else
        throw(LoweringError(ex, """
        $(_var_str(new_k)) name `$(NameKey(ex).name)` conflicts with an \
        existing $(_var_str(old_k)) from the same scope"""))
    end
end

# globals are added to both `scope` and the top scope
function declare_in_scope!(ctx, scope::ScopeInfo, ex, bk::Symbol; kws...)
    nk = NameKey(ex)
    if bk === :global
        declaration_scope = top_scope(ctx)
        mod = ctx.scope_layers[ex.scope_layer].mod
    else
        declaration_scope = scope
        mod = nothing
    end
    b = _new_binding(ctx, ex, nk.name, bk; mod, kws...)
    declaration_scope.vars[nk] = b.id
    scope.vars[nk] = b.id
    @assert !haskey(enclosing_lambda(ctx, scope).locals_capt, b.id)
    record_lambda_var!(ctx, scope, b, capt=false)
    return b.id
end

# If `b` is local and not yet recorded in the lambda bindings, mark it as
# `capt`.  Also, (if `capt==true`), add it to any parent lambdas.
function record_lambda_var!(ctx, scope::ScopeInfo, b; capt)
    if b.kind === :global || b.is_ssa
        return
    end
    lam = enclosing_lambda(ctx, scope)
    if !haskey(lam.locals_capt, b.id)
        lam.locals_capt[b.id] = capt
        b.is_captured = capt
        s2 = parent(ctx, lam)
        if capt && !isnothing(s2)
            record_lambda_var!(ctx, s2, b, capt=true)
        end
    end
end

function needs_resolution(ex)
    kind(ex) === K"Identifier" ||
        !is_leaf(ex) && !is_quoted(ex) && !(kind(ex) in KSet"toplevel module")
end

function resolve_name(ctx, ex; exclude_toplevel_globals=false)
    for sid in reverse(ctx.scope_stack)
        bid = get(ctx.scopes[sid].vars, NameKey(ex), nothing)
        isnothing(bid) && continue
        b = get_binding(ctx, bid)
        if !exclude_toplevel_globals || sid !== top_scope(ctx).id || b.kind !== :global
            return b
        end
    end
end

function _find_scope_decls!(ctx, scope, ex)
    k = kind(ex)
    if k === K"local" && kind(ex[1]) === K"Identifier"
        var_k = getmeta(ex, :is_destructured_arg, false) ?
            :destructured_arg : :local
        if getmeta(ex, :is_internal, false)
            declare_in_scope!(ctx, scope, ex[1], var_k; is_internal=true)
        else
            maybe_declare_in_scope!(ctx, scope, ex[1], var_k)
        end
    elseif k === K"global" && kind(ex[1]) === K"Identifier"
        maybe_declare_in_scope!(ctx, scope, ex[1], :global)
    elseif k in KSet"= constdecl assign_or_constdecl_if_global function_decl"
        k1 = kind(ex[1])
        if k1 === K"BindingId"
            b = get_binding(ctx, ex[1])
            if k === K"function_decl" && !b.is_ssa && b.kind !== :global
                @assert false "allow local BindingId as function name?"
            end
            get!(scope.binding_assignments, b.id, ex[1]._id)
        elseif k1 === K"Identifier"
            get!(scope.assignments, NameKey(ex[1]), ex[1]._id)
        elseif k1 === K"Placeholder"
            @assert k !== K"function_decl"
        else
            @assert false "Unknown kind in assignment"
        end
        if (k === K"=" || k === K"assign_or_constdecl_if_global" ||
            k === K"constdecl" && numchildren(ex) == 2)
            _find_scope_decls!(ctx, scope, ex[2])
        end
    elseif k === K"symbolicblock"
        # Only recurse into the body (second child), not the label name (first child)
        _find_scope_decls!(ctx, scope, ex[2])
    elseif k === K"break" && numchildren(ex) >= 2
        # For break with value, only recurse into the value expression (second child), not the label
        _find_scope_decls!(ctx, scope, ex[2])
    elseif needs_resolution(ex) && !(k === K"scope_block" || k === K"lambda")
        for e in children(ex)
            _find_scope_decls!(ctx, scope, e)
        end
    end
end

# Produce a complete ScopeInfo and add it to the stack of active scopes.  This
# means finding all variables declared and used in the scope `ex` and generating
# the (identifier,layer)=>binding_id mapping `scope.vars`
function enter_scope!(ctx, ex)
    @assert kind(ex) in KSet"lambda scope_block"
    # Note that generated functions produce lambdas with this false
    is_toplevel_thunk = kind(ex) === K"lambda" && ex.is_toplevel_thunk
    parent_id = (is_toplevel_thunk || isempty(ctx.scope_stack)) ?
        0 : ctx.scopes[ctx.scope_stack[end]].id
    scope = ScopeInfo(ctx, parent_id, ex)
    lambda_scope = ctx.scopes[scope.lambda_id]
    push!(ctx.scope_stack, scope.id)

    #---------------------------------------------------------------------------
    # Find explicit decls that may influence assignment assignment resolution
    if kind(ex) === K"lambda"
        for c in children(ex[1])
            @assert kind(c) in KSet"Identifier BindingId Placeholder"
            maybe_declare_in_scope!(ctx, scope, c, :argument)
        end
        for c in children(ex[2])
            @assert kind(c) in KSet"Identifier BindingId Placeholder"
            maybe_declare_in_scope!(ctx, scope, c, :static_parameter)
        end
        for c in children(ex)[3:end]
            _find_scope_decls!(ctx, scope, c)
        end
    else
        for c in children(ex)
            _find_scope_decls!(ctx, scope, c)
        end
    end

    #---------------------------------------------------------------------------
    # Find assignment targets, possibly introducing implicit locals and globals
    for (bid, node_id) in sort!(collect(scope.binding_assignments))
        # Mutable nameless bindings may be introduced in desugaring.  These
        # should be capturable, and may be local to the nearest lambda or
        # global.  Desugaring should ensure these are never used undef.
        maybe_declare_in_scope!(ctx, scope, SyntaxTree(ctx.graph, node_id),
                                get_binding(ctx, bid).kind)
    end
    for (vk, node_id) in sort!(collect(scope.assignments))
        local ex = SyntaxTree(ctx.graph, node_id)
        b = resolve_name(ctx, ex)
        if b === nothing
            if is_toplevel_thunk && !ctx.scope_layers[vk.layer].is_macro_expansion
                push!(ctx.soft_assignable_globals, vk)
                declare_in_scope!(ctx, top_scope(ctx), ex, :global)
            elseif scope.is_permeable && is_defined_and_owned_global(
                ctx.scope_layers[vk.layer].mod, Symbol(vk.name))
                # special soft scope rules: existing global variables are assigned to
                if ctx.enable_soft_scopes
                    push!(ctx.soft_assignable_globals, vk)
                    declare_in_scope!(ctx, top_scope(ctx), ex, :global)
                else
                    declare_in_scope!(ctx, scope, ex, :local; is_ambiguous_local=true)
                end
            else
                declare_in_scope!(ctx, scope, ex, :local)
            end
        elseif b.kind === :global
            if is_toplevel_thunk
                # assign-existing and make visible to soft scope
                push!(ctx.soft_assignable_globals, vk)
            elseif !isnothing(resolve_name(ctx, ex; exclude_toplevel_globals=true)) ||
                (ctx.enable_soft_scopes && scope.is_permeable &&
                vk in ctx.soft_assignable_globals)
                # assign-existing-global if this is an explicit global that
                # isn't at top level, or if the soft scope exception applies
            else
                declare_in_scope!(ctx, scope, ex, :local)
            end
        elseif b.kind === :static_parameter
            throw(LoweringError(ex, "cannot overwrite a static parameter"))
        elseif b.kind === :local || b.kind === :argument
            # unambiguous assignment to existing variable
            record_lambda_var!(ctx, scope, b, capt=true)
        end
    end

    return scope
end

function add_local_decls!(ctx, stmts, srcref, scope)
    # Add local decls to start of block so that closure conversion can
    # initialize if necessary.
    for id in sort!(collect(values(scope.vars)))
        binfo = get_binding(ctx, id)
        if binfo.kind == :local
            push!(stmts, @ast ctx srcref [K"local" binding_ex(ctx, id)])
        end
    end
end

function _resolve_scopes(ctx, ex::SyntaxTree,
                         @nospecialize(scope::Union{Nothing, ScopeInfo}))
    k = kind(ex)
    @assert scope isa ScopeInfo || k === K"lambda"
    if k == K"Identifier"
        b = resolve_name(ctx, ex)
        # Unresolved names are assumed global
        if isnothing(b)
            gid = declare_in_scope!(ctx, top_scope(ctx), ex, :global)
            b = get_binding(ctx, gid)
        end
        # Locals not present in the current lambda need capturing
        record_lambda_var!(ctx, scope, b, capt=true)
        newleaf(ctx, ex, K"BindingId", b.id)
    elseif k === K"BindingId"
        record_lambda_var!(ctx, scope, get_binding(ctx, ex), capt=true)
        ex
    elseif k == K"softscope"
        newleaf(ctx, ex, K"TOMBSTONE")
    elseif k == K"break" && numchildren(ex) >= 2
        # For break with value (break label value), process the value expression but not the label
        # This must come BEFORE !needs_resolution check since K"break" is in is_quoted
        @ast ctx ex [K"break" ex[1] _resolve_scopes(ctx, ex[2], scope)]
    elseif !needs_resolution(ex)
        ex
    elseif k == K"local"
        # Local declarations have a value of `nothing` according to flisp
        # lowering.
        # TODO: Should local decls be disallowed in value position?
        @ast ctx ex "nothing"::K"core"
    elseif k == K"decl"
        ex_out = mapchildren(e->_resolve_scopes(ctx, e, scope), ctx, ex)
        name = ex_out[1]
        if kind(name) != K"Placeholder"
            binfo = get_binding(ctx, name)
            if binfo.kind == :global && !is_top_scope(enclosing_lambda(ctx, scope))
                throw(LoweringError(ex, "type declarations for global variables must be at top level, not inside a function"))
            end
        end
        id = ex_out[1]
        if kind(id) != K"Placeholder"
            binfo = get_binding(ctx, id)
            if !isnothing(binfo.type)
                throw(LoweringError(ex, "multiple type declarations found for `$(binfo.name)`"))
            end
            binfo.type = ex_out[2]
        end
        ex_out
    elseif k == K"always_defined"
        resolve_name(ctx, ex[1]).is_always_defined = true
        newleaf(ctx, ex, K"TOMBSTONE")
    elseif k == K"lambda"
        newscope = enter_scope!(ctx, ex)
        arg_bindings = _resolve_scopes(ctx, ex[1], newscope)
        sparam_bindings = _resolve_scopes(ctx, ex[2], newscope)

        self_id = numchildren(arg_bindings) === 0 ? 0 : arg_bindings[1].var_id
        lambda_bindings = LambdaBindings(self_id, newscope.id, newscope.locals_capt)
        body_stmts = SyntaxList(ctx)
        add_local_decls!(ctx, body_stmts, ex, newscope)
        body = _resolve_scopes(ctx, ex[3], newscope)
        if kind(body) == K"block"
            append!(body_stmts, children(body))
        else
            push!(body_stmts, body)
        end
        ret_var = numchildren(ex) == 4 ?
            _resolve_scopes(ctx, ex[4], newscope) : nothing
        pop!(ctx.scope_stack)

        @ast ctx ex [K"lambda"(;lambda_bindings=lambda_bindings,
                               is_toplevel_thunk=ex.is_toplevel_thunk,
                               toplevel_pure=false)
            arg_bindings
            sparam_bindings
            [K"block"
                body_stmts...
            ]
            ret_var
        ]
    elseif k == K"scope_block"
        newscope = enter_scope!(ctx, ex)
        stmts = SyntaxList(ctx)
        add_local_decls!(ctx, stmts, ex, newscope)
        for e in children(ex)
            push!(stmts, _resolve_scopes(ctx, e, newscope))
        end
        pop!(ctx.scope_stack)
        @ast ctx ex [K"block" stmts...]
    elseif k == K"islocal"
        e1 = ex[1]
        islocal = kind(e1) == K"Identifier" &&
            let b = resolve_name(ctx, e1)
                !isnothing(b) && b.kind !== :global
            end
        @ast ctx ex islocal::K"Bool"
    elseif k == K"isglobal"
        e1 = ex[1]
        isglobal = kind(e1) == K"Identifier" &&
            let b = resolve_name(ctx, e1)
                isnothing(b) || b.kind === :global
            end
        @ast ctx ex isglobal::K"Bool"
    elseif k == K"locals"
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
        for sid in ctx.scope_stack
            for id in values(ctx.scopes[sid].vars)
                binfo = get_binding(ctx, id)
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
        newnode(ctx, ex, K"block", stmts)
    elseif k == K"assert"
        etype = extension_type(ex)
        if etype == "require_existing_locals"
            for v in ex[2:end]
                b = resolve_name(ctx, v)
                if isnothing(b) || !(b.kind in (:local, :argument))
                    throw(LoweringError(v, "`outer` annotations must match with a local variable in an outer scope but no such variable was found"))
                end
            end
        elseif etype == "global_toplevel_only"
            if !is_top_scope(scope)
                e = ex[2][1]
                throw(LoweringError(e, "$(kind(e)) is only allowed in global scope"))
            end
        elseif etype == "toplevel_only"
            if !is_top_scope(enclosing_lambda(ctx, scope))
                e = ex[2][1]
                throw(LoweringError(e, "this syntax is only allowed in top level code"))
            end
        else
            throw(LoweringError(ex, "Unknown syntax assertion"))
        end
        newleaf(ctx, ex, K"TOMBSTONE")
    elseif k == K"function_decl"
        resolved = mapchildren(e->_resolve_scopes(ctx, e, scope), ctx, ex)
        name = resolved[1]
        if kind(name) == K"BindingId"
            bk = get_binding(ctx, name).kind
            if bk == :argument
                throw(LoweringError(name, "Cannot add method to a function argument"))
            elseif bk == :global && !is_top_scope(enclosing_lambda(ctx, scope))
                throw(LoweringError(name, """
                    Global method definition needs to be placed at the top \
                    level, or use `eval()`"""))
            end
        end
        resolved
    elseif k == K"constdecl"
        resolved = mapchildren(e->_resolve_scopes(ctx, e, scope), ctx, ex)
        @assert kind(resolved[1]) === K"BindingId"
        if get_binding(ctx, resolved[1].var_id).kind === :local
            throw(LoweringError(ex, "unsupported `const` declaration on local variable"))
        elseif !is_top_scope(enclosing_lambda(ctx, scope))
            throw(LoweringError(ex, "unsupported `const` inside function"))
        end
        resolved
    elseif k == K"assign_or_constdecl_if_global"
        id = _resolve_scopes(ctx, ex[1], scope)
        bk = get_binding(ctx, id).kind
        @assert numchildren(ex) === 2
        assignment_kind = bk == :global ? K"constdecl" : K"="
        @ast ctx ex _resolve_scopes(ctx, [assignment_kind ex[1] ex[2]], scope)
    elseif k == K"symbolicblock"
        # Only recurse into the body (second child), not the label name (first child)
        @ast ctx ex [K"symbolicblock" ex[1] _resolve_scopes(ctx, ex[2], scope)]
    else
        mapchildren(e->_resolve_scopes(ctx, e, scope), ctx, ex)
    end
end

function _resolve_scopes(ctx, exs::AbstractVector, scope)
    out = SyntaxList(ctx)
    for e in exs
        push!(out, _resolve_scopes(ctx, e, scope))
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

struct VariableAnalysisContext{Attrs} <: AbstractLoweringContext
    graph::SyntaxGraph{Attrs}
    bindings::Bindings
    mod::Module
    scopes::Vector{ScopeInfo}
    lambda_bindings::LambdaBindings
    # Stack of method definitions for closure naming
    method_def_stack::SyntaxList{Attrs, Vector{NodeId}}
    # Collection of information about each closure, principally which methods
    # are part of the closure (and hence captures).
    closure_bindings::Dict{IdTag,ClosureBindings}
end

function VariableAnalysisContext(graph, bindings, mod, scopes, lambda_bindings)
    graph = ensure_attributes(graph, lambda_bindings=LambdaBindings)
    VariableAnalysisContext(graph, bindings, mod, scopes, lambda_bindings,
                            SyntaxList(graph), Dict{IdTag,ClosureBindings}())
end

function init_closure_bindings!(ctx, fname)
    func_name_id = fname.var_id
    @assert get_binding(ctx, func_name_id).kind === :local
    get!(ctx.closure_bindings, func_name_id) do
        name_stack = Vector{String}()
        for parentname in ctx.method_def_stack
            if kind(parentname) == K"BindingId"
                push!(name_stack, get_binding(ctx, parentname).name)
            end
        end
        push!(name_stack, get_binding(ctx, func_name_id).name)
        ClosureBindings(name_stack)
    end
end

function find_any_local_binding(ctx, ex)
    k = kind(ex)
    if k == K"BindingId"
        bkind = get_binding(ctx, ex.var_id).kind
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

function add_assign!(b::BindingInfo)
    b.is_assigned_once = !b.is_assigned
    b.is_assigned = true
end

# Update ctx.bindings metadata based on binding usage
function analyze_variables!(ctx, ex)
    k = kind(ex)
    if k == K"BindingId"
        b = get_binding(ctx, ex.var_id)
        b.is_read = true
        # The type of typed locals is invisible in the previous pass,
        # but is filled in here.
        scope = ctx.scopes[ctx.lambda_bindings.scope_id]
        record_lambda_var!(ctx, scope, b, capt=true)
        @assert b.kind === :global || b.is_ssa || haskey(ctx.lambda_bindings.locals_capt, b.id)
    elseif k == K"Identifier"
        @assert false
    elseif k == K"break" && numchildren(ex) >= 2
        # For break with value, only analyze the value expression (second child), not the label
        # This must come BEFORE !needs_resolution check since K"break" is in is_quoted
        analyze_variables!(ctx, ex[2])
        return
    elseif !needs_resolution(ex)
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
            b = get_binding(ctx, lhs)
            add_assign!(b)
            scope = ctx.scopes[ctx.lambda_bindings.scope_id]
            record_lambda_var!(ctx, scope, b, capt=true)
            if !isnothing(b.type)
                # Assignments introduce a variable's type later during closure
                # conversion, but we must model that explicitly here.
                analyze_variables!(ctx, b.type)
            end
        end
        analyze_variables!(ctx, ex[2])
    elseif k == K"function_decl"
        name = ex[1]
        b = get_binding(ctx, name.var_id)
        if b.kind === :local
            init_closure_bindings!(ctx, name)
        end
        add_assign!(b)
    elseif k == K"function_type"
        if kind(ex[1]) != K"BindingId" || get_binding(ctx, ex[1]).kind !== :local
            analyze_variables!(ctx, ex[1])
        end
    elseif k == K"constdecl"
        b = get_binding(ctx, ex[1].var_id)
        b.is_const = true
        add_assign!(b)
    elseif k == K"call"
        name = ex[1]
        if kind(name) == K"BindingId"
            get_binding(ctx, name.var_id).is_called = true
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
                if get_binding(ctx, func_name_id).kind === :local
                    push!(ctx.closure_bindings[func_name_id].lambdas, lambda_bindings)
                end
            end
        end
        ctx2 = VariableAnalysisContext(
            ctx.graph, ctx.bindings, ctx.mod, ctx.scopes, lambda_bindings,
            ctx.method_def_stack, ctx.closure_bindings)
        foreach(e->analyze_variables!(ctx2, e), ex[3:end]) # body & return type
    elseif k == K"symbolicblock"
        # Only analyze the body (second child), not the label name (first child)
        analyze_variables!(ctx, ex[2])
    else
        foreach(e->analyze_variables!(ctx, e), children(ex))
    end
    nothing
end

function resolve_scopes(ctx::ScopeResolutionContext, ex)
    if kind(ex) != K"lambda"
        # Wrap in a top level thunk if we're not already expanding a lambda.
        # (Maybe this should be done elsewhere?)
        ex = @ast ctx ex [K"lambda"(is_toplevel_thunk=true, toplevel_pure=false)
            [K"block"]
            [K"block"]
            ex
        ]
    end
    _resolve_scopes(ctx, ex, nothing)
end

"""
This pass analyzes scopes and the names (locals/globals etc) used within them.

Names of kind `K"Identifier"` are transformed into binding identifiers of
kind `K"BindingId"`. The associated `Bindings` table in the context records
metadata about each binding.

This pass also records the set of binding IDs used locally within the
enclosing lambda form and information about variables captured by closures.
"""
@fzone "JL: resolve_scopes" function resolve_scopes(ctx::DesugaringContext, ex)
    ctx2 = ScopeResolutionContext(ctx, ex)
    ex2 = resolve_scopes(ctx2, reparent(ctx2, ex))
    ctx3 = VariableAnalysisContext(
        ctx2.graph, ctx2.bindings, ctx2.mod, ctx2.scopes, ex2.lambda_bindings)
    analyze_variables!(ctx3, ex2)
    analyze_def_and_use!(ctx3, ex2)
    ctx3, ex2
end
