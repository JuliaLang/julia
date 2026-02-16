#-------------------------------------------------------------------------------
# @chk: Basic AST structure checking tool
#
# Check a condition involving an expression, throwing a LoweringError if it
# doesn't evaluate to true. Does some very simple pattern matching to attempt
# to extract the expression variable from the left hand side.
#
# Forms:
# @chk pred(ex)
# @chk pred(ex) msg
# @chk pred(ex) (msg_display_ex, msg)
macro chk(cond, msg=nothing)
    if Meta.isexpr(msg, :tuple)
        ex = msg.args[1]
        msg = msg.args[2]
    else
        ex = cond
        while true
            if ex isa Symbol
                break
            elseif ex.head == :call
                ex = ex.args[2]
            elseif ex.head == :ref
                ex = ex.args[1]
            elseif ex.head == :.
                ex = ex.args[1]
            elseif ex.head in (:(==), :(in), :<, :>)
                ex = ex.args[1]
            else
                error("Can't analyze $cond")
            end
        end
    end
    quote
        ex = $(esc(ex))
        @assert ex isa SyntaxTree
        ok = try
            $(esc(cond))
        catch
            false
        end
        if !ok
            throw(LoweringError(ex, $(isnothing(msg) ? "expected `$cond`" : esc(msg))))
        end
    end
end

#-------------------------------------------------------------------------------
abstract type AbstractLoweringContext end

"""
Bindings for the current lambda being processed.

Lowering passes prior to scope resolution return `nothing` and bindings are
collected later.
"""
current_lambda_bindings(ctx::AbstractLoweringContext) = nothing

function JuliaSyntax.syntax_graph(ctx::AbstractLoweringContext)
    ctx.graph
end

"""
Unique symbolic identity for a variable, constant, label, or other entity
"""
const IdTag = Int

"""
Id for hygienic scope layers in macro expansion
"""
const LayerId = Int

"""
A `ScopeLayer` is a mechanism for automatic hygienic macros; every identifier
is assigned to a particular layer and can only match against bindings which are
themselves part of that layer.

Normal code contains a single scope layer, whereas each macro expansion
generates a new layer.
"""
struct ScopeLayer
    id::LayerId
    mod::Module
    parent_layer::LayerId # Index of parent layer in a macro expansion. Equal to 0 for no parent
    is_macro_expansion::Bool # FIXME
    is_internal::Bool
end

"""
Lexical scope ID
"""
const ScopeId = Int

function JuliaSyntax.newleaf(ctx::AbstractLoweringContext,
                    prov::Union{SyntaxTree, SourceAttrType},
                    k::Kind)
    newleaf(syntax_graph(ctx), prov, k)
end

function JuliaSyntax.newleaf(ctx, prov, k, @nospecialize(value))
    leaf = newleaf(ctx, prov, k)
    if k == K"Identifier" || k == K"core" || k == K"top" || k == K"Symbol" ||
            k == K"globalref" || k == K"Placeholder"
        setattr!(leaf._graph, leaf._id, :name_val, value)
    elseif k == K"BindingId"
        setattr!(leaf._graph, leaf._id, :var_id, value)
    elseif k == K"label"
        setattr!(leaf._graph, leaf._id, :id, value)
    elseif k == K"symboliclabel"
        setattr!(leaf._graph, leaf._id, :name_val, value)
    elseif k in KSet"TOMBSTONE SourceLocation latestworld latestworld_if_toplevel
                     softscope"
        # no attributes
    else
        val = k == K"Integer" ? convert(Int,     value) :
              k == K"Float"   ? convert(Float64, value) :
              k == K"String"  ? convert(String,  value) :
              k == K"Char"    ? convert(Char,    value) :
              k == K"Value"   ? value                   :
              k == K"Bool"    ? value                   :
              k == K"VERSION" ? value                   :
              error("Unexpected leaf kind `$k`")
        setattr!(leaf._graph, leaf._id, :value, val)
    end
    leaf
end

JuliaSyntax.newnode(ctx::AbstractLoweringContext,
                    prov::Union{SyntaxTree, SourceAttrType},
                    k::Kind, cs) =
    newnode(syntax_graph(ctx), prov, k, cs)

# Convenience functions to create leaf nodes referring to identifiers within
# the Core and Top modules.
core_ref(ctx, ex, name) = newleaf(ctx, ex, K"core", name)
nothing_(ctx, ex) = core_ref(ctx, ex, "nothing")

# Assign `ex` to an SSA variable.
# Return (variable, assignment_node)
function assign_tmp(ctx::AbstractLoweringContext, ex, name="tmp")
    var = ssavar(ctx, ex, name)
    assign_var = newnode(ctx, ex, K"=", tree_ids(var, ex))
    var, assign_var
end

function emit_assign_tmp(stmts::SyntaxList, ctx, ex, name="tmp")
    if is_ssa(ctx, ex)
        return ex
    end
    var = ssavar(ctx, ex, name)
    push!(stmts, newnode(ctx, ex, K"=", tree_ids(var, ex)))
    var
end

#-------------------------------------------------------------------------------
# @ast macro

_node_id(graph::SyntaxGraph, ex::SyntaxTree) = (check_compatible_graph(graph, ex); ex._id)

_node_ids(graph::SyntaxGraph) = ()
_node_ids(graph::SyntaxGraph, ::Nothing, cs...) = _node_ids(graph, cs...)
_node_ids(graph::SyntaxGraph, c, cs...) = (_node_id(graph, c), _node_ids(graph, cs...)...)
_node_ids(graph::SyntaxGraph, cs::SyntaxList, cs1...) = (_node_ids(graph, cs...)..., _node_ids(graph, cs1...)...)
function _node_ids(graph::SyntaxGraph, cs::SyntaxList)
    check_compatible_graph(graph, cs)
    cs.ids
end

function _node_id(graph::SyntaxGraph, ex)
    # Fallback to give a comprehensible error message for use with the @ast macro
    error("Attempt to use `$(repr(ex))` of type `$(typeof(ex))` as an AST node. Try annotating with `::K\"your_intended_kind\"?`")
end
function _node_id(graph::SyntaxGraph, ex::AbstractVector{<:SyntaxTree})
    # Fallback to give a comprehensible error message for use with the @ast macro
    error("Attempt to use vector as an AST node. Did you mean to splat this? (content: `$(repr(ex))`)")
end

function _push_nodeid!(graph::SyntaxGraph, ids::Vector{NodeId}, val)
    push!(ids, _node_id(graph, val))
end
function _push_nodeid!(graph::SyntaxGraph, ids::Vector{NodeId}, val::Nothing)
    nothing
end
function _append_nodeids!(graph::SyntaxGraph, ids::Vector{NodeId}, vals)
    for v in vals
        _push_nodeid!(graph, ids, v)
    end
end
function _append_nodeids!(graph::SyntaxGraph, ids::Vector{NodeId}, vals::SyntaxList)
    check_compatible_graph(graph, vals)
    append!(ids, vals.ids)
end

function _match_srcref(ex)
    if Meta.isexpr(ex, :macrocall) && ex.args[1] == Symbol("@HERE")
        QuoteNode(ex.args[2])
    else
        esc(ex)
    end
end

function _kw_to_pair(ex)
    if ex isa Expr && ex.head === :kw && ex.args[1] isa Symbol
        (QuoteNode(ex.args[1]), esc(ex.args[2]))
    elseif ex isa Symbol
        (QuoteNode(ex), esc(ex))
    else
        @assert false "invalid keyword form in @ast $ex"
    end
end

function _match_kind(srcref, ex)
    kws = []
    if Meta.isexpr(ex, :call)
        kind = esc(ex.args[1])
        args = ex.args[2:end]
        if Meta.isexpr(args[1], :parameters)
            kws = map(_kw_to_pair, args[1].args)
            popfirst!(args)
        end
        while length(args) >= 1 && Meta.isexpr(args[end], :kw)
            pushfirst!(kws, _kw_to_pair(pop!(args)))
        end
        if length(args) == 1
            srcref_tmp = gensym("srcref")
            return (kind, _match_srcref(args[1]), kws)
        elseif length(args) > 1
            error("Unexpected: extra srcref argument in `$ex`?")
        end
    else
        kind = esc(ex)
    end
    return (kind, srcref, kws)
end

function _expand_ast_tree(ctx, srcref, tree)
    if Meta.isexpr(tree, :(::))
        # Leaf node
        if length(tree.args) == 2
            val = esc(tree.args[1])
            kindspec = tree.args[2]
        else
            val = nothing
            kindspec = tree.args[1]
        end
        let (kind, srcref, kws) = _match_kind(srcref, kindspec)
            n = :(newleaf($ctx, $srcref, $kind, $val))
            for (attr, val) in kws
                n = :(setattr!($n, $attr, $val))
            end
            n
        end
    elseif Meta.isexpr(tree, :call) && tree.args[1] === :(=>)
        # Leaf node with copied attributes
        kind = esc(tree.args[3])
        srcref2 = esc(tree.args[2])
        :(setattr!(mkleaf($srcref2), :kind, $kind))
    elseif Meta.isexpr(tree, (:vcat, :hcat, :vect))
        # Interior node
        flatargs = []
        for a in tree.args
            if Meta.isexpr(a, :row)
                append!(flatargs, a.args)
            else
                push!(flatargs, a)
            end
        end
        children_ex = :(let child_ids = Vector{NodeId}(), graph = syntax_graph($ctx)
        end)
        child_stmts = children_ex.args[2].args
        for a in flatargs[2:end]
            child = _expand_ast_tree(ctx, srcref, a)
            if Meta.isexpr(child, :(...))
                push!(child_stmts, :(_append_nodeids!(graph, child_ids, $(child.args[1]))))
            else
                push!(child_stmts, :(_push_nodeid!(graph, child_ids, $child)))
            end
        end
        push!(child_stmts, :(child_ids))
        let (kind, srcref, kws) = _match_kind(srcref, flatargs[1])
            n = :(newnode($ctx, $srcref, $kind, $children_ex))
            for (attr, val) in kws
                n = :(setattr!($n, $attr, $val))
            end
            n
        end
    elseif Meta.isexpr(tree, :(:=))
        lhs = tree.args[1]
        rhs = _expand_ast_tree(ctx, srcref, tree.args[2])
        ssadef = gensym("ssadef")
        quote
            ($(esc(lhs)), $ssadef) = assign_tmp($ctx, $rhs, $(string(lhs)))
            $ssadef
        end
    elseif Meta.isexpr(tree, :macrocall)
        esc(tree)
    elseif tree isa Expr
        Expr(tree.head, map(a->_expand_ast_tree(ctx, srcref, a), tree.args)...)
    else
        esc(tree)
    end
end

"""
    @ast ctx srcref tree

Syntactic s-expression shorthand for constructing a `SyntaxTree` AST.

* `ctx` - SyntaxGraph context
* `srcref` - Reference to the source code from which this AST was derived.

The `tree` contains syntax of the following forms:
* `[kind child₁ child₂]` - construct an interior node with children
* `value :: kind`        - construct a leaf node
* `ex => kind`           - convert a leaf node to the given `kind`, copying attributes
                           from it and also using `ex` as the source reference.
* `var := ex`            - Set `var=ssavar(...)` and return an assignment node `\$var=ex`.
                           `var` may be used outside `@ast`
* `cond ? ex1 : ex2`     - Conditional; `ex1` and `ex2` will be recursively expanded.
                           `if ... end` and `if ... else ... end` also work with this.

Any `kind` can be replaced with an expression of the form
* `kind(srcref)` - override the source reference for this node and its children
* `kind(attr=val)` - set an additional attribute
* `kind(srcref; attr₁=val₁, attr₂=val₂)` - the general form

In any place `srcref` is used, the special form `@HERE()` can be used to instead
to indicate that the "primary" location of the source is the location where
`@HERE` occurs.


# Examples

```
@ast ctx srcref [
   K"toplevel"
   [K"using"
       [K"importpath"
           "Base"       ::K"Identifier"(src)
       ]
   ]
   [K"function"
       [K"call"
           "eval"       ::K"Identifier"
           "x"          ::K"Identifier"
       ]
       [K"call"
           "eval"       ::K"core"
           mn           =>K"Identifier"
           "x"          ::K"Identifier"
       ]
   ]
]
```
"""
macro ast(ctx, srcref, tree)
    quote
        ctx = $(esc(ctx))
        srcref::$SyntaxTree = $(_match_srcref(srcref))
        $(_expand_ast_tree(:ctx, :srcref, tree))
    end
end

#-------------------------------------------------------------------------------
function set_scope_layer(ctx, ex, layer_id, force)
    k = kind(ex)
    new_layer = force ? layer_id : get(ex, :scope_layer, layer_id)

    ex2 = if k == K"module" || k == K"toplevel" || k == K"inert" || k == K"inert_syntaxtree"
        mknode(ex, children(ex))
    elseif k == K"."
        cs = tree_ids(set_scope_layer(ctx, ex[1], layer_id, force), ex[2])
        mknode(ex, cs)
    elseif !is_leaf(ex)
        mapchildren(e->set_scope_layer(ctx, e, layer_id, force), ctx, ex)
    else
        mkleaf(ex)
    end
    setattr!(ex2, :scope_layer, new_layer)
end

"""
    adopt_scope(ex, ref)

Copy `ex`, adopting the scope layer of `ref`.
"""
function adopt_scope(ex::SyntaxTree, scope_layer::LayerId)
    set_scope_layer(ex._graph, ex, scope_layer, true)
end

function adopt_scope(ex::SyntaxTree, layer::ScopeLayer)
    adopt_scope(ex, layer.id)
end

function adopt_scope(ex::SyntaxTree, ref::SyntaxTree)
    adopt_scope(ex, ref.scope_layer)
end

function adopt_scope(exs::SyntaxList, ref)
    out = SyntaxList(syntax_graph(exs))
    for e in exs
        push!(out, adopt_scope(e, ref))
    end
    return out
end

# Type for `meta` attribute, to replace `Expr(:meta)`.
# It's unclear how much flexibility we need here - is a dict good, or could we
# just use a struct? Likely this will be sparse. Alternatively we could just
# use individual attributes but those aren't easy to add on an ad-hoc basis in
# the middle of a pass.
const CompileHints = Base.ImmutableDict{Symbol,Any}

function setmeta!(ex::SyntaxTree, key::Symbol, @nospecialize(val))
    meta = begin
        m = get(ex, :meta, nothing)
        isnothing(m) ? CompileHints(key, val) : CompileHints(m, key, val)
    end
    setattr!(ex, :meta, meta)
    ex
end

setmeta(ex::SyntaxTree, k::Symbol, @nospecialize(v)) =
    setmeta!(copy_node(ex), k, v)

function getmeta(ex::SyntaxTree, name::Symbol, default)
    meta = get(ex, :meta, nothing)
    isnothing(meta) ? default : get(meta, name, default)
end

name_hint(name) = CompileHints(:name_hint, name)

#-------------------------------------------------------------------------------
# Predicates and accessors working on expression trees

# For historical reasons, `cglobal` and `ccall` are their own special
# quasi-identifier-like syntax but with special handling inside lowering which
# means they can't be used as normal identifiers.
function is_ccall_or_cglobal(name::AbstractString)
    return name == "ccall" || name == "cglobal"
end

function is_quoted(ex)
    kind(ex) in KSet"Symbol quote top core globalref break inert
                     inert_syntaxtree meta inbounds inline noinline loopinfo"
end

function extension_type(ex)
    @assert kind(ex) == K"assert"
    @chk numchildren(ex) >= 1
    @chk kind(ex[1]) == K"Symbol"
    ex[1].name_val
end

function is_sym_decl(x)
    k = kind(x)
    k == K"Identifier" || k == K"::"
end

function is_eventually_call(ex::SyntaxTree)
    k = kind(ex)
    return k == K"call" || ((k == K"where" || k == K"::") && is_eventually_call(ex[1]))
end

function find_parameters_ind(exs)
    i = length(exs)
    while i >= 1
        k = kind(exs[i])
        if k == K"parameters"
            return i
        elseif k != K"do"
            break
        end
        i -= 1
    end
    return 0
end

function has_parameters(ex::SyntaxTree)
    find_parameters_ind(children(ex)) != 0
end

function has_parameters(args::AbstractVector)
    find_parameters_ind(args) != 0
end

function any_assignment(exs)
    any(kind(e) == K"=" for e in exs)
end

function is_valid_modref(ex)
    return kind(ex) == K"." && kind(ex[2]) == K"Symbol" &&
           (kind(ex[1]) == K"Identifier" || is_valid_modref(ex[1]))
end

function is_core_ref(ex, name)
    kind(ex) == K"core" && ex.name_val == name
end

function is_core_nothing(ex)
    is_core_ref(ex, "nothing")
end

function is_core_Any(ex)
    is_core_ref(ex, "Any")
end

function is_simple_atom(ctx, ex)
    k = kind(ex)
    # TODO thismodule
    is_literal(k) || k == K"Symbol" || k == K"Value" || is_ssa(ctx, ex) || is_core_nothing(ex)
end

function is_identifier_like(ex)
    k = kind(ex)
    k == K"Identifier" || k == K"BindingId" || k == K"Placeholder"
end

function decl_var(ex)
    kind(ex) == K"::" ? ex[1] : ex
end

# Given the signature of a `function`, return the symbol that will ultimately
# be assigned to in local/global scope, if any.
function assigned_function_name(ex)
    while kind(ex) == K"where"
        # f() where T
        ex = ex[1]
    end
    if kind(ex) == K"::" && numchildren(ex) == 2
        # f()::T
        ex = ex[1]
    end
    if kind(ex) != K"call"
        throw(LoweringError(ex, "Expected call syntax in function signature"))
    end
    ex = ex[1]
    if kind(ex) == K"curly"
        # f{T}()
        ex = ex[1]
    end
    if kind(ex) == K"::" || kind(ex) == K"."
        # (obj::CallableType)(args)
        # A.b.c(args)
        nothing
    elseif is_identifier_like(ex)
        ex
    else
        throw(LoweringError(ex, "Unexpected name in function signature"))
    end
end

# Remove empty parameters block, eg, in the arg list of `f(x, y;)`
function remove_empty_parameters(args)
    i = length(args)
    while i > 0 && kind(args[i]) == K"parameters" && numchildren(args[i]) == 0
        i -= 1
    end
    args[1:i]
end

function to_symbol(ctx, ex)
    @ast ctx ex ex=>K"Symbol"
end

function new_scope_layer(ctx, mod_ref::Module=ctx.mod)
    new_layer = ScopeLayer(length(ctx.scope_layers)+1, ctx.mod, 0, false, true)
    push!(ctx.scope_layers, new_layer)
    new_layer.id
end

function new_scope_layer(ctx, mod_ref::SyntaxTree)
    @assert kind(mod_ref) == K"Identifier"
    new_scope_layer(ctx, ctx.scope_layers[mod_ref.scope_layer].mod)
end

#-------------------------------------------------------------------------------
# Context wrapper which helps to construct a list of statements to be executed
# prior to some expression. Useful when we need to use subexpressions multiple
# times.
struct StatementListCtx{Ctx, Attrs} <: AbstractLoweringContext
    ctx::Ctx
    stmts::SyntaxList{Attrs, Vector{NodeId}}
end

function Base.getproperty(ctx::StatementListCtx, field::Symbol)
    if field === :ctx
        getfield(ctx, :ctx)
    elseif field === :stmts
        getfield(ctx, :stmts)
    else
        getproperty(getfield(ctx, :ctx), field)
    end
end

function emit(ctx::StatementListCtx, ex)
    push!(ctx.stmts, ex)
end

function emit_assign_tmp(ctx::StatementListCtx, ex, name="tmp")
    emit_assign_tmp(ctx.stmts, ctx.ctx, ex, name)
end

with_stmts(ctx, stmts) = StatementListCtx(ctx, stmts)
with_stmts(ctx::StatementListCtx, stmts) = StatementListCtx(ctx.ctx, stmts)

function with_stmts(ctx)
    StatementListCtx(ctx, SyntaxList(ctx))
end

with_stmts(ctx::StatementListCtx) = StatementListCtx(ctx.ctx)
