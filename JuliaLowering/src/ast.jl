#-------------------------------------------------------------------------------
abstract type AbstractLoweringContext end

"""
Unique symbolic identity for a variable
"""
const VarId = Int

const LayerId = Int

function syntax_graph(ctx::AbstractLoweringContext)
    ctx.graph
end

function new_var_id(ctx::AbstractLoweringContext)
    id = ctx.next_var_id[]
    ctx.next_var_id[] += 1
    return id
end

#-------------------------------------------------------------------------------
# AST creation utilities
_node_id(ex::NodeId) = ex
_node_id(ex::SyntaxTree) = ex.id

_node_id(graph::SyntaxGraph, ex::SyntaxTree) = (check_same_graph(graph, ex); ex.id)

_node_ids(graph::SyntaxGraph) = ()
_node_ids(graph::SyntaxGraph, ::Nothing, cs...) = _node_ids(graph, cs...)
_node_ids(graph::SyntaxGraph, c, cs...) = (_node_id(graph, c), _node_ids(graph, cs...)...)
_node_ids(graph::SyntaxGraph, cs::SyntaxList, cs1...) = (_node_ids(graph, cs...)..., _node_ids(graph, cs1...)...)
function _node_ids(graph::SyntaxGraph, cs::SyntaxList)
    check_same_graph(graph, cs)
    cs.ids
end

function makeleaf(graph::SyntaxGraph, srcref, head; attrs...)
    id = newnode!(graph)
    source = srcref isa SyntaxTree ? _node_id(graph, srcref) : srcref
    setattr!(graph, id; source=source, attrs...)
    sethead!(graph, id, head)
    return SyntaxTree(graph, id)
end

function makenode(graph::SyntaxGraph, srcref, head, children...; attrs...)
    id = newnode!(graph)
    setchildren!(graph, id, _node_ids(graph, children...))
    source = srcref isa SyntaxTree ? _node_id(graph, srcref) : srcref
    setattr!(graph, id; source=source, attrs...)
    sethead!(graph, id, head)
    return SyntaxTree(graph, id)
end

function makenode(ctx, srcref, head, children...; attrs...)
    makenode(syntax_graph(ctx), srcref, head, children...; attrs...)
end

function makeleaf(ctx, srcref, kind; kws...)
    makeleaf(syntax_graph(ctx), srcref, kind; kws...)
end

function makeleaf(ctx, srcref, kind, value; kws...)
    graph = syntax_graph(ctx)
    if kind == K"Identifier" || kind == K"core" || kind == K"top" || kind == K"Symbol" || kind == K"globalref"
        makeleaf(graph, srcref, kind; name_val=value, kws...)
    elseif kind == K"SSAValue"
        makeleaf(graph, srcref, kind; var_id=value, kws...)
    else
        val = kind == K"Integer" ? convert(Int,     value) :
              kind == K"Float"   ? convert(Float64, value) :
              kind == K"String"  ? convert(String,  value) :
              kind == K"Char"    ? convert(Char,    value) :
              kind == K"Value"   ? value                   :
              error("Unexpected leaf kind `$kind`")
        makeleaf(graph, srcref, kind; value=val, kws...)
    end
end

# Convenience functions to create leaf nodes referring to identifiers within
# the Core and Top modules.
core_ref(ctx, ex, name) = makeleaf(ctx, ex, K"core", name)
Any_type(ctx, ex) = core_ref(ctx, ex, "Any")
svec_type(ctx, ex) = core_ref(ctx, ex, "svec")
nothing_(ctx, ex) = core_ref(ctx, ex, "nothing")
unused(ctx, ex) = core_ref(ctx, ex, "UNUSED")

top_ref(ctx, ex, name) = makeleaf(ctx, ex, K"top", name)

# Create a new SSA variable
function ssavar(ctx::AbstractLoweringContext, srcref)
    makenode(ctx, srcref, K"SSAValue", var_id=new_var_id(ctx))
end

# Assign `ex` to an SSA variable.
# Return (variable, assignment_node)
function assign_tmp(ctx::AbstractLoweringContext, ex)
    var = ssavar(ctx, ex)
    assign_var = makenode(ctx, ex, K"=", var, ex)
    var, assign_var
end


#-------------------------------------------------------------------------------
# @ast macro
function _match_srcref(ex)
    if Meta.isexpr(ex, :macrocall) && ex.args[1] == Symbol("@HERE")
        QuoteNode(ex.args[2])
    else
        esc(ex)
    end
end

function _match_kind_ex(defs, srcref, ex)
    kws = []
    if Meta.isexpr(ex, :call)
        kind = esc(ex.args[1])
        args = ex.args[2:end]
        if Meta.isexpr(args[1], :parameters)
            kws = map(esc, args[1].args)
            popfirst!(args)
        end
        while length(args) >= 1 && Meta.isexpr(args[end], :kw)
            pushfirst!(kws, esc(pop!(args)))
        end
        if length(args) == 1
            srcref = Symbol("srcref_$(length(defs))")
            push!(defs, :($srcref = $(_match_srcref(args[1]))))
        elseif length(args) > 1
            error("Unexpected: extra srcref argument in `$ex`?")
        end
    else
        kind = esc(ex)
    end
    kind, srcref, kws
end

function _expand_ast_tree(defs, ctx, srcref, tree)
    if Meta.isexpr(tree, :(::))
        # Leaf node
        kind, srcref, kws = _match_kind_ex(defs, srcref, tree.args[2])
        :(makeleaf($ctx, $srcref, $kind, $(esc(tree.args[1])), $(kws...)))
    elseif Meta.isexpr(tree, :call) && tree.args[1] === :(=>)
        # Leaf node with copied attributes
        kind = esc(tree.args[3])
        srcref = esc(tree.args[2])
        :(mapleaf($ctx, $srcref, $kind))
    elseif Meta.isexpr(tree, (:vcat, :hcat, :vect))
        # Interior node
        flatargs = []
        for a in tree.args
            if Meta.isexpr(a, :row)
                append!(flagargs, a.args)
            else
                push!(flatargs, a)
            end
        end
        kind, srcref, kws = _match_kind_ex(defs, srcref, flatargs[1])
        children = map(a->_expand_ast_tree(defs, ctx, srcref, a), flatargs[2:end])
        :(makenode($ctx, $srcref, $kind, $(children...), $(kws...)))
    elseif Meta.isexpr(tree, :(=))
        lhs = esc(tree.args[1])
        rhs = _expand_ast_tree(defs, ctx, srcref, tree.args[2])
        ssadef = Symbol("ssadef$(length(defs))")
        push!(defs, :(($lhs, $ssadef) = assign_tmp($ctx, $rhs)))
        ssadef
    elseif Meta.isexpr(tree, :if)
        Expr(:if, esc(tree.args[1]),
             map(a->_expand_ast_tree(defs, ctx, srcref, a), tree.args[2:end])...)
    elseif Meta.isexpr(tree, (:block, :tuple))
        Expr(tree.head, map(a->_expand_ast_tree(defs, ctx, srcref, a), tree.args)...)
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
* `var=ex`               - Set `var=ssavar(...)` and return an assignment node `\$var=ex`.
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
    defs = []
    push!(defs, :(ctx = $(esc(ctx))))
    push!(defs, :(srcref = $(_match_srcref(srcref))))
    ex = _expand_ast_tree(defs, :ctx, :srcref, tree)
    quote
        $(defs...)
        $ex
    end
end

#-------------------------------------------------------------------------------
# Mapping and copying of AST nodes
function copy_attrs!(dest, src)
    # TODO: Make this faster?
    for (k,v) in pairs(dest.graph.attributes)
        if (k !== :source && k !== :kind && k !== :syntax_flags) && haskey(v, src.id)
            v[dest.id] = v[src.id]
        end
    end
end

function mapleaf(ctx, src, kind)
    ex = makeleaf(syntax_graph(ctx), src, kind)
    # TODO: Value coersion might be broken here due to use of `name_val` vs
    # `value` vs ... ?
    copy_attrs!(ex, src)
    ex
end

function mapchildren(f, ctx, ex)
    if !haschildren(ex)
        return ex
    end
    orig_children = children(ex)
    cs = nothing
    for (i,e) in enumerate(orig_children)
        c = f(e)
        if isnothing(cs)
            if c == e
                continue
            else
                cs = SyntaxList(ctx)
                append!(cs, orig_children[1:i-1])
            end
        end
        push!(cs::SyntaxList, c)
    end
    if isnothing(cs)
        # This function should be allocation-free if no children were changed
        # by the mapping.
        return ex
    end
    cs::SyntaxList
    ex2 = makenode(ctx, ex, head(ex), cs)
    copy_attrs!(ex2, ex)
    return ex2
end

"""
Copy AST `ex` into `ctx`
"""
function copy_ast(ctx, ex)
    if haschildren(ex)
        cs = SyntaxList(ctx)
        for e in children(ex)
            push!(cs, copy_ast(ctx, e))
        end
        ex2 = makenode(ctx, sourceref(ex), head(ex), cs)
    else
        ex2 = makeleaf(ctx, sourceref(ex), head(ex))
    end
    for (name,attr) in pairs(ex.graph.attributes)
        if (name !== :source && name !== :kind && name !== :syntax_flags) &&
                haskey(attr, ex.id)
            attr2 = getattr(ex2.graph, name, nothing)
            if !isnothing(attr2)
                attr2[ex2.id] = attr[ex.id]
            end
        end
    end
    return ex2
end

"""
    adopt_scope(ex, ref)

Copy `ex`, adopting the scope layer of `ref`.
"""
function adopt_scope(ex, scope_layer::LayerId)
    ex1 = copy_ast(ex, ex)
    set_scope_layer_recursive!(ex1, scope_layer, true)
    ex1
end

function adopt_scope(ex, ref::SyntaxTree)
    adopt_scope(ex, ref.scope_layer)
end

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

function is_valid_name(ex)
    n = identifier_name(ex).name_val
    n !== "ccall" && n !== "cglobal"
end

function identifier_name(ex)
    kind(ex) == K"var" ? ex[1] : ex
end

function decl_var(ex)
    kind(ex) == K"::" ? ex[1] : ex
end

# Remove empty parameters block, eg, in the arg list of `f(x, y;)`
function remove_empty_parameters(args)
    i = length(args)
    while i > 0 && kind(args[i]) == K"parameters" && numchildren(args[i]) == 0
        i -= 1
    end
    args[1:i]
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

