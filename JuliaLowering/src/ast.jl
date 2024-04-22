# AST creation utilities
_node_id(ex::NodeId) = ex
_node_id(ex::SyntaxTree) = ex.id

_node_ids() = ()
_node_ids(::Nothing, cs...) = _node_ids(cs...)
_node_ids(c, cs...) = (_node_id(c), _node_ids(cs...)...)

function _makenode(graph::SyntaxGraph, srcref, head, children; attrs...)
    id = newnode!(graph)
    # TODO: Having this list seeems hacky? Use makeleaf everywhere instead.
    if isnothing(children) || kind(head) in (K"Identifier", K"core", K"top", K"SSAValue", K"Value", K"slot") || is_literal(head)
        @assert isnothing(children) || length(children) == 0
    else
        setchildren!(graph, id, children)
    end
    srcref_attr = srcref isa SyntaxTree ? srcref.id : srcref
    setattr!(graph, id; source=srcref_attr, attrs...)
    sethead!(graph, id, head)
    return SyntaxTree(graph, id)
end

function makenode(ctx, srcref, head, children::Union{Nothing,SyntaxTree}...; attrs...)
    _makenode(syntax_graph(ctx), srcref, head, _node_ids(children...); attrs...)
end

function makenode(ctx::Union{AbstractLoweringContext,SyntaxTree},
                  srcref, head, children::SyntaxList; attrs...)
    graph = syntax_graph(ctx)
    syntax_graph(ctx) === syntax_graph(children) || error("Mismatching graphs")
    _makenode(graph, srcref, head, children.ids; attrs...)
end

function makeleaf(ctx, srcref, kind, value; kws...)
    graph = syntax_graph(ctx)
    if kind == K"Identifier" || kind == K"core" || kind == K"top"
        _makenode(graph, srcref, kind, nothing; name_val=value, kws...)
    elseif kind == K"SSAValue"
        _makenode(graph, srcref, kind, nothing; var_id=value, kws...)
    else
        val = kind == K"Integer" ? convert(Int,     value) :
              kind == K"Float"   ? convert(Float64, value) :
              kind == K"String"  ? convert(String,  value) :
              kind == K"Symbol"  ? convert(String,  value) :
              kind == K"Char"    ? convert(Char,    value) :
              kind == K"Value"   ? value                   :
              error("Unexpected leaf kind `$kind`")
        _makenode(graph, srcref, kind, nothing; value=val, kws...)
    end
end

function makeleaf(ctx, srcref, kind)
    _makenode(syntax_graph(ctx), srcref, kind, nothing)
end

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
    elseif Meta.isexpr(tree, (:vcat, :hcat))
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
* `value :: kind`         - construct a leaf node
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
           mn.name_val  ::K"Identifier"
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

function mapchildren(f, ctx, ex)
    if haschildren(ex)
        cs = SyntaxList(ctx)
        for e in children(ex)
            push!(cs, f(e))
        end
        ex2 = makenode(ctx, ex, head(ex), cs)
    else
        ex2 = makeleaf(ctx, ex, head(ex))
    end
    # Copy all attributes.
    # TODO: Make this type stable and efficient
    for v in values(ex.graph.attributes)
        if haskey(v, ex.id)
            v[ex2.id] = v[ex.id]
        end
    end
    return ex2
end

# Convenience functions to create leaf nodes referring to identifiers within
# the Core and Top modules.
core_ref(ctx, ex, name) = makeleaf(ctx, ex, K"core", name)
Any_type(ctx, ex) = core_ref(ctx, ex, "Any")
svec_type(ctx, ex) = core_ref(ctx, ex, "svec")
nothing_(ctx, ex) = core_ref(ctx, ex, "nothing")
unused(ctx, ex) = core_ref(ctx, ex, "UNUSED")

top_ref(ctx, ex, name) = makeleaf(ctx, ex, K"top", name)

#-------------------------------------------------------------------------------
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

