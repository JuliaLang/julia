# Data structures used by macro-expansion and lowering

mutable struct ScopeLayer
    const mod::Module
    const escaped::Union{Nothing, ScopeLayer}
end

"""
Each node has a SyntaxContext describing its macro expansion and edition.
`SyntaxContext` is shared between all nodes of a single macro expansion, and is
one-to-one with ScopeLayer, with a few exceptions (contexts sharing same layer):
- `escape` and adopt_scope
- Desugaring creates internal contexts in its better version of `gensym`

We may want to move layer out of this struct for easier adopt_scope and
rebase_layer operations, but assuming mostly hygienic macros and few
scope-changing functions, this is most compact.
"""
mutable struct SyntaxContext
    const layer::Union{Nothing, ScopeLayer}
    # For provenance; is not affected by escaping
    const unexpanded::Any # Union{SyntaxTree, Nothing}
    const edition::Tuple{Int, Int}
    const internal::Bool
end

# Reference to bytes within a source file
struct SourceRef
    file::Any # Base.RefValue{JuliaSyntax.SourceFile}
    first_byte::UInt32
    last_byte::UInt32
end

mutable struct SyntaxTree
    const head::Symbol
    # Should be considered immutable
    const children::Union{Nothing, Vector{SyntaxTree}}
    const value::Any
    const source::Union{SyntaxTree,SourceRef,LineNumberNode}
    const context::SyntaxContext
    const jl_source::Union{Nothing, LineNumberNode}
    meta::Union{Nothing, Base.ImmutableDict{Symbol,Any}}
    # TODO: this is rarely used, and should just be part of context
    const mod::Union{Nothing, Module}
    # TODO: this is almost never populated and semantically irrelevant after
    # parsing
    const syntax_flags::UInt16
end
const SourceAttrType = Union{SyntaxTree,SourceRef,LineNumberNode}

# A default context corresponding to no expansion
function SyntaxContext(mod::Module, edition::Tuple{Int, Int})
    SyntaxContext(ScopeLayer(mod, nothing), nothing, edition, false)
end

const JL_NEW_EDITION = (1, 15)
const JL_OLD_EDITION = (1, 14)

function SyntaxTree(head::Symbol, children, @nospecialize(value), source, context)
    SyntaxTree(head, children, value, source, context,
               nothing, nothing, nothing, UInt16(0))
end

head(ex::SyntaxTree) = ex.head

is_leaf(ex::SyntaxTree) = ex.children === nothing

function numchildren(ex::SyntaxTree)
    cs = ex.children
    isnothing(cs) ? 0 : length(cs)
end

# TODO: Better to make this an error, since it can cause nodes that were
# intended to be leaves `SyntaxTree(head, children(old), ...)` to be non-leaves
const NO_CHILDREN = SyntaxTree[]

function children(ex::SyntaxTree)
    cs = ex.children
    cs === nothing ? NO_CHILDREN : cs
end

function Base.getindex(ex::SyntaxTree, i::Integer)
    ex.children[i]
end

function Base.getindex(ex::SyntaxTree, r::UnitRange)
    @view ex.children[r]
end

Base.firstindex(::SyntaxTree) = 1
Base.lastindex(ex::SyntaxTree) = numchildren(ex)

#-------------------------------------------------------------------------------
# AST creation utilities

# fallback printing.  TODO: vulnerable to invalidations
function node_string(ex::SyntaxTree, depth=2)
    out = "(head="*string(head(ex))
    for n in sort!(collect(fieldnames(typeof(ex))))
        val = getproperty(ex, n)
        if !isnothing(val) && n !== :head
            val_str = if val isa SyntaxTree && depth > 1
                node_string(val, depth-1)
            elseif isbits(val) || val isa
                Union{AbstractString, Symbol, Module, LineNumberNode}
                repr(val)
            else
                repr(typeof(val))
            end
            out *= ", "*string(n)*"="*val_str
        end
    end
    if is_leaf(ex)
        out *= ", leaf"
    elseif depth > 1
        out *= ", children=["
        for c in children(ex)
            out *= "\n"*node_string(c, depth-1)
        end
        out *= "]"
    end
    out *= ")"
    return out
end

# Tree invariants assumed everywhere, including `show`, so fallback printing
# should be used on failure.  (These checks really belong in the type system.)
# Failure should only be possible working on internals.
function assert_syntaxtree(st::SyntaxTree, recursive=true)
    vr = recursive ? _assert_syntaxtree(st, SyntaxTree[]) :
        _assert_syntaxtree_node(st)
    if vr !== nothing
        err_st, err = vr
        msg = string("assert_syntaxtree failed: ", node_string(st),
                     "\n  failing node: ", node_string(err_st),
                     "\n  reason: ", err)
        error(msg)
    end
    nothing
end

function _assert_syntaxtree_node(st::SyntaxTree)
    h = head(st)
    if is_leaf(st)
        if h === :globalref && st.mod === nothing
            return (st, "leaf globalref requires module in .mod")
        end
        (needs_val, valtype) =
            h === :identifier ? (true, String) :
            h === :value ? (true, Any) :
            h === :core ? (true, String) :
            h === :top ? (true, String) :
            h === :symbol ? (true, String) :
            h === :globalref ? (true, String) :
            h === :placeholder ? (false, Any) :
            h === :bindingid ? (true, Int) :
            h === :label ? (true, Int) :
            h === :symboliclabel ? (true, String) :
            h === :symbolicgoto ? (true, String) :
            h === :slot ? (true, Int) :
            h === :static_parameter ? (true, Int) :
            h === :ssavalue ? (true, Int) :
            h === :nothing ? (false, Any) :
            h === :tombstone ? (false, Any) :
            h === :error ? (false, Any) :
            h === :sourcelocation ? (false, Any) :
            h === :latestworld ? (false, Any) :
            h === :latestworld_if_toplevel ? (false, Any) :
            h === :strmacroname ? (true, String) :
            h === :cmdmacroname ? (true, String) :
            h === :lambdabindings ? (true, Any) : # JL.LambdaBindings
            h === :slots ? (true, Vector) : # Vector{JL.Slot}
            h === :version ? (true, VersionNumber) :
            # too lenient, but the green tree includes unspecified leaf nodes.
            # we hit this when JuliaSyntax.is_trivia(node) with some operators.
            st.source isa SourceRef ? (false, Any) :
                (return (st, "unrecognized leaf $(h)"))
        if needs_val && !(st.value isa valtype)
            return (st, "needs value ::"*string(valtype))
        end
    else
        # Note some kinds can show up as non-leaves too (mostly from Expr)
        if h in (:identifier, :value, :placeholder, :bindingid, :label, :symbol,
                 :nothing, :tombstone, :sourcelocation,
                 :lambdabindings, :slots)
            return (st, "Found leaf-only kind with children")
        end
    end
    nothing
end

# Cyclic references are still possible with children (as they are stored in a
# mutable vector), but other cycles (e.g. source) should be impossible by
# construction
function _assert_syntaxtree(st::SyntaxTree, parents::Vector{SyntaxTree})
    if st in parents
        err = "cycle detected: ["
        for p in parents
            err *= "\n" * node_string(p)
        end
        return (st, err*"]")
    end
    vr = _assert_syntaxtree_node(st)
    isnothing(vr) || return vr

    push!(parents, st)
    is_leaf(st) || for c in children(st)
        vr = _assert_syntaxtree(c, parents)
        isnothing(vr) || return vr
    end
    pop!(parents)
    nothing
end

const _DEFAULT_NODE = SyntaxTree(
    :none, nothing, nothing, LineNumberNode(0), SyntaxContext(Core, (0, 0)))

const DEBUG_LOWERING = true

"""
    @mknode(old; attr=val...)

Create a node `new` that is an immutable update of `old`, but setting `old` as
its provenance, and setting jl_source to macrocall's location.  `attrs` may
override `old`'s fields (so if `old` is not provided, some attrs are required.)

This is the main operation used by syntax transformations in lowering.
"""
macro mknode(attrs, old)
    Base.remove_linenums!(old)
    Base.remove_linenums!(attrs)
    old_gs = gensym()
    if !(isnothing(attrs) || attrs isa Expr && Meta.isexpr(attrs, :parameters))
        throw(ArgumentError("usage: @mknode(old; attr=val...)"))
    end
    out_args = Vector(undef, fieldcount(SyntaxTree))
    for (i, n) in enumerate(fieldnames(SyntaxTree))
        out_args[i] = (DEBUG_LOWERING && n === :jl_source) ? __source__ :
            n === :source ? old_gs :
            Expr(:(.), old_gs, QuoteNode(n))
    end
    seen_attrs = Set{Symbol}()
    attrs isa Expr && for a in attrs.args
        (aname, aval) = if Meta.isexpr(a, :(kw), 2) && a.args[1] isa Symbol
            (a.args[1]::Symbol, a.args[2])
        elseif a isa Symbol
            (a, a)
        else
            throw(ArgumentError("usage: @mknode(old; attr=val...)"))
        end
        aname in seen_attrs && throw(ArgumentError("duplicate attr provided $__source__"))
        push!(seen_attrs, aname)
        out_args[Base.fieldindex(SyntaxTree, aname)] = aval
    end
    old === _DEFAULT_NODE && !((:head, :source, :context) ⊆ seen_attrs) &&
        throw(ArgumentError("brand-new node from @mknode requires more attrs $__source__"))

    out = Expr(:let,
               Expr(:block, Expr(:(=), old_gs, old)),
               Expr(:block, Expr(:call, SyntaxTree, out_args...)))
    DEBUG_LOWERING && (out.args[end] = Expr(:call, _debug_check_attrs, out.args[end]))
    esc(out)
end
macro mknode(x)
    (old, attrs) = Meta.isexpr(x, :parameters) ? (_DEFAULT_NODE, x) : (x, nothing)
    esc(Expr(:macrocall, var"@mknode", __source__, attrs, old))
end

function _debug_check_attrs(x)
    assert_syntaxtree(x, false)
    x
end

Base.setproperty!(ex::SyntaxTree, name::Symbol, @nospecialize(val)) =
    error("SyntaxTree: this can't be mutated")

# This function should be allocation-free if no children were changed
function mapchildren(f::Function, ex::SyntaxTree)
    if is_leaf(ex)
        return ex
    end
    orig_children = children(ex)
    cs = nothing
    for (i,e) in enumerate(orig_children)
        newchild = f(e)::SyntaxTree
        if isnothing(cs)
            if newchild == e
                continue
            else
                cs = Vector{SyntaxTree}(undef, length(orig_children))
                copyto!(cs, orig_children[1:i-1])
            end
        end
        cs[i] = newchild
    end
    if isnothing(cs)
        return ex
    end
    cs::Vector{SyntaxTree}
    ex2 = @mknode(ex; children=cs)
    return ex2
end

#-------------------------------------------------------------------------------
# Context and layers

is_base_layer(sc::SyntaxContext) = (sc.layer::ScopeLayer).escaped === nothing

# The scope corresponding to no macro expansion.  Use with caution: macros may
# expand to top-level forms, so "base layer" !== "this top-level thunk's
# pre-expansion context" (usually ctx.syntax_context).  Throws with no layer.
function base_layer(sc::SyntaxContext)
    l = sc.layer::ScopeLayer
    while l.escaped !== nothing
        l = l.escaped
    end
    return l
end

function escape_layer(sc::SyntaxContext, recursive::Bool)
    l2 = recursive ? base_layer(sc) : (sc.layer::ScopeLayer).escaped
    SyntaxContext(l2, sc.unexpanded, sc.edition, sc.internal)
end

syntax_module(sc::SyntaxContext) = (sc.layer::ScopeLayer).mod
function syntax_module(st::SyntaxTree)
    st_mod = st.mod
    st_mod === nothing || return st_mod::Module
    syntax_module(st.context)
end

edition(st::SyntaxTree) = st.context.edition
edition(@nospecialize(st)) = JL_OLD_EDITION

_with_context(st, sc) =
    @mknode(st; context=sc, source=st.source, jl_source=st.jl_source)

# Unconditional; tramples existing scope, and includes quoted forms.  Only
# changes layer where it needs changing.
function adopt_scope(sc_in::SyntaxContext, st::SyntaxTree, scmap)
    st_sc = st.context
    sc2 = get(scmap, st_sc, nothing)
    if isnothing(sc2)
        sc2 = scmap[st_sc] = st_sc.layer === sc_in.layer ? st_sc :
            SyntaxContext(
                sc_in.layer, st_sc.unexpanded, st_sc.edition, st_sc.internal)
    end
    if is_leaf(st) || numchildren(st) == 0
        sc2 === st_sc ? st : _with_context(st, sc2)
    else
        mapchildren(c->adopt_scope(sc_in, c, scmap),
                    sc2 === st_sc ? st : _with_context(st, sc2))
    end
end
function adopt_scope(reference::SyntaxTree, st::SyntaxTree)
    adopt_scope(reference.context, st, Dict{SyntaxContext, SyntaxContext}())
end

function fill_context(st::SyntaxTree, sc::SyntaxContext)
    mapchildren(c->fill_context(c, sc),
                sc === st.context ? st : _with_context(st, sc))
end

function remove_scope(st::SyntaxTree, scmap)
    st_sc = st.context
    sc2 = get(scmap, st.context, nothing)
    if isnothing(sc2)
        sc2 = scmap[st_sc] = st_sc.layer === nothing ? st_sc :
            SyntaxContext(nothing, st_sc.unexpanded, st_sc.edition, st_sc.internal)
    end
    if is_leaf(st) || numchildren(st) == 0
        sc2 === st_sc ? st : _with_context(st, sc2)
    else
        mapchildren(c->remove_scope(c, scmap),
                    sc2 === st_sc ? st : _with_context(st, sc2))
    end
end
remove_scope(st::SyntaxTree) =
    remove_scope(st, Dict{SyntaxContext, SyntaxContext}())

function Base.show(io::IO, ::MIME"text/plain", sl::ScopeLayer)
    color = isnothing(sl.escaped) ? :normal : :cyan
    printstyled(io, "SL("; color)
    print(io, string(sl.mod))
    print(io, ",")
    !isnothing(sl.escaped) && print(io, sl.escaped)
    print(io, ",")
    printstyled(io, string(objectid(sl);base=62); color)
    printstyled(io, ")"; color)
end
Base.show(io::IO, sl::ScopeLayer) = Base.show(io::IO, MIME"text/plain"(), sl)

function Base.show(io::IO, ::MIME"text/plain", sc::SyntaxContext)
    color = sc.internal ? :light_black :
        sc.edition == JL_NEW_EDITION ? :normal : :blue
    printstyled(io, "["; color)
    if sc.edition != JL_NEW_EDITION
        printstyled(io, "old,"; color)
    end
    if sc.internal
        printstyled(io, "internal,"; color)
    end
    print(io, sc.layer)
    print(io, ",")
    if sc.unexpanded isa SyntaxTree
        k = head(sc.unexpanded)
        k === :macrocall ? print(io, sc.unexpanded[1]) : print(io, k)
    end
    printstyled(io, "]"; color)
end
Base.show(io::IO, sc::SyntaxContext) = Base.show(io::IO, MIME"text/plain"(), sc)

#-------------------------------------------------------------------------------
# Provenance

"""
Provenance notes: A SyntaxTree `st` has `.source` equal to one of:
- SyntaxTree (of the SyntaxTree `st` was transformed from)
- a reference to source text (either SourceRef or LineNumberNode).

Let "textref" refer to a SyntaxTree with non-SyntaxTree `.source`.  Every SyntaxTree
is either a textref or has one at the end of its `.source` chain.

All invariants noted in this section are awaiting the design of the "new macro"
API.  As of writing this, the user has more freedom than they should have.
"""

"""
Returns [st.source, st.source.source, ..., textref]
"""
function provenance(st::SyntaxTree)
    prov = SyntaxTree[]
    s = st.source
    while s isa SyntaxTree
        push!(prov, s)
        s = s.source
    end
    return prov
end

"`provenance(st)[1]`, or `st` if that's empty"
function prov(st::SyntaxTree)
    source = st.source
    source isa SyntaxTree ? source : st
end

"textref of st (possibly == st)"
function prov_end(st::SyntaxTree)
    out = st
    while out.source isa SyntaxTree
        out = prov(out)
    end
    return out
end

"`st`'s textref's `.source`, ignoring all expansions"
function sourceref(st::SyntaxTree)
    src = prov_end(st)
    src.source::Union{LineNumberNode, SourceRef}
end

"The last macro expansion `st` was involved in, or nothing"
function macro_prov(st::SyntaxTree)
    msrc = st.context.unexpanded
    isnothing(msrc) ? nothing : msrc::typeof(st)
end

"The first macro expansion `st` was involved in (chronologically), or nothing"
function macro_prov_end(st::SyntaxTree)
    lastmp = mp = macro_prov(st)
    while !isnothing(mp)
        lastmp, mp = mp, macro_prov(mp)
    end
    return lastmp
end

"The top-level location of `st`"
function unexpanded_sourceref(st::SyntaxTree)
    mp = macro_prov_end(st)
    isnothing(mp) ? sourceref(st) : sourceref(mp)
end

"""
A list of textrefs associated with `st`.  The number of returned trees should
equal one plus the number of macro expansions `st` "went through":

- For new macros, this is the number of macro expansions `st` was both an input
  and output of, so if `st` was created in a macro body, `flattened_provenance`
  returns a list of length 1.

- For old macros, we can't determine whether expanded syntax is from the
  macrocall args or macro body (it will have LineNumberNode .source), so all
  expanded syntax counts as having "went through" the macrocall.

The resulting list should be in the order
`[outermost_macrocall, innermost_macrocall, ..., expression_textref]`.
"""
function flattened_provenance(st::SyntaxTree)
    _flattened_provenance(st, SyntaxTree[])
end

# Only recurse on the first macro source in any source chain
function _flattened_provenance(st::SyntaxTree, out)
    msrc = macro_prov(st)
    # macro source === source means `st` is from the `msrc` macro body
    !isnothing(msrc) && msrc != prov(st) &&
        _flattened_provenance(msrc, out)
    push!(out, prov_end(st))
    out
end
