mutable struct ScopeLayer
    const mod::Module
    const escaped::Union{Nothing, ScopeLayer}
end
Base.var"=="(si1::ScopeLayer, si2::ScopeLayer) = si1 === si2

"""
Each node has a SyntaxContext describing its macro expansion and syntax version.
`SyntaxContext` is shared between all nodes of a single macro expansion, and is
one-to-one with ScopeLayer, with a few exceptions (contexts sharing same layer):
- `escape` and adopt_scope
- Desugaring creates internal contexts in its better version of `gensym`

We may want to move layer out of this struct for easier adopt_scope and
rebase_layer operations, but assuming mostly hygienic macros and few
scope-changing functions, this is most compact.
"""
mutable struct SyntaxContext
    const layer::ScopeLayer
    # For provenance; is not affected by escaping
    const unexpanded::Any # Union{SyntaxTree, Nothing}
    const version::VersionNumber
    const internal::Bool
end

# Reference to bytes within a source file
struct SourceRef
    file::Base.RefValue{SourceFile}
    first_byte::UInt32
    last_byte::UInt32
end

mutable struct SyntaxTree
    kind::Kind
    # Should be considered immutable
    children::Union{Nothing, Vector{SyntaxTree}}
    value::Any
    source::Union{SyntaxTree,SourceRef,LineNumberNode}
    context::Union{Nothing, SyntaxContext}
    jl_source::Union{Nothing, LineNumberNode}
    meta::Union{Nothing, Base.ImmutableDict{Symbol,Any}}
    # TODO: this is rarely used, and should just be part of context
    mod::Union{Nothing, Module}
    # TODO: this is almost never populated and semantically irrelevant after
    # parsing
    syntax_flags::UInt16
end

function SyntaxTree(kind::Kind, children, @nospecialize(value), source, context)
    SyntaxTree(kind, children, value, source, context,
               nothing, nothing, nothing, UInt16(0))
end

Base.var"=="(st1::SyntaxTree, st2::SyntaxTree) = st1 === st2

const NodeId = SyntaxTree
const SourceAttrType = Union{SyntaxTree,SourceRef,LineNumberNode}

function setchildren!(id::NodeId, children::AbstractVector{NodeId})
    setfield!(id, :children, children)
end

# fallback printing.  TODO: vulnerable to invalidations
function node_string(ex::SyntaxTree, depth=0)
    out = "(kind="*string(kind(ex))
    for n in sort!(collect(fieldnames(typeof(ex))))
        val = getproperty(ex, n)
        if !isnothing(val) && n !== :kind
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

function Base.get(ex::SyntaxTree, name::Symbol, default)
    !hasattr(ex, name) && return default
    name === :kind  && return getfield(ex, :kind)
    name === :source  && return getfield(ex, :source)
    name === :context  && return getfield(ex, :context)
    name === :value && let val = getfield(ex, :value)
        k = getfield(ex, :kind)
        (!isnothing(val) || k === K"Value") && return val
    end
    getfield(ex, name)
end

function Base.getindex(ex::SyntaxTree, i::Integer)
    ex.children[i]
end

function Base.getindex(ex::SyntaxTree, r::UnitRange)
    @view ex.children[r]
end

Base.firstindex(::SyntaxTree) = 1
Base.lastindex(ex::SyntaxTree) = numchildren(ex)

function Base.:≈(ex1::SyntaxTree, ex2::SyntaxTree)
    if kind(ex1) != kind(ex2) || is_leaf(ex1) != is_leaf(ex2)
        return false
    end
    if is_leaf(ex1)
        return hasattr(ex1, :value) == hasattr(ex2, :value) &&
               get(ex1, :value, nothing) == get(ex2, :value, nothing)
    else
        if numchildren(ex1) != numchildren(ex2)
            return false
        end
        return all(c1 ≈ c2 for (c1,c2) in zip(children(ex1), children(ex2)))
    end
end

function hasattr(ex::SyntaxTree, name::Symbol)
    name === :kind && return true
    # children is not an attr
    name === :value && return getfield(ex, :value) !== nothing ||
        (getfield(ex, :kind) === K"Value")
    name === :source && return true
    name === :context && return getfield(ex, :context) !== nothing
    name === :jl_source && return getfield(ex, :jl_source) !== nothing
    name === :meta && return getfield(ex, :meta) !== nothing
    name === :mod && return getfield(ex, :mod) !== nothing
    name === :syntax_flags && return true
    return false
end

function setattr!(ex::SyntaxTree, name::Symbol, @nospecialize(val))
    setfield!(ex, name, val)
    ex
end
setattr(ex::SyntaxTree, name::Symbol, @nospecialize(val)) =
    setattr!(is_leaf(ex) ? mkleaf(ex) : mknode(ex, children(ex)), name, val)

function deleteattr!(ex::SyntaxTree, name::Symbol)
    setfield!(ex, name, nothing)
    ex
end

const CompileHints = Base.ImmutableDict{Symbol,Any}
function setmeta!(st::SyntaxTree, key::Symbol, @nospecialize(val))
    meta = let m = get(st, :meta, nothing)
        isnothing(m) ? CompileHints(key, val) : CompileHints(m, key, val)
    end
    setfield!(st, :meta, meta)
    st
end
function setmeta(st::SyntaxTree, key::Symbol, @nospecialize(val))
    setmeta!(is_leaf(st) ? mkleaf(st) : mknode(st, children(st)), key, val)
end
function getmeta(st, name, @nospecialize(default))
    meta = get(st, :meta, nothing)
    isnothing(meta) ? default : get(meta, name, default)
end

Base.setproperty!(ex::SyntaxTree, name::Symbol, @nospecialize(val)) =
    error("SyntaxTree: this can't be mutated")

# JuliaSyntax tree API

function is_leaf(ex::SyntaxTree)
    ex.children === nothing
end

function numchildren(ex::SyntaxTree)
    cs = ex.children
    isnothing(cs) ? 0 : length(cs)
end

# TODO: Better to make this an error, since it can cause nodes that were
# intended to be leaves `SyntaxTree(kind, children(old), ...)` to be non-leaves
const NO_CHILDREN = SyntaxTree[]

function children(ex::SyntaxTree)
    is_leaf(ex) ? NO_CHILDREN : ex.children
end

function head(ex::SyntaxTree)
    SyntaxHead(kind(ex), flags(ex))
end

function kind(ex::SyntaxTree)
    ex.kind
end

function flags(ex::SyntaxTree)
    ex.syntax_flags
end

# A default context corresponding to no expansion
function SyntaxContext(mod::Module, version::VersionNumber)
    SyntaxContext(ScopeLayer(mod, nothing), nothing, version, false)
end

# TODO: switch from bool-based `expr_compat_mode` to `version`
const JL_NEW_SYNTAX_VERSION = v"1.14"
const JL_OLD_SYNTAX_VERSION = v"1.13"

is_base_layer(sc::SyntaxContext) = sc.layer.escaped === nothing

# The scope corresponding to no macro expansion.  Use with caution: macros may
# expand to top-level forms, so "base layer" !== "this top-level thunk's
# pre-expansion context" (usually ctx.syntax_context)
function base_layer(sc::SyntaxContext)
    l = sc.layer
    while l.escaped !== nothing
        l = l.escaped
    end
    return l
end

function escape_layer(sc::SyntaxContext, recursive::Bool)
    l2 = recursive ? base_layer(sc) : sc.layer.escaped
    SyntaxContext(l2, sc.unexpanded, sc.version, sc.internal)
end

syntax_module(sc::SyntaxContext) = sc.layer.mod
function syntax_module(st::SyntaxTree)
    st_mod = get(st, :mod, nothing)
    st_mod === nothing || return st_mod::Module
    syntax_module(st.context::SyntaxContext)
end

is_flisp_compat(sc::SyntaxContext) = sc.version < JL_NEW_SYNTAX_VERSION
is_flisp_compat(st::SyntaxTree) = is_flisp_compat(st.context)

# Unconditional; tramples existing scope, and includes quoted forms.  Only
# changes layer where it needs changing.
function adopt_scope(sc_in::SyntaxContext, st::SyntaxTree, scmap)
    st_sc = get(st, :context, nothing)
    sc2 = st_sc isa SyntaxContext ? get(scmap, st_sc, nothing) : nothing
    if isnothing(sc2) && st_sc isa SyntaxContext
        sc2 = scmap[st_sc] = st_sc.layer === sc_in.layer ? st_sc :
            SyntaxContext(
                sc_in.layer, st_sc.unexpanded, st_sc.version, st_sc.internal)
    elseif isnothing(sc2)
        sc2 = sc_in
    end
    if is_leaf(st) || numchildren(st) == 0
        sc2 === st_sc ? st : setattr(st, :context, sc2)
    else
        out = mapchildren(c->adopt_scope(sc_in, c, scmap), st)
        sc2 === st_sc ? out :
            out !== st ? setattr!(out, :context, sc2) :
            setattr(out, :context, sc2)
    end
end
function adopt_scope(reference::SyntaxTree, st::SyntaxTree)
    adopt_scope(reference.context::SyntaxContext, st,
                Dict{SyntaxContext, SyntaxContext}())
end

function fill_context!(st::SyntaxTree, sc::SyntaxContext)
    setattr!(st, :context, sc)
    !is_leaf(st) && for c in children(st)
        fill_context!(c, sc)
    end
    st
end
fill_context(st, sc) = fill_context!(mktree(st), sc)

function remove_context!(st::SyntaxTree)
    sc = get(st, :context, nothing)
    isnothing(sc) || JuliaSyntax.deleteattr!(st, :context)
    for c in children(st)
        remove_context!(c)
    end
    st
end
remove_context(st) = remove_context!(mktree(st))

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
        sc.version == JL_NEW_SYNTAX_VERSION ? :normal : :blue
    printstyled(io, "["; color)
    if sc.version != JL_NEW_SYNTAX_VERSION
        printstyled(io, "old,"; color)
    end
    if sc.internal
        printstyled(io, "internal,"; color)
    end
    print(io, sc.layer)
    print(io, ",")
    if sc.unexpanded isa SyntaxTree
        k = kind(sc.unexpanded)
        k === K"macrocall" ? print(io, sc.unexpanded[1]) : print(io, k)
    end
    printstyled(io, "]"; color)
end
Base.show(io::IO, sc::SyntaxContext) = Base.show(io::IO, MIME"text/plain"(), sc)

sourcefile(src::SourceRef) = src.file[]
first_byte(src::SourceRef) = Int(src.first_byte)
last_byte(src::SourceRef) = Int(src.last_byte)
byte_range(src::SourceRef) = first_byte(src):last_byte(src)

# TODO: Adding these methods to support LineNumberNode is kind of hacky but we
# can remove these after JuliaLowering becomes self-bootstrapping for macros
# and we a proper SourceRef for @ast's @HERE form.
byte_range(::LineNumberNode) = 0:0
source_location(src::LineNumberNode) = (src.line, 0)
source_location(::Type{LineNumberNode}, src::LineNumberNode) = src
source_line(src::LineNumberNode) = src.line
# The following somewhat strange cases are for where LineNumberNode is standing in
# for SourceFile because we've only got Expr-based provenance info
sourcefile(src::LineNumberNode) = src
sourcetext(::LineNumberNode) = SubString("")
source_location(src::LineNumberNode, _byte_index::Integer) = (src.line, 0)
source_location(::Type{LineNumberNode}, src::LineNumberNode, _byte_index::Integer) = src
filename(src::LineNumberNode) = string(src.file)

function highlight(io::IO, src::LineNumberNode; note="")
    print(io, src, " - ", note)
end

function highlight(io::IO, src::SourceRef; kws...)
    highlight(io, sourcefile(src), first_byte(src):last_byte(src); kws...)
end

function Base.show(io::IO, ::MIME"text/plain", src::SourceRef)
    highlight(io, src; note="these are the bytes you're looking for 😊", context_lines_inner=20)
end

"""
Provenance notes: A SyntaxTree `st` has `.source` equal to one of:
- NodeId (of the SyntaxTree `st` was transformed from)
- a reference to source text (either SourceRef or LineNumberNode).

Let "textref" refer to a SyntaxTree with non-NodeId `.source`.  Every SyntaxTree
is either a textref or has one at the end of its `.source` chain.

All invariants noted in this section are awaiting the design of the "new macro"
API.  As of writing this, the user has more freedom than they should have.
"""

"""
SyntaxList of [st.source, st.source.source, ..., textref]
"""
function provenance(st::SyntaxTree)
    prov = SyntaxList()
    s = st.source
    while s isa NodeId
        push!(prov, s)
        s = s.source
    end
    return prov
end

"`provenance(st)[1]`, or `st` if that's empty"
function prov(st::SyntaxTree)
    st.source isa NodeId ? st.source : st
end

"textref of st (possibly == st)"
function prov_end(st::SyntaxTree)
    out = st
    while out.source isa NodeId
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
    sc = get(st, :context, nothing)
    isnothing(sc) && return nothing
    msrc = (sc::SyntaxContext).unexpanded
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
A SyntaxList of textrefs associated with `st`.  The number of returned trees
should equal one plus the number of macro expansions `st` "went through":

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
    _flattened_provenance(st, SyntaxList())
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

sourcefile(ex::SyntaxTree) = sourcefile(sourceref(ex))
byte_range(ex::SyntaxTree) = byte_range(sourceref(ex))

function sourcetext(ex::SyntaxTree)
    sf = sourcefile(ex)
    sf isa LineNumberNode && return SubString("")
    view(sf, byte_range(ex))
end

# TODO (refactoring): make SyntaxList an immutable wrapper around node children
const SyntaxList = Vector{SyntaxTree}

SyntaxList(rest::SyntaxTree...) = SyntaxTree[rest...]

function mapsyntax(f, exs::AbstractVector{SyntaxTree})
    out = SyntaxList()
    for ex in exs
        push!(out, f(ex))
    end
    out
end

function mapindex(sl::SyntaxList, i::Int)
    out = SyntaxList()
    for st in sl
        push!(out, getindex(st, i))
    end
    out
end

#-------------------------------------------------------------------------------
# AST creation utilities

"""
    newnode(prov::SourceAttrType, k::Kind, children)

Create a new node with reference to parsed source text `prov`.
"""
function newnode(prov::SourceAttrType, k::Kind, children)
    context = prov isa SyntaxTree ? prov.context : nothing
    SyntaxTree(k, children, nothing, prov, context)
end
function newleaf(prov::SourceAttrType, k::Kind)
    context = prov isa SyntaxTree ? prov.context : nothing
    SyntaxTree(k, nothing, nothing, prov, context)
end

function mknode(old::SyntaxTree, children)
    st = mkleaf(old)
    setchildren!(st, children)
    return st
end
function mkleaf(old::SyntaxTree)
    st = SyntaxTree(old.kind, nothing, old.value, old, old.context,
                    old.jl_source, old.meta, old.mod, old.syntax_flags)
end
function mktree(old::SyntaxTree)
    if is_leaf(old)
        mkleaf(old)
    else
        cs = mapsyntax(mktree, children(old))
        mknode(old, cs)
    end
end

#-------------------------------------------------------------------------------
# Mapping and copying of AST nodes

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
                cs = SyntaxList(undef, length(orig_children))
                copyto!(cs, orig_children[1:i-1])
            end
        end
        cs[i] = newchild
    end
    if isnothing(cs)
        return ex
    end
    cs::SyntaxList
    ex2 = mknode(ex, cs)
    return ex2
end

"""
Recursively copy AST `ex` into `ctx`.  Every node in `ex` should be copied at
most once.

TODO: Likely unecessary with immutable tree
"""
function copy_ast(ctx, ex::SyntaxTree)
    id2 = _copy_ast(ex, Dict{NodeId, NodeId}())
    return id2
end

function _copy_ast(id1::NodeId, seen)
    let copied = get(seen, id1, nothing)
        isnothing(copied) || return copied
    end
    id2 = is_leaf(id1) ? mkleaf(id1) : mknode(id1, children(id1))
    seen[id1] = id2
    if !is_leaf(id1)
        cs = NodeId[]
        for cid in children(id1)
            push!(cs, _copy_ast(cid, seen))
        end
        setchildren!(id2, cs)
    end
    src1 = get(id1, :source, nothing)
    if src1 isa NodeId
        src2 =  _copy_ast(src1, seen)
        setattr!(id2, :source, src2)
    elseif !isnothing(src1)
        setattr!(id2, :source, src1)
    else
        throw("bad source?")
    end
    return id2
end

"""
    unalias_nodes(st::SyntaxTree)

Return a tree where each descendent of `st` has exactly one parent in `st`.  The
returned tree is identical to `st` in all but underlying representation, where
every additional parent to a subtree generates a copy of that subtree.  Apart
from achieving this, `unalias_nodes` should not allocate new nodes.

    unalias_nodes(sl::SyntaxList)

If a `SyntaxList` is given, every resulting tree will be unique with respect to
each other as well as internally.  A duplicate entry will produce a copied tree.
"""
unalias_nodes(st::SyntaxTree) =
    _unalias_nodes(st, Set{SyntaxTree}(), Base.IdSet{Vector{SyntaxTree}}())

function unalias_nodes(sl::SyntaxList)
    seen = Set{SyntaxTree}()
    seen_children = Base.IdSet{Vector{SyntaxTree}}()
    mapsyntax(st->_unalias_nodes(st, seen, seen_children), sl)
end

function _unalias_copy_tree(old::SyntaxTree)
    out = if is_leaf(old)
        mkleaf(old)
    else
        cs = mapsyntax(_unalias_copy_tree, children(old))
        mknode(old, cs)
    end
    # difference from mktree: don't add to provenance chain
    setattr!(out, :source, old.source)
end

function _unalias_nodes(st::SyntaxTree, seen::Set{SyntaxTree},
                        seen_children::Base.IdSet{Vector{SyntaxTree}})
    if st in seen
        return _unalias_copy_tree(st)
    end
    push!(seen, st)
    if !is_leaf(st)
        cs = children(st)
        if cs in seen_children
            cs = copy(cs)
            setchildren!(st, cs)
        end
        push!(seen_children, cs)
        for (i, c) in enumerate(cs)
            c2 = _unalias_nodes(c, seen, seen_children)
            c !== c2 && (cs[i] = c2)
        end
    end
    return st
end

"""
Give each descendent of `st` a `parent::NodeId` attribute.
"""
function annotate_parent!(st::SyntaxTree)
    st = unalias_nodes(st)
    mapchildren(t->_annotate_parent!(t, st), st)
end

function _annotate_parent!(st::SyntaxTree, pid::NodeId)
    setmeta!(st, :parent, pid)
    mapchildren(t->_annotate_parent!(t, st), st)
end

#-------------------------------------------------------------------------------
# AST destructuring utilities

raw"""
Simple `SyntaxTree` pattern matching

Returns the first result where its corresponding pattern matches `syntax_tree`
and each extra `cond` is true.  Throws an error if no match is found.

## Patterns

A pattern is used as both a conditional (does this syntax tree have a certain
structure?) and a `let` (bind trees to these names if so).  Each pattern uses a
limited version of the @ast syntax:

```
<pattern> = <tree_identifier>
          | [K"<kind>" <pattern>*]
          | [K"<kind>" <pattern>* <list_identifier>... <pattern>*]

# note "*" is the meta-operator meaning one or more, and "..." is literal
```

where a `[K"k" p1 p2 ps...]` form matches any tree with kind `k` and >=2
children (bound to `p1` and `p2`), and `ps` is bound to the possibly-empty
SyntaxList of children `3:end`.  Identifiers (except `_`) can't be re-used, but
may check for some form of tree equivalence in a future implementation.

## Extra condition: `when`

Like an escape hatch to the structure-matching mechanism.  `when=cond` requires
`cond` to evaluate to `true` for this branch to be taken.  `cond` may also bind
variables or printf-debug the matching process, as it runs only when its pattern
matches and no previous branch was taken.  `cond` may not mutate the object
being matched.

## Scope of variables

Every `(pattern, when=cond) -> result` introduces a local scope.  Identifiers in
the pattern are let-bound when evaluating `cond` and `result`. `cond` can
introduce variables for use in `result`.  User code in `cond` and `result` (but
not `pattern`) can refer to outer variables.

## Example

```
julia> st = JuliaSyntax.parsestmt(
    JuliaSyntax.SyntaxTree, "function foo(x,y,z); x; end")

julia> JuliaSyntax.@stm st begin
    [K"function" [K"call" fname [K"parameters" kws...]] body] ->
        "no positional args, only kwargs: $(kws)"
    [K"function" fname] ->
        "zero-method function $fname"
    [K"function" [K"call" fname args...] body] ->
        "normal function $fname"
    ([K"=" [K"call" _...] _...], when=(args=if_valid_get_args(st[1]); !isnothing(args))) ->
        "deprecated call-equals form with args $args"
    (_, when=(show("printf debugging is great"); true)) -> "something else"
    _ -> "unreachable due to the case above"
end
"normal function foo"
```

See [Racket `match`](https://docs.racket-lang.org/reference/match.html) for the
inspiration for this macro and an example of a much more featureful pattern
language.
"""
macro stm(st, pats)
    _stm(__source__, st, pats; debug=false)
end

"Like `@stm`, but prints a trace during matching."
macro stm_debug(st, pats)
    _stm(__source__, st, pats; debug=true)
end

# TODO: SyntaxList pattern matching could take similar syntax and use most of
# the same machinery

function _stm(line::LineNumberNode, st, pats; debug=false)
    _stm_check_usage(pats)
    # We leave most code untouched, so the user probably wants esc(output)
    st_gs, result_gs, k_gs, nc_gs = gensym.("st", "result", "k", "nc")
    out_blk = Expr(:let, Expr(:block, :($st_gs = $st::$SyntaxTree),
                              :($result_gs),
                              :($k_gs = $kind($st_gs)),
                              :($nc_gs = $numchildren($st_gs))),
                   Expr(:if, false, nothing))
    case_list_tail = out_blk.args[2].args
    for pcr in pats.args
        pcr isa LineNumberNode && (line = pcr; continue)
        p, cond, result = _stm_destruct_pat(pcr)
        pat_ok = p isa Symbol ? true : _stm_matches(p, st_gs, k_gs, nc_gs, debug)
        # We need to let-bind patvars in both cond and the result, so result
        # needs to live in the first argument of :if with the extra conditions.
        case = Expr(:elseif,
                    Expr(:&&, pat_ok,
                         Expr(:let, _stm_assigns(p, st_gs),
                              Expr(:&&, cond,
                                   Expr(:block, line,
                                        :($result_gs = $result), true)))),
                    result_gs)
        push!(case_list_tail, case)
        case_list_tail = case_list_tail[3].args
    end
    push!(case_list_tail,
          :(throw(ErrorException(string(
              "No match found for `", $st_gs, "` at ", $(string(line)))))))
    return esc(out_blk)
end

# recursively flatten `vcat` expressions
function _stm_vcat_to_hcat(p::Expr)
    if Meta.isexpr(p, :vcat)
        out = Expr(:hcat)
        for a in p.args
            Meta.isexpr(a, :row) ? append!(out.args, a.args) : push!(out.args, a)
        end
    else
        out = Expr(p.head, p.args...)
    end
    for i in eachindex(out.args)
        out.args[i] = _stm_vcat_to_hcat(out.args[i])
    end
    return out
end
_stm_vcat_to_hcat(x) = x

# return (pat_expr, when_expr|nothing, res_expr)
function _stm_destruct_pat(pcr::Expr)
    pc, r = pcr.args[1:2]
    Base.remove_linenums!(pc) # errors in lhs of `->` are caught in usage check
    (p_vcat, c) = Meta.isexpr(pc, :tuple) ?
        (pc.args[1], pc.args[2].args[2]) : (pc, true)
    return (_stm_vcat_to_hcat(p_vcat), c, r)
end

function _stm_matches_wrapper(p::Expr, st_ex, debug)
    st_gs, k_gs, nc_gs = gensym.("st", "k", "nc")
    Expr(:let, Expr(:block, :($st_gs = $st_ex::$SyntaxTree),
                          :($k_gs = $kind($st_gs)),
                          :($nc_gs = $numchildren($st_gs))),
               _stm_matches(p, st_gs, k_gs, nc_gs, debug))
end

function _stm_matches(p::Expr, st_gs::Symbol, k_gs::Symbol, nc_gs::Symbol, debug)
    pat_k = Kind(p.args[1].args[3])
    out = Expr(:&&, :($pat_k === $k_gs))
    debug && push!(out.args, Expr(:block, :(printstyled(
        string("[kind]: ", $k_gs, "\n"); color=:yellow)), true))

    p_args = p.args[2:end]
    dots_i = findfirst(x->Meta.isexpr(x, :(...)), p_args)
    dots_start = something(dots_i, length(p_args) + 1)
    n_after_dots = length(p_args) - dots_start # -1 if no dots

    push!(out.args, isnothing(dots_i) ?
        :($nc_gs === $(length(p_args))) :
        :($nc_gs >= $(length(p_args) - 1)))
    debug && push!(out.args, Expr(:block, :(printstyled(
        string("[numc]: ", $nc_gs, "\n"); color=:yellow)), true))

    for i in 1:dots_start-1
        p_args[i] isa Symbol && continue
        push!(out.args,
              _stm_matches_wrapper(p_args[i], :($st_gs[$i]), debug))
    end
    for i in n_after_dots-1:-1:0
        p_args[end-i] isa Symbol && continue
        push!(out.args,
              _stm_matches_wrapper(p_args[end-i], :($st_gs[end-$i]), debug))
    end
    debug && push!(out.args, Expr(:block, :(printstyled(
        string("matched: ", $st_gs, " with ", $(QuoteNode(p)), "\n");
        color=:green)), true))
    return out
end

# Assuming _stm_matches, construct an Expr that assigns syms to SyntaxTrees.
# Note st_rhs_expr is a ref-expr with a SyntaxTree/List value (in context).
function _stm_assigns(p, st_rhs_expr; assigns=Expr(:block))
    if p isa Symbol
        p != :_ && push!(assigns.args, Expr(:(=), p, st_rhs_expr))
        return assigns
    elseif p isa Expr
        p_args = p.args[2:end]
        dots_i = findfirst(x->Meta.isexpr(x, :(...)), p_args)
        dots_start = something(dots_i, length(p_args) + 1)
        n_after_dots = length(p_args) - dots_start
        for i in 1:dots_start-1
            _stm_assigns(p_args[i], :($st_rhs_expr[$i]); assigns)
        end
        if !isnothing(dots_i)
            _stm_assigns(p_args[dots_i].args[1],
                         :($st_rhs_expr[$dots_i:end-$n_after_dots]); assigns)
            for i in n_after_dots-1:-1:0
                _stm_assigns(p_args[end-i], :($st_rhs_expr[end-$i]); assigns)
            end
        end
        return assigns
    end
    @assert false "unexpected syntax; enable or fix `_stm_check_usage`"
end

# Check for correct pattern syntax.  Not needed outside of development.
function _stm_check_pattern(p, syms::Set{Symbol})
    if Meta.isexpr(p, :(...), 1)
        p = p.args[1]
        @assert(p isa Symbol, "Expected symbol before `...` in $p")
    end
    if p isa Symbol
        # No support for duplicate syms for now (user is either looking for
        # some form of equality we don't implement, or they made a mistake)
        dup = p in syms && p !== :_
        push!(syms, p)
        @assert(!dup, "invalid duplicate non-underscore identifier $p")
        return nothing
    elseif Meta.isexpr(p, :vect)
        @assert(length(p.args) === 1,
                "use spaces, not commas, in @stm []-patterns")
    elseif Meta.isexpr(p, :hcat)
        @assert(length(p.args) >= 2)
    elseif Meta.isexpr(p, :vcat)
        p = _stm_vcat_to_hcat(p)
        @assert(length(p.args) >= 2)
    else
        @assert(false, "malformed pattern $p")
    end
    @assert(count(x->Meta.isexpr(x, :(...)), p.args[2:end]) <= 1,
            "Multiple `...` in a pattern is ambiguous")

    # This exact `K"kind"` syntax is not necessary since the kind can't be
    # provided by a variable, but requiring [K"kinds"] is consistent with
    # `@ast` and allows us to implement list matching later.
    @assert(Meta.isexpr(p.args[1], :macrocall, 3) &&
        p.args[1].args[1] === Symbol("@K_str") &&
        p.args[1].args[3] isa String, "first pattern elt must be K\"\"")

    for subp in p.args[2:end]
        _stm_check_pattern(subp, syms)
    end
    return nothing
end

function _stm_check_usage(pats::Expr)
    @assert Meta.isexpr(pats, :block) "Usage: @stm st begin; ...; end"
    for pcr in pats.args
        pcr isa LineNumberNode && continue
        @assert(Meta.isexpr(pcr, :(->), 2), "Expected pat -> res, got malformed case: $pcr")
        if Meta.isexpr(pcr.args[1], :tuple)
            @assert(length(pcr.args[1].args) === 2,
                    "Expected `pat` or `(pat, when=cond)`, got $(pcr.args[1])")
            p = pcr.args[1].args[1]
            c = pcr.args[1].args[2]
            @assert(Meta.isexpr(c, :(=), 2) && c.args[1] === :when,
                    "Expected `(when=cond)` in tuple pattern, got $(c)")
        else
            p = pcr.args[1]
        end
        _stm_check_pattern(p, Set{Symbol}())
    end
end

#-------------------------------------------------------------------------------
# RawGreenNode->SyntaxTree1

function build_tree(::Type{SyntaxTree}, stream::ParseStream;
                    filename=nothing, first_line=1)
    cursor = RedTreeCursor(stream)
    sf = Ref(SourceFile(stream; filename, first_line))
    source = SourceRef(sf, first_byte(stream), last_byte(stream))
    cs = SyntaxList()
    for c in reverse_toplevel_siblings(cursor)
        is_trivia(c) && !is_error(c) && continue
        push!(cs, SyntaxTree(sf, c))
    end
    # There may be multiple non-trivia toplevel nodes (e.g. parse error)
    length(cs) === 1 && return only(cs)
    id = SyntaxTree(K"wrapper", reverse(cs), nothing, source, nothing)
    return id
end

function SyntaxTree(sf::Base.RefValue{SourceFile}, cursor::RedTreeCursor)
    green_id = GC.@preserve sf begin
        raw_offset, txtbuf = _unsafe_wrap_substring(sf[].code)
        offset = raw_offset - sf[].byte_offset
        _insert_green(sf, txtbuf, offset, cursor)
    end
    gst = green_id
    out = _green_to_est(gst, 0, gst)
    @assert !isnothing(out) "SyntaxTree requires >0 nontrivia nodes"
    return out
end

function _insert_green(sf::Base.RefValue{SourceFile},
                       txtbuf::Vector{UInt8}, offset::Int,
                       cursor::RedTreeCursor)
    source = SourceRef(sf, first_byte(cursor), last_byte(cursor))
    id = SyntaxTree(kind(cursor), nothing, nothing, source, nothing)
    let f = remove_flags(flags(cursor), NON_TERMINAL_FLAG)
        f != 0 && setattr!(id, :syntax_flags, f)
    end
    if !is_leaf(cursor)
        cs = SyntaxList()
        for c in reverse(cursor)
            push!(cs, _insert_green(sf, txtbuf, offset, c))
        end
        setchildren!(id, reverse!(cs))
    else
        v = parse_julia_literal(txtbuf, head(cursor), byte_range(cursor) .+ offset)
        if v isa Symbol
            # TODO: Fixes in JuliaSyntax to avoid ever converting to Symbol
            setattr!(id, :value, string(v))
        elseif !isnothing(v)
            setattr!(id, :value, v)
        end
    end
    return id
end

"""
Convert green `st` to a SyntaxTree with Expr structure.  `parent_i` is the final
position of `convert(st)` (our return value) within `convert(parent)`.  If
`parent_i == 0`, neither it nor our `parent` are known or relevant to this
conversion.

We can't assume much about `st` since it's anything the parser produces.  Our
correctness is defined against existing text->Expr transformations.

All node rearrangements and head changes are determined before recursing on
children, unlike in `node_to_expr`.  This is because knowing our parent's kind
and our position within it ahead-of-time makes conversion simpler.  By default,
for each node `st`, we
  1. let `cs` be `children(st)` minus (non-recursively) all trivia and parens
  2. rearrange `cs` based on length(cs), their/our/parent's kind/flags, etc.
  3. let `ret_cs` be `map(convert, cs)`
  4. return our new node `convert(st)` with `ret_cs` as children.
However, we can stop and return an answer between any of these steps.  For
example, deleting a child is easy in (2), but new non-leaf children we insert
should be added to `ret_cs` rather than `cs` (unless the new child has
pre-transformation structure and we're OK with step 3 creating it again).
"""
function _green_to_est(parent::SyntaxTree, parent_i::Int,
                       st::SyntaxTree; kw_in_params=false)
    if !should_include_node(st)
        @assert kind(parent) === K"None" && parent_i === 0
        return nothing
    end

    k = kind(st)
    syntax_name(x) = x.value::String
    symleaf(s::String) = setattr!(newleaf(st, K"Identifier"), :value, s)
    core_globalref(s::String) = setattr!(symleaf(s), :mod, Core)
    valleaf(@nospecialize(v)) = setattr!(newleaf(st, K"Value"), :value, v)

    if k === K"DotsIdentifier"
        # `..`/`...` used as an ordinary identifier (eg the `..` operator, or
        # `...` quoted as in `:(...)`). The dots are held as trivia children, so
        # this is not a leaf; represent it as a plain identifier named by the
        # dots themselves (the dot count is stored in the numeric flags).
        return symleaf(repeat('.', numeric_flags(st)))
    end

    if is_leaf(st)
        return if k === K"CmdMacroName" || k === K"StrMacroName"
            name = lower_identifier_name(syntax_name(st), k)
            symleaf(name)
        elseif k === K"VERSION"
            valleaf(version_to_expr(st))
        elseif (v = get(st, :value, nothing); v isa Union{Int128,UInt128,BigInt})
            # syntax TODO: likely unnecessary; this is just to match RGN->Expr,
            # which added this to match flisp parsing text->Expr.
            macname = v isa Int128 ? "@int128_str" :
                v isa UInt128 ? "@uint128_str" : "@big_str"
            mac = core_globalref(macname)
            arg = valleaf(replace(sourcetext(st), '_'=>""))
            ret_cids = SyntaxList(mac, valleaf(nothing), arg)
            newnode(st, K"macrocall", ret_cids)
        elseif is_error(k)
            mkleaf(st)
        elseif hasattr(st, :value) && !(k in KSet"Identifier Value" || is_literal(k))
            # certain kinds should really be identifiers.  known: &, |, :
            symleaf(syntax_name(st))
        else
            st
        end
    end

    # Non-leaf cases: each branch should either set `ret_k` and `cs` or recurse
    # manually and return a finished SyntaxTree
    ret_k::Kind = k
    cs = preprocessed_green_children(st)
    n_cs = length(cs)

    if k === K"string" && n_cs > 0
        return _string_to_est(st, cs; unwrap_literal=true)
    elseif k === K"cmdstring" && n_cs > 0
        # (cmdstring _...) => (macrocall Core.@cmd lno joined_str)
        cmd_arg = _string_to_est(st, cs; unwrap_literal=true)
        loc_st = valleaf(source_location(LineNumberNode, st))
        return newnode(st, K"macrocall", SyntaxList(
            core_globalref("@cmd"), loc_st, cmd_arg))
    elseif k === K"macro_name" && n_cs === 1
        # "M.@x" => (. M (macro_name x)) => (. M @x)
        # "@M.x" => (macro_name (. M x)) => (. M @x)
        #           (macro_name else) => else
        if kind(cs[1]) === K"Identifier"
            return symleaf(lower_identifier_name(syntax_name(cs[1]), K"macro_name"))
        else
            inner_st = cs[1]
            inner_cs = preprocessed_green_children(inner_st)
            if (length(inner_cs) === 2 && kind(inner_st) === K"." &&
                kind(inner_cs[2]) === K"Identifier")
                (lhs, raw_m) = _green_to_est(cs[1], 1, inner_cs[1]), inner_cs[2]
                mname_s = lower_identifier_name(syntax_name(raw_m), K"macro_name")
                mname = setattr!(mkleaf(raw_m), :value, mname_s)
                mname_inert = newnode(raw_m, K"inert", SyntaxList(mname))
                return mknode(inner_st, SyntaxList(lhs, mname_inert))
            else
                return _green_to_est(parent, 1, inner_st)
            end
        end
    elseif k === K"?"
        ret_k = K"if"
    elseif k === K"op=" && n_cs === 3
        # (op= a + b) => (+= a b)
        # (.op= a + b) => (.+= a b) below
        op_s = string(cs[2]) * '='
        lhs = _green_to_est(st, 0, cs[1])
        rhs = _green_to_est(st, 0, cs[3])
        out = newnode(st, K"unknown_head", SyntaxList(lhs, rhs))
        return setattr!(out, :value, op_s)
    elseif k === K".op=" && n_cs === 3
        op_s = '.' * string(cs[2]) * '='
        lhs = _green_to_est(st, 0, cs[1])
        rhs = _green_to_est(st, 0, cs[3])
        out = newnode(st, K"unknown_head", SyntaxList(lhs, rhs))
        return setattr!(out, :value, op_s)
    elseif k === K"op=" && n_cs === 1
        # (op= +) => +=   (the operator name itself, eg when quoted as `:(+=)`)
        return symleaf(string(cs[1]) * '=')
    elseif k === K".op=" && n_cs === 1
        # (.op= +) => .+=
        return symleaf('.' * string(cs[1]) * '=')
    elseif k === K"macrocall" && n_cs > 0
        # LineNumberNodes are not usually added to the tree as they are in Expr,
        # but this specifically inserts the macrocall child for compatibility
        loc_st = let loc = source_location(LineNumberNode, st)
            if n_cs >= 2 && kind(cs[2]) === K"VERSION"
                v = version_to_expr(popat!(cs, 2))
                @static if isdefined(Core, :MacroSource)
                    loc = Core.MacroSource(loc, v)
                end
            end
            valleaf(loc)
        end
        insert!(cs, 2, loc_st)
        # foo`x` parses to (macrocall foo::CmdMacroName (cmdstring ::CmdString))
        # so we need to unwrap the CmdString or else we get two macrocalls
        if n_cs >= 2 && kind(cs[1]) === K"CmdMacroName"
            ret_cs = _map_green_to_est(st, cs)
            ret_cs[3] = ret_cs[3][3] # node leak
            return mknode(st, ret_cs)
        end
        do_ex = kind(cs[end]) === K"do" ? pop!(cs) : nothing
        _reorder_parameters!(cs, 3)
        !isnothing(do_ex) && return _make_do_expression(st, cs, do_ex)
    elseif k === K"doc"
        # (doc str obj) => (macrocall Core.@doc lno str obj)
        ret_k = K"macrocall"
        pushfirst!(cs, valleaf(source_location(LineNumberNode, st)))
        pushfirst!(cs, core_globalref("@doc"))
    elseif k === K"dotcall" || k === K"call" && n_cs > 0
        if is_infix_op_call(st) || is_postfix_op_call(st)
            cs[2], cs[1] = cs[1], cs[2]
        end
        if is_postfix_op_call(st) && kind(cs[1]) == K"Identifier" &&
            syntax_name(cs[1]) === "'"
            popfirst!(cs)
            ret_k = K"'"
        end
        do_ex = kind(cs[end]) === K"do" ? pop!(cs) : nothing
        _reorder_parameters!(cs, 2)
        if k === K"dotcall"
            if is_prefix_call(st)
                # (dotcall f args...) => (. f (tuple args...))
                ret_cs = _map_green_to_est(st, cs)
                tuple = newnode(st, K"tuple", ret_cs[2:end])
                return newnode(st, K".", SyntaxList(ret_cs[1], tuple))
            else
                # (dotcall + args...) => (call .+ args...)
                ret_k = K"call"
                if kind(cs[1]) === K"Identifier"
                    cs[1] = symleaf('.' * syntax_name(cs[1]))
                end
            end
        end
        !isnothing(do_ex) && return _make_do_expression(st, cs, do_ex)
    elseif k === K"."
        if n_cs === 2
            # (. lhs rhs) => (. lhs (inert rhs))
            lhs = _green_to_est(st, 1, cs[1])
            rhs = _green_to_est(st, 2, cs[2])
            inert_rhs = kind(rhs) in KSet"quote inert" ? rhs :
                newnode(cs[2], K"inert", SyntaxList(rhs))
            return mknode(st, SyntaxList(lhs, inert_rhs))
        elseif n_cs === 1
            # (. x) => (. x) or .x
            # TODO: This is the one place where K"parens" change the result,
            # meaning that either Expr is doing something wrong or SyntaxNode is
            # deleting semantics.
            paren_st = filter(should_include_node, children(parent))[1]
            coalesce_dot = !(kind(paren_st) === K"parens") && parent_i === 1 &&
                kind(parent) in KSet"call dotcall curly quote"

            if (coalesce_dot || is_syntactic_operator(kind(cs[1])) ||
                kind(parent) === K"comparison" && iseven(parent_i))
                return symleaf('.' * syntax_name(cs[1]))
            end
        end
    elseif k === K"ref" || k === K"curly"
        _reorder_parameters!(cs, 2)
    elseif k === K"for" && n_cs === 2
        # (for (iteration iter1) body) => (for iter1 body)
        iters = preprocessed_green_children(cs[1])
        if length(iters) === 1
            cs[1] = iters[1]
        end
    elseif k === K"iteration"
        # (for (iteration iter1 iters...) body) => (for (block iter1 iters...) body)
        @assert kind(parent) === K"for" && parent_i === 1
        ret_k = K"block"
    elseif k === K"vect" || k === K"braces"
        _reorder_parameters!(cs, 1)
    elseif k === K"tuple"
        # Unwrap singleton, no-trailing-comma tuple in a couple cases:
        # (function (tuple (... xs)) body) => (function (... xs) body)
        # (-> (tuple _) body) => (-> _ body), assuming _ not parameters
        if n_cs === 1 && parent_i === 1 &&
            !has_flags(st, TRAILING_COMMA_FLAG)
            p_k = kind(parent)
            c_k = kind(cs[1])
            if (p_k === K"function" && c_k === K"...") ||
                (p_k === K"->" && c_k !== K"parameters")
                return _green_to_est(parent, parent_i, cs[1])
            end
        elseif n_cs === 2 && kind(parent) === K"->" && parent_i === 1 &&
            kind(cs[2]) === K"parameters" && kind(cs[1]) !== K"..."
            # This case should really be deleted.
            # (-> (tuple x (parameters y)) _) => (-> (block x y) _)
            c2_cs = preprocessed_green_children(cs[2])
            if length(c2_cs) === 0
                ret_k = K"block"
                pop!(cs)
            elseif length(c2_cs) === 1
                ret_k = K"block"
                cs[2] = c2_cs[1]
            end
        end
        _reorder_parameters!(cs, 1)
    elseif k === K"where" && n_cs === 2
        # (where lhs (braces a b c)) => (where lhs a b c)
        if kind(cs[2]) === K"braces"
            rhs = pop!(cs)
            append!(cs, preprocessed_green_children(rhs))
            _reorder_parameters!(cs, 2)
        end
    elseif k === K"try"
        # anything => (try try_block e catch_block [finally_block] [else_block])
        try_ = cs[1]
        st_false = valleaf(false)
        catch_var = catch_ = else_ = finally_ = st_false
        for c in cs[2:end]
            inner_cs = preprocessed_green_children(c)
            if kind(c) === K"catch"
                if kind(inner_cs[1]) !== K"Placeholder"
                    catch_var = inner_cs[1]
                end
                catch_ = inner_cs[2]
            elseif kind(c) === K"else"
                else_ = only(inner_cs)
            elseif kind(c) === K"finally"
                finally_ = only(inner_cs)
            elseif is_error(kind(c))
                return mknode(st, cs) # give up
            else
                @assert false "Illegal subclause in `try`"
            end
        end
        empty!(cs)
        push!(cs, try_, catch_var, catch_)
        if finally_ != st_false || else_ != st_false
            push!(cs, finally_)
            if else_ != st_false
                push!(cs, else_)
            end
        end
    elseif k === K"generator" && n_cs >= 2
        # let (g2 x iter) mean (generator x iter.children...)
        # (generator val iter_1 ... iter_n) =>
        # (flatten (g2 (... (flatten (g2 (g2 val i_n) i_{n-1})) ...) i_1))
        g_out = _green_to_est(st, 1, popfirst!(cs))
        for c in Iterators.reverse(cs)
            gen_cs = let rest = kind(c) === K"iteration" ?
                preprocessed_green_children(c) : SyntaxList(c)
                rest = _map_green_to_est(st, rest; undef_parent=true)
                pushfirst!(rest, g_out)
            end
            g_out = mknode(st, gen_cs)
            if c !== cs[end]
                g_out = newnode(c, K"flatten", SyntaxList(g_out))
            end
        end
        return setattr!(g_out, :source, st) # outermost provenance
    elseif k === K"filter"
        @assert n_cs === 2
        # (filter (iteration is...) cond) => (filter cond is...)
        cond = pop!(cs)
        cs = preprocessed_green_children(cs[1])
        pushfirst!(cs, cond)
    elseif k === K"in"
        ret_k = K"="
    elseif k === K"nrow" || k === K"ncat"
        pushfirst!(cs, valleaf(numeric_flags(flags(st))))
    elseif k === K"typed_ncat"
        insert!(cs, 2, valleaf(numeric_flags(flags(st))))
    elseif k === K"elseif"
        # (elseif cond body) => (elseif (block cond) body)
        # RGN->Expr block-wraps for linenodes; we do it for parity
        ret_cs = _map_green_to_est(st, cs)
        ret_cs[1] = newnode(cs[1], K"block", SyntaxList(ret_cs[1]))
        return mknode(st, ret_cs)
    elseif k === K"->" && kind(cs[2]) !== K"block"
        ret_cs = _map_green_to_est(st, cs)
        ret_cs[2] = newnode(cs[2], K"block", SyntaxList(ret_cs[2]))
        return mknode(st, ret_cs)
    elseif k === K"function" && n_cs >= 2 &&
        has_flags(st, SHORT_FORM_FUNCTION_FLAG)
        # (function-= callex body) => (= callex (block body))
        # exception: no block on "x' = y", or if body is already a block
        if kind(cs[2]) !== K"block" && !is_postfix_op_call(cs[1])
            ret_cs = _map_green_to_est(st, cs)
            ret_cs[2] = newnode(cs[2], K"block", SyntaxList(ret_cs[2]))
            return newnode(st, K"=", ret_cs)
        end
        ret_k = K"="
    elseif k === K"module"
        not_bare = valleaf(!has_flags(st, BARE_MODULE_FLAG))
        insert!(cs, kind(cs[1]) === K"VERSION" ? 2 : 1, not_bare)
    elseif k === K"quote" && n_cs === 1
        # (quote something_simple) => (inert something_simple)
        ret_c = _green_to_est(st, 1, cs[1])
        return is_leaf(ret_c) && kind(ret_c) !== K"Bool" ?
            newnode(st, K"inert", SyntaxList(ret_c)) :
            mknode(st, SyntaxList(ret_c))
    elseif k === K"do"
        ret_k = K"->"
    elseif k === K"block"
        # (let (block x) _...) => (let x _...)
        # (let (block (= x y)) _...) => (let (= x y) _...)
        # (let (block (:: x y)) _...) => (let (:: x y) _...)
        # (struct _ (block (doc "foo" field1) (doc "bar" field2))) =>
        # (struct _ (block "foo" field1 "bar" field2))
        if kind(parent) === K"let" && parent_i === 1 && n_cs === 1
            out = _green_to_est(st, 1, cs[1])
            return kind(out) in KSet"Identifier = ::" ? out :
                mknode(st, SyntaxList(out))
        elseif kind(parent) === K"struct" && parent_i === 3
            cs_tmp = SyntaxList()
            for c in cs
                kind(c) === K"doc" ?
                    append!(cs_tmp, preprocessed_green_children(c)) :
                    push!(cs_tmp, c)
            end
            cs = cs_tmp
        end
    elseif (k === K"local" || k === K"global") && n_cs === 1
        # (local (const _)) => (const (local _))
        # (local (tuple a b c)) => (local a b c)
        if kind(cs[1]) === K"const"
            ret_c1_cs = _map_green_to_est(st, preprocessed_green_children(cs[1]))
            ret_cs = SyntaxList(mknode(st, ret_c1_cs))
            return mknode(cs[1], ret_cs)
        elseif kind(cs[1]) === K"tuple"
            cs = preprocessed_green_children(cs[1])
        end
    elseif k === K"return" && n_cs === 0
        push!(cs, valleaf(nothing))
    elseif k === K"juxtapose"
        ret_k = K"call"
        pushfirst!(cs, symleaf("*"))
    elseif k === K"struct"
        is_mutable = valleaf(has_flags(st, MUTABLE_FLAG))
        pushfirst!(cs, is_mutable)
    elseif k === K"importpath"
        ret_k = K"."
        for i in eachindex(cs)
            if kind(cs[i]) === K"inert"
                inner_cs = preprocessed_green_children(cs[i])
                length(inner_cs) === 1 && (cs[i] = only(inner_cs))
            end
        end
    elseif k === K"wrapper" # parse errors only
        ret_k = K"block"
    elseif k === K"parameters"
        kw_in_params = kind(parent) === K"parameters" && parent_i === 1 ?
            kw_in_params : !(kind(parent) in KSet"vect curly braces ref")
    elseif k === K"="
        p_k = kind(parent)
        because_params = p_k === K"parameters" && parent_i >= 1 && kw_in_params
        because_call = parent_i > 1 && (p_k == K"ref" ||
            p_k in KSet"call dotcall" && is_prefix_call(parent))
        ret_k = because_params || because_call ? K"kw" : K"="
    elseif k in KSet"var char parens" && n_cs === 1
        # Reachable if this is the top node
        return _green_to_est(parent, parent_i, cs[1])
    end

    # Recurse on `cs`.  If no children change, just return `st`.
    ret_cs = _map_green_to_est(st, cs; kw_in_params)
    return ret_cs == children(st) && ret_k == kind(st) ?
        st : setattr!(mknode(st, ret_cs), :kind, ret_k)
end

function _map_green_to_est(parent::SyntaxTree, cs;
                           kw_in_params=false, undef_parent=false)
    ret_cs = SyntaxList()
    for (i, c) in enumerate(cs)
        new_c = _green_to_est(parent, undef_parent ? 0 : i, c; kw_in_params)
        @assert should_include_node(new_c)
        push!(ret_cs, new_c)
    end
    ret_cs
end

# When converting, first delete trivia and wrapper nodes in children so we can
# observe child kinds before recursing, thus creating fewer "temporary" nodes
function preprocessed_green_children(st::SyntaxTree)
    cs = filter(should_include_node, children(st))
    for i in eachindex(cs)
        while kind(cs[i]) in KSet"var char parens"
            inner_cs = preprocessed_green_children(cs[i])
            if length(inner_cs) === 1
                cs[i] = inner_cs[1]
            else
                break
            end
        end
    end
    return cs
end

# (call f a b (parameters c d) (parameters e)) =>
# (call f (parameters (parameters e) c d) a b)
function _reorder_parameters!(cs::SyntaxList, params_pos::Int)
    (length(cs) > params_pos && kind(cs[end]) === K"parameters") || return cs
    local param_ball = pop!(cs)
    while length(cs) >= 1 && kind(cs[end]) === K"parameters"
        next_ball_cs = pushfirst!(copy(children(cs[end])), param_ball)
        # `mknode` leaks nodes, but having multiple `parameters` blocks is
        # extremely rare nonsense syntax (`f(a,b;c=d;e)`)
        param_ball = mknode(cs[end], next_ball_cs)
        pop!(cs)
    end
    insert!(cs, params_pos, param_ball)
    nothing
end

# (call args... (do _...)) -> (do (call args...) (-> _...))
#
# Expects preprocessed and rearranged `args`
function _make_do_expression(st::SyntaxTree, args::SyntaxList, doex::SyntaxTree)
    ret_doex = _green_to_est(st, 0, doex)
    ret_callex = mknode(st, _map_green_to_est(st, args))
    return newnode(st, K"do", SyntaxList(ret_callex, ret_doex))
end

# A `string` or `cmdstring` may have multiple literal strings within (from
# newlines when triple-quoting).  A `string` may have interpolated values.
#
# (string "a" "b" "c") => "abc" # unwrap_literal=true
# (string "a" "b" "c" 1) => (string "abc" 1)
# (string "a" "b" (string "c" "d")) => (string "ab" (string "cd"))
#
# (cmdstring "a"::CmdString "b"::CmdString) => "ab"::CmdString
#
# Converting children-first (as _string_to_Expr does) would make this much
# harder by converting literal strings without the parent's knowledge
function _string_to_est(st::SyntaxTree, cs::SyntaxList; unwrap_literal)
    ret_cs = SyntaxList()
    literal_k = kind(st) === K"cmdstring" ? K"CmdString" : K"String"
    cur_str = false
    next_str = length(cs) > 0 && kind(cs[1]) === literal_k
    buf = IOBuffer()
    for i in eachindex(cs)
        c = cs[i]
        (prev_str, cur_str) = (cur_str, next_str)
        next_str = i != lastindex(cs) && kind(cs[i+1]) === literal_k
        # optimization: push the current child mostly unchanged if the following
        # one isn't a literal string
        if !prev_str && cur_str && !next_str
            push!(ret_cs, c)
        elseif cur_str
            write(buf, c.value)
            if !next_str
                ret_c = newleaf(st, literal_k)
                setattr!(ret_c, :value, String(take!(buf)))
                push!(ret_cs, ret_c)
            end
        else
            ret_c = kind(c) === K"string" ?
                _string_to_est(c, preprocessed_green_children(c);
                               unwrap_literal=false) :
                _green_to_est(st, i, c)

            push!(ret_cs, ret_c)
        end
    end
    if unwrap_literal && length(ret_cs) === 1 && kind(ret_cs[1]) === literal_k
        return ret_cs[1]
    end
    return mknode(st, ret_cs)
end
