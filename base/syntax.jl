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
    const unexpanded::Any # Union{Syntax, Nothing}
    const edition::Tuple{Int, Int}
    const internal::Bool
end

# Similar to JuliaSyntax.SourceFile
mutable struct SourceCode
    const text::SubString{String}
    const byte_offset::Int
    const filename::Symbol
    const first_line::Int
    const line_starts::Vector{Int}
end

# Reference to bytes within a source file
struct SourceRef
    code::SourceCode
    first_byte::UInt32
    last_byte::UInt32
end

mutable struct Syntax
    const head::Symbol
    # Should be considered immutable
    const children::Union{Nothing, Vector{Syntax}}
    const value::Any
    const source::Union{Syntax,SourceRef,LineNumberNode}
    const context::SyntaxContext
    const jl_source::Union{Nothing, LineNumberNode}
    meta::Union{Nothing, Base.ImmutableDict{Symbol,Any}}
    # TODO: this is rarely used, and should just be part of context
    const mod::Union{Nothing, Module}
    # TODO: this is almost never populated and semantically irrelevant after
    # parsing
    const syntax_flags::UInt16
    function Syntax(head, children, value, source, context, jl_source, meta, mod, syntax_flags)
        @nospecialize
        new(head, children, value, source, context, jl_source, meta, mod, syntax_flags)
    end
end
const SourceAttrType = Union{Syntax,SourceRef,LineNumberNode}

# A default context corresponding to no expansion
function SyntaxContext(mod::Module, edition::Tuple{Int, Int})
    SyntaxContext(ScopeLayer(mod, nothing), nothing, edition, false)
end

const JL_NEW_EDITION = (1, 15)
const JL_OLD_EDITION = (1, 14)

function Syntax(head::Symbol, children, @nospecialize(value), source, context)
    Syntax(head, children, value, source, context,
               nothing, nothing, nothing, UInt16(0))
end

head(ex::Syntax) = ex.head

is_leaf(ex::Syntax) = ex.children === nothing

function numchildren(ex::Syntax)
    cs = ex.children
    isnothing(cs) ? 0 : length(cs)
end

# TODO: Better to make this an error, since it can cause nodes that were
# intended to be leaves `Syntax(head, children(old), ...)` to be non-leaves
const NO_CHILDREN = Syntax[]

function children(ex::Syntax)
    cs = ex.children
    cs === nothing ? NO_CHILDREN : cs
end

function Base.getindex(ex::Syntax, i::Integer)
    ex.children[i]
end

function Base.getindex(ex::Syntax, r::UnitRange)
    @view ex.children[r]
end

Base.firstindex(::Syntax) = 1
Base.lastindex(ex::Syntax) = numchildren(ex)

#-------------------------------------------------------------------------------
# AST creation utilities

# fallback printing.  TODO: vulnerable to invalidations
function node_string(ex::Syntax, depth=2)
    out = "(head="*string(head(ex))
    for n in sort!(collect(fieldnames(typeof(ex))))
        val = getproperty(ex, n)
        if !isnothing(val) && n !== :head
            val_str = if val isa Syntax && depth > 1
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
function assert_syntax(st::Syntax, recursive=true)
    vr = recursive ? _assert_syntax(st, Syntax[]) :
        _assert_syntax_node(st)
    if vr !== nothing
        err_st, err = vr
        msg = string("assert_syntax failed: ", node_string(st),
                     "\n  failing node: ", node_string(err_st),
                     "\n  reason: ", err)
        error(msg)
    end
    nothing
end

function _assert_syntax_node(st::Syntax)
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
function _assert_syntax(st::Syntax, parents::Vector{Syntax})
    if st in parents
        err = "cycle detected: ["
        for p in parents
            err *= "\n" * node_string(p)
        end
        return (st, err*"]")
    end
    vr = _assert_syntax_node(st)
    isnothing(vr) || return vr

    push!(parents, st)
    is_leaf(st) || for c in children(st)
        vr = _assert_syntax(c, parents)
        isnothing(vr) || return vr
    end
    pop!(parents)
    nothing
end

const _DEFAULT_NODE = Syntax(
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
    out_args = Vector(undef, fieldcount(Syntax))
    for (i, n) in enumerate(fieldnames(Syntax))
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
        out_args[Base.fieldindex(Syntax, aname)] = aval
    end
    old === _DEFAULT_NODE && !((:head, :source, :context) ⊆ seen_attrs) &&
        throw(ArgumentError("brand-new node from @mknode requires more attrs $__source__"))

    out = Expr(:let,
               Expr(:block, Expr(:(=), old_gs, old)),
               Expr(:block, Expr(:call, Syntax, out_args...)))
    DEBUG_LOWERING && (out.args[end] = Expr(:call, _debug_check_attrs, out.args[end]))
    esc(out)
end
macro mknode(x)
    (old, attrs) = Meta.isexpr(x, :parameters) ? (_DEFAULT_NODE, x) : (x, nothing)
    esc(Expr(:macrocall, var"@mknode", __source__, attrs, old))
end

function _debug_check_attrs(x)
    assert_syntax(x, false)
    x
end

Base.setproperty!(ex::Syntax, name::Symbol, @nospecialize(val)) =
    error("Syntax: this can't be mutated")

# This function should be allocation-free if no children were changed
function mapchildren(f::Function, ex::Syntax)
    if is_leaf(ex)
        return ex
    end
    orig_children = children(ex)
    cs = nothing
    for (i,e) in enumerate(orig_children)
        newchild = f(e)::Syntax
        if isnothing(cs)
            if newchild == e
                continue
            else
                cs = Vector{Syntax}(undef, length(orig_children))
                copyto!(cs, orig_children[1:i-1])
            end
        end
        cs[i] = newchild
    end
    if isnothing(cs)
        return ex
    end
    cs::Vector{Syntax}
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
function syntax_module(st::Syntax)
    st_mod = st.mod
    st_mod === nothing || return st_mod::Module
    syntax_module(st.context)
end
syntax_name(s::Syntax) = s.value::String

edition(st::Syntax) = st.context.edition
edition(@nospecialize(st)) = JL_OLD_EDITION

_with_context(st, sc) =
    @mknode(st; context=sc, source=st.source, jl_source=st.jl_source)

# Unconditional; tramples existing scope, and includes quoted forms.  Only
# changes layer where it needs changing.
function adopt_scope(sc_in::SyntaxContext, st::Syntax, scmap)
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
function adopt_scope(reference::Syntax, st::Syntax)
    adopt_scope(reference.context, st, Dict{SyntaxContext, SyntaxContext}())
end

function fill_context(st::Syntax, sc::SyntaxContext)
    mapchildren(c->fill_context(c, sc),
                sc === st.context ? st : _with_context(st, sc))
end

function remove_scope(st::Syntax, scmap)
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
remove_scope(st::Syntax) =
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
    if sc.unexpanded isa Syntax
        k = head(sc.unexpanded)
        k === :macrocall ? print(io, sc.unexpanded[1]) : print(io, k)
    end
    printstyled(io, "]"; color)
end
Base.show(io::IO, sc::SyntaxContext) = Base.show(io::IO, MIME"text/plain"(), sc)

#-------------------------------------------------------------------------------
# Provenance

"""
Provenance notes: A Syntax `st` has `.source` equal to one of:
- Syntax (of the Syntax `st` was transformed from)
- a reference to source text (either SourceRef or LineNumberNode).

Let "textref" refer to a Syntax with non-Syntax `.source`.  Every Syntax
is either a textref or has one at the end of its `.source` chain.

All invariants noted in this section are awaiting the design of the "new macro"
API.  As of writing this, the user has more freedom than they should have.
"""

"""
Returns [st.source, st.source.source, ..., textref]
"""
function provenance(st::Syntax)
    prov = Syntax[]
    s = st.source
    while s isa Syntax
        push!(prov, s)
        s = s.source
    end
    return prov
end

"`provenance(st)[1]`, or `st` if that's empty"
function prov(st::Syntax)
    source = st.source
    source isa Syntax ? source : st
end

"textref of st (possibly == st)"
function prov_end(st::Syntax)
    out = st
    while out.source isa Syntax
        out = prov(out)
    end
    return out
end

"`st`'s textref's `.source`, ignoring all expansions"
function sourceref(st::Syntax)
    src = prov_end(st)
    src.source::Union{LineNumberNode, SourceRef}
end

"The last macro expansion `st` was involved in, or nothing"
function macro_prov(st::Syntax)
    msrc = st.context.unexpanded
    isnothing(msrc) ? nothing : msrc::typeof(st)
end

"The first macro expansion `st` was involved in (chronologically), or nothing"
function macro_prov_end(st::Syntax)
    lastmp = mp = macro_prov(st)
    while !isnothing(mp)
        lastmp, mp = mp, macro_prov(mp)
    end
    return lastmp
end

"The top-level location of `st`"
function unexpanded_sourceref(st::Syntax)
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
function flattened_provenance(st::Syntax)
    _flattened_provenance(st, Syntax[])
end

# Only recurse on the first macro source in any source chain
function _flattened_provenance(st::Syntax, out)
    msrc = macro_prov(st)
    # macro source === source means `st` is from the `msrc` macro body
    !isnothing(msrc) && msrc != prov(st) &&
        _flattened_provenance(msrc, out)
    push!(out, prov_end(st))
    out
end

# TODO: We want a bigger API (probably like Compiler.source_location),
# but avoid duplication with JuliaSyntax for now
function filename(s::Syntax)
    sr = sourceref(s)
    n = sr isa LineNumberNode ? sr.file : sr.code.filename
    n === nothing ? :var"" : n
end
function source_line(src::SourceCode, b)
    src.first_line - 1 +
        searchsortedlast(src.line_starts, b - src.byte_offset)
end
first_linenode(s::Syntax) = first_linenode(sourceref(s))
first_linenode(sr::SourceRef) =
    LineNumberNode(source_line(sr.code, sr.first_byte), sr.code.filename)
first_linenode(lnn::LineNumberNode) = lnn

#-------------------------------------------------------------------------------
# Expr <-> Syntax
#
# Adding more cases to these functions is almost certainly wrong, since these
# operates on arbitrary heads and arguments throughout macro expansion, not
# well-formed syntax after expansion is done.  Most of the complexity here is
# LineNumberNode absorption logic: linenodes are always considered provenance if
# unquoted, then removed in certain forms.  If `src` is not an linenode, it is
# assumed to be a better provenance source, so linenodes in `e` are not used for
# provenance (but still removed).

function _first_linenode(e::Expr)
    e.head in (:macrocall, :quote, :inert) || for a in e.args
        a isa LineNumberNode && return a
        if a isa Expr
            a_out = _first_linenode(a)
            a_out isa LineNumberNode && return a_out
        end
    end
    return nothing
end
first_linenode(e::Expr) = something(_first_linenode(e), LineNumberNode(0, :var""))

_unescape_lnn(@nospecialize(e)) =
    e isa LineNumberNode ? e :
    (e isa Expr &&
    (e.head === :escape || e.head === Symbol("hygienic-scope")) &&
    length(e.args) > 0) ? _unescape_lnn(e.args[1]) : nothing

# Linenodes usually apply to the following form, but some forms contain the
# relevant line node as an argument.
function _get_inner_lnn(e::Expr, default::LineNumberNode)
    e.head in (:function, :macro, :module, :(=)) || return default
    length(e.args) >= 2 || return default
    b = e.args[end]
    b isa Expr || return default
    b.head === :block || return default
    length(b.args) >= 1 || return default
    b_lnn = _unescape_lnn(b.args[1])
    return b_lnn isa LineNumberNode ? b_lnn : default
end

# List of Expr-AST forms that are always converted to some Syntax form and
# never inserted as an opaque `:value`. Note no LineNumberNode, which appears
# unwrapped in a macrocall (possibly generated functions too, TODO check)
isa_lowering_ast_node(@nospecialize(e)) =
    e isa Symbol || e isa QuoteNode || e isa Expr || e isa GlobalRef

function expr_to_syntax(@nospecialize(e),
                        src::Union{LineNumberNode, SourceRef}=
                            e isa Expr ? first_linenode(e) : LineNumberNode(0, :var""),
                        context=SyntaxContext(nothing, nothing, JL_OLD_EDITION, false))
    _expr_to_syntax(e, context, src, false)[1]
end
function expr_to_syntax(@nospecialize(e), src::Syntax)
    _expr_to_syntax(e, src.context, src, false)[1]
end
function _expr_to_syntax(@nospecialize(e), context::SyntaxContext,
                      src::SourceAttrType, quoted::Bool)
    s = if e isa Symbol
        @mknode(;head=:identifier, value=String(e), source=src, context)
    elseif e isa QuoteNode
        cid, _ = _expr_to_syntax(e.value, context, src, true)
        @mknode(;head=:inert, source=src, children=Syntax[cid], context)
    elseif e isa Expr
        h = e.head
        if h === :value || h === :identifier
            error("expr heads :value and :identifier are reserved")
        end
        src = old_src = src isa LineNumberNode ? _get_inner_lnn(e, src) : src
        cs = Syntax[]
        rm_linenodes = h in (:block, :toplevel)
        quoted |= h in (:quote, :inert)
        for arg in e.args
            if rm_linenodes && (lnn = quoted ? arg : _unescape_lnn(arg);
                                lnn isa LineNumberNode)
                src isa LineNumberNode && (src = lnn)
            else
                cid, src = _expr_to_syntax(arg, context, src, quoted)
                push!(cs, cid)
            end
        end
        @mknode(;head=h, source=old_src, children=cs, context)
    elseif e isa GlobalRef
        # Represent globalref as :identifier with :mod attribute
        @mknode(;head=:identifier, source=src, value=string(e.name),
                mod=e.mod, context)
    else
        # We may want additional special cases for other types where
        # `Base.isa_ast_node(e)`, but `:value` should be fine for most, since
        # most are produced in or after lowering
        if e isa LineNumberNode && src isa LineNumberNode
            # linenode outside of block or toplevel
            src = e
        end
        @mknode(;head=:value, value=e, source=src, context)
    end
    @assert isa_lowering_ast_node(e) || head(s) === :value s

    return s, src
end

# @__doc__ is brittle
function _is_meta_doc_block(s::Syntax)
    head(s) === :block && numchildren(s) == 2 && let s1 = s[1]
        head(s1) === :meta && numchildren(s1) == 1 && let s11 = s1[1]
            head(s11) === :identifier && s11.value === "doc"
        end
    end
end

# `suppress_linenodes` is true if `st`'s parent knows `st` is an exception to
# normal linenode rules.  It only applies to `st`, and not transitively to its
# children.
function syntax_to_expr(s::Syntax, suppress_linenodes=false)
    h = head(s)
    if h === :identifier
        # @assert scope layer is base
        n = Symbol(s.value::String)
        mod = s.mod
        !isnothing(mod) ? GlobalRef(mod, n) : n
    elseif h === :value
        v = s.value
        # Let `s.value isa Symbol` (or other AST node).  Since we enforce that
        # this is never produced by the reverse Expr->Syntax transformation,
        # there is no lonely Expr for which `s` is the only Syntax
        # representation.  This means we can pick some other expr this
        # represents, namely Expr(`(inert ,s.value)) rather than
        # Expr(s.value).
        isa_lowering_ast_node(v) ? QuoteNode(v) : v
    elseif h === :inert
        QuoteNode(syntax_to_expr(s[1]))
    else
        # TODO: should handle post-lowering forms as well
        @assert !is_leaf(s) (s, "syntax_to_expr should only be used pre-desugaring")
        out = Expr(h)

        # (Move the following assumptions to the docs if they turn out accurate)
        # The only mandatory LineNumberNode is the second macrocall argument.
        # Other than that, optional linenodes may show up anywhere within:
        # - `block`, unless the block is the first child of `for` or `let`
        # - `toplevel`
        # Macro authors are responsible for handling any linenodes that follow
        # the rules above (but the presence of optional linenodes can't be
        # counted upon).
        need_lnns = h in (:block, :toplevel) && !suppress_linenodes &&
            !_is_meta_doc_block(s)
        for (i, c) in enumerate(children(s))
            need_lnns && push!(out.args, first_linenode(c))
            let suppress_c = i == 1 && (h == :for || h == :let)
                push!(out.args, syntax_to_expr(c, suppress_c))
            end
        end
        # Add extra linenodes to some blocks for better provenance.  Note no
        # short-form function, since we don't know here whether the original rhs
        # was a block
        if h === :block && length(out.args) == 0 && !suppress_linenodes
            push!(out.args, first_linenode(s))
        elseif h in (:module, :function, :macro) && length(out.args) > 0
            let b = out.args[end]
                b isa Expr && b.head === :block && pushfirst!(
                    b.args, first_linenode(s))
            end
        elseif h in (:for, :while) && length(out.args) > 0
            let b = out.args[end]
                b isa Expr && b.head === :block && let sr = sourceref(s)
                    last_lno = sr isa LineNumberNode ? sr :
                        LineNumberNode(source_line(sr.code, sr.last_byte),
                                       filename(s))
                    push!(b.args, last_lno)
                end
            end
        end
        out
    end
end

# convenience function for `jl_parse`
function _c_parseall_expr(code::Core.SimpleVector, filename::String,
                          lineno::Int, mod::Union{Module, Nothing})
    (ptr, len) = code
    str = String(unsafe_wrap(Array, ptr, len))
    pfm = Meta.parser_for_module(mod)
    ex, offset = Meta._parse_string(str, filename, lineno, 1, :all, Expr, pfm)
    return Core.svec(ex, offset-1)
end

function fl_toplevel_eval(mod::Module, @nospecialize(x))
    ex = if x isa Syntax
        Expr(:toplevel, first_linenode(x), syntax_to_expr(x))
    else
        x
    end
    ccall(:jl_toplevel_eval, Any, (Any, Any), mod, ex)
end

#-------------------------------------------------------------------------------
# Printing

attrsummary(name, _value) = string(name)
attrsummary(name, value::Number) = "$name=$value"
attrsummary(name, value::LineNumberNode) = "$name=L$(value.line)"
attrsummary(name, value::Module) = "$name=$value"

function subscript_str(i)
     replace(string(i),
             "0"=>"₀", "1"=>"₁", "2"=>"₂", "3"=>"₃", "4"=>"₄",
             "5"=>"₅", "6"=>"₆", "7"=>"₇", "8"=>"₈", "9"=>"₉")
end

function _value_string(ex)
    k = head(ex)
    str = k == :identifier  ? syntax_name(ex)           :
          k == :placeholder ? syntax_name(ex)           :
          k == :ssavalue    ? "%"                   :
          k == :bindingid   ? "#"                   :
          k == :label       ? "label"               :
          k == :nothing     ? "core.nothing"        :
          k == :core        ? "core.$(syntax_name(ex))" :
          k == :top         ? "top.$(syntax_name(ex))"  :
          k == :symbol      ? ":$(syntax_name(ex))" :
          k == :globalref   ? "$(ex.mod).$(syntax_name(ex))" :
          k == :slot        ? "slot" :
          k == :slots       ? "Slots" :
          k == :lambdabindings ? "LambdaBindings" :
          k == :latestworld ? "latestworld" :
          k == :static_parameter ? "static_parameter" :
          k == :symboliclabel ? "label:$(syntax_name(ex))" :
          k == :symbolicgoto ? "goto:$(syntax_name(ex))" :
          k == :sourcelocation ?
              "SourceLocation:$(first_linenode(ex).line)" :
              k == :value ?
              (ex.value isa SourceRef ?
              "SourceRef:$(first_linenode(ex).line)" :
              ex.value isa SyntaxContext ? "SyntaxContext(#=omitted=#)" : repr(ex.value)) :
              ex.value !== nothing ? repr(ex.value) : "::$k"

    if head(ex) in (:bindingid, :slot, :ssavalue, :static_parameter, :label)
        idstr = subscript_str(ex.value::Int)
        str = "$(str)$idstr"
    end
    if k == :slot || k == :bindingid
        for p in provenance(ex)
            if head(p) == :identifier
                str = "$(str)/$(syntax_name(p))"
                break
            end
        end
    end
    return str
end

function _show_syntax_tree(io, ex, indent, show_kinds, @nospecialize(parent_sc))
    nodestr = !is_leaf(ex) ? "[$(string(head(ex)))]" : _value_string(ex)

    treestr = rpad(string(indent, nodestr), 40)
    if show_kinds && is_leaf(ex)
        treestr = treestr*" :: "*string(head(ex))
    end

    std_attrs = Set([:value,:head,:syntax_flags,:source,:context])
    attrstr = join([attrsummary(n, getproperty(ex, n))
                    for n in fieldnames(typeof(ex)) if n ∉ std_attrs &&
                        getproperty(ex, n) !== nothing], ",")
    print(io, rpad(treestr, 60))
    print(io, " | ")
    sc = ex.context
    if sc !== parent_sc
        print(io, sc)
        print(io, ",")
    end
    print(io, attrstr)
    println(io)

    if !is_leaf(ex)
        new_indent = indent*"  "
        for n in children(ex)
            _show_syntax_tree(io, n, new_indent, show_kinds, sc)
        end
    end
end

function Base.show(io::IO, ::MIME"text/plain", ex::Syntax, show_kinds=true)
    assert_syntax(ex)
    _show_syntax_tree(io, ex, "", show_kinds, nothing)
end
function _show_syntax_tree_sexpr(io, ex)
    if is_leaf(ex)
        print(io, _value_string(ex))
    else
        print(io, "(", string(head(ex)))
        for n in children(ex)
            print(io, ' ')
            _show_syntax_tree_sexpr(io, n)
        end
        print(io, ')')
    end
end

function Base.show(io::IO, ::MIME"text/x.sexpression", node::Syntax)
    assert_syntax(node)
    _show_syntax_tree_sexpr(io, node)
end

function Base.show(io::IO, node::Syntax)
    assert_syntax(node)
    _show_syntax_tree_sexpr(io, node)
end
