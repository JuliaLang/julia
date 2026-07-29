#-------------------------------------------------------------------------------
# @jl_assert: Produce an internal error that surfaces one or more trees.
# Example: `@jl_assert 1 === 1 (tree1, "message1"), tree2, (tree3, "message3")`
@static if DEBUG
    macro jl_assert(cond, args...)
        usage = "usage: @jl_assert(condition, tree|(tree, message)...)"
        @assert(!isempty(args), usage)
        sts = Expr(:call, SyntaxList)
        msgs = Expr(:call, Base.vect)
        for a in args
            if Meta.isexpr(a, :tuple, 2)
                push!(sts.args, a.args[1])
                push!(msgs.args, a.args[2])
            else
                push!(sts.args, a)
                push!(msgs.args, string(a))
            end
        end
        # just add assertion string to first msg
        msgs.args[2] = Expr(
            :string, "`jl_assert(", QuoteNode(cond), ", _)`: ", msgs.args[2])
        :($(esc(cond)) ? nothing : begin
              throw(LoweringError($(esc(sts)), $(esc(msgs)), true))
          end)
    end
else
    # allow @jl_assert false in value position to not change rettype
    macro jl_assert(cond, args...)
        cond === false ? :(throw("@jl_assert false")) : nothing
    end
end

abstract type AbstractLoweringContext end

"""
Bindings for the current lambda being processed.

Lowering passes prior to scope resolution return `nothing` and bindings are
collected later.
"""
current_lambda_bindings(::AbstractLoweringContext) = nothing

"""
Unique symbolic identity for a variable, constant, label, or other entity
"""
const IdTag = Int

"""
Lexical scope ID
"""
const ScopeId = Int

const DEFAULT_NODE = SyntaxTree(
    K"None", nothing, nothing, LineNumberNode(0), nothing)

const LOWERING_FLAG_SYNTHESIZED = UInt8(0x01)
const LOWERING_FLAG_FORWARDING_METHOD = UInt8(0x02)

_forwarding_method_flags(is_forwarding_method::Bool) =
    is_forwarding_method ? LOWERING_FLAG_FORWARDING_METHOD : UInt8(0)
_forwarding_method_flags(ex::SyntaxTree) =
    ex.lowering_flags & LOWERING_FLAG_FORWARDING_METHOD

function _mark_synthesized!(node::SyntaxTree)
    setfield!(node, :lowering_flags, node.lowering_flags | LOWERING_FLAG_SYNTHESIZED)
    return node
end

function _inherit_synthesized!(node::SyntaxTree, @nospecialize(srcref))
    srcref isa SyntaxTree || return node
    is_synthesized(srcref) && _mark_synthesized!(node)
    return node
end

_inherit_synthesized!(node::SyntaxTree) =
    _inherit_synthesized!(node, node.source)

function JuliaSyntax.newleaf(prov::SyntaxTree, k::Kind)
    _inherit_synthesized!(SyntaxTree(k, nothing, nothing, prov, prov.context), prov)
end

function JuliaSyntax.newnode(prov::SyntaxTree, k::Kind, cs)
    _inherit_synthesized!(SyntaxTree(k, cs, nothing, prov, prov.context), prov)
end

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
        out_args[i] = (DEBUG && n === :jl_source) ? __source__ :
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
    old === DEFAULT_NODE && !((:kind, :source, :context) ⊆ seen_attrs) &&
        throw(ArgumentError("brand-new node from @mknode requires more attrs $__source__"))

    out = Expr(:let,
               Expr(:block, Expr(:(=), old_gs, old)),
               Expr(:block, Expr(:call, SyntaxTree, out_args...)))
    out.args[end] = Expr(:call, _inherit_synthesized!, out.args[end])
    DEBUG && (out.args[end] = Expr(:call, _debug_check_attrs, out.args[end]))
    esc(out)
end
macro mknode(x)
    (old, attrs) = Meta.isexpr(x, :parameters) ? (DEFAULT_NODE, x) : (x, nothing)
    esc(Expr(:macrocall, var"@mknode", __source__, attrs, old))
end

function _debug_check_attrs(x)
    assert_syntaxtree(x, false)
    x
end

function JuliaSyntax.newleaf(prov, k, @nospecialize(value))
    context = prov isa SyntaxTree ? prov.context : nothing
    @jl_assert k === K"Value" || value !== nothing (
        prov, "only Value may contain nothing")
    if k == K"Identifier" || k == K"BindingId" || k == K"Value" ||
        k == K"core" || k == K"top" || k == K"Symbol" || k == K"globalref" ||
        k == K"Placeholder" || k == K"label" || k == K"symboliclabel" ||
        k == K"symbolicgoto"
        @mknode(;kind=k, source=prov, context, value)
    elseif k in KSet"TOMBSTONE SourceLocation latestworld latestworld_if_toplevel
                     softscope nothing"
        @mknode(;kind=k, source=prov, context)
    else
        val = k == K"Integer" ? convert(Int,     value) :
              k == K"Float"   ? convert(Float64, value) :
              k == K"String"  ? convert(String,  value) :
              k == K"Char"    ? convert(Char,    value) :
              k == K"Bool"    ? value                   :
              k == K"LambdaBindings" ? value :
              k == K"Slots" ? value :
              k == K"SSAValue" ? value :
              k == K"slot" ? value :
              k == K"static_parameter" ? value :
              k == K"VERSION" ? value :
              error("Unexpected leaf kind `$k`")
        @mknode(;kind=k, source=prov, value=val, context)
    end
end

function syntax_name(st)
    @jl_assert kind(st) in KSet"""
    Identifier Placeholder Symbol core top globalref symboliclabel symbolicgoto
    unknown_head oldsymbolicgoto
    """ st
    st.value::String
end

# Convenience functions to create leaf nodes referring to identifiers within
# the Core and Top modules.
nothing_(ctx, ex) = newleaf(ex, K"nothing")

# Assign `ex` to an SSA variable.
# Return (variable, assignment_node)
function assign_tmp(ctx::AbstractLoweringContext, ex, name="tmp")
    var = ssavar(ctx, ex, name)
    assign_var = @mknode(;source=ex, context=ex.context, kind=K"=",
                         children=SyntaxList(var, ex))
    var, assign_var
end

function emit_assign_tmp(stmts::SyntaxList, ctx, ex, name="tmp")
    if is_ssa(ctx, ex)
        return ex
    end
    var = ssavar(ctx, ex, name)
    push!(stmts, newnode(ex, K"=", SyntaxList(var, ex)))
    var
end

"""
    is_synthesized(ex::SyntaxTree) -> Bool

Return `true` if `ex` was introduced by lowering rather than written by the
user. Lowering passes set the synthesized lowering flag on nodes they emit to
implement source-level constructs (currently: the per-property assignments
in named-tuple destructure like `(; a, b) = rhs`). Consumers that walk the
EST against user source ranges — editor features answering "what's at this
byte range" — use this to skip synthetic nodes that would otherwise shadow
the user's expressions.

The flag set at the initial emission site automatically propagates to nodes
the same site (or any later pass) creates from that srcref via [`@ast`](@ref),
so individual passes don't have to hand-propagate through every recreation.
"""
is_synthesized(ex::SyntaxTree) = !iszero(ex.lowering_flags & LOWERING_FLAG_SYNTHESIZED)

"""
    is_forwarding_method(ex::SyntaxTree) -> Bool

Return `true` if `ex` is a `K"method"` generated to initialize default
arguments or route a keyword call to the method containing the user-written
body.
"""
is_forwarding_method(ex::SyntaxTree) = !iszero(_forwarding_method_flags(ex))

#-------------------------------------------------------------------------------
# @ast macro

# Fallbacks to give comprehensible error messages for use with the @ast macro
function _push_nodeid!(::Vector{SyntaxTree}, ex)
    error("Attempt to use `$(repr(ex))` of type `$(typeof(ex))` as an AST node. Try annotating with `::K\"your_intended_kind\"?`")
end
function _push_nodeid!(::Vector{SyntaxTree}, ex::AbstractVector{<:SyntaxTree})
    error("Attempt to use vector as an AST node. Did you mean to splat this? (content: `$(repr(ex))`)")
end
function _push_nodeid!(ids::Vector{SyntaxTree}, st::SyntaxTree)
    push!(ids, st)
end
function _push_nodeid!(::Vector{SyntaxTree}, ::Nothing)
    nothing
end
function _append_nodeids!(ids::Vector{SyntaxTree}, vals)
    for v in vals
        _push_nodeid!(ids, v)
    end
end
function _append_nodeids!(ids::Vector{SyntaxTree}, vals::SyntaxList)
    append!(ids, vals)
end

function _match_kind(srcref, ex, jl_line)
    kws = Expr(:parameters)
    seen = Set{Symbol}()
    if Meta.isexpr(ex, :call)
        kind = ex.args[1]
        args = ex.args[2:end]
        if Meta.isexpr(args[1], :parameters)
            for a in args[1].args
                a isa Symbol && push!(seen, a)
                Meta.isexpr(a, :kw, 2) && a.args[1] isa Symbol && push!(seen, a.args[1])
            end
            append!(kws.args, args[1].args)
            popfirst!(args)
        end
        if length(args) == 1 && !Meta.isexpr(args[1], :kw)
            srcref = args[1]
        elseif length(args) > 1
            error("Unexpected srcref argument in `$ex`")
        end
    else
        kind = ex
    end
    :source in seen || push!(kws.args, Expr(:kw, :source, srcref))
    :kind in seen || push!(kws.args, Expr(:kw, :kind, kind))
    :context in seen || push!(kws.args, Expr(
        :kw, :context, Expr(:., srcref, QuoteNode(:context))))
    DEBUG && push!(kws.args, Expr(:kw, :jl_source, jl_line))
    return kws
end

function _expand_ast_tree(ctx, srcref, tree, jl_line::QuoteNode)
    if Meta.isexpr(tree, :(::))
        # Leaf node
        if length(tree.args) == 2
            val = tree.args[1]
            kindspec = tree.args[2]
        else
            val = nothing
            kindspec = tree.args[1]
        end
        let kws = _match_kind(srcref, kindspec, jl_line)
            !isnothing(val) && push!(kws.args, Expr(:kw, :value, val))
            Expr(:macrocall, var"@mknode", jl_line.value, kws)
        end
    elseif Meta.isexpr(tree, :call) && tree.args[1] === :(=>)
        # Leaf node with copied attributes
        kind = tree.args[3]
        srcref2 = tree.args[2]
        kws = Expr(:parameters, Expr(:kw, :kind, kind), Expr(:kw, :children, nothing))
        DEBUG && push!(kws.args, Expr(:kw, :jl_source, jl_line))
        Expr(:macrocall, var"@mknode", jl_line.value, kws, srcref2)
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
        children_ex = :(let child_ids = Vector{$SyntaxTree}()
        end)
        child_stmts = children_ex.args[2].args
        for a in flatargs[2:end]
            child = _expand_ast_tree(ctx, srcref, a, jl_line)
            if Meta.isexpr(child, :(...))
                push!(child_stmts, :($_append_nodeids!(child_ids, $(child.args[1]))))
            else
                push!(child_stmts, :($_push_nodeid!(child_ids, $child)))
            end
        end
        push!(child_stmts, :(child_ids))
        let kws = _match_kind(srcref, flatargs[1], jl_line)
            push!(kws.args, Expr(:kw, :children, children_ex))
            Expr(:macrocall, var"@mknode", jl_line.value, kws)
        end
    elseif Meta.isexpr(tree, :(:=))
        ctx === nothing && throw(ArgumentError(
            "@ast requires ctx arg for `:=` assignments $jl_line"))
        lhs = tree.args[1]
        rhs = _expand_ast_tree(ctx, srcref, tree.args[2], jl_line)
        ssadef = gensym("ssadef")
        quote
            ($lhs, $ssadef) = assign_tmp($ctx, $rhs, $(string(lhs)))
            $ssadef
        end
    elseif Meta.isexpr(tree, :macrocall)
        tree
    elseif tree isa Expr
        Expr(tree.head, map(a->_expand_ast_tree(ctx, srcref, a, jl_line), tree.args)...)
    else
        tree
    end
end

"""
    @ast ctx srcref tree

Syntactic s-expression shorthand for constructing a `SyntaxTree` AST.

* `ctx` - Lowering context
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
* `kind(;attr=val)` - set an additional attribute
* `kind(srcref; attr₁=val₁, attr₂=val₂)` - the general form


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
    @gensym ctx_gs srcref_gs
    assigns = if ctx isa Symbol && all(==('_'), string(ctx))
        :(let $srcref_gs = $srcref::$SyntaxTree
              $(_expand_ast_tree(nothing, srcref_gs, tree, QuoteNode(__source__)))
          end)
    else
        :(let $ctx_gs = $ctx, $srcref_gs = $srcref::$SyntaxTree
              $(_expand_ast_tree(ctx_gs, srcref_gs, tree, QuoteNode(__source__)))
          end)
    end |> esc
end

name_hint(name) = JuliaSyntax.CompileHints(:name_hint, name)

#-------------------------------------------------------------------------------
# Predicates and accessors working on expression trees

function is_quoted(ex)
    kind(ex) in KSet"Symbol quote top core globalref break inert
                     syntaxinert meta inbounds inline noinline loopinfo"
end

function extension_type(ex)
    @jl_assert kind(ex) == K"assert" ex
    @jl_assert numchildren(ex) >= 1 ex
    @jl_assert kind(ex[1]) == K"Symbol" ex
    syntax_name(ex[1])
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

function is_core_Any(ex)
    kind(ex) === K"core" && syntax_name(ex) === "Any"
end

function is_simple_atom(ctx, ex)
    k = kind(ex)
    # TODO thismodule
    is_literal(k) || k == K"Symbol" || k == K"Value" || is_ssa(ctx, ex) ||
        k == K"nothing"
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

#-------------------------------------------------------------------------------
# Context wrapper which helps to construct a list of statements to be executed
# prior to some expression. Useful when we need to use subexpressions multiple
# times.
struct StatementListCtx{Ctx} <: AbstractLoweringContext
    ctx::Ctx
    stmts::Vector{SyntaxTree}
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
    StatementListCtx(ctx, SyntaxList())
end
