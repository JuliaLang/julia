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
    :none, nothing, nothing, LineNumberNode(0),
    SyntaxContext(JuliaLowering, (0, 0)))

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
    old === DEFAULT_NODE && !((:head, :source, :context) ⊆ seen_attrs) &&
        throw(ArgumentError("brand-new node from @mknode requires more attrs $__source__"))

    out = Expr(:let,
               Expr(:block, Expr(:(=), old_gs, old)),
               Expr(:block, Expr(:call, SyntaxTree, out_args...)))
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

function JuliaSyntax.newleaf(prov::SyntaxTree, k::Symbol, @nospecialize(value))
    context = prov.context
    @jl_assert k === :value || value !== nothing (
        prov, "only Value may contain nothing")
    if k == :identifier || k == :bindingid || k == :value ||
        k == :core || k == :top || k == :symbol || k == :globalref ||
        k == :placeholder || k == :label || k == :symboliclabel ||
        k == :symbolicgoto
        @mknode(;head=k, source=prov, context, value)
    elseif k in (:tombstone, :sourcelocation, :latestworld, :latestworld_if_toplevel,
                 :softscope, :nothing)
        @mknode(;head=k, source=prov, context)
    else
        val = k == :lambdabindings ? value :
              k == :slots ? value :
              k == :ssavalue ? value :
              k == :slot ? value :
              k == :static_parameter ? value :
              k == :version ? value :
              error("Unexpected leaf kind `$k`")
        @mknode(;head=k, source=prov, value=val, context)
    end
end

function syntax_name(st)
    @jl_assert head(st) in (:identifier, :placeholder, :symbol, :core, :top, :globalref,
                            :symboliclabel, :symbolicgoto) st
    st.value::String
end

# Convenience functions to create leaf nodes referring to identifiers within
# the Core and Top modules.
nothing_(ctx, ex) = newleaf(ex, :nothing)

# Assign `ex` to an SSA variable.
# Return (variable, assignment_node)
function assign_tmp(ctx::AbstractLoweringContext, ex, name="tmp")
    var = ssavar(ctx, ex, name)
    assign_var = @mknode(;source=ex, context=ex.context, head=:(=),
                         children=SyntaxList(var, ex))
    var, assign_var
end

function emit_assign_tmp(stmts::SyntaxList, ctx, ex, name="tmp")
    if is_ssa(ctx, ex)
        return ex
    end
    var = ssavar(ctx, ex, name)
    push!(stmts, newnode(ex, :(=), SyntaxList(var, ex)))
    var
end

#-------------------------------------------------------------------------------
# @ast macro

# Fallbacks to give comprehensible error messages for use with the @ast macro
function _push_nodeid!(::Vector{SyntaxTree}, ex)
    error("Attempt to use `$(repr(ex))` of type `$(typeof(ex))` as an AST node. Try annotating with `::your_intended_head`?")
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

function _match_head(srcref, ex, jl_line, leaf::Bool)
    kws = Expr(:parameters)
    seen = Set{Symbol}()
    if Meta.isexpr(ex, :call)
        h = ex.args[1]
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
        h = ex
    end
    leaf && h isa Symbol && (h = QuoteNode(h))
    :source in seen || push!(kws.args, Expr(:kw, :source, srcref))
    :head in seen || push!(kws.args, Expr(:kw, :head, h))
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
        let kws = _match_head(srcref, kindspec, jl_line, true)
            !isnothing(val) && push!(kws.args, Expr(:kw, :value, val))
            Expr(:macrocall, var"@mknode", jl_line.value, kws)
        end
    elseif Meta.isexpr(tree, :call) && tree.args[1] === :(=>)
        # Leaf node with copied attributes
        h = tree.args[3]
        srcref2 = tree.args[2]
        kws = Expr(:parameters, Expr(:kw, :head, h), Expr(:kw, :children, nothing))
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
        let kws = _match_head(srcref, flatargs[1], jl_line, false)
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
* `[:head child₁ child₂]` - construct an interior node with children
* `value :: head`        - construct a leaf node
* `ex => :head`          - convert a leaf node to the given head, copying attributes
                           from it and also using `ex` as the source reference.
* `var := ex`            - Set `var=ssavar(...)` and return an assignment node `\$var=ex`.
                           `var` may be used outside `@ast`
* `cond ? ex1 : ex2`     - Conditional; `ex1` and `ex2` will be recursively expanded.
                           `if ... end` and `if ... else ... end` also work with this.

Any `head` can be replaced with an expression of the form
* `head(srcref)` - override the source reference for this node and its children
* `head(;attr=val)` - set an additional attribute
* `head(srcref; attr₁=val₁, attr₂=val₂)` - the general form


# Examples

```
@ast ctx srcref [
   :toplevel
   [:using
       [:importpath
           "Base"       ::identifier(src)
       ]
   ]
   [:function
       [:call
           "eval"       ::identifier
           "x"          ::identifier
       ]
       [:call
           "eval"       ::core
           mn           =>:identifier
           "x"          ::identifier
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
    head(ex) in (:symbol, :quote, :top, :core, :globalref, :inert,
                 :syntaxinert, :meta, :inbounds, :inline, :noinline, :loopinfo)
end

function extension_type(ex)
    @jl_assert head(ex) == :assert ex
    @jl_assert numchildren(ex) >= 1 ex
    @jl_assert head(ex[1]) == :symbol ex
    syntax_name(ex[1])
end

function is_eventually_call(ex::SyntaxTree)
    k = head(ex)
    return k == :call || ((k == :where || k == :(::)) && is_eventually_call(ex[1]))
end

function find_parameters_ind(exs)
    i = length(exs)
    while i >= 1
        k = head(exs[i])
        if k == :parameters
            return i
        elseif k != :do
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
    any(head(e) == :(=) for e in exs)
end

function is_valid_modref(ex)
    return head(ex) == :. && head(ex[2]) == :symbol &&
           (head(ex[1]) == :identifier || is_valid_modref(ex[1]))
end

function is_core_Any(ex)
    head(ex) === :core && syntax_name(ex) === "Any"
end

function is_simple_atom(ctx, ex)
    k = head(ex)
    # TODO thismodule
    k == :symbol || k == :value || is_ssa(ctx, ex) || k == :nothing
end

function is_identifier_like(ex)
    k = head(ex)
    k == :identifier || k == :bindingid || k == :placeholder
end

function decl_var(ex)
    head(ex) == :(::) ? ex[1] : ex
end

# Given the signature of a `function`, return the symbol that will ultimately
# be assigned to in local/global scope, if any.
function assigned_function_name(ex)
    while head(ex) == :where
        # f() where T
        ex = ex[1]
    end
    if head(ex) == :(::) && numchildren(ex) == 2
        # f()::T
        ex = ex[1]
    end
    if head(ex) != :call
        throw(LoweringError(ex, "Expected call syntax in function signature"))
    end
    ex = ex[1]
    if head(ex) == :curly
        # f{T}()
        ex = ex[1]
    end
    if head(ex) == :(::) || head(ex) == :.
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
    while i > 0 && head(args[i]) == :parameters && numchildren(args[i]) == 0
        i -= 1
    end
    args[1:i]
end

function to_symbol(ctx, ex)
    @ast ctx ex ex=>:symbol
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
