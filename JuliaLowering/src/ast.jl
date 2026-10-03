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
Lexical scope ID
"""
const ScopeId = Int

# TODO: this is now redundant; replace calls with @mknode
function newleaf(prov::SyntaxTree, k::Symbol, @nospecialize(value))
    context = prov.context
    @jl_assert k === :value || value !== nothing (
        prov, "only Value may contain nothing")
    @mknode(;head=k, context, source=prov, value)
end
newleaf(prov::SyntaxTree, k::Symbol) =
    @mknode(;source=prov, context=prov.context, head=k)

# TODO: redundant, `map` should be fine
function mapsyntax(f, exs::AbstractVector{SyntaxTree})
    out = SyntaxList()
    for ex in exs
        push!(out, f(ex))
    end
    out
end

function mapindex(sl::Vector{SyntaxTree}, i::Int)
    out = SyntaxList()
    for st in sl
        push!(out, getindex(st, i))
    end
    out
end

function mktree(old::SyntaxTree)
    if is_leaf(old)
        @mknode(old; children=nothing)
    else
        cs = mapsyntax(mktree, children(old))
        @mknode(old; children=cs)
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

function emit_assign_tmp(stmts::Vector{SyntaxTree}, ctx, ex, name="tmp")
    if is_ssa(ctx, ex)
        return ex
    end
    var = ssavar(ctx, ex, name)
    push!(stmts, @mknode(;source=ex, context=ex.context,
                         head=:(=), children=SyntaxList(var, ex)))
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
function _append_nodeids!(ids::Vector{SyntaxTree}, vals::Vector{SyntaxTree})
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

const SyntaxMeta = Base.ImmutableDict{Symbol,Any}
function setmeta!(st::SyntaxTree, key::Symbol, @nospecialize(val))
    meta = let m = st.meta
        isnothing(m) ? SyntaxMeta(key, val) : SyntaxMeta(m, key, val)
    end
    setfield!(st, :meta, meta)
    st
end
function setmeta(st::SyntaxTree, key::Symbol, @nospecialize(val))
    setmeta!(is_leaf(st) ? @mknode(st; children=nothing) :
        @mknode(st; children=children(st)), key, val)
end
function getmeta(st, name, @nospecialize(default))
    meta = st.meta
    isnothing(meta) ? default : get(meta, name, default)
end
name_hint(name) = SyntaxMeta(:name_hint, name)

#-------------------------------------------------------------------------------
# Predicates and accessors working on expression trees

is_flisp_compat(sc::SyntaxContext) = sc.edition < JL_NEW_EDITION
is_flisp_compat(st::SyntaxTree) = is_flisp_compat(st.context)

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
          | [<head> <pattern>*]
          | [<head> <pattern>* <list_identifier>... <pattern>*]

# note "*" is the meta-operator meaning one or more, and "..." is literal
```

where a `[:h p1 p2 ps...]` form matches any tree with head :h and >=2
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
julia> st = parsestmt(SyntaxTree, "function foo(x,y,z); x; end")

julia> @stm st begin
    [:function [:call fname [:parameters kws...]] body] ->
        "no positional args, only kwargs: $(kws)"
    [:function fname] ->
        "zero-method function $fname"
    [:function [:call fname args...] body] ->
        "normal function $fname"
    ([:(=) [:call _...] _...], when=(args=if_valid_get_args(st[1]); !isnothing(args))) ->
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
                              :($k_gs = $head($st_gs)),
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
                          :($k_gs = $head($st_gs)),
                          :($nc_gs = $numchildren($st_gs))),
               _stm_matches(p, st_gs, k_gs, nc_gs, debug))
end

function _stm_matches(p::Expr, st_gs::Symbol, k_gs::Symbol, nc_gs::Symbol, debug)
    pat_k = p.args[1]::QuoteNode
    out = Expr(:&&, :($pat_k === $k_gs))
    debug && push!(out.args, Expr(:block, :(printstyled(
        string("[head]: ", $k_gs, "\n"); color=:yellow)), true))

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

    # This exact `:head` syntax is not necessary since the head can't be
    # provided by a variable, but requiring it allows us to implement list
    # matching later.
    @assert(p.args[1] isa QuoteNode && p.args[1].value isa Symbol,
            "first pattern elt must be quoted :head")

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
