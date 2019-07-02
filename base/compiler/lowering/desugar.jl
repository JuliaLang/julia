# Lowering pass 1: Syntax desugaring
#
# In this pass, we simplify the AST by transforming much of the rich surface
# syntax into a smaller core syntax containing fewer expression heads.
#
# Some of this core syntax is also part of the surface syntax, but some is
# unique to the lowered code. For example, `Expr(:scope_block, ...)` all
# scoping in the core syntax is
# handled by the scope_block

using Core: SSAValue

#-------------------------------------------------------------------------------
# AST tools

# :(f(arg; par)).arg s =>  [:f, Expr(:parameters, :par), :arg]
has_parameters(args) = length(args) >= 2 && args[2] isa Expr && args[2].head === :parameters
has_assignment(args) = any(isassignment, args)

isquoted(ex) = ex isa QuoteNode || (ex isa Expr &&
               ex.head in (:quote, :top, :core, :globalref, :outerref, :break, :inert, :meta))
issymbollike(ex) = ex isa Symbol || ex isa SSAValue

isassignment(ex) = ex isa Expr && ex.head == :(=)

# True if `ex` is trivially free of side effects (and hence safe to repeat)
iseffectfree(ex) = !(ex isa Expr) || isquoted(ex)

# FIXME: Counter Should be thread local or in expansion ctx
let ssa_index = Ref(0)
    global make_ssavalue() = SSAValue(ssa_index[] += 1)
end

top(ex) = Expr(:top, ex)
topcall(head, args...) = Expr(:call, Expr(:top, head), args...)
core(ex) = Expr(:core, ex)
blockify(ex) = ex isa Expr && ex.head !== :block ? ex : Expr(:block, ex) # TODO: null Expr?
mapargs(f, ex) = ex isa Expr ? Expr(ex.head, map(f, ex.args)...) : ex

# Symbol `s` occurs in ex
occursin_ex(s::Symbol, ex::Symbol) = s === ex
occursin_ex(s::Symbol, ex::Expr) = occursin_ex(s, ex.args)
occursin_ex(s::Symbol, exs) = any(e->occursin_ex(s, e), exs)

"""
    make_ssa_if(need_ssa, ex, stmts)

Return a name for the value of `ex` that can be used multiple times.
An extra assignment is recorded into `stmts` if necessary.
"""
function make_ssa_if(need_ssa::Bool, ex, stmts)
    if need_ssa
        v = make_ssavalue()
        push!(stmts, Expr(:(=), v, ex))
        v
    else
        ex
    end
end

make_ssa_if(need_ssa::Function, ex, stmts) = make_ssa_if(need_ssa(ex), ex, stmts)

function check_no_assigments(ex)
    for e in ex.args
        !isassignment(e) || throw(LoweringError("misplaced assigment statement in `$ex`"))
    end
end
error_unexpected_semicolon(ex) = throw(LoweringError("unexpected semicolon in `$ex`"))


#-------------------------------------------------------------------------------
struct LoweringError <: Exception
    msg::AbstractString
end

"""
Replace `end` for the closest ref expression; don't go inside nested refs
`preceding_splats` are a list of the splatted arguments that precede index `n`.
`end`s are replaced with a call to `lastindex(a)` if `n == nothing`, or
`lastindex(a,n)`.
"""
function replace_end(ex, a, n, preceding_splats)
    if ex === :end
        # the appropriate computation for an `end` symbol for indexing
        # the array `a` in the `n`th index.
        if isempty(preceding_splats)
            n === nothing ? topcall(:lastindex, a) :
                            topcall(:lastindex, a, n)
        else
            dimno = topcall(:+, n - length(preceding_splats),
                            map(t->:(topcall(:length, t)), preceding_splats)...)
            topcall(:lastindex, a, dimno)
        end
    elseif !(ex isa Expr) || isquoted(ex)
        ex
    elseif ex.head == :ref
        # Only recurse into first argument of ref, not into index list.
        Expr(:ref, replace_end(ex.args[1], a, n, preceding_splats), ex.args[2:end]...)
    else
        mapargs(x->replace_end(x, a, n, preceding_splats), ex)
    end
end

# Expand Expr(:ref, indexable, indices...) by replacing `end` within `indices`
# as necessary
function partially_expand_ref(ex)
    a = ex.args[1]
    stmts = []
    arr = make_ssa_if(!iseffectfree, a, stmts)
    preceding_splats = []
    new_idxs = []
    N = length(ex.args) - 1
    # go through indices and replace any embedded `end` symbols
    for i = 1:N
        idx = ex.args[i+1]
        n = N == 1 ? nothing : i
        if idx isa Expr && idx.head == :...
            idx = replace_end(idx.args[1], arr, n, preceding_splats)
            tosplat = make_ssa_if(issymbollike, idx, stmts)
            push!(preceding_splats, tosplat)
            push!(new_idxs, Expr(:..., tosplat))
        else
            push!(new_idxs, replace_end(idx, arr, n, preceding_splats))
        end
    end
    Expr(:block,
         stmts...,
         topcall(:getindex, arr, new_idxs...))
end

function expand_hvcat(ex)
    # rows inside vcat -> hvcat
    lengths = Int[]
    vals = []
    istyped = ex.head == :typed_vcat
    for i in (istyped ? 2 : 1):length(ex.args)
        e = ex.args[i]
        if e isa Expr && e.head == :row
            push!(lengths, length(e.args))
            append!(vals, e.args)
        else
            push!(lengths, 1)
            push!(vals, e)
        end
    end
    if istyped
        expand_forms(topcall(:typed_hvcat, ex.args[1], Expr(:tuple, lengths...), vals...))
    else
        expand_forms(topcall(:hvcat, Expr(:tuple, lengths...), vals...))
    end
end

#-------------------------------------------------------------------------------
# Expansion entry point

function expand_todo(ex)
    Expr(ex.head, map(e->expand_forms(e), ex.args)...)
end

function expand_forms(ex)
    if !(ex isa Expr)
        return ex
    end
    head = ex.head
    args = ex.args
    # TODO: Use a hash table here like expand-table?
    if head == :function
        expand_todo(ex) # expand-function-def
    elseif head == :->
        expand_todo(ex) # expand-arrow
    elseif head == :let
        expand_todo(ex) # expand-let
    elseif head == :macro
        expand_todo(ex) # expand-macro-def
    elseif head == :struct
        expand_todo(ex) # expand-struct-def
    elseif head == :try
        expand_todo(ex) # expand-try
    elseif head == :lambda
        expand_todo(ex) # expand-table
    elseif head == :block
        expand_todo(ex) # expand-table
    elseif head == :.
        expand_todo(ex) # expand-fuse-broadcast
    elseif head == :.=
        expand_todo(ex) # expand-fuse-broadcast
    elseif head == :<:
        expand_forms(Expr(:call, :<:, args...))
    elseif head == :>:
        expand_forms(Expr(:call, :>:, args...))
    elseif head == :where
        expand_todo(ex) # expand-wheres
    elseif head == :const
        expand_todo(ex) # expand-const-decl
    elseif head == :local
        expand_todo(ex) # expand-local-or-global-decl
    elseif head == :global
        expand_todo(ex) # expand-local-or-global-decl
    elseif head == :local_def
        expand_todo(ex) # expand-local-or-global-decl
    elseif head == :(=)
        expand_todo(ex) # expand-table
    elseif head == :abstract
        expand_todo(ex) # expand-table
    elseif head == :primitive
        expand_todo(ex) # expand-table
    elseif head == :comparison
        expand_todo(ex) # expand-compare-chain
    elseif head == :ref
        !has_parameters(args) || error_unexpected_semicolon(ex)
        expand_forms(partially_expand_ref(ex))
    elseif head == :curly
        expand_todo(ex) # expand-table
    elseif head == :call
        expand_todo(ex) # expand-table
    elseif head == :do
        expand_todo(ex) # expand-table
    elseif head == :tuple
        # TODO: NamedTuple lower-named-tuple
        #if has_parameters(args)
        #end
        expand_forms(Expr(:call, core(:tuple), args...))
    elseif head == :braces
        expand_todo(ex) # expand-table
    elseif head == :bracescat
        expand_todo(ex) # expand-table
    elseif head == :string
        expand_forms(topcall(:string, args...))
    elseif head == :(::)
        expand_todo(ex) # expand-table
    elseif head == :while
        Expr(:break_block, :loop_exit,
             Expr(:_while, expand_forms(args[1]),
                  Expr(:break_block, :loop_cont,
                       Expr(:scope_block,
                            blockify(map(expand_forms, args[2:end])...)))))
    elseif head == :break
        isempty(args) ? Expr(:break, :loop_exit) : ex
    elseif head == :continue
        isempty(args) ? Expr(:break, :loop_cont) : ex
    elseif head == :for
        expand_todo(ex) # expand-for
    elseif head == :&&
        expand_todo(ex) # expand-and
    elseif head == :||
        expand_todo(ex) # expand-or
    elseif head in (:(+=), :(-=), :(*=), :(.*=), :(/=), :(./=), :(//=), :(.//=),
                    :(\=), :(.\=), :(.+=), :(.-=), :(^=), :(.^=), :(÷=), :(.÷=),
                    :(%=), :(.%=), :(|=), :(.|=), :(&=), :(.&=), :($=), :(⊻=),
                    :(.⊻=), :(<<=), :(.<<=), :(>>=), :(.>>=), :(>>>=), :(.>>>=))
        expand_todo(ex) # lower-update-op
    elseif head == :...
        expand_todo(ex) # expand-table
    elseif head == :$
        throw(LoweringError("`\$` expression outside quote"))
    elseif head == :vect
        !has_parameters(args) || error_unexpected_semicolon(ex)
        check_no_assigments(ex)
        expand_forms(topcall(:vect, args...))
    elseif head == :hcat
        check_no_assigments(ex)
        expand_forms(topcall(:hcat, args...))
    elseif head == :vcat
        check_no_assigments(ex)
        if any(e->e isa Expr && e.head == :row, args)
            expand_hvcat(ex)
        else
            expand_forms(topcall(:vcat, args...))
        end
    elseif head == :typed_hcat
        check_no_assigments(ex)
        expand_forms(topcall(:typed_hcat, args...))
    elseif head == :typed_vcat
        check_no_assigments(ex)
        if any(e->e isa Expr && e.head == :row, args)
            expand_hvcat(ex)
        else
            expand_forms(topcall(:typed_vcat, args...))
        end
    elseif head == Symbol("'")
        expand_forms(topcall(:adjoint, args...))
    elseif head == :generator
        expand_todo(ex) # expand-generator
    elseif head == :flatten
        expand_todo(ex) # expand-generator
    elseif head == :comprehension
        expand_todo(ex) # expand-table
    elseif head == :typed_comprehension
        expand_todo(ex) # lower-comprehension
    else
        Expr(head, map(e->expand_forms(e), args)...)
    end
end
