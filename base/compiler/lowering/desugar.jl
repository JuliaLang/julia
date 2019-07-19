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

# AST predicates
#---------------

isquoted(ex) = ex isa QuoteNode || (ex isa Expr &&
               ex.head in (:quote, :top, :core, :globalref,
                           :outerref, :break, :inert, :meta))

issymbollike(ex) = ex isa Symbol || ex isa SSAValue

isassignment(ex) = ex isa Expr && ex.head == :(=)

isdecl(ex)       = ex isa Expr && ex.head == :(::)

# True if `ex` is trivially free of side effects (and hence safe to repeat)
iseffectfree(ex) = !(ex isa Expr) || isquoted(ex)

# True if `ex` is lhs of short-form function definition
# f(args...)
is_eventually_call(ex) = ex isa Expr &&
                         (ex.head == :call || (ex.head in (:where, :(::)) &&
                                               is_eventually_call(ex.args[1])))

# Symbol `s` occurs in ex, excluding expression heads and quoted Exprs
function occursin_ex(s::Symbol, ex)
    s === ex || (ex isa Expr && !isquoted(ex) && any(e->occursin_ex(s, e), ex.args))
end

# As above, but test each expression with predicate `pred`. Optionally, filter
# expressions with `filt`.
function occursin_ex(pred::Function, ex; filt=e->true)
    filt(ex) && (pred(ex) || (ex isa Expr && !isquoted(ex) &&
                              any(e->occursin_ex(pred, e, filt=filt), ex.args)))
end

# Check for `f(args...; pars...)` syntax
has_parameters(ex::Expr) = length(ex.args) >= 2 && ex.args[2] isa Expr &&
                           ex.args[2].head === :parameters

# has_assignment(args) = any(isassignment, args)

# AST matching
#-------------

decl_var(ex) = isdecl(ex) ? ex.args[1] : ex

# Given a complex assignment LHS, return the symbol that will ultimately be assigned to
function assigned_name(ex)
    if ex isa Expr && ex.head in (:call, :curly, :where) || (ex.head == :(::) &&
                                                             is_eventually_call(ex))
        assigned_name(ex.args[1])
    else
        ex
    end
end

# Get list of variable names on lhs of expression
lhs_vars(ex) = lhs_vars!(Symbol[], ex)
function lhs_vars!(vars, ex)
    if ex isa Symbol
        push!(vars, ex)
    elseif isdecl(ex)
        push!(vars, decl_var(ex))
    elseif ex isa Expr && ex.head == :tuple
        foreach(e->lhs_vars!(vars, e), ex.args)
    end
    vars
end

# Error checking utilities
#-------------------------
struct LoweringError <: Exception
    msg::String
    ex
end
LoweringError(msg::AbstractString) = LoweringError(msg, nothing)

function Base.show(io::IO, err::LoweringError)
    print(io, err.msg)
    if err.ex !== nothing
        print(io, " in `", err.ex, "`")
    end
end

function check_no_assignments(ex)
    for e in ex.args
        !isassignment(e) || throw(LoweringError("misplaced assignment statement", ex))
    end
end
error_unexpected_semicolon(ex) = throw(LoweringError("unexpected semicolon", ex))


# Utilities for constructing lowered ASTs
#----------------------------------------

topcall(head, args...) = Expr(:call, Expr(:top, head), args...)
corecall(head, args...) = Expr(:call, Expr(:core, head), args...)
blockify(ex) = ex isa Expr && ex.head !== :block ? ex : Expr(:block, ex) # TODO: null Expr?
mapargs(f, ex) = ex isa Expr ? Expr(ex.head, map(f, ex.args)...) : ex

# FIXME: Counter Should be thread local or in expansion ctx
let ssa_index = Ref(0)
    global make_ssavalue() = SSAValue(ssa_index[] += 1)
end

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


#-------------------------------------------------------------------------------

function find_symbolic_labels!(labels, gotos, ex)
    if ex isa Expr
        if ex.head == :symboliclabel
            push!(labels, ex.args[1])
        elseif ex.head == :symbolicgoto
            push!(gotos, ex.args[1])
        elseif !isquoted(ex)
            for arg in ex.args
                find_symbolic_labels!(labels, gotos, arg)
            end
        end
    end
end

function has_unmatched_symbolic_goto(ex)
    labels = Set{Symbol}()
    gotos = Set{Symbol}()
    find_symbolic_labels!(labels, gotos, ex)
    !all(target in labels for target in gotos)
end

function expand_try(ex)
    if length(ex.args) < 3 || length(ex.args) > 4
        throw(LoweringError("invalid `try` form", ex))
    end
    try_block   = ex.args[1]
    exc_var     = ex.args[2]
    catch_block = ex.args[3]
    finally_block = length(ex.args) < 4 ? false : ex.args[4]
    if has_unmatched_symbolic_goto(try_block)
        throw(LoweringError("goto from a try/finally block is not permitted", ex))
    end
    if exc_var !== false
        catch_block = Expr(:block,
                           Expr(:(=), exc_var, Expr(:the_exception)),
                           catch_block)
    end
    trycatch = catch_block !== false ?
        Expr(:trycatch,
             Expr(:scope_block, try_block),
             Expr(:scope_block, catch_block)) :
        Expr(:scope_block, try_block)
    lowered = finally_block !== false ?
        Expr(:tryfinally,
             trycatch,
             Expr(:scope_block, finally_block)) : trycatch
    expand_forms(lowered)
end

function expand_let(ex)
    bindings = !(ex.args[1] isa Expr) ? throw(LoweringError("Invalid let syntax", ex)) :
               ex.args[1].head == :block ? ex.args[1].args : [ex.args[1]]
    body = isempty(bindings) ?  Expr(:scope_block, blockify(ex.args[2])) : ex.args[2]
    for binding in reverse(bindings)
        body =
        if binding isa Symbol || isdecl(binding)
            # Just symbol -> add local
            Expr(:scope_block,
                 Expr(:block,
                      Expr(:local, binding),
                      body))
        elseif binding isa Expr && binding.head == :(=) && length(binding.args) == 2
            # Some kind of assignment
            lhs = binding.args[1]
            rhs = binding.args[2]
            if is_eventually_call(lhs)
                # f() = c
                Expr(:scope_block, body) # FIXME Needs expand_function to be implemented
            elseif lhs isa Symbol || isdecl(lhs)
                # `x = c` or `x::T = c`
                varname = decl_var(lhs)
                if occursin_ex(varname, rhs)
                    tmp = make_ssavalue()
                    Expr(:scope_block,
                         Expr(:block,
                              Expr(:(=), tmp, rhs),
                              Expr(:scope_block,
                                   Expr(:block,
                                        Expr(:local_def, lhs),
                                        Expr(:(=), varname, tmp),
                                        body))))
                else
                    Expr(:scope_block,
                         Expr(:block,
                              Expr(:local_def, lhs),
                              Expr(:(=), varname, rhs),
                              body))
                end
            elseif lhs isa Expr && lhs.head == :tuple
                # (a, b, c, ...) = rhs
                vars = lhs_vars(lhs)
                if occursin_ex(e->e isa Symbol && e in vars, rhs)
                    tmp = make_ssavalue()
                    Expr(:scope_block,
                         Expr(:block,
                              Expr(:(=), tmp, rhs),
                              Expr(:scope_block,
                                   Expr(:block,
                                        [Expr(:local_def, v) for v in vars]...,
                                        Expr(:(=), lhs, tmp),
                                        body))))
                else
                    Expr(:scope_block,
                         Expr(:block,
                              [Expr(:local_def, v) for v in vars]...,
                              binding,
                              body))
                end
            else
                throw(LoweringError("invalid binding in let syntax", binding))
            end
        else
            throw(LoweringError("invalid binding in let syntax", binding))
        end
    end
    expand_forms(body)
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

# Flatten nested Expr(head, args) with depth first traversal of args.
function flatten_ex_args!(args, head, ex)
    if ex isa Expr && ex.head == head
        for a in ex.args
            flatten_ex_args!(args, head, a)
        end
    else
        push!(args, ex)
    end
    args
end

function expand_and(ex)
    args = flatten_ex_args!([], :&&, ex)
    @assert length(args) > 1
    e = args[end]
    for i = length(args)-1:-1:1
        e = Expr(:if, args[i], e, false)
    end
    e
end

function expand_or(ex)
    args = flatten_ex_args!([], :||, ex)
    @assert length(args) > 1
    e = args[end]
    for i = length(args)-1:-1:1
        e = Expr(:if, args[i], true, e)
    end
    e
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
        expand_let(ex)
    elseif head == :macro
        expand_todo(ex) # expand-macro-def
    elseif head == :struct
        expand_todo(ex) # expand-struct-def
    elseif head == :try
        expand_try(ex)
    elseif head == :lambda
        expand_todo(ex) # expand-table
    elseif head == :block
        if length(args) == 0
            nothing
        elseif length(args) == 1 && !(args[1] isa LineNumberNode)
            expand_forms(args[1])
        else
            Expr(:block, map(expand_forms, args)...)
        end
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
        expand_todo(ex)
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
        !has_parameters(ex) || error_unexpected_semicolon(ex)
        expand_forms(partially_expand_ref(ex))
    elseif head == :curly
        expand_todo(ex) # expand-table
    elseif head == :call
        expand_todo(ex) # expand-table
    elseif head == :do
        callex = args[1]
        anonfunc = args[2]
        expand_forms(has_parameters(callex) ?
            Expr(:call, callex.args[1], callex.args[2], anonfunc, callex.args[3:end]...) :
            Expr(:call, callex.args[1], anonfunc, callex.args[2:end]...)
        )
    elseif head == :tuple
        # TODO: NamedTuple lower-named-tuple
        #if has_parameters(ex)
        #end
        expand_forms(corecall(:tuple, args...))
    elseif head == :braces
        throw(LoweringError("{ } vector syntax is discontinued", ex))
    elseif head == :bracescat
        throw(LoweringError("{ } matrix syntax is discontinued", ex))
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
        expand_forms(expand_and(ex))
    elseif head == :||
        expand_forms(expand_or(ex))
    elseif head in (:(+=), :(-=), :(*=), :(.*=), :(/=), :(./=), :(//=), :(.//=),
                    :(\=), :(.\=), :(.+=), :(.-=), :(^=), :(.^=), :(÷=), :(.÷=),
                    :(%=), :(.%=), :(|=), :(.|=), :(&=), :(.&=), :($=), :(⊻=),
                    :(.⊻=), :(<<=), :(.<<=), :(>>=), :(.>>=), :(>>>=), :(.>>>=))
        expand_todo(ex) # lower-update-op
    elseif head == :...
        throw(LoweringError("`...` expression outside call", ex))
    elseif head == :$
        throw(LoweringError("`\$` expression outside quote", ex))
    elseif head == :vect
        !has_parameters(ex) || error_unexpected_semicolon(ex)
        check_no_assignments(ex)
        expand_forms(topcall(:vect, args...))
    elseif head == :hcat
        check_no_assignments(ex)
        expand_forms(topcall(:hcat, args...))
    elseif head == :vcat
        check_no_assignments(ex)
        if any(e->e isa Expr && e.head == :row, args)
            expand_hvcat(ex)
        else
            expand_forms(topcall(:vcat, args...))
        end
    elseif head == :typed_hcat
        check_no_assignments(ex)
        expand_forms(topcall(:typed_hcat, args...))
    elseif head == :typed_vcat
        check_no_assignments(ex)
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
