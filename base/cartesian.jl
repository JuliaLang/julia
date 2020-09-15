# This file is a part of Julia. License is MIT: https://julialang.org/license

module Cartesian

export @nloops, @nref, @ncall, @nexprs, @nextract, @nall, @nany, @ntuple, @nif

### Cartesian-specific macros

"""
    @nloops N itersym rangeexpr bodyexpr
    @nloops N itersym rangeexpr preexpr bodyexpr
    @nloops N itersym rangeexpr preexpr postexpr bodyexpr

Generate `N` nested loops, using `itersym` as the prefix for the iteration variables.
`rangeexpr` may be an anonymous-function expression, or a simple symbol `var` in which case
the range is `axes(var, d)` for dimension `d`.

Optionally, you can provide "pre" and "post" expressions. These get executed first and last,
respectively, in the body of each loop. For example:

    @nloops 2 i A d -> j_d = min(i_d, 5) begin
        s += @nref 2 A j
    end

would generate:

    for i_2 = axes(A, 2)
        j_2 = min(i_2, 5)
        for i_1 = axes(A, 1)
            j_1 = min(i_1, 5)
            s += A[j_1, j_2]
        end
    end

If you want just a post-expression, supply [`nothing`](@ref) for the pre-expression. Using
parentheses and semicolons, you can supply multi-statement expressions.
"""
macro nloops(N, itersym, rangeexpr, args...)
    _nloops(N, itersym, rangeexpr, args...)
end

function _nloops(N::Int, itersym::Symbol, arraysym::Symbol, args::Expr...)
    @gensym d
    _nloops(N, itersym, :($d->Base.axes($arraysym, $d)), args...)
end

function _nloops(N::Int, itersym::Symbol, rangeexpr::Expr, args::Expr...)
    if rangeexpr.head !== :->
        throw(ArgumentError("second argument must be an anonymous function expression to compute the range"))
    end
    if !(1 <= length(args) <= 3)
        throw(ArgumentError("number of arguments must be 1 ≤ length(args) ≤ 3, got $nargs"))
    end
    body = args[end]
    ex = Expr(:escape, body)
    for dim = 1:N
        itervar = inlineanonymous(itersym, dim)
        rng = inlineanonymous(rangeexpr, dim)
        preexpr = length(args) > 1 ? inlineanonymous(args[1], dim) : (:(nothing))
        postexpr = length(args) > 2 ? inlineanonymous(args[2], dim) : (:(nothing))
        ex = quote
            for $(esc(itervar)) = $(esc(rng))
                $(esc(preexpr))
                $ex
                $(esc(postexpr))
            end
        end
    end
    ex
end

"""
    @nref N A indexexpr

Generate expressions like `A[i_1, i_2, ...]`. `indexexpr` can either be an iteration-symbol
prefix, or an anonymous-function expression.

# Examples
```jldoctest
julia> @macroexpand Base.Cartesian.@nref 3 A i
:(A[i_1, i_2, i_3])
```
"""
macro nref(N::Int, A::Symbol, ex)
    vars = Any[ inlineanonymous(ex,i) for i = 1:N ]
    Expr(:escape, Expr(:ref, A, vars...))
end

"""
    @ncall N f sym...

Generate a function call expression. `sym` represents any number of function arguments, the
last of which may be an anonymous-function expression and is expanded into `N` arguments.

For example, `@ncall 3 func a` generates

    func(a_1, a_2, a_3)

while `@ncall 2 func a b i->c[i]` yields

    func(a, b, c[1], c[2])

"""
macro ncall(N::Int, f, args...)
    pre = args[1:end-1]
    ex = args[end]
    vars = Any[ inlineanonymous(ex,i) for i = 1:N ]
    Expr(:escape, Expr(:call, f, pre..., vars...))
end

"""
    @nexprs N expr

Generate `N` expressions. `expr` should be an anonymous-function expression.

# Examples
```jldoctest
julia> @macroexpand Base.Cartesian.@nexprs 4 i -> y[i] = A[i+j]
quote
    y[1] = A[1 + j]
    y[2] = A[2 + j]
    y[3] = A[3 + j]
    y[4] = A[4 + j]
end
```
"""
macro nexprs(N::Int, ex::Expr)
    exs = Any[ inlineanonymous(ex,i) for i = 1:N ]
    Expr(:escape, Expr(:block, exs...))
end

"""
    @nextract N esym isym

Generate `N` variables `esym_1`, `esym_2`, ..., `esym_N` to extract values from `isym`.
`isym` can be either a `Symbol` or anonymous-function expression.

`@nextract 2 x y` would generate

    x_1 = y[1]
    x_2 = y[2]

while `@nextract 3 x d->y[2d-1]` yields

    x_1 = y[1]
    x_2 = y[3]
    x_3 = y[5]

"""
macro nextract(N::Int, esym::Symbol, isym::Symbol)
    aexprs = Any[ Expr(:escape, Expr(:(=), inlineanonymous(esym, i), :(($isym)[$i]))) for i = 1:N ]
    Expr(:block, aexprs...)
end

macro nextract(N::Int, esym::Symbol, ex::Expr)
    aexprs = Any[ Expr(:escape, Expr(:(=), inlineanonymous(esym, i), inlineanonymous(ex,i))) for i = 1:N ]
    Expr(:block, aexprs...)
end

"""
    @nall N expr

Check whether all of the expressions generated by the anonymous-function expression `expr`
evaluate to `true`.

`@nall 3 d->(i_d > 1)` would generate the expression `(i_1 > 1 && i_2 > 1 && i_3 > 1)`. This
can be convenient for bounds-checking.
"""
macro nall(N::Int, criterion::Expr)
    if criterion.head !== :->
        throw(ArgumentError("second argument must be an anonymous function expression yielding the criterion"))
    end
    conds = Any[ Expr(:escape, inlineanonymous(criterion, i)) for i = 1:N ]
    Expr(:&&, conds...)
end

"""
    @nany N expr

Check whether any of the expressions generated by the anonymous-function expression `expr`
evaluate to `true`.

`@nany 3 d->(i_d > 1)` would generate the expression `(i_1 > 1 || i_2 > 1 || i_3 > 1)`.
"""
macro nany(N::Int, criterion::Expr)
    if criterion.head !== :->
        error("Second argument must be an anonymous function expression yielding the criterion")
    end
    conds = Any[ Expr(:escape, inlineanonymous(criterion, i)) for i = 1:N ]
    Expr(:||, conds...)
end

"""
    @ntuple N expr

Generates an `N`-tuple. `@ntuple 2 i` would generate `(i_1, i_2)`, and `@ntuple 2 k->k+1`
would generate `(2,3)`.
"""
macro ntuple(N::Int, ex)
    vars = Any[ inlineanonymous(ex,i) for i = 1:N ]
    Expr(:escape, Expr(:tuple, vars...))
end

"""
    @nif N conditionexpr expr
    @nif N conditionexpr expr elseexpr

Generates a sequence of `if ... elseif ... else ... end` statements. For example:

    @nif 3 d->(i_d >= size(A,d)) d->(error("Dimension ", d, " too big")) d->println("All OK")

would generate:

    if i_1 > size(A, 1)
        error("Dimension ", 1, " too big")
    elseif i_2 > size(A, 2)
        error("Dimension ", 2, " too big")
    else
        println("All OK")
    end
"""
macro nif(N, condition, operation...)
    # Handle the final "else"
    ex = esc(inlineanonymous(length(operation) > 1 ? operation[2] : operation[1], N))
    # Make the nested if statements
    for i = N-1:-1:1
        ex = Expr(:if, esc(inlineanonymous(condition,i)), esc(inlineanonymous(operation[1],i)), ex)
    end
    ex
end

## Utilities

# Simplify expressions like :(d->3:size(A,d)-3) given an explicit value for d
function inlineanonymous(ex::Expr, val)
    if ex.head !== :->
        throw(ArgumentError("not an anonymous function"))
    end
    if !isa(ex.args[1], Symbol)
        throw(ArgumentError("not a single-argument anonymous function"))
    end
    sym = ex.args[1]::Symbol
    ex = ex.args[2]::Expr
    exout = lreplace(ex, sym, val)
    exout = poplinenum(exout)
    exprresolve(exout)
end

# Given :i and 3, this generates :i_3
inlineanonymous(base::Symbol, ext) = Symbol(base,'_',ext)

# Replace a symbol by a value or a "coded" symbol
# E.g., for d = 3,
#    lreplace(:d, :d, 3) -> 3
#    lreplace(:i_d, :d, 3) -> :i_3
#    lreplace(:i_{d-1}, :d, 3) -> :i_2
# This follows LaTeX notation.
struct LReplace{S<:AbstractString}
    pat_sym::Symbol
    pat_str::S
    val::Int
end
LReplace(sym::Symbol, val::Integer) = LReplace(sym, string(sym), val)

lreplace(ex::Expr, sym::Symbol, val) = lreplace!(copy(ex), LReplace(sym, val))

function lreplace!(sym::Symbol, r::LReplace)
    sym == r.pat_sym && return r.val
    Symbol(lreplace!(string(sym), r))
end

function lreplace!(str::AbstractString, r::LReplace)
    i = firstindex(str)
    pat = r.pat_str
    j = firstindex(pat)
    matching = false
    local istart::Int
    while i <= ncodeunits(str)
        cstr = str[i]
        i = nextind(str, i)
        if !matching
            if cstr != '_' || i > ncodeunits(str)
                continue
            end
            istart = i
            cstr = str[i]
            i = nextind(str, i)
        end
        if j <= lastindex(pat)
            cr = pat[j]
            j = nextind(pat, j)
            if cstr == cr
                matching = true
            else
                matching = false
                j = firstindex(pat)
                i = istart
                continue
            end
        end
        if matching && j > lastindex(pat)
            if i > lastindex(str) || str[i] == '_'
                # We have a match
                return string(str[1:prevind(str, istart)], r.val, lreplace!(str[i:end], r))
            end
            matching = false
            j = firstindex(pat)
            i = istart
        end
    end
    str
end

function lreplace!(ex::Expr, r::LReplace)
    # Curly-brace notation, which acts like parentheses
    if ex.head === :curly && length(ex.args) == 2 && isa(ex.args[1], Symbol) && endswith(string(ex.args[1]::Symbol), "_")
        excurly = exprresolve(lreplace!(ex.args[2], r))
        if isa(excurly, Int)
            return Symbol(ex.args[1]::Symbol, excurly)
        else
            ex.args[2] = excurly
            return ex
        end
    end
    for i in 1:length(ex.args)
        ex.args[i] = lreplace!(ex.args[i], r)
    end
    ex
end

lreplace!(arg, r::LReplace) = arg

poplinenum(arg) = arg
function poplinenum(ex::Expr)
    if ex.head === :block
        if length(ex.args) == 1
            return ex.args[1]
        elseif length(ex.args) == 2 && isa(ex.args[1], LineNumberNode)
            return ex.args[2]
        elseif (length(ex.args) == 2 && isa(ex.args[1], Expr) && ex.args[1].head === :line)
            return ex.args[2]
        end
    end
    ex
end

## Resolve expressions at parsing time ##

const exprresolve_arith_dict = Dict{Symbol,Function}(:+ => +,
    :- => -, :* => *, :/ => /, :^ => ^, :div => div)
const exprresolve_cond_dict = Dict{Symbol,Function}(:(==) => ==,
    :(<) => <, :(>) => >, :(<=) => <=, :(>=) => >=)

function exprresolve_arith(ex::Expr)
    if ex.head === :call
        callee = ex.args[1]
        if isa(callee, Symbol)
            if haskey(exprresolve_arith_dict, callee) && all(Bool[isa(ex.args[i], Number) for i = 2:length(ex.args)])
                return true, exprresolve_arith_dict[callee](ex.args[2:end]...)
            end
        end
    end
    false, 0
end
exprresolve_arith(arg) = false, 0

exprresolve_conditional(b::Bool) = true, b
function exprresolve_conditional(ex::Expr)
    if ex.head === :call
        callee = ex.args[1]
        if isa(callee, Symbol)
            if callee ∈ keys(exprresolve_cond_dict) && isa(ex.args[2], Number) && isa(ex.args[3], Number)
                return true, exprresolve_cond_dict[callee](ex.args[2], ex.args[3])
            end
        end
    end
    false, false
end
exprresolve_conditional(arg) = false, false

exprresolve(arg) = arg
function exprresolve(ex::Expr)
    for i = 1:length(ex.args)
        ex.args[i] = exprresolve(ex.args[i])
    end
    # Handle simple arithmetic
    can_eval, result = exprresolve_arith(ex)
    if can_eval
        return result
    elseif ex.head === :call && (ex.args[1] === :+ || ex.args[1] === :-) && length(ex.args) == 3 && ex.args[3] == 0
        # simplify x+0 and x-0
        return ex.args[2]
    end
    # Resolve array references
    if ex.head === :ref && isa(ex.args[1], Array)
        for i = 2:length(ex.args)
            if !isa(ex.args[i], Real)
                return ex
            end
        end
        return ex.args[1][ex.args[2:end]...]
    end
    # Resolve conditionals
    if ex.head === :if
        can_eval, tf = exprresolve_conditional(ex.args[1])
        if can_eval
            ex = tf ? ex.args[2] : ex.args[3]
        end
    end
    ex
end

end
