# This file is a part of Julia. License is MIT: http://julialang.org/license

module Cartesian

export @nloops, @nref, @ncall, @nexprs, @nextract, @nall, @nany, @ntuple, @nif

### Cartesian-specific macros

# Generate nested loops
macro nloops(N, itersym, rangeexpr, args...)
    _nloops(N, itersym, rangeexpr, args...)
end

_nloops(N::Int, itersym::Symbol, arraysym::Symbol, args::Expr...) = _nloops(N, itersym, :(d->1:size($arraysym,d)), args...)

function _nloops(N::Int, itersym::Symbol, rangeexpr::Expr, args::Expr...)
    if rangeexpr.head != :->
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

# Generate expression A[i1, i2, ...]
macro nref(N, A, sym)
    _nref(N, A, sym)
end

function _nref(N::Int, A::Symbol, ex)
    vars = [ inlineanonymous(ex,i) for i = 1:N ]
    Expr(:escape, Expr(:ref, A, vars...))
end

# Generate f(arg1, arg2, ...)
macro ncall(N, f, sym...)
    _ncall(N, f, sym...)
end

function _ncall(N::Int, f, args...)
    pre = args[1:end-1]
    ex = args[end]
    vars = [ inlineanonymous(ex,i) for i = 1:N ]
    Expr(:escape, Expr(:call, f, pre..., vars...))
end

# Generate N expressions
macro nexprs(N, ex)
    _nexprs(N, ex)
end

function _nexprs(N::Int, ex::Expr)
    exs = [ inlineanonymous(ex,i) for i = 1:N ]
    Expr(:escape, Expr(:block, exs...))
end

# Make variables esym1, esym2, ... = isym
macro nextract(N, esym, isym)
    _nextract(N, esym, isym)
end

function _nextract(N::Int, esym::Symbol, isym::Symbol)
    aexprs = [Expr(:escape, Expr(:(=), inlineanonymous(esym, i), :(($isym)[$i]))) for i = 1:N]
    Expr(:block, aexprs...)
end

function _nextract(N::Int, esym::Symbol, ex::Expr)
    aexprs = [Expr(:escape, Expr(:(=), inlineanonymous(esym, i), inlineanonymous(ex,i))) for i = 1:N]
    Expr(:block, aexprs...)
end

# Check whether variables i1, i2, ... all satisfy criterion
macro nall(N, criterion)
    _nall(N, criterion)
end

function _nall(N::Int, criterion::Expr)
    if criterion.head != :->
        throw(ArgumentError("second argument must be an anonymous function expression yielding the criterion"))
    end
    conds = [Expr(:escape, inlineanonymous(criterion, i)) for i = 1:N]
    Expr(:&&, conds...)
end

# Check whether any of variables i1, i2, ... satisfy criterion
macro nany(N, criterion)
    _nany(N, criterion)
end

function _nany(N::Int, criterion::Expr)
    if criterion.head != :->
        error("Second argument must be an anonymous function expression yielding the criterion")
    end
    conds = [Expr(:escape, inlineanonymous(criterion, i)) for i = 1:N]
    Expr(:||, conds...)
end

macro ntuple(N, ex)
    _ntuple(N, ex)
end

function _ntuple(N::Int, ex)
    vars = [ inlineanonymous(ex,i) for i = 1:N ]
    Expr(:escape, Expr(:tuple, vars...))
end

# if condition1; operation1; elseif condition2; operation2; else operation3
# You can pass one or two operations; the second, if present, is used in the final "else"
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
    if ex.head != :->
        throw(ArgumentError("not an anonymous function"))
    end
    if !isa(ex.args[1], Symbol)
        throw(ArgumentError("not a single-argument anonymous function"))
    end
    sym = ex.args[1]
    ex = ex.args[2]
    exout = lreplace(ex, sym, val)
    exout = poplinenum(exout)
    exprresolve(exout)
end

# Given :i and 3, this generates :i_3
inlineanonymous(base::Symbol, ext) = symbol(base,"_",string(ext))

# Replace a symbol by a value or a "coded" symbol
# E.g., for d = 3,
#    lreplace(:d, :d, 3) -> 3
#    lreplace(:i_d, :d, 3) -> :i_3
#    lreplace(:i_{d-1}, :d, 3) -> :i_2
# This follows LaTeX notation.
immutable LReplace{S<:AbstractString}
    pat_sym::Symbol
    pat_str::S
    val::Int
end
LReplace(sym::Symbol, val::Integer) = LReplace(sym, string(sym), val)

lreplace(ex, sym::Symbol, val) = lreplace!(copy(ex), LReplace(sym, val))

function lreplace!(sym::Symbol, r::LReplace)
    sym == r.pat_sym && return r.val
    symbol(lreplace!(string(sym), r))
end

function lreplace!(str::AbstractString, r::LReplace)
    i = start(str)
    pat = r.pat_str
    j = start(pat)
    matching = false
    while !done(str, i)
        cstr, i = next(str, i)
        if !matching
            if cstr != '_' || done(str, i)
                continue
            end
            istart = i
            cstr, i = next(str, i)
        end
        if !done(pat, j)
            cr, j = next(pat, j)
            if cstr == cr
                matching = true
            else
                matching = false
                j = start(pat)
                i = istart
                continue
            end
        end
        if matching && done(pat, j)
            if done(str, i) || next(str, i)[1] == '_'
                # We have a match
                return string(str[1:prevind(str, istart)], r.val, lreplace!(str[i:end], r))
            end
            matching = false
            j = start(pat)
            i = istart
        end
    end
    str
end

function lreplace!(ex::Expr, r::LReplace)
    # Curly-brace notation, which acts like parentheses
    if ex.head == :curly && length(ex.args) == 2 && isa(ex.args[1], Symbol) && endswith(string(ex.args[1]), "_")
        excurly = Base.Cartesian.exprresolve(lreplace!(ex.args[2], r))
        if isa(excurly, Number)
            return symbol(ex.args[1],excurly)
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
    if ex.head == :block
        if length(ex.args) == 1
            return ex.args[1]
        elseif length(ex.args) == 2 && isa(ex.args[1], LineNumberNode)
            return ex.args[2]
        elseif (length(ex.args) == 2 && isa(ex.args[1], Expr) && ex.args[1].head == :line)
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
    if ex.head == :call && haskey(exprresolve_arith_dict, ex.args[1]) && all([isa(ex.args[i], Number) for i = 2:length(ex.args)])
        return true, exprresolve_arith_dict[ex.args[1]](ex.args[2:end]...)
    end
    false, 0
end
exprresolve_arith(arg) = false, 0

exprresolve_conditional(b::Bool) = true, b
function exprresolve_conditional(ex::Expr)
    if ex.head == :comparison && isa(ex.args[1], Number) && isa(ex.args[3], Number)
        return true, exprresolve_cond_dict[ex.args[2]](ex.args[1], ex.args[3])
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
    elseif ex.head == :call && (ex.args[1] == :+ || ex.args[1] == :-) && length(ex.args) == 3 && ex.args[3] == 0
        # simplify x+0 and x-0
        return ex.args[2]
    end
    # Resolve array references
    if ex.head == :ref && isa(ex.args[1], Array)
        for i = 2:length(ex.args)
            if !isa(ex.args[i], Real)
                return ex
            end
        end
        return ex.args[1][ex.args[2:end]...]
    end
    # Resolve conditionals
    if ex.head == :if
        can_eval, tf = exprresolve_conditional(ex.args[1])
        if can_eval
            ex = tf?ex.args[2]:ex.args[3]
        end
    end
    ex
end

end
