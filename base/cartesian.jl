module Cartesian

export @ngenerate, @nsplat, @nloops, @nref, @ncall, @nexprs, @nextract, @nall, @ntuple, @nif, ngenerate

const CARTESIAN_DIMS = 4

### @ngenerate, for auto-generation of separate versions of functions for different dimensionalities
# Examples (deliberately trivial):
#     @ngenerate N returntype myndims{T,N}(A::Array{T,N}) = N
# or alternatively
#     function gen_body(N::Int)
#         quote
#             return $N
#         end
#     end
#     eval(ngenerate(:N, returntypeexpr, :(myndims{T,N}(A::Array{T,N})), gen_body))
# The latter allows you to use a single gen_body function for both ngenerate and
# when your function maintains its own method cache (e.g., reduction or broadcasting).
#
# Special syntax for function prototypes:
#   @ngenerate N returntype function myfunction(A::AbstractArray, I::NTuple{N, Int}...)
# for N = 3 translates to
#   function myfunction(A::AbstractArray, I_1::Int, I_2::Int, I_3::Int)
# and for the generic (cached) case as
#   function myfunction(A::AbstractArray, I::Int...)
#     @nextract N I I
# with N = length(I). N should _not_ be listed as a parameter of the function unless
# earlier arguments use it that way.
# To avoid ambiguity, it would be preferable to have some specific syntax for this, such as
#   myfunction(A::AbstractArray, I::Int...N)
# where N can be an integer or symbol. Currently T...N generates a parser error.
macro ngenerate(itersym, returntypeexpr, funcexpr)
    if isa(funcexpr, Expr) && funcexpr.head == :macrocall && funcexpr.args[1] == symbol("@inline")
        funcexpr = Base._inline(funcexpr.args[2])
    end
    isfuncexpr(funcexpr) || error("Requires a function expression")
    esc(ngenerate(itersym, returntypeexpr, funcexpr.args[1], N->sreplace!(copy(funcexpr.args[2]), itersym, N)))
end

# @nsplat takes an expression like
#    @nsplat N 2:3 myfunction(A, I::NTuple{N,Real}...) = getindex(A, I...)
# and generates
#    myfunction(A, I_1::Real, I_2::Real) = getindex(A, I_1, I_2)
#    myfunction(A, I_1::Real, I_2::Real, I_3::Real) = getindex(A, I_1, I_2, I_3)
#    myfunction(A, I::Real...) = getindex(A, I...)
# An @nsplat function _cannot_ have any other Cartesian macros in it.
# If you omit the range, it uses 1:CARTESIAN_DIMS.
macro nsplat(itersym, args...)
    local rng
    if length(args) == 1
        rng = 1:CARTESIAN_DIMS
        funcexpr = args[1]
    elseif length(args) == 2
        rangeexpr = args[1]
        funcexpr = args[2]
        if !isa(rangeexpr, Expr) || rangeexpr.head != :(:) || length(rangeexpr.args) != 2
            error("First argument must be a from:to expression")
        end
        rng = rangeexpr.args[1]:rangeexpr.args[2]
    else
        error("Wrong number of arguments")
    end
    if isa(funcexpr, Expr) && funcexpr.head == :macrocall && funcexpr.args[1] == symbol("@inline")
        funcexpr = Base._inline(funcexpr.args[2])
    end
    isfuncexpr(funcexpr) || error("Second argument must be a function expression")
    prototype = funcexpr.args[1]
    body = funcexpr.args[2]
    varname, T = get_splatinfo(prototype, itersym)
    isempty(varname) && error("Last argument must be a splat")
    explicit = [Expr(:function, resolvesplat!(copy(prototype), varname, T, N),
                     resolvesplats!(copy(body), varname, N)) for N in rng]
    protosplat = resolvesplat!(copy(prototype), varname, T, 0)
    protosplat.args[end] = Expr(:..., protosplat.args[end])
    splat = Expr(:function, protosplat, body)
    esc(Expr(:block, explicit..., splat))
end

generate1(itersym, prototype, bodyfunc, N::Int, varname, T) =
    Expr(:function, spliceint!(sreplace!(resolvesplat!(copy(prototype), varname, T, N), itersym, N)),
         resolvesplats!(bodyfunc(N), varname, N))

function ngenerate(itersym, returntypeexpr, prototype, bodyfunc, dims=1:CARTESIAN_DIMS, makecached::Bool = true)
    varname, T = get_splatinfo(prototype, itersym)
    # Generate versions for specific dimensions
    fdim = [generate1(itersym, prototype, bodyfunc, N, varname, T) for N in dims]
    if !makecached
        return Expr(:block, fdim...)
    end
    # Generate the generic cache-based version
    if isempty(varname)
        setitersym, extractvarargs = :(), N -> nothing
    else
        s = symbol(varname)
        setitersym = hasparameter(prototype, itersym) ? (:(@assert $itersym == length($s))) : (:($itersym = length($s)))
        extractvarargs = N -> Expr(:block, map(popescape, _nextract(N, s, s).args)...)
    end
    fsym = funcsym(prototype)
    dictname = symbol(string(fsym)*"_cache")
    fargs = funcargs(prototype)
    if !isempty(varname)
        fargs[end] = Expr(:..., fargs[end].args[1])
    end
    flocal = funcrename(copy(prototype), :_F_)
    F = Expr(:function, resolvesplat!(prototype, varname, T), quote
             $setitersym
             if !haskey($dictname, $itersym)
                 gen1 = Base.Cartesian.generate1($(symbol(itersym)), $(Expr(:quote, flocal)), $bodyfunc, $itersym, $varname, $T)
                 $(dictname)[$itersym] = eval(quote
                     local _F_
                     $gen1
                     _F_
                 end)
             end
             ($(dictname)[$itersym]($(fargs...)))::$returntypeexpr
         end)
    Expr(:block, fdim..., quote
            let $dictname = Dict{Int,Function}()
            $F
            end
        end)
end

isfuncexpr(ex::Expr) =
    ex.head == :function || (ex.head == :(=) && typeof(ex.args[1]) == Expr && ex.args[1].head == :call)
isfuncexpr(arg) = false

sreplace!(arg, sym, val) = arg
function sreplace!(ex::Expr, sym, val)
    for i = 1:length(ex.args)
        ex.args[i] = sreplace!(ex.args[i], sym, val)
    end
    ex
end
sreplace!(s::Symbol, sym, val) = s == sym ? val : s

# If using the syntax that will need "desplatting",
#     myfunction(A::AbstractArray, I::NTuple{N, Int}...)
# return the variable name (as a string) and type
function get_splatinfo(ex::Expr, itersym::Symbol)
    if ex.head == :call
        a = ex.args[end]
        if  isa(a, Expr) && a.head == :... && length(a.args) == 1
            b = a.args[1]
            if isa(b, Expr) && b.head == :(::)
                varname = string(b.args[1])
                c = b.args[2]
                if isa(c, Expr) && c.head == :curly && c.args[1] == :NTuple && c.args[2] == itersym
                    T = c.args[3]
                    return varname, T
                end
            end
        end
    end
    "", Void
end

# Replace splatted with desplatted for a specific number of arguments
function resolvesplat!(prototype, varname, T::Union(Type,Symbol,Expr), N::Int)
    if !isempty(varname)
        prototype.args[end] = N > 0 ? Expr(:(::), symbol(string(varname, "_1")), T) :
                                      Expr(:(::), symbol(varname), T)
        for i = 2:N
            push!(prototype.args, Expr(:(::), symbol(string(varname, "_", i)), T))
        end
    end
    prototype
end

# Return the generic splatting form, e.g.,
#     myfunction(A::AbstractArray, I::Int...)
function resolvesplat!(prototype, varname, T::Union(Type,Symbol,Expr))
    if !isempty(varname)
        svarname = symbol(varname)
        prototype.args[end] = Expr(:..., :($svarname::$T))
    end
    prototype
end

# Desplatting function calls: replace func(a, b, I...) with func(a, b, I_1, I_2, I_3)
resolvesplats!(arg, varname, N) = arg
function resolvesplats!(ex::Expr, varname, N::Int)
    if ex.head == :call
        for i = 2:length(ex.args)-1
            resolvesplats!(ex.args[i], varname, N)
        end
        a = ex.args[end]
        if isa(a, Expr) && a.head == :... && a.args[1] == symbol(varname)
            ex.args[end] = symbol(string(varname, "_1"))
            for i = 2:N
                push!(ex.args, symbol(string(varname, "_", i)))
            end
        else
            resolvesplats!(a, varname, N)
        end
    else
        for i = 1:length(ex.args)
            resolvesplats!(ex.args[i], varname, N)
        end
    end
    ex
end

# Remove any function parameters that are integers
function spliceint!(ex::Expr)
    if ex.head == :escape
        return esc(spliceint!(ex.args[1]))
    end
    ex.head == :call || error(string(ex, " must be a call"))
    if isa(ex.args[1], Expr) && ex.args[1].head == :curly
        args = ex.args[1].args
        for i = length(args):-1:1
            if isa(args[i], Int)
                deleteat!(args, i)
            end
        end
    end
    ex
end

function popescape(ex::Expr)
    while ex.head == :escape
        ex = ex.args[1]
    end
    ex
end

# Extract the "function name"
function funcsym(prototype::Expr)
    prototype = popescape(prototype)
    prototype.head == :call || error(string(prototype, " must be a call"))
    tmp = prototype.args[1]
    if isa(tmp, Expr) && tmp.head == :curly
        tmp = tmp.args[1]
    end
    return tmp
end

function funcrename(prototype::Expr, name::Symbol)
    prototype = popescape(prototype)
    prototype.head == :call || error(string(prototype, " must be a call"))
    tmp = prototype.args[1]
    if isa(tmp, Expr) && tmp.head == :curly
        tmp.args[1] = name
    else
        prototype.args[1] = name
    end
    return prototype
end

function hasparameter(prototype::Expr, sym::Symbol)
    prototype = popescape(prototype)
    prototype.head == :call || error(string(prototype, " must be a call"))
    tmp = prototype.args[1]
    if isa(tmp, Expr) && tmp.head == :curly
        for i = 2:length(tmp.args)
            if tmp.args[i] == sym
                return true
            end
        end
    end
    false
end

# Extract the symbols of the function arguments
funcarg(s::Symbol) = s
funcarg(ex::Expr) = ex.args[1]
function funcargs(prototype::Expr)
    prototype = popescape(prototype)
    prototype.head == :call || error(string(prototype, " must be a call"))
    map(a->funcarg(a), prototype.args[2:end])
end

### Cartesian-specific macros

# Generate nested loops
macro nloops(N, itersym, rangeexpr, args...)
    _nloops(N, itersym, rangeexpr, args...)
end

_nloops(N::Int, itersym::Symbol, arraysym::Symbol, args::Expr...) = _nloops(N, itersym, :(d->1:size($arraysym,d)), args...)

function _nloops(N::Int, itersym::Symbol, rangeexpr::Expr, args::Expr...)
    if rangeexpr.head != :->
        error("Second argument must be an anonymous function expression to compute the range")
    end
    if !(1 <= length(args) <= 3)
        error("Too many arguments")
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
        error("Second argument must be an anonymous function expression yielding the criterion")
    end
    conds = [Expr(:escape, inlineanonymous(criterion, i)) for i = 1:N]
    Expr(:&&, conds...)
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
        error("Not an anonymous function")
    end
    if !isa(ex.args[1], Symbol)
        error("Not a single-argument anonymous function")
    end
    sym = ex.args[1]
    ex = ex.args[2]
    exout = lreplace(ex, sym, val)
    exout = poplinenum(exout)
    exprresolve(exout)
end

# Given :i and 3, this generates :i_3
inlineanonymous(base::Symbol, ext) = symbol(string(base)*"_"*string(ext))

# Replace a symbol by a value or a "coded" symbol
# E.g., for d = 3,
#    lreplace(:d, :d, 3) -> 3
#    lreplace(:i_d, :d, 3) -> :i_3
#    lreplace(:i_{d-1}, :d, 3) -> :i_2
# This follows LaTeX notation.
lreplace(ex, sym::Symbol, val) = lreplace!(copy(ex), sym, val, Regex("_"*string(sym)*"(\$|(?=_))"))
lreplace!(arg, sym::Symbol, val, r) = arg
function lreplace!(s::Symbol, sym::Symbol, val, r::Regex)
    if (s == sym)
        return val
    end
    symbol(replace(string(s), r, "_"*string(val)))
end
function lreplace!(ex::Expr, sym::Symbol, val, r)
    # Curly-brace notation, which acts like parentheses
    if ex.head == :curly && length(ex.args) == 2 && isa(ex.args[1], Symbol) && endswith(string(ex.args[1]), "_")
        excurly = exprresolve(lreplace!(ex.args[2], sym, val, r))
        if isa(excurly, Number)
            return symbol(string(ex.args[1])*string(excurly))
        else
            ex.args[2] = excurly
            return ex
        end
    end
    for i in 1:length(ex.args)
        ex.args[i] = lreplace!(ex.args[i], sym, val, r)
    end
    ex
end

poplinenum(arg) = arg
function poplinenum(ex::Expr)
    if ex.head == :block
        if length(ex.args) == 1
            return ex.args[1]
        elseif length(ex.args) == 2 && ex.args[1].head == :line
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
