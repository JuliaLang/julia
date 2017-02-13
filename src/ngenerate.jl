# Uncomment the depwarns when we drop 0.4 support

module CompatCartesian

export @ngenerate, @nsplat

macro ngenerate(itersym, returntypeexpr, funcexpr)
    if isa(funcexpr, Expr) && funcexpr.head == :macrocall && funcexpr.args[1] == Symbol("@inline")
        funcexpr = Base._inline(funcexpr.args[2])
    end
    isfuncexpr(funcexpr) || error("Requires a function expression")
    esc(Expr(:block, # :(Base.depwarn("@ngenerate is deprecated, used @generated function or (preferably) tuples/CartesianIndex instead", Symbol("@ngenerate"))),
             _ngenerate(itersym, funcexpr)))
end

function _ngenerate(itersym::Symbol, funcexpr::Expr)
    prototype = funcexpr.args[1]
    body = funcexpr.args[2]
    varname, T = get_splatinfo(prototype, itersym)
    ex = Expr(:$, itersym)
    sreplace!(body, itersym, ex)
    if !isempty(varname)
        prototype, body = _nsplat(prototype, body, varname, T, itersym)
    else
        body = Expr(:quote, body)
    end
    Expr(:stagedfunction, prototype, body)
end

macro nsplat(itersym, args...)
    if length(args) == 1
        funcexpr = args[1]
    elseif length(args) == 2
        funcexpr = args[2]
    else
        error("Wrong number of arguments")
    end
    if isa(funcexpr, Expr) && funcexpr.head == :macrocall && funcexpr.args[1] == Symbol("@inline")
        funcexpr = Base._inline(funcexpr.args[2])
    end
    isfuncexpr(funcexpr) || error("Second argument must be a function expression")
    prototype = funcexpr.args[1]
    body = funcexpr.args[2]
    varname, T = get_splatinfo(prototype, itersym)
    isempty(varname) && error("Last argument must be a splat")
    prototype, body = _nsplat(prototype, body, varname, T, itersym)
        esc(Expr(:block, # :(Base.depwarn("@nsplat is deprecated, using inlining instead", Symbol("@nsplat"))),
                 Expr(:stagedfunction, prototype, body)))
end

function _nsplat(prototype, body, varname, T, itersym)
    varsym = Symbol(varname)
    prototype.args[end] = Expr(:..., Expr(:(::), varsym, T)) # :($varsym::$T...)
    varquot = Expr(:quote, varsym)
    bodyquot = Expr(:quote, body)
    newbody = quote
        $itersym = length($varsym)
        quote
            Base.Cartesian.@nexprs $($itersym) (d->$($(Expr(:quote, Symbol(varsym, "_d")))) = $($varquot)[d])
            $(Compat.CompatCartesian.resolvesplats!($bodyquot, $varquot, $itersym))
        end
    end
    prototype, newbody
end

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

resolvesplats!(arg, varname, N) = arg
function resolvesplats!(ex::Expr, varname, N::Int)
    if ex.head == :call
        for i = 2:length(ex.args)-1
            resolvesplats!(ex.args[i], varname, N)
        end
        a = ex.args[end]
        if isa(a, Expr) && a.head == :... && a.args[1] == Symbol(varname)
            ex.args[end] = :($varname[1]) # Expr(:ref, varname, 1)
            for i = 2:N
                push!(ex.args, :($varname[$i])) # Expr(:ref, varname, i))
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

end
