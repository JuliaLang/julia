function depwarn_ex(msg, name)
    return quote
        if VERSION >= v"0.6.0"
            Base.depwarn($msg, Symbol($name))
        end
    end
end

macro Dict(pairs...)
    esc(Expr(:block, depwarn_ex("@Dict is deprecated, use Dict instead", "@Dict"),
                       Expr(:call, :Dict, pairs...)))
end

macro AnyDict(pairs...)
    esc(Expr(:block, depwarn_ex("@AnyDict is deprecated, use Dict{Any,Any} instead", "@AnyDict"),
             Expr(:call, :(Base.AnyDict), pairs...)))
end

module CompatCartesian

import ..Compat: depwarn_ex

export @ngenerate, @nsplat

macro ngenerate(itersym, returntypeexpr, funcexpr)
    if isa(funcexpr, Expr) && funcexpr.head == :macrocall && funcexpr.args[1] == Symbol("@inline")
        funcexpr = Base._inline(funcexpr.args[2])
    end
    isfuncexpr(funcexpr) || error("Requires a function expression")
    esc(Expr(:block, depwarn_ex("@ngenerate is deprecated, used @generated function or (preferably) tuples/CartesianIndex instead", "@ngenerate"),
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
        esc(Expr(:block, depwarn_ex("@nsplat is deprecated, using inlining instead", "@nsplat"),
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

using .CompatCartesian
export @ngenerate, @nsplat

function primarytype(@nospecialize(t))
    tn = t.name
    if isdefined(tn, :primary)
        return tn.primary
    else
        return tn.wrapper
    end
end

export @functorize
macro functorize(f)
    code = f === :scalarmax          ? :(Base.scalarmax) :
           f === :scalarmin          ? :(Base.scalarmin) :
           f === :centralizedabs2fun ? :(primarytype(typeof(Base.centralizedabs2fun(0)))) :
           f
    warning = depwarn_ex("@functorize is deprecated as functor objects are no longer supported in julia", "@functorize")
    return quote
        $warning
        $code
    end
end

@static if VERSION >= v"0.6.0"
    Base.@deprecate_binding KERNEL Sys.KERNEL
    Base.@deprecate_binding UTF8String Core.String
    Base.@deprecate_binding ASCIIString Core.String
    Base.@deprecate_binding unsafe_convert Base.unsafe_convert
    Base.@deprecate_binding remote_do Base.remote_do
    Base.@deprecate_binding Filesystem Base.Filesystem
    Base.@deprecate_binding AsyncCondition Base.AsyncCondition
    Base.@deprecate_binding promote_eltype_op Base.promote_eltype_op
    @eval Base.@deprecate_binding $(Symbol("@irrational")) Base.$(Symbol("@irrational"))
    @eval Base.@deprecate_binding $(Symbol("@blasfunc")) Base.LinAlg.BLAS.$(Symbol("@blasfunc"))
else
    const KERNEL = Sys.KERNEL
    const UTF8String = Core.String
    const ASCIIString = Core.String
    import Base.unsafe_convert
    import Base.remote_do
    import Base.Filesystem
    import Base.AsyncCondition
    import Base.promote_eltype_op
    import Base.@irrational
    import Base.LinAlg.BLAS.@blasfunc
end

if VERSION < v"0.7.0-DEV.2915"
    Base.@deprecate textwidth Compat.Unicode.textwidth
end
