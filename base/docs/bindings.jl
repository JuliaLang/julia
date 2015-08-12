# This file is a part of Julia. License is MIT: http://julialang.org/license

export @var

moduleusings(mod) = ccall(:jl_module_usings, Any, (Any,), mod)

function findsource(mod::Module, var::Symbol, seen = Set{Module}())
    mod in seen && return
    var in names(mod, true) && return mod
    push!(seen, mod)
    sources = filter(m -> m ≠ nothing && m ∉ seen,
                     map(n -> findsource(n, var, seen),
                         moduleusings(mod)))
    isempty(sources) ? nothing : collect(sources)[1]
end

immutable Binding
    mod::Module
    var::Symbol

    function Binding(mod::Module, var::Symbol)
        mod′ = findsource(mod, var)
        mod′ == nothing && error("$mod.$var not found.")
        new(mod′, var)
    end
end

function splitexpr(x::Expr)
    isexpr(x, :macrocall) && return splitexpr(x.args[1])
    isexpr(x, :.)         && return (x.args[1], x.args[2].args[1])
    error("Invalid @var syntax `$x`.")
end
splitexpr(s::Symbol) = (module_name(current_module()), s)
splitexpr(other)     = error("Invalid @var syntax `$other`.")

isvar(x) = isexpr(x, :macrocall, :.)
isvar(::Symbol) = true

macro var(x)
    mod, var = splitexpr(x)
    :(Binding($(esc(mod)), $(quot(var))))
end

Base.show(io::IO, x::Binding) = print(io, "$(x.mod).$(x.var)")
