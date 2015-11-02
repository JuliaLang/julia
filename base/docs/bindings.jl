# This file is a part of Julia. License is MIT: http://julialang.org/license

immutable Binding
    mod::Module
    var::Symbol
    Binding(m::Module, v::Symbol) = new(Base.which_module(m, v), v)
end

function splitexpr(x::Expr)
    isexpr(x, :macrocall) ? splitexpr(x.args[1]) :
    isexpr(x, :.)         ? (esc(x.args[1]), x.args[2]) :
    error("Invalid @var syntax `$x`.")
end
splitexpr(s::Symbol) = :(current_module()), quot(s)
splitexpr(other)     = error("Invalid @var syntax `$other`.")

isvar(x) = isexpr(x, [:macrocall, :.])
isvar(::Symbol) = true

Base.show(io::IO, x::Binding) = print(io, x.mod, ".", x.var)
