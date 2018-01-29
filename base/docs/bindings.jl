# This file is a part of Julia. License is MIT: https://julialang.org/license

export @var

struct Binding
    mod::Module
    var::Symbol

    function Binding(m::Module, v::Symbol)
        # Normalise the binding module for module symbols so that:
        #   Binding(Base, :Base) === Binding(Main, :Base)
        m = nameof(m) === v ? parentmodule(m) : m
        new(Base.binding_module(m, v), v)
    end
end

bindingexpr(x) = Expr(:call, Binding, splitexpr(x)...)

defined(b::Binding) = isdefined(b.mod, b.var)
resolve(b::Binding) = getfield(b.mod, b.var)

function splitexpr(x::Expr)
    isexpr(x, :macrocall) ? splitexpr(x.args[1]) :
    isexpr(x, :.)         ? (x.args[1], x.args[2]) :
    error("Invalid @var syntax `$x`.")
end
splitexpr(s::Symbol) = Expr(:macrocall, getfield(Base, Symbol("@__MODULE__")), nothing), quot(s)
splitexpr(other)     = error("Invalid @var syntax `$other`.")

macro var(x)
    esc(bindingexpr(x))
end

function Base.show(io::IO, b::Binding)
    if b.mod === Main
        print(io, b.var)
    else
        print(io, b.mod, '.', Base.isoperator(b.var) ? ":" : "", b.var)
    end
end

aliasof(b::Binding)     = defined(b) ? (a = aliasof(resolve(b), b); defined(a) ? a : b) : b
aliasof(d::DataType, b) = Binding(d.name.module, d.name.name)
aliasof(λ::Function, b) = (m = typeof(λ).name.mt; Binding(m.module, m.name))
aliasof(m::Module,   b) = Binding(m, nameof(m))
aliasof(other,       b) = b
