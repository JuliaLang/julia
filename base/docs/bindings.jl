# This file is a part of Julia. License is MIT: http://julialang.org/license

export @var

immutable Binding
    mod::Module
    var::Symbol

    function Binding(m::Module, v::Symbol)
        # Normalise the binding module for module symbols so that:
        #   Binding(Base, :Base) === Binding(Main, :Base)
        m = module_name(m) === v ? module_parent(m) : m
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
splitexpr(s::Symbol) = Expr(:call, current_module), quot(s)
splitexpr(other)     = error("Invalid @var syntax `$other`.")

macro var(x)
    esc(bindingexpr(x))
end

Base.show(io::IO, b::Binding) = b.mod === Main ? print(io, b.var) : print(io, b.mod, '.', b.var)

aliasof(b::Binding)     = defined(b) ? (a = aliasof(resolve(b), b); defined(a) ? a : b) : b
aliasof(d::DataType, b) = Binding(d.name.module, d.name.name)
aliasof(λ::Function, b) = (m = typeof(λ).name.mt; Binding(m.module, m.name))
aliasof(m::Module,   b) = Binding(m, module_name(m))
aliasof(other,       b) = b
