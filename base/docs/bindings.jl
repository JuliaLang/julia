# This file is a part of Julia. License is MIT: https://julialang.org/license

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

defined(b::Binding) = invokelatest(isdefinedglobal, b.mod, b.var)
resolve(b::Binding) = invokelatest(getglobal, b.mod, b.var)

function splitexpr(x::Expr)
    isexpr(x, :.) ? (x.args[1], x.args[2]) : error("Could not find something to document in `$x`.")
end
splitexpr(s::Symbol) = :($Base.@__MODULE__), quot(s) # this somewhat complex form allows deferring resolving the Module for module docstring until after the module is created
splitexpr(r::GlobalRef) = r.mod, quot(r.name)
splitexpr(other)     = error("Could not find something to document in `$other`.")

function Base.show(io::IO, b::Binding)
    if b.mod === Base.active_module()
        print(io, b.var)
    else
        print(io, b.mod, '.', Base.isoperator(b.var) ? ":" : "", b.var)
    end
end

aliasof(b::Binding)     = defined(b) ? (a = aliasof(resolve(b), b); defined(a) ? a : b) : b
aliasof(d::DataType, b) = Binding(d.name.module, d.name.name)
aliasof(λ::Function, b) = (m = typeof(λ).name; Binding(m.module, m.singletonname))
aliasof(m::Module,   b) = Binding(m, nameof(m))
aliasof(other,       b) = b
