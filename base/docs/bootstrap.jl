# This file is a part of Julia. License is MIT: http://julialang.org/license

macro doc(args...)
    DocBootstrap._expand_(args...)
end

macro __doc__(ex) esc(Expr(:block, symbol("#doc#"), ex)) end

module DocBootstrap

type List
    head
    tail
end

docs = nothing

_expand_ = nothing

setexpand!(f) = global _expand_ = f

function __bootexpand(str, obj)
    global docs = List((ccall(:jl_get_current_module, Any, ()), str, obj), docs)
    (isa(obj, Expr) && obj.head === :call) && return nothing
    (isa(obj, Expr) && obj.head === :module) && return esc(Expr(:toplevel, obj))
    esc(obj)
end

function __bootexpand(expr::Expr)
    if expr.head !== :->
        throw(ArgumentError("Wrong argument to @doc"))
    end
    __bootexpand(expr.args...)
end

setexpand!(__bootexpand)

"""
    DocBootstrap :: Module

Basic docstring bootstrapping module that accumulates docstrings prior to the real docsystem
being defined in `base/docs/Docs.jl`. Once the proper docsystem is loaded all docstrings
that were stored in `DocBootstrap.docs` are migrated to their correct modules using
`DocBootstrap.loaddocs()`.
"""
DocBootstrap

"""
    loaddocs()

Move all docstrings from `DocBootstrap.docs` to their module's `__META__` dict.
"""
function loaddocs()
    node = docs
    while node ≠ nothing
        mod, str, obj = node.head
        eval(mod, :(Base.@doc($str, $obj, false)))
        node = node.tail
    end
    global docs = nothing
end

end
