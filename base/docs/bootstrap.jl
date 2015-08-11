# This file is a part of Julia. License is MIT: http://julialang.org/license

module DocBootstrap

export @doc

type List
    head
    tail
end

docs = nothing

_expand_ = nothing

setexpand!(f) = global _expand_ = f

macro doc(args...)
    _expand_(args...)
end

setexpand!() do str, obj
    global docs = List((ccall(:jl_get_current_module, Any, ()), str, obj), docs)
    return esc(obj)
end

"""
    DocBootstrap :: Module

Basic docstring bootstrapping module that accumulates docstrings prior to the real docsystem
being defined in `base/docs/Docs.jl`. Once the proper docsystem is loaded all docstrings
that were stored in `DocBootstrap.docs` are migrated to their correct modules using
`DocBootstrap.loaddocs()`.
"""
DocBootstrap

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
