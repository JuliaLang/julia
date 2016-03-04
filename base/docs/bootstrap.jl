# This file is a part of Julia. License is MIT: http://julialang.org/license

macro doc(args...)
    DocBootstrap._expand_(args...)
end

macro __doc__(ex) esc(Expr(:block, Expr(:meta, :doc), ex)) end

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
    expr.head === :-> || throw(ArgumentError("Wrong argument to @doc"))
    str = Core.arrayref(expr.args, 1)
    obj = Core.arrayref(Core.arrayref(expr.args, 2).args, 2)
    __bootexpand(str, obj)
end

setexpand!(__bootexpand)

# The following docstrings are basic sanity checks for `__bootexpand`.

@doc """
Captures and stores docstrings in `DocBootstrap.docs` before the real docsystem is defined.

Note: this expander is later replaced by the more feature-complete `Docs.docm`.
""" ->
__bootexpand(str, obj)

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

Move all docstrings from `DocBootstrap.docs` to their module's metadata dict.
"""
function loaddocs()
    # To keep the ordering of docstrings consistent within the entire docsystem we need to
    # reverse the contents of `docs` (a stack) before evaluating each docstring.
    node  = docs
    stack = []
    while node ≠ nothing
        push!(stack, node.head)
        node = node.tail
    end
    while !isempty(stack)
        mod, str, obj = pop!(stack)
        eval(mod, :(Base.@doc($str, $obj, false)))
    end
    global docs = nothing
end

end
