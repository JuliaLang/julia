# This file is a part of Julia. License is MIT: http://julialang.org/license

macro doc(args...)
    DocBootstrap._expand_(args...)
end

macro __doc__(ex) esc(Expr(:block, symbol("#doc#"), ex)) end

type DocObj
    content
    parser::Symbol
    path::Symbol
    function DocObj(doc, parser, path)
        if path === nothing
            path = Symbol("")
        end
        if isa(doc, DocObj)
            new(doc.content, doc.parser, path)
        else
            new(doc, parser, path)
        end
    end
end

macro doc_str(content, parser...)
    if isempty(parser)
        parser = :Markdown
    elseif length(parser) == 1
        parser = parser[1]
    else
        throw(ArgumentError("Wrong number of arguments to @doc_str"))
    end
    return :(DocObj($(QuoteNode(content)), $(QuoteNode(parser)), DocBootstrap.@__FILE__))
end

module DocBootstrap

import Base: DocObj

current_module() = ccall(:jl_get_current_module, Any, ())

function __FILE__()
    fn = Intrinsics.pointerref(cglobal(:jl_filename, Ptr{UInt8}), 1)
    return ccall(:jl_symbol, Any, (Ptr{UInt8},), fn)::Symbol
end
macro __FILE__()
    return QuoteNode(__FILE__())
end

type List
    head
    tail
end

docs = nothing

function doc!(ex, str, file)
    cm = current_module()
    if (isa(ex, Expr) && ex.head === :call)
        obj = nothing
    else
        # eval for the intended side-effect of `ex`
        obj = eval(cm, ex)
    end
    if isa(str, AbstractString)
        parser = :Markdown
    else
        str = eval(cm, str)
        parser = :string
    end
    doc = (obj, cm, ex, DocObj(str, parser, file))
    global docs = List(doc, docs)
end

function __bootexpand(expr::Expr)
    if expr.head !== :->
        throw(ArgumentError("Wrong argument to @doc"))
    end
    return __bootexpand(expr.args...)
end

function __bootexpand(str, obj)
    :(DocBootstrap.doc!($(QuoteNode(obj)), $(QuoteNode(str)), DocBootstrap.@__FILE__))
end

_expand_ = __bootexpand
setexpand!(f) = global _expand_ = f

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
    global docs = nothing
    while node ≠ nothing
        obj, mod, ex, data = node.head
        eval(mod, :(Base.@doc($data, $ex, false)))
        node = node.tail
    end
end

end
