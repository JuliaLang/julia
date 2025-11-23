# This file is a part of Julia. License is MIT: https://julialang.org/license

module CoreDocs

import Core: @nospecialize, SimpleVector

struct DocLinkedList
    doc::SimpleVector
    next::DocLinkedList
    DocLinkedList() = new()
    DocLinkedList(doc::SimpleVector, next::DocLinkedList) = new(doc, next)
end

global DOCS = DocLinkedList()
function doc!(source::LineNumberNode, mod::Module, str, ex)
    global DOCS
    DOCS = DocLinkedList(Core.svec(mod, ex, str, source.file, source.line), DOCS)
    nothing
end

isexpr(x, h::Symbol) = isa(x, Expr) && x.head === h

lazy_iterpolate(s::AbstractString) = Expr(:call, Core.svec, s)
lazy_iterpolate(x) = isexpr(x, :string) ? Expr(:call, Core.svec, x.args...) : x

function docm(source::LineNumberNode, mod::Module, str, x)
    out = Expr(:call, doc!, QuoteNode(source), mod, lazy_iterpolate(str), QuoteNode(x))
    if isexpr(x, :module)
        out = Expr(:toplevel, out, x)
    elseif isexpr(x, :call)
    else
        out = Expr(:block, x, out)
    end
    return Expr(:escape, out)
end
docm(source::LineNumberNode, mod::Module, x) =
    (isa(x, Expr) && x.head === :->) ? docm(source, mod, x.args[1], x.args[2].args[2]) : error("invalid '@doc'.")

end
