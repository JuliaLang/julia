# This file is a part of Julia. License is MIT: https://julialang.org/license

module CoreDocs

import ..esc, ..push!, ..getindex, ..unsafe_load, ..Csize_t, ..@nospecialize

@nospecialize # don't specialize on any arguments of the methods declared herein

function doc!(source::LineNumberNode, mod::Module, str, ex)
    push!(DOCS, Core.svec(mod, ex, str, source.file, source.line))
    nothing
end
const DOCS = Array{Core.SimpleVector,1}()

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
    return esc(out)
end
docm(source::LineNumberNode, mod::Module, x) =
    isexpr(x, :->) ? docm(source, mod, x.args[1], x.args[2].args[2]) : error("invalid '@doc'.")

end
