# This file is a part of Julia. License is MIT: https://julialang.org/license

module CoreDocs

import ..esc, ..push!, ..getindex, ..current_module, ..unsafe_load, ..Csize_t

function doc!(source::LineNumberNode, str, ex)
    push!(DOCS, (current_module(), ex, str, source.file, source.line))
end
const DOCS = Array{Any, 1}()

isexpr(x, h) = isa(x, Expr) && x.head === h

lazy_iterpolate(s::AbstractString) = Expr(:call, Core.svec, s)
lazy_iterpolate(x) = isexpr(x, :string) ? Expr(:call, Core.svec, x.args...) : x

function docm(source::LineNumberNode, str, x)
    out = esc(Expr(:call, doc!, QuoteNode(source), lazy_iterpolate(str), Expr(:quote, x)))
    isexpr(x, :module) ? Expr(:toplevel, out, esc(x)) :
    isexpr(x, :call) ? out : Expr(:block, esc(x), out)
end
docm(source::LineNumberNode, x) =
    isexpr(x, :->) ? docm(source, x.args[1], x.args[2].args[2]) : error("invalid '@doc'.")

end
