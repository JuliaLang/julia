# This file is a part of Julia. License is MIT: https://julialang.org/license

module CoreDocs

import ..esc, ..push!, ..getindex, ..unsafe_load, ..Csize_t

function doc!(source::LineNumberNode, mod::Module, str, ex)
    push!(DOCS, (mod, ex, str, source.file, source.line))
end
const DOCS = Array{Any, 1}()

isexpr(x, h) = isa(x, Expr) && x.head === h

lazy_iterpolate(s::AbstractString) = Expr(:call, Core.svec, s)
lazy_iterpolate(x) = isexpr(x, :string) ? Expr(:call, Core.svec, x.args...) : x

function docm(source::LineNumberNode, mod::Module, str, x)
    out = esc(Expr(:call, doc!, QuoteNode(source), mod, lazy_iterpolate(str), Expr(:quote, x)))
    isexpr(x, :module) ? Expr(:toplevel, out, esc(x)) :
    isexpr(x, :call) ? out : Expr(:block, esc(x), out)
end
docm(source::LineNumberNode, mod::Module, x) =
    isexpr(x, :->) ? docm(source, mod, x.args[1], x.args[2].args[2]) : error("invalid '@doc'.")

end
