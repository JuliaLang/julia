# This file is a part of Julia. License is MIT: http://julialang.org/license

module CoreDocs

import ..esc, ..push!, ..getindex, ..current_module, ..unsafe_load, ..Csize_t

function doc!(str, ex)
    ptr  = unsafe_load(Core.Intrinsics.cglobal(:jl_filename, Ptr{UInt8}))
    len  = ccall(:strlen, Csize_t, (Ptr{UInt8},), ptr)
    file = ccall(:jl_symbol_n, Any, (Ptr{UInt8}, Csize_t), ptr, len)
    line = unsafe_load(Core.Intrinsics.cglobal(:jl_lineno, Int32)) # Cint
    push!(DOCS, (current_module(), ex, str, file, line))
end
const DOCS = Array{Any, 1}()

isexpr(x, h) = isa(x, Expr) && x.head === h

lazy_iterpolate(s::AbstractString) = Expr(:call, Core.svec, s)
lazy_iterpolate(x) = isexpr(x, :string) ? Expr(:call, Core.svec, x.args...) : x

function docm(str, x)
    out = esc(Expr(:call, doc!, lazy_iterpolate(str), Expr(:quote, x)))
    isexpr(x, :module) ? Expr(:toplevel, out, esc(x)) :
    isexpr(x, :call) ? out : Expr(:block, esc(x), out)
end
docm(x) = isexpr(x, :->) ? docm(x.args[1], x.args[2].args[2]) : error("invalid '@doc'.")

end
