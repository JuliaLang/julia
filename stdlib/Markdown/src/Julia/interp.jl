# This file is a part of Julia. License is MIT: https://julialang.org/license

function _parse(stream::IO; greedy::Bool = true, raise::Bool = true)
    pos = position(stream)
    ex, Δ = Meta.parse(read(stream, String), 1, greedy = greedy, raise = raise)
    seek(stream, pos + Δ - 1)
    return ex
end

function interpinner(stream::IO, greedy = false)
    startswith(stream, '$') || return
    (eof(stream) || peek(stream, Char) in whitespace) && return
    try
        return _parse(stream, greedy = greedy)
    catch e
        if isa(e, Meta.ParseError)
            return nothing
        else
            rethrow()
        end
    end
end

@trigger '$' ->
function interp(stream::IO, md::MD)
    withstream(stream) do
        ex = interpinner(stream)
        return ex
    end
end

function blockinterp(stream::IO, md::MD)
    withstream(stream) do
        ex = interpinner(stream)
        if ex ≡ nothing
            return false
        else
            push!(md, ex)
            return true
        end
    end
end

toexpr(x) = x

toexpr(xs::Union{Vector{Any},Vector{Vector{Any}}}) =
    Expr(:call, GlobalRef(Base,:getindex), Any, map(toexpr, xs)...)

for T in Any[MD, Paragraph, Header, Link, Bold, Italic]
    @eval function toexpr(md::$T)
        Expr(:call, typeof(md), $(map(x->:(toexpr(md.$x)), fieldnames(Base.unwrap_unionall(T)))...))
    end
end

function toexpr(md::Table)
    Expr(:call, Table, toexpr(md.rows), md.align)
end

function toexpr(md::List)
    Expr(:call, List, toexpr(md.items), md.ordered, md.loose)
end
