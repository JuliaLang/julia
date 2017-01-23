# This file is a part of Julia. License is MIT: http://julialang.org/license

function Base.parse(stream::IO; greedy::Bool = true, raise::Bool = true)
    pos = position(stream)
    ex, Δ = Base.parse(readstring(stream), 1, greedy = greedy, raise = raise)
    seek(stream, pos + Δ - 1)
    return ex
end

function interpinner(stream::IO, greedy = false)
    startswith(stream, '$') || return
    (eof(stream) || Char(peek(stream)) in whitespace) && return
    try
        return Base.parse(stream::IOBuffer, greedy = greedy)
    catch e
        return
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

toexpr(xs::Vector{Any}) = Expr(:call, GlobalRef(Base,:vector_any), map(toexpr, xs)...)

for T in Any[MD, Paragraph, Header, Link, Bold, Italic]
    @eval function toexpr(md::$T)
        Expr(:call, typeof(md), $(map(x->:(toexpr(md.$x)), fieldnames(Base.unwrap_unionall(T)))...))
    end
end
