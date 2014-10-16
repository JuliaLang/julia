module Compat
using Base.Meta

if VERSION < v"0.4.0-dev+980"
    macro Dict(pairs...)
        esc(Expr(:dict, pairs...))
    end
    macro AnyDict(pairs...)
        esc(Expr(:typed_dict, :(Any=>Any), pairs...))
    end
else
    macro Dict(pairs...)
        esc(Expr(:call, :Dict, pairs...))
    end
    macro AnyDict(pairs...)
        esc(Expr(:call, :(Base.AnyDict), pairs...))
    end
end

function rewrite_dict(ex)
    length(ex.args) == 1 && return ex

    f = ex.args[1]
    if isexpr(f, :curly)
        newex = Expr(:typed_dict, :($(f.args[2])=>$(f.args[3])))
    else
        newex = Expr(:dict)
    end

    for i = 2:length(ex.args)
        pair = ex.args[i]
        !isexpr(pair, :(=>)) && return ex
        push!(newex.args, pair)
    end
    newex
end

function _compat!(ex::Expr)
    if ex.head == :call
        f = ex.args[1]
        if VERSION < v"0.4.0-dev+980" && (f == :Dict || (isexpr(f, :curly) && length(f.args) == 3 && f.args[1] == :Dict))
            ex = rewrite_dict(ex)
        end
    end
    for i = 1:length(ex.args)
        ex.args[i] = _compat!(ex.args[i])
    end
    ex
end
_compat!(ex) = ex

macro compat(ex)
    esc(_compat!(ex))
end

export @compat

end # module
