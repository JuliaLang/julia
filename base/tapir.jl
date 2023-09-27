module Tapir

macro sync_end(token)
    Expr(:sync, esc(token))
end

macro syncregion()
    Expr(:syncregion)
end

const tokenname = gensym(:token)

"""
    Tapir.@sync block
"""
macro sync(block)
    var = esc(tokenname)
    quote
        let $var = @syncregion()
            v = $(esc(block))
            @sync_end($var)
            v
        end
    end
end

"""
    Tapir.@spawn expression
"""
macro spawn(expr)
    Expr(:spawn, esc(tokenname), esc(Expr(:block, expr)))
end

loop_spawning_strategy() = (Symbol("tapir.loop.spawning.strategy"), 1)
loop_grainsize(n) = (Symbol("tapir.loop.grainsize"), convert(Int, n))
simd() = Symbol("julia.simdloop")
ivdep() = Symbol("julia.ivdep")

@eval function foreach(f::F, iterator) where F
    @sync for I in iterator
        @spawn @inline f(I)
        $(Expr(:loopinfo, simd(), ivdep(), loop_spawning_strategy()))
    end
    return nothing
end

end
