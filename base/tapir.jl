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

end
