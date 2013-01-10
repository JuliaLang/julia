macro memoize(ex)
    local f, u
    if isa(ex,Expr) && (ex.head == :function || ex.head == symbol("=")) &&
       !isempty(ex.args) && ex.args[1].head == :call && !isempty(ex.args[1].args)
        f = ex.args[1].args[1]
        ex.args[1].args[1] = u = symbol(string(f,"_unmemoized"))
    else
        error("@memoize must be applied to a method definition")
    end
    f_cache = esc(symbol(string(f,"_cache")))
    quote
        $(esc(ex))
        const ($f_cache) = (Tuple=>Any)[]
        $(esc(f))(args...) = has(($f_cache),args) ?
            ($f_cache)[args] : (($f_cache)[args] = $(esc(u))(args...))
    end
end
