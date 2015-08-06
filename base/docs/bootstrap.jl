# This file is a part of Julia. License is MIT: http://julialang.org/license

module DocBootstrap

export @doc

const docs = []

_expand_ = nothing

setexpand!(f) = global _expand_ = f

macro doc(args...)
    _expand_(args...)
end

setexpand!() do str, obj
    push!(docs, (current_module(), str, obj))
    return esc(obj)
end

function loaddocs()
    for (mod, str, obj) in docs
        eval(mod, :(Base.@doc($str, $obj, false)))
    end
end

end
