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
  # str, obj = ex.args[1], ex.args[2]
  push!(docs, (current_module(), str, obj))
  return esc(obj)
end

end
