module DocBootstrap

export @doc

const docs = []

_expand_ = nothing

setexpand!(f) = global _expand_ = f

macro doc (args...)
  _expand_(args...)
end

setexpand!((args...) -> push!(docs, args))

end
