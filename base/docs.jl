module Docs

import Base.Markdown: @doc_str, @doc_mstr

export doc, @doc

# Basic API

const META = Dict()

function doc(obj, meta)
  META[obj] = meta
end

doc(obj) = get(META, obj, nothing)

doc(obj::Union(Symbol, String)) = get(META, current_module().(symbol(obj)), nothing)

# Macros

isexpr(x::Expr, ts...) = x.head in ts
isexpr{T}(x::T, ts...) = T in ts

function mdify(ex)
  if isexpr(ex, String)
    :(@doc_str $(esc(ex)))
  elseif isexpr(ex, :macrocall) && ex.args[1] == symbol("@mstr")
    :(@doc_mstr $(esc(ex.args[2])))
  else
    esc(ex)
  end
end

function getdoc(ex)
  if isexpr(ex, :macrocall)
    :(doc($(esc(ex.args[1]))))
  else
    :(doc($(esc(ex))))
  end
end

macro doc (ex)
  isexpr(ex, :(->)) || return getdoc(ex)
  meta, def = ex.args
  quote
    f = $(esc(def))
    doc(f, $(mdify(meta)))
    f
  end
end

end
