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

function unblock (ex)
  isexpr(ex, :block) || return ex
  exs = filter(ex->!isexpr(ex, :line), ex.args)
  length(exs) > 1 && return ex
  return exs[1]
end

function mdify(ex)
  if isa(ex, String)
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

function macrodoc(meta, def)
  name = esc(symbol(string("@", unblock(def).args[1].args[1])))
  quote
    $(esc(def))
    doc($name, $(mdify(meta)))
    nothing
  end
end

function objdoc(meta, def)
  quote
    f = $(esc(def))
    doc(f, $(mdify(meta)))
    f
  end
end

macro doc (ex)
  isexpr(ex, :(->)) || return getdoc(ex)
  meta, def = ex.args
  isexpr(unblock(def), :macro) && return macrodoc(meta, def)
  return objdoc(meta, def)
end

end
