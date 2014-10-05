module Docs

import Base.Markdown: @doc_str, @doc_mstr

export doc, @doc

const META = Dict()

function doc(obj, meta)
  META[obj] = meta
end

doc(obj) = get(META, obj, nothing)

function mdify(ex)
  if isa(ex, String)
    :(@doc_str $(esc(ex)))
  elseif isa(ex, Expr) && ex.head == :macrocall && ex.args[1] == symbol("@mstr")
    :(@doc_mstr $(esc(ex.args[2])))
  else
    esc(ex)
  end
end

macro doc (ex)
  (isa(ex, Expr) && ex.head == :(->)) || error("Please use @doc meta -> func")
  meta, def = ex.args
  quote
    f = $(esc(def))
    doc(f, $(mdify(meta)))
    f
  end
end

end
