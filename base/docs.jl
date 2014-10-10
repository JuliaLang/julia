module Docs

import Base.Markdown: @doc_str, @doc_mstr

export doc, @doc

# Basic API / Storage

const modules = Module[]

meta() = current_module().META

macro init ()
  META = esc(:META)
  quote
    if !isdefined(:META)
      const $META = ObjectIdDict()
      push!(modules, current_module())
      nothing
    end
  end
end

function doc(obj, data)
  meta()[obj] = data
end

function doc(obj)
  for mod in modules
    haskey(mod.META, obj) && return mod.META[obj]
  end
end

function doc(obj::Union(Symbol, String))
  doc(current_module().(symbol(obj)))
end

# Function / Method support

function newmethod(defs)
  keylen = -1
  key = nothing
  for def in defs
    length(def.sig) > keylen && (keylen = length(def.sig); key = def)
  end
  return key
end

function newmethod(funcs, f)
  applicable = Method[]
  for def in methods(f)
    (!haskey(funcs, def) || funcs[def] != def.func) && push!(applicable, def)
  end
  return newmethod(applicable)
end

function trackmethod(def)
  name = unblock(def).args[1].args[1]
  f = esc(name)
  quote
    if isdefined($(Expr(:quote, name))) && isgeneric($f)
      funcs = [def => def.func for def in methods($f)]
      $(esc(def))
      $f, newmethod(funcs, $f)
    else
      $(esc(def))
      $f, newmethod(methods($f))
    end
  end
end

type FuncDoc
  order::Vector{Method}
  meta::Dict{Method, Any}
  source::Dict{Method, Any}
end

FuncDoc() = FuncDoc(Method[], Dict(), Dict())

getset(coll, key, default) = coll[key] = get(coll, key, default)

function doc(f::Function, m::Method, data, source)
  fd = getset(meta(), f, FuncDoc())
  isa(fd, FuncDoc) || error("Can't document a method when the function already has metadata")
  !haskey(fd.meta, m) && push!(fd.order, m)
  fd.meta[m] = data
  fd.source[m] = source
end

function doc(f::Function)
  docs = {}
  for mod in modules
    if haskey(mod.META, f)
      fd = mod.META[f]
      if isa(fd, FuncDoc)
        for m in fd.order
          push!(docs, fd.meta[m])
        end
      elseif length(docs) == 0
        return fd
      end
    end
  end
  return catdoc(docs...)
end

catdoc() = nothing
catdoc(xs...) = [xs...]

# Macros

isexpr(x::Expr, ts...) = x.head in ts
isexpr{T}(x::T, ts...) = T in ts

function unblock(ex)
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
    @init
    $(esc(def))
    doc($name, $(mdify(meta)))
    nothing
  end
end

function funcdoc(meta, def)
  quote
    @init
    f, m = $(trackmethod(def))
    doc(f, m, $(mdify(meta)), $(esc(Expr(:quote, def))))
    f
  end
end

function objdoc(meta, def)
  quote
    @init
    f = $(esc(def))
    doc(f, $(mdify(meta)))
    f
  end
end

fexpr(ex) = isexpr(ex, :function) || (isexpr(ex, :(=)) && isexpr(ex.args[1], :call))

macro doc (ex)
  isexpr(ex, :->) || return getdoc(ex)
  meta, def = ex.args
  isexpr(unblock(def), :macro) && return macrodoc(meta, def)
  fexpr(unblock(def)) && return funcdoc(meta, def)
  return objdoc(meta, def)
end

# Text / HTML objects

import Base: print, writemime

export HTML, @html_str, @html_mstr

export HTML, Text

type HTML{T}
  content::T
end

function HTML(xs...)
  HTML() do io
    for x in xs
      writemime(io, MIME"text/html"(), x)
    end
  end
end

writemime(io::IO, ::MIME"text/html", h::HTML) = print(io, h.content)
writemime(io::IO, ::MIME"text/html", h::HTML{Function}) = h.content(io)

macro html_str (s)
  :(HTML($s))
end

macro html_mstr (s)
  :(HTML($(Base.triplequoted(s))))
end

function catdoc(xs::HTML...)
  HTML() do io
    for x in xs
      writemime(io, MIME"text/html"(), x)
    end
  end
end

export Text, @text_str, @text_mstr

type Text{T}
  content::T
end

print(io::IO, t::Text) = print(io, t.content)
print(io::IO, t::Text{Function}) = t.content(io)
writemime(io::IO, ::MIME"text/plain", t::Text) = print(io, t)

macro text_str (s)
  :(Text($s))
end

macro text_mstr (s)
  :(Text($(Base.triplequoted(s))))
end

function catdoc(xs::Text...)
  Text() do io
    for x in xs
      writemime(io, MIME"text/plain"(), x)
    end
  end
end

end
