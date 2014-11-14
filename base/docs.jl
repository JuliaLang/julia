module Docs

import Base.Markdown: @doc_str, @doc_mstr, MD

export doc, @doc

# Basic API / Storage

const modules = Module[]

meta() = current_module().META

macro init ()
  META = esc(:META)
  quote
    if !isdefined(:META)
      const $META = ObjectIdDict()
      doc($META, doc"Documentation metadata for $(string(current_module())).")
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

function doc(obj::Union(Symbol, AbstractString))
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
  name = uncurly(unblock(def).args[1].args[1])
  f = esc(name)
  quote
    if $(isexpr(name, Symbol)) && isdefined($(Expr(:quote, name))) && isgeneric($f)
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
  docs = []
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

function doc(f::Function, m::Method)
  for mod in modules
    haskey(mod.META, f) && isa(mod.META[f], FuncDoc) && haskey(mod.META[f].meta, m) &&
      return mod.META[f].meta[m]
  end
end

catdoc() = nothing
catdoc(xs...) = [xs...]

# Macros

isexpr(x::Expr, ts...) = x.head in ts
isexpr{T}(x::T, ts...) = T in ts

function unblock(ex)
  isexpr(ex, :block) || return ex
  exs = filter(ex->!isexpr(ex, :line), ex.args)
  length(exs) == 1 || return ex
  return exs[1]
end

uncurly(ex) = isexpr(ex, :curly) ? ex.args[1] : ex

namify(ex::Expr) = namify(ex.args[1])
namify(sy::Symbol) = sy

function mdify(ex)
  if isa(ex, AbstractString)
    :(@doc_str $(esc(ex)))
  elseif isexpr(ex, :macrocall) && namify(ex) == symbol("@mstr")
    :(@doc_mstr $(esc(ex.args[2])))
  else
    esc(ex)
  end
end

function namedoc(meta, def, name)
  quote
    @init
    $(esc(def))
    doc($(esc(name)), $(mdify(meta)))
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

function docm(meta, def)
  def′ = unblock(def)
  isexpr(def′, :macro) && return namedoc(meta, def,
                                         symbol(string("@", namify(def′))))
  isexpr(def′, :type) && return namedoc(meta, def, namify(def′.args[2]))
  isexpr(def′, :abstract) && return namedoc(meta, def, namify(def′))
  fexpr(def′) && return funcdoc(meta, def)
  isexpr(def, :macrocall) && (def = namify(def))
  return objdoc(meta, def)
end

function docm(ex)
  isexpr(ex, :->) && return docm(ex.args...)
  isexpr(ex, :call) && return :(doc($(esc(ex.args[1])), @which $(esc(ex))))
  isexpr(ex, :macrocall) && (ex = namify(ex))
  :(doc($(esc(ex))))
end

macro doc (args...)
  docm(args...)
end

# Metametadata

@doc """
  # Documentation
  The `@doc` macro can be used to both set and retrieve documentation /
  metadata. By default, documentation is written as Markdown, but any
  object can be placed before the arrow. For example:

      @doc \"""
        # The Foo Function
        `foo(x)`: Foo the living hell out of `x`.
      \""" ->
      function foo() ...

  The `->` is not required if the object is on the same line, e.g.

      @doc "foo" foo

  # Retrieving Documentation
  You can retrieve docs for functions, macros and other objects as
  follows:

      @doc foo
      @doc @time
      @doc md""

  # Functions & Methods
  Placing documentation before a method definition (e.g. `function foo()
  ...` or `foo() = ...`) will cause that specific method to be
  documented, as opposed to the whole function. Method docs are
  concatenated together in the order they were defined to provide docs
  for the function.
  """ @doc

@doc "`doc(obj)`: Get the doc metadata for `obj`." doc

@doc """
  `catdoc(xs...)`: Combine the documentation metadata `xs` into a single meta object.
  """ catdoc

# Text / HTML objects

import Base: print, writemime

export HTML, @html_str, @html_mstr

export HTML, Text

@doc """
`HTML(s)`: Create an object that renders `s` as html.

    HTML("<div>foo</div>")

You can also use a stream for large amounts of data:

    HTML() do io
      println(io, "<div>foo</div>")
    end
""" ->
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

@doc "Create an `HTML` object from a literal string." ->
macro html_str (s)
  :(HTML($s))
end

@doc (@doc html"") ->
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

# @doc """
# `Text(s)`: Create an object that renders `s` as plain text.

#     HTML("foo")

# You can also use a stream for large amounts of data:

#     Text() do io
#       println(io, "foo")
#     end
# """ ->
type Text{T}
  content::T
end

print(io::IO, t::Text) = print(io, t.content)
print(io::IO, t::Text{Function}) = t.content(io)
writemime(io::IO, ::MIME"text/plain", t::Text) = print(io, t)

@doc "Create a `Text` object from a literal string." ->
macro text_str (s)
  :(Text($s))
end

@doc (@doc text"") ->
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

# MD support

catdoc(md::MD...) = MD(md...)

# REPL help

const intro = doc"""
  **Welcome to Julia $(string(VERSION)).** The full manual is available at

      http://docs.julialang.org/

  as well many great tutorials and learning resources:

      http://julialang.org/learning/

  For help on a specific function or macro, type `?` followed
  by its name, e.g. `?fft`, `?@time` or `?html""`, and press
  enter.

  You can also use `apropos("...")` to search the documentation.
  """

function replhelp(ex)
  if ex === :? || ex === :help
    return intro
  else
    quote
      # Backwards-compatible with the previous help system, for now
      let doc = @doc $(esc(ex))
        doc ≠ nothing ? doc : Base.Help.@help_ $(esc(ex))
      end
    end
  end
end

end
