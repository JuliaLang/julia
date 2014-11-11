module Markdown

import Base: writemime

typealias String AbstractString

include("parse/config.jl")
include("parse/util.jl")
include("parse/parse.jl")

include("Common/Common.jl")
include("GitHub/GitHub.jl")
include("IPython/IPython.jl")
include("Julia/Julia.jl")

include("render/plain.jl")
include("render/html.jl")
# include("render/latex.jl")

include("render/terminal/render.jl")

export readme, license, @md_str, @md_mstr, @doc_str, @doc_mstr

parse(markdown::String; flavor = julia) = parse(IOBuffer(markdown), flavor = flavor)
parse_file(file::String; flavor = julia) = parse(readall(file), flavor = flavor)

readme(pkg::String; flavor = github) = parse_file(Pkg.dir(pkg, "README.md"), flavor = flavor)
readme(pkg::Module; flavor = github) = readme(string(pkg), flavor = flavor)

license(pkg::String; flavor = github) = parse_file(Pkg.dir(pkg, "LICENSE.md"), flavor = flavor)
license(pkg::Module; flavor = github) = license(string(pkg), flavor = flavor)

function mdexpr(s, flavor = :julia)
  md = parse(s, flavor = symbol(flavor))
  esc(toexpr(md))
end

function docexpr(s, flavor = :julia)
  quote
    let md = $(mdexpr(s, flavor))
      md.meta[:path] = @__FILE__
      md.meta[:module] = current_module()
      md
    end
  end
end

macro md_str(s, t...)
  mdexpr(s, t...)
end

macro md_mstr(s, t...)
  s = Base.triplequoted(s)
  mdexpr(s, t...)
end

macro doc_str(s, t...)
  docexpr(s, t...)
end

macro doc_mstr(s, t...)
  s = Base.triplequoted(s)
  docexpr(s, t...)
end

function Base.display(d::Base.REPL.REPLDisplay, md::Vector{MD})
  for md in md
    display(d, md)
  end
end

end
