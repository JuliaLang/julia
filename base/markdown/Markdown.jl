module Markdown

include("parse/config.jl")
include("parse/util.jl")
include("parse/parse.jl")

include("Common/Common.jl")
include("GitHub/GitHub.jl")
include("Julia/Julia.jl")

include("render/plain.jl")
include("render/html.jl")
# include("render/latex.jl")

include("render/terminal/render.jl")

export readme, license, @md_str, @md_mstr, @doc_str, @doc_mstr

parse(markdown::String; flavour = julia) = parse(IOBuffer(markdown), flavour = flavour)
parse_file(file::String; flavour = julia) = parse(readall(file), flavour = flavour)

readme(pkg::String; flavour = github) = parse_file(Pkg.dir(pkg, "README.md"), flavour = flavour)
readme(pkg::Module; flavour = github) = readme(string(pkg), flavour = flavour)

license(pkg::String; flavour = github) = parse_file(Pkg.dir(pkg, "LICENSE.md"), flavour = flavour)
license(pkg::Module; flavour = github) = license(string(pkg), flavour = flavour)

function mdexpr(s, flavour = :julia)
  md = parse(s, flavour = symbol(flavour))
  esc(toexpr(md))
end

function docexpr(s, flavour = :julia)
  quote
    let md = $(mdexpr(s, flavour))
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

function writemime(io::IO, m, md::Vector{MD})
  for md in md
    writemime(io, m, md)
  end
end

function Base.display(d::Base.REPL.REPLDisplay, md::Vector{MD})
  for md in md
    display(d, md)
  end
end

end
