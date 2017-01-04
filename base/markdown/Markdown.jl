# This file is a part of Julia. License is MIT: http://julialang.org/license

module Markdown

import Base: show, ==
import Core: @doc_str

include("parse/config.jl")
include("parse/util.jl")
include("parse/parse.jl")

include("Common/Common.jl")
include("GitHub/GitHub.jl")
include("IPython/IPython.jl")
include("Julia/Julia.jl")

include("render/plain.jl")
include("render/html.jl")
include("render/latex.jl")
include("render/rst.jl")

include("render/terminal/render.jl")

export readme, license, @md_str, @doc_str

parse(markdown::AbstractString; flavor = julia) = parse(IOBuffer(markdown), flavor = flavor)
parse_file(file::AbstractString; flavor = julia) = parse(readstring(file), flavor = flavor)

readme(pkg::AbstractString; flavor = github) = parse_file(Pkg.dir(pkg, "README.md"), flavor = flavor)
readme(pkg::Module; flavor = github) = readme(string(pkg), flavor = flavor)

license(pkg::AbstractString; flavor = github) = parse_file(Pkg.dir(pkg, "LICENSE.md"), flavor = flavor)
license(pkg::Module; flavor = github) = license(string(pkg), flavor = flavor)

function mdexpr(s, flavor = :julia)
    md = parse(s, flavor = Symbol(flavor))
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

doc_str(md, file, mod) = (md.meta[:path] = file; md.meta[:module] = mod; md)
doc_str(md::AbstractString, file, mod) = doc_str(parse(md), file, mod)

macro doc_str(s::AbstractString, t...)
    :($(doc_str)($(mdexpr(s, t...)), $(Base).@__FILE__, $(current_module)()))
end

function Base.display(d::Base.REPL.REPLDisplay, md::Vector{MD})
    for md in md
        display(d, md)
    end
end

end
