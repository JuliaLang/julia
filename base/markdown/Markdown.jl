# This file is a part of Julia. License is MIT: http://julialang.org/license

module Markdown

import Base: writemime, ==

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

export readme, license, @md_str

parse(markdown::AbstractString; flavor = julia) = parse(IOBuffer(markdown), flavor = flavor)
parse_file(file::AbstractString; flavor = julia) = parse(readall(file), flavor = flavor)

readme(pkg::AbstractString; flavor = github) = parse_file(Pkg.dir(pkg, "README.md"), flavor = flavor)
readme(pkg::Module; flavor = github) = readme(string(pkg), flavor = flavor)

license(pkg::AbstractString; flavor = github) = parse_file(Pkg.dir(pkg, "LICENSE.md"), flavor = flavor)
license(pkg::Module; flavor = github) = license(string(pkg), flavor = flavor)

function mdexpr(s, flavor = :julia_interp)
    md = parse(s, flavor = symbol(flavor))
    esc(toexpr(md))
end

macro md_str(s, t...)
    mdexpr(s, t...)
end

function Base.display(d::Base.REPL.REPLDisplay, md::Vector{MD})
    for md in md
        display(d, md)
    end
end

"""
    stripmd(x)

Strip all Markdown markup from x, leaving the result in plain text. Used
internally by apropos to make docstrings containing more than one markdown
element searchable.
"""
stripmd(x::AbstractString) = x  # base case
stripmd(x::Vector) = string(map(stripmd, x)...)
stripmd(x::Markdown.BlockQuote) = "$(stripmd(x.content))"
stripmd(x::Markdown.Bold) = "$(stripmd(x.text))"
stripmd(x::Markdown.Code) = "$(stripmd(x.code))"
stripmd{N}(x::Markdown.Header{N}) = stripmd(x.text)
stripmd(x::Markdown.HorizontalRule) = " "
stripmd(x::Markdown.Image) = "$(stripmd(x.alt)) $(x.url)"
stripmd(x::Markdown.Italic) = "$(stripmd(x.text))"
stripmd(x::Markdown.LaTeX) = "$(x.formula)"
stripmd(x::Markdown.LineBreak) = " "
stripmd(x::Markdown.Link) = "$(stripmd(x.text)) $(x.url)"
stripmd(x::Markdown.List) = join(map(stripmd, x.items), " ")
stripmd(x::Markdown.MD) = join(map(stripmd, x.content), " ")
stripmd(x::Markdown.Paragraph) = stripmd(x.content)
stripmd(x::Markdown.Table) =
    join([join(map(stripmd, r), " ") for r in x.rows], " ")

## Markdown search simply strips all markup and searches plain text version
Base.Docs.docsearch(haystack::Markdown.MD, needle) =
    Base.Docs.docsearch(stripmd(haystack.content), needle)

# Merge Markdown documents for Base.Docs
Base.Docs.doc_renders[:Markdown] = Base.Docs.doc_renders[:md] =
    (io::IO, mime::MIME, docs::Base.DocObj) -> writemime(io, mime, parse(docs.content))

end
