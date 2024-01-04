# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Markdown

Tools for working with the Markdown file format. Mainly for documentation.
The `Markdown` module provides the (internal) `MD` struct (type?) as well as the string 
literals `md"..."` and `doc"..."`.
"""
module Markdown

import Base: show, ==, with_output_color, mapany
using Base64: stringmime

# Margin for printing in terminal.
const margin = 2

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

export @md_str, @doc_str

parse(markdown::AbstractString; flavor = julia) = parse(IOBuffer(markdown), flavor = flavor)
parse_file(file::AbstractString; flavor = julia) = parse(read(file, String), flavor = flavor)

function mdexpr(s, flavor = :julia)
    md = parse(s, flavor = Symbol(flavor))
    esc(toexpr(md))
end

function docexpr(source::LineNumberNode, mod::Module, s, flavor = :julia)
    :($doc_str($(mdexpr(s, flavor)), $(QuoteNode(source)), $mod))
end

"""
    macro md_str(s)

Parse the given string as Markdown text and return a corresponding `Markdown.MD` object.

# Example
```jldoctest
julia> html(md"# Hello, world!")
"<h1>Hello, world&#33;</h1>\\n"

```
"""
macro md_str(s, t...)
    mdexpr(s, t...)
end

function doc_str(md, source::LineNumberNode, mod::Module)
    md.meta[:path] = isa(source.file, Symbol) ? String(source.file) : ""
    md.meta[:module] = mod
    md
end
doc_str(md::AbstractString, source::LineNumberNode, mod::Module) = doc_str(parse(md), source, mod)

"""
    macro doc_str(s)

Parse the given string as Markdown text, add line and module information and return a 
corresponding `Markdown.MD` object.
"""
macro doc_str(s::AbstractString, t...)
    docexpr(__source__, __module__, s, t...)
end

import Base.Docs: catdoc

catdoc(md::MD...) = MD(md...)

end
