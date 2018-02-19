# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

"""
Tools for working with the Markdown file format. Mainly for documentation.
"""
module Markdown

import Base: show, ==, with_output_color
using Base64: stringmime

include(joinpath("parse", "config.jl"))
include(joinpath("parse", "util.jl"))
include(joinpath("parse", "parse.jl"))

include(joinpath("Common", "Common.jl"))
include(joinpath("GitHub", "GitHub.jl"))
include(joinpath("IPython", "IPython.jl"))
include(joinpath("Julia", "Julia.jl"))

include(joinpath("render", "plain.jl"))
include(joinpath("render", "html.jl"))
include(joinpath("render", "latex.jl"))
include(joinpath("render", "rst.jl"))

include(joinpath("render", "terminal", "render.jl"))

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

macro md_str(s, t...)
    mdexpr(s, t...)
end

function doc_str(md, source::LineNumberNode, mod::Module)
    md.meta[:path] = isa(source.file, Symbol) ? String(source.file) : ""
    md.meta[:module] = mod
    md
end
doc_str(md::AbstractString, source::LineNumberNode, mod::Module) = doc_str(parse(md), source, mod)

macro doc_str(s::AbstractString, t...)
    docexpr(__source__, __module__, s, t...)
end

import Base.Docs: catdoc

catdoc(md::MD...) = MD(md...)

end
