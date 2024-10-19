# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Markdown

Tools for working with the Markdown markup language for formatted text, used within Julia for documentation.
The `Markdown` module provides the (internal) [`MD`](@ref) type as well as the string
literals `md"..."` and `doc"..."`.
"""
module Markdown

import Base: AnnotatedString, AnnotatedIOBuffer, show, ==, with_output_color, mapany
using Base64: stringmime

using StyledStrings: StyledStrings, Face, addface!, @styled_str, styled
using JuliaSyntaxHighlighting: highlight, highlight!

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

const MARKDOWN_FACES = [
    :markdown_header => Face(weight=:bold),
    :markdown_h1 => Face(height=1.25, inherit=:markdown_header),
    :markdown_h2 => Face(height=1.20, inherit=:markdown_header),
    :markdown_h3 => Face(height=1.15, inherit=:markdown_header),
    :markdown_h4 => Face(height=1.12, inherit=:markdown_header),
    :markdown_h5 => Face(height=1.08, inherit=:markdown_header),
    :markdown_h6 => Face(height=1.05, inherit=:markdown_header),
    :markdown_admonition => Face(weight=:bold),
    :markdown_code => Face(inherit=:code),
    :markdown_julia_prompt => Face(inherit=:repl_prompt_julia),
    :markdown_footnote => Face(inherit=:bright_yellow),
    :markdown_hrule => Face(inherit=:shadow),
    :markdown_inlinecode => Face(inherit=:markdown_code),
    :markdown_latex => Face(inherit=:magenta),
    :markdown_link => Face(underline=:bright_blue),
    :markdown_list => Face(foreground=:blue),
]

__init__() = foreach(addface!, MARKDOWN_FACES)

parse(markdown::String; flavor = julia) = parse(IOBuffer(markdown), flavor = flavor)
parse(markdown::AbstractString; flavor = julia) = parse(String(markdown), flavor = flavor)
parse_file(file::AbstractString; flavor = julia) = parse(read(file, String), flavor = flavor)

function mdexpr(s, flavor = :julia)
    md = parse(s, flavor = Symbol(flavor))
    esc(toexpr(md))
end

function docexpr(source::LineNumberNode, mod::Module, s, flavor = :julia)
    :($doc_str($(mdexpr(s, flavor)), $(QuoteNode(source)), $mod))
end

"""
    @md_str -> MD

Parse the given string as Markdown text and return a corresponding [`MD`](@ref) object.

# Examples
```jldoctest
julia> s = md"# Hello, world!"
  Hello, world!
  ≡≡≡≡≡≡≡≡≡≡≡≡≡

julia> typeof(s)
Markdown.MD

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
    @doc_str -> MD

Parse the given string as Markdown text, add line and module information and return a
corresponding [`MD`](@ref) object.

`@doc_str` can be used in conjunction with the [`Base.Docs`](@ref) module. Please also refer to
the manual section on [documentation](@ref man-documentation) for more information.

# Examples
```
julia> s = doc"f(x) = 2*x"
  f(x) = 2*x

julia> typeof(s)
Markdown.MD

```
"""
macro doc_str(s::AbstractString, t...)
    docexpr(__source__, __module__, s, t...)
end

import Base.Docs: catdoc

catdoc(md::MD...) = MD(md...)

if Base.generating_output()
    # workload to reduce latency
    md"""
    # H1
    ## H2
    ### H3
    **bold text**
    *italicized text*
    > blockquote
    1. First item
    2. Second item
    3. Third item
    - First item
    - Second item
    - Third item
    `code`
    Horizontal Rule
    ---
    """
end

end
