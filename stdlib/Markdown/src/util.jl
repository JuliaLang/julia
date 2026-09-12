# prewalk/postwalk inspired by MacroTools.jl
# (which is Copyright (c) 2015: Mike Innes, Julia Computing & contributors,
#  and distributed under the MIT "expat" license, like julia itself).

walk(x, inner, outer) = outer(x)

walk(x::AbstractVector, inner, outer) = map(x -> walk(x, inner, outer), x)
walk(x::Vector{Any}, inner, outer) = Any[walk(x, inner, outer) for x in x]

# Common/block.jl:

walk(x::T, inner, outer) where T<:Union{Paragraph,BlockQuote} =
    outer(T(walk(x.content, inner, outer)))
walk(x::Admonition, inner, outer) =
    outer(Admonition(x.category, x.title, walk(x.content, inner, outer)))
walk(x::List, inner, outer) =
    outer(List(walk(x.items, inner, outer), x.ordered, x.loose))

walk(x::Header{level}, inner, outer) where {level} =
    outer(Header{level}(inner(x.text)))
walk(x::Footnote, inner, outer) = outer(Footnote(x.id, inner(x.text)))

#  x.Code is not ordinary text, use fallback outer(x)
#      walk(x::Code, inner, outer) = outer(x)

# Common/inline.jl:
walk(x::T, inner, outer) where T<:Union{Italic,Bold} =
    outer(T(inner(x.text)))
walk(x::Image, inner, outer) = outer(Image(x.url, inner(x.alt)))
walk(x::Link, inner, outer) = outer(Link(inner(x.text), x.url))

# GitHub/table.jl:
walk(x::Table, inner, outer) =
    outer(Table(walk(x.rows, inner, outer), x.align))

# IPython/IPython.jl:
#     x.formula is not ordinary text, use fallback outer(x):
#             walk(x::LaTeX, inner, outer) = outer(x)

# parse/parse.jl
walk(x::MD, inner, outer) =
    outer(MD(walk(x.content, inner, outer), x.meta))

"""
    postwalk(f, md)

Applies `f` to each node in the given markdown expression `md`, returning the result.
`f` sees expressions *after* they have been transformed by the walk.

When `f(node)` is called, its argument `node` should typically be either a markdown
expression object like `Paragraph` or `Bold` or `Code` *or* an ordinary `String`
corresponding to the underlying "plain text".  (A `String` argument will never
be code, formulas, URLs, or similar text with a special technical meaning; such
special text will always be wrapped in a markdown expression object when passed
to `f`.)

For example, the following code transforms all "plain text" to uppercase:
```jldoctest
julia> using Markdown

julia> txt = md"*Italic* and *bold* with `const code` and [link text](url)."
*Italic* and *bold* with `const code` and [link text](url).

julia> postwalk(x -> x isa String ? uppercase(x) : x, txt)
*ITALIC* AND *BOLD* WITH `const code` AND [LINK TEXT](url).
```

See also: [`prewalk`](@ref).
"""
postwalk(f, x) = walk(x, x -> postwalk(f, x), f)

"""
    prewalk(f, expr)

Applies `f` to each node in the given markdown expression `md`, returning the result.
`f` sees expressions *before* they have been transformed by the walk, and the
walk will be applied to whatever `f` returns.

This makes `prewalk` somewhat prone to infinite loops; you probably want to try
[`postwalk`](@ref) first.
"""
prewalk(f, x)  = walk(f(x), x -> prewalk(f, x), identity)
