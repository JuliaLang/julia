# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type MarkdownElement end

include("block.jl")
include("inline.jl")

"""
    const Markdown.common

A constant indicating the CommonMark flavor of Markdown.

See also [`Markdown.parse`](@ref), [`Markdown.github`](@ref), [`Markdown.julia`](@ref).
"""
@flavor common [list, indentcode, blockquote, admonition, footnote, hashheader, horizontalrule,
                paragraph,

                linebreak, escapes, inline_code,
                asterisk_bold, underscore_bold, asterisk_italic, underscore_italic, image, footnote_link, link, autolink]
