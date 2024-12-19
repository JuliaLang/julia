# This file is a part of Julia. License is MIT: https://julialang.org/license

# This file contains markdown extensions designed to make documenting
# Julia easy peasy.
#
# We start by borrowing GitHub's `fencedcode` extension – more to follow.

include("interp.jl")

"""
    const Markdown.julia

A constant indicating Julia's flavor of Markdown.

See also [`Markdown.parse`](@ref), [`Markdown.common`](@ref), [`Markdown.github`](@ref).
"""
@flavor julia [blocktex, blockinterp, hashheader, list, indentcode, fencedcode,
               blockquote, admonition, footnote, github_table, horizontalrule, setextheader, paragraph,

               linebreak, escapes, tex, interp, en_dash, inline_code,
               asterisk_bold, underscore_bold, asterisk_italic, underscore_italic, image, footnote_link, link, autolink]
