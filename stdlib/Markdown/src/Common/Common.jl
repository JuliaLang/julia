# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type MarkdownElement end

include("block.jl")
include("entities.jl")
include("inline.jl")

@flavor common [fencedcode, horizontalrule, list, indentcode, blockquote, admonition, footnote, hashheader,
                html_block, html_block_type7, setextheader, paragraph,

                linebreak, escapes, entity,
                inline_code,
                asterisk_bold, underscore_bold,
                asterisk_italic, underscore_italic,
                image, footnote_link, link, autolink]
