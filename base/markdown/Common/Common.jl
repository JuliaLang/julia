# This file is a part of Julia. License is MIT: https://julialang.org/license

include("block.jl")
include("inline.jl")

@flavor common [list, indentcode, blockquote, admonition, footnote, hashheader, horizontalrule,
                paragraph,

                linebreak, escapes, inline_code,
                asterisk_bold, underscore_bold, asterisk_italic, underscore_italic, image, footnote_link, link, autolink]

