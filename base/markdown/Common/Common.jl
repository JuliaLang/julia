# This file is a part of Julia. License is MIT: http://julialang.org/license

include("block.jl")
include("inline.jl")

@flavor common [list, indentcode, blockquote, hashheader, horizontalrule,
                paragraph,

                linebreak, escapes, inline_tex, inline_code,
                asterisk_bold, asterisk_italic, image, footnote, link]
