# This file is a part of Julia. License is MIT: http://julialang.org/license

include("blockquote.jl")
include("bold.jl")
include("code.jl")
include("escapes.jl")
include("header.jl")
include("horizontalrule.jl")
include("image.jl")
include("italic.jl")
include("linebreak.jl")
include("link.jl")
include("list.jl")
include("paragraph.jl")
include("vector.jl")

@flavor common [list, indentcode, blockquote, hashheader, horizontalrule,
                fencedcode, paragraph,

                linebreak, escapes, inline_code,
                asterisk_bold, asterisk_italic, image, link]
