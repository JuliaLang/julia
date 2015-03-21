include("block.jl")
include("inline.jl")

@flavor common [list, indentcode, blockquote, hashheader, horizontalrule,
                paragraph,

                linebreak, escapes, inline_code,
                asterisk_bold, asterisk_italic, image, link]
