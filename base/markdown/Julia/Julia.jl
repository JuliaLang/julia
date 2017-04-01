# This file is a part of Julia. License is MIT: http://julialang.org/license

# This file contains markdown extensions designed to make documenting
# Julia easy peasy.
#
# We start by borrowing GitHub's `fencedcode` extension – more to follow.

include("interp.jl")

@flavor julia [blocktex, blockinterp, hashheader, list, indentcode, fencedcode,
               blockquote, admonition, footnote, github_table, horizontalrule, setextheader, paragraph,

               linebreak, escapes, tex, interp, en_dash, inline_code,
               asterisk_bold, asterisk_italic, image, footnote_link, link, autolink]

