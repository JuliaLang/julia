"""
This file contains markdown extensions designed to make documenting
Julia easy peasy.

We start by borrowing GitHub's `fencedcode` extension – more to follow.
"""

include("interp.jl")

@flavor julia [blocktex, blockinterp, hashheader, list, indentcode, fencedcode,
               blockquote, github_table, horizontalrule, paragraph,

               linebreak, escapes, latex, interp, inline_code,
               asterisk_bold, asterisk_italic, image, link]
