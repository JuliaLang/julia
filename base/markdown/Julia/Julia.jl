"""
This file contains markdown extensions designed to make documenting
Julia easy peasy.

We start by borrowing GitHub's `fencedcode` extension – more to follow.
"""

include("interp.jl")

@flavor julia [blockinterp, hashheader, list, indentcode, fencedcode, blockquote, paragraph,
                escapes, interp, en_dash, inline_code, asterisk_bold, asterisk_italic, image, link]
