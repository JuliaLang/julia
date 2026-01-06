# This file is a part of Julia. License is MIT: https://julialang.org/license

# This file contains markdown extensions designed to make documenting
# Julia easy peasy.
#
# We start by borrowing GitHub's `fencedcode` extension – more to follow.

include("interp.jl")

@flavor julia [blocktex, blockinterp,
               horizontalrule, list, indentcode, blockquote, admonition, footnote, hashheader,
               fencedcode, github_table, setextheader, paragraph,

               linebreak, escapes,
               tex, interp,
               en_or_em_dash, inline_code,
               double_tilde_strikethrough, tilde_strikethrough,
               asterisk_bold, underscore_bold,
               asterisk_italic, underscore_italic,
               image, footnote_link, link, autolink]
