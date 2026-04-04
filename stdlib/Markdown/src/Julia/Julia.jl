# This file is a part of Julia. License is MIT: https://julialang.org/license

# This file contains markdown extensions designed to make documenting
# Julia easy peasy.
#
# We start by borrowing GitHub's `fencedcode` extension – more to follow.

include("interp.jl")

@flavor julia [blocktex, blockinterp,
               fencedcode, horizontalrule, list, indentcode, blockquote, admonition, footnote, hashheader,
               html_block, html_block_type7, github_table, setextheader, paragraph,

               # Backslash escapes do not work in code blocks, code spans, autolinks, or raw HTML
               inline_code, autolink, html_inline,
               linebreak, escapes, entity,
               tex, interp,
               en_or_em_dash,
               double_tilde_strikethrough, tilde_strikethrough,
               asterisk_bold, underscore_bold,
               asterisk_italic, underscore_italic,
               image, footnote_link, link]
