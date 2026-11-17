# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    DocBrowser

A full-screen, mouse-aware browser for Julia's manual and the documentation of every loaded module.

Open it with Enter on an empty `help?>` prompt, or by calling [`DocBrowser.browse`](@ref).
"""
module DocBrowser

import REPL
using Base: PkgId, UUID, AnnotatedString, annotations, annotatedstring
using Base.Docs: Binding, MultiDoc, DocStr
using Markdown
using StyledStrings: StyledStrings, Face, addface!, face!, AnnotatedIOBuffer
import InteractiveUtils
using Base64: base64encode

export browse

# The defaults use only the 16 named colours, so they follow the terminal's palette and
# work on any terminal, light or dark. Each pairing keeps a WCAG contrast of at least 4.5:1
# (3:1 for bold text) on the default palettes of common dark terminals, and on light ones
# where the ANSI colours allow. Secondary text uses `shadow`, which no dark theme draws at
# 4.5:1, so it only holds detail. See `rich_faces` for terminals with more colours.
const FACES = (
    :REPL_DocBrowser_title        => Face(weight=:bold),
    :REPL_DocBrowser_pane_title   => Face(weight=:bold),
    :REPL_DocBrowser_border       => Face(inherit=:shadow),
    :REPL_DocBrowser_section      => Face(weight=:bold),
    :REPL_DocBrowser_bar          => Face(),
    :REPL_DocBrowser_selected     => Face(background=:blue, foreground=:bright_white, weight=:bold),
    :REPL_DocBrowser_unfocused    => Face(inherit=:region, foreground=:bright_white),
    :REPL_DocBrowser_hover        => Face(underline=true),
    :REPL_DocBrowser_selection    => Face(inverse=true),
    :REPL_DocBrowser_dim          => Face(inherit=:shadow),
    :REPL_DocBrowser_key          => Face(weight=:bold, foreground=:bright_blue),
    :REPL_DocBrowser_tip          => Face(foreground=:green),
    :REPL_DocBrowser_tip_title    => Face(foreground=:green, weight=:bold),
    :REPL_DocBrowser_match        => Face(weight=:bold, underline=true),
    :REPL_DocBrowser_searchbox    => Face(foreground=:bright_white, background=:blue, weight=:bold),
    :REPL_DocBrowser_link         => Face(foreground=:bright_blue, underline=true),
    :REPL_DocBrowser_link_active  => Face(foreground=:bright_white, background=:blue),
    :REPL_DocBrowser_exported     => Face(foreground=:green),
    :REPL_DocBrowser_public       => Face(foreground=:bright_blue),
    :REPL_DocBrowser_internal     => Face(inherit=:shadow),
    :REPL_DocBrowser_kind_module   => Face(foreground=:magenta),
    :REPL_DocBrowser_kind_type     => Face(foreground=:cyan),
    :REPL_DocBrowser_kind_function => Face(foreground=:bright_blue),
    :REPL_DocBrowser_kind_macro    => Face(foreground=:yellow),
    :REPL_DocBrowser_kind_constant => Face(foreground=:green),
    :REPL_DocBrowser_kind_global   => Face(foreground=:bright_red),
    :REPL_DocBrowser_logo_red      => Face(foreground=:red),
    :REPL_DocBrowser_logo_green    => Face(foreground=:green),
    :REPL_DocBrowser_logo_purple   => Face(foreground=:magenta),
)

# With 256 or more colours, and once the REPL has learnt whether the terminal is light or
# dark, the Julia logo's colours replace the named ones, and the header and footer are
# shaded. Without both, the named colours are kept, since exact colours chosen for one
# kind of background are unreadable on the other.
function rich_faces()
    Base.get_have_truecolor() || get(Base.current_terminfo(), :colors, 0) >= 256 || return ()
    theme = terminal_theme()
    theme === nothing && return ()
    rgb(c) = StyledStrings.SimpleColor(c)
    shade(α) = StyledStrings.blend(:background, :foreground, α)
    dark = theme === :dark
    blue, green, purple, red = dark ? (0x6b8cf0, 0x58b947, 0xb07cd0, 0xe0605a) :
                                      (0x3355c4, 0x2f8a22, 0x8a4ca8, 0xbf3a31)
    accent, onaccent = dark ? (0x2b3f86, 0xffffff) : (0x4063d8, 0xffffff)
    return (
        :REPL_DocBrowser_border       => Face(foreground=shade(0.15)),
        :REPL_DocBrowser_bar          => Face(background=shade(0.05)),
        :REPL_DocBrowser_selected     => Face(background=rgb(accent), foreground=rgb(onaccent), weight=:bold),
        :REPL_DocBrowser_unfocused    => Face(background=shade(0.14), foreground=:foreground),
        :REPL_DocBrowser_key          => Face(weight=:bold, foreground=rgb(blue)),
        :REPL_DocBrowser_tip          => Face(foreground=rgb(green)),
        :REPL_DocBrowser_tip_title    => Face(foreground=rgb(green), weight=:bold),
        :REPL_DocBrowser_searchbox    => Face(foreground=rgb(0xffffff), background=rgb(0x4063d8), weight=:bold),
        :REPL_DocBrowser_link         => Face(foreground=rgb(blue), underline=shade(0.3)),
        :REPL_DocBrowser_link_active  => Face(foreground=rgb(0xffffff), background=rgb(0x4063d8)),
        :REPL_DocBrowser_exported     => Face(foreground=rgb(green)),
        :REPL_DocBrowser_public       => Face(foreground=rgb(blue)),
        :REPL_DocBrowser_kind_module   => Face(foreground=rgb(purple)),
        :REPL_DocBrowser_kind_type     => Face(foreground=rgb(dark ? 0x5fb8c9 : 0x1c7f8f)),
        :REPL_DocBrowser_kind_function => Face(foreground=rgb(blue)),
        :REPL_DocBrowser_kind_macro    => Face(foreground=rgb(dark ? 0xd7a94b : 0x9a6a00)),
        :REPL_DocBrowser_kind_constant => Face(foreground=rgb(green)),
        :REPL_DocBrowser_kind_global   => Face(foreground=rgb(red)),
        :REPL_DocBrowser_logo_red      => Face(foreground=rgb(0xcb3c33)),
        :REPL_DocBrowser_logo_green    => Face(foreground=rgb(0x389826)),
        :REPL_DocBrowser_logo_purple   => Face(foreground=rgb(0x9558b2)),
    )
end

# `:light` or `:dark` once the REPL has asked the terminal for its colours and
# StyledStrings has applied the matching theme, which is all that changes `region`.
function terminal_theme()
    region = get(StyledStrings.FACES.current[], :region, nothing)
    region === nothing && return nothing
    for theme in (:light, :dark)
        region == get(getfield(StyledStrings.FACES.themes, theme), :region, nothing) && return theme
    end
    return nothing
end

include("catalog.jl")
include("pages.jl")
include("manual.jl")
include("start.jl")
include("input.jl")
include("ui.jl")
# Without this workload the first open takes about 2 s in every session, nearly all of it
# type inference, which the JIT object cache does not keep. It costs about 11 MiB.
include("precompile.jl")

__init__() = foreach(addface!, FACES)

end
