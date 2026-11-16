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

# Each colour pairing keeps a WCAG contrast of at least 4.5:1 (3:1 for bold text) on the
# default palettes of common dark terminals, and on light ones where the ANSI colours allow.
# Secondary text uses `shadow`, which no dark theme draws at 4.5:1, so it only holds detail.
const FACES = (
    :REPL_DocBrowser_title        => Face(weight=:bold, foreground=:bright_magenta),
    :REPL_DocBrowser_pane_title   => Face(weight=:bold),
    :REPL_DocBrowser_border       => Face(inherit=:shadow),
    :REPL_DocBrowser_section      => Face(weight=:bold),
    :REPL_DocBrowser_selected     => Face(background=:blue, foreground=:bright_white, weight=:bold),
    :REPL_DocBrowser_unfocused    => Face(inherit=:region, foreground=:bright_white),
    :REPL_DocBrowser_hover        => Face(underline=true),
    :REPL_DocBrowser_selection    => Face(inverse=true),
    :REPL_DocBrowser_dim          => Face(inherit=:shadow),
    :REPL_DocBrowser_key          => Face(weight=:bold, foreground=:bright_magenta),
    :REPL_DocBrowser_tip          => Face(foreground=:cyan),
    :REPL_DocBrowser_tip_title    => Face(foreground=:cyan, weight=:bold),
    :REPL_DocBrowser_match        => Face(weight=:bold, underline=true),
    :REPL_DocBrowser_searchbox    => Face(foreground=:black, background=:cyan, weight=:bold),
    :REPL_DocBrowser_link         => Face(underline=true),
    :REPL_DocBrowser_link_active  => Face(foreground=:black, background=:cyan),
    :REPL_DocBrowser_exported     => Face(foreground=:green),
    :REPL_DocBrowser_public       => Face(foreground=:cyan),
    :REPL_DocBrowser_internal     => Face(inherit=:shadow),
    :REPL_DocBrowser_kind_module   => Face(foreground=:bright_magenta),
    :REPL_DocBrowser_kind_type     => Face(foreground=:cyan),
    :REPL_DocBrowser_kind_function => Face(foreground=:bright_blue),
    :REPL_DocBrowser_kind_macro    => Face(foreground=:yellow),
    :REPL_DocBrowser_kind_constant => Face(foreground=:green),
    :REPL_DocBrowser_kind_global   => Face(foreground=:bright_red),
)

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
