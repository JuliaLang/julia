# This file is a part of Julia. License is MIT: https://julialang.org/license

module History

using ..REPL: REPL

using StyledStrings: @styled_str as @S_str, Face, addface!, face!, annotations, AnnotatedIOBuffer, AnnotatedString, AnnotatedChar
using JuliaSyntaxHighlighting: highlight
using Base.Threads
using Dates
using InteractiveUtils: clipboard

export HistoryFile, HistEntry, update!, runsearch

const FACES = (
    :REPL_History_search_separator   => Face(foreground=:blue),
    :REPL_History_search_prefix      => Face(foreground=:magenta),
    :REPL_History_search_selected    => Face(foreground=:blue),
    :REPL_History_search_unselected  => Face(foreground=:grey),
    # :REPL_History_search_preview_box => Face(foreground=:grey),
    :REPL_History_search_hint        => Face(foreground=:magenta, slant=:italic, weight=:light),
    :REPL_History_search_results     => Face(inherit=:shadow),
    :REPL_History_search_match       => Face(weight = :bold, underline = true),
)

include("histfile.jl")
include("resumablefiltering.jl")
include("prompt.jl")
include("display.jl")
include("search.jl")

__init__() = foreach(addface!, FACES)

end
