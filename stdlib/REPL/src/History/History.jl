# This file is a part of Julia. License is MIT: https://julialang.org/license

module History

using ..REPL: REPL

using StyledStrings: @styled_str as @S_str, @face_str, @defpalette!, @registerpalette!, face!, Face, annotations, AnnotatedString, AnnotatedChar
using JuliaSyntaxHighlighting: highlight
using Base.Threads
using Dates
using InteractiveUtils: clipboard

export HistoryFile, HistEntry, update!, runsearch

@defpalette! begin
    search_separator  = Face(foreground = blue)
    search_prefix     = Face(foreground = magenta)
    search_selected   = Face(foreground = blue)
    search_unselected = Face(foreground = grey)
    # search_preview_box => Face(foreground = grey)
    search_hint       = Face(foreground = magenta, slant = :italic, weight = :light)
    search_results    = Face(inherit = shadow)
    search_match      = Face(weight = :bold, underline = true)
end

include("histfile.jl")
include("resumablefiltering.jl")
include("prompt.jl")
include("display.jl")
include("search.jl")

__init__() = @registerpalette!

end
