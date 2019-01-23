module PlotlyBase

using Base.Iterators
using JSON
using UUIDs
using Dates

import Base: ==

mean(A) = sum(A) / length(A)

# export some names from JSON
export json

_symbol_dict(x) = x
_symbol_dict(d::AbstractDict) =
    Dict{Symbol,Any}([(Symbol(k), _symbol_dict(v)) for (k, v) in d])

# include these here because they are used below
include("traces_layouts.jl")
include("styles.jl")

# core plot object
mutable struct Plot{TT<:AbstractTrace}
    data::Vector{TT}
    layout::AbstractLayout
    divid::UUID
    style::Style
end

# Default `convert` fallback constructor
Plot(p::Plot) = p

# include the rest of the core parts of the package
include("util.jl")
include("json.jl")
include("subplots.jl")
include("api.jl")
include("convenience_api.jl")
include("recession_bands.jl")
include("output.jl")

# Set some defaults for constructing `Plot`s
function Plot(;style::Style=CURRENT_STYLE[])
    Plot(GenericTrace{Dict{Symbol,Any}}[], Layout(), uuid4(), style)
end

function Plot(data::AbstractVector{T}, layout=Layout();
              style::Style=CURRENT_STYLE[]) where T<:AbstractTrace
    Plot(data, layout, uuid4(), style)
end

function Plot(data::AbstractTrace, layout=Layout();
              style::Style=CURRENT_STYLE[])
    Plot([data], layout; style=style)
end


# NOTE: we export trace constructing types from inside api.jl
# NOTE: we export names of shapes from traces_layouts.jl
export

    # core types
    Plot, GenericTrace, Layout, Shape, AbstractTrace, AbstractLayout,

    # plotly.js api methods
    restyle!, relayout!, update!, addtraces!, deletetraces!, movetraces!,
    redraw!, extendtraces!, prependtraces!, purge!, to_image, download_image,
    react!,

    # non-!-versions (forks, then applies, then returns fork)
    restyle, relayout, update, addtraces, deletetraces, movetraces, redraw,
    extendtraces, prependtraces, react,

    # helper methods
    plot, fork, vline, hline, attr,

    # new trace types
    stem,

    # convenience stuff
    add_recession_bands!,

    # styles
    use_style!, style, Style, Cycler, STYLES,

    # other
    savejson, savefig, html_body


end # module
