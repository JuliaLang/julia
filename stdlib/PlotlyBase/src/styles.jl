#=
Note that the following styles used values from the matplotlib style library
(https://github.com/matplotlib/matplotlib/tree/master/lib/matplotlib/mpl-data/stylelib):

- ggplot
- fivethirtyeight
- seaborn

=#

struct Cycler
    vals::Vector
end

Base.isempty(c::Cycler) = isempty(c.vals)
Base.length(c::Cycler) = length(c.vals)
Cycler(t::Tuple) = Cycler(collect(t))
Cycler(x::Union{String,Number,Date,Symbol}) = Cycler([x])

function Base.getindex(c::Cycler, ix::Integer)
    n = length(c.vals)
    @inbounds v = c.vals[mod1(ix, n)]
    v
end

function Base.getindex(c::Cycler, ixs::AbstractVector{<:Integer})
    [c[i] for i in ixs]
end

Base.iterate(c::Cycler, s::Int=1) = c[s], s+1
Base.IteratorSize(::Cycler) = IsInfinite()

struct Style
    layout::Layout
    global_trace::PlotlyAttribute
    trace::Dict{Symbol,PlotlyAttribute}
end

function Style(;
        color_cycle=[],
        layout=Layout(), global_trace=attr(),
        trace=Dict{Symbol,PlotlyAttribute}(),
    )
    if !isempty(color_cycle)
        msg = """
        `color_cycle` argument deprecated. Set the `marker_color` attribute
        on `global_trace` instead using a Cycler like this:

        Style(global_trace=attr(marker_color=Cycler($(color_cycle))))
        """
        global_trace[:marker_color] = Cycler(color_cycle)
        @warn msg
    end
    Style(layout, global_trace, trace)
end

function Style(ps1::Style, ps2::Style)
    la = deepcopy(ps1.layout)
    for (k, v) in ps2.layout.fields
        la[k] = v
    end

    gta = merge(ps1.global_trace, ps2.global_trace)

    ta = deepcopy(ps1.trace)
    for (k, v) in ps2.trace
        ta_k = get(ta, k, attr())
        merge!(ta_k, v)
        ta[k] = ta_k
    end

    Style(layout=la, global_trace=gta, trace=ta)
end

Style(pss::Style...) = foldl(Style, pss[1], pss[2:end])

function Style(base::Style;
        color_cycle=[], layout=Layout(),
        global_trace=attr(), trace=Dict{Symbol,PlotlyAttribute}()
    )
    new_style = Style(
        color_cycle=color_cycle, layout=layout, global_trace=global_trace,
        trace=trace
    )
    Style(base, new_style)
end

function Base.show(io::IO, ::MIME"text/plain", s::Style)
    ctx = IOContext(io, :limit=>true)
    println(io, "Style with:")

    if !isempty(s.layout)
        print(io, "  - "); show(ctx, MIME"text/plain"(), s.layout)
    end

    if !isempty(s.global_trace)
        print(io, "  - global_trace: ")
        show(ctx, MIME"text/plain"(), s.global_trace)
    end

    if !isempty(s.trace)
        println(io, "  - trace: ")
        for (k, v) in s.trace
            print(io, "    - ", k, ": "); show(ctx, MIME"text/plain"(), v)
        end
    end
end

function ==(s1::Style, s2::Style)
    all(nm -> getfield(s1, nm) == getfield(s2, nm), fieldnames(s1))
end

function ggplot_style()
    axis = attr(showgrid=true, gridcolor="white", linewidth=1.0,
                linecolor="white", titlefont_color="#555555",
                titlefont_size=14, ticks="outside",
                tickcolor="#555555", automargin=true
                )
    layout = Layout(plot_bgcolor="#E5E5E5",
                    paper_bgcolor="white",
                    font_size=10,
                    xaxis=axis,
                    yaxis=axis,
                    titlefont_size=14,
		    margin=attr(t=65),
		    autosize=true)

    gta = attr(marker_line_width=0.5, marker_line_color="#348ABD")

    colors = ["#E24A33", "#348ABD", "#988ED5", "#777777", "#FBC15E",
              "#8EBA42", "#FFB5B8"]
    gta[:marker_color] = Cycler(colors)
    Style(layout=layout, global_trace=gta)
end

function fivethirtyeight_style()
    ta = Dict(:scatter=>attr(line_width=4))
    axis = attr(showgrid=true, gridcolor="#cbcbcb",
                linewidth=1.0, linecolor="#f0f0f0",
                ticklen=0.0, tickcolor="#555555", ticks="outside",
                titlefont_size=12, titlefont_color="#555555",
		automargin=true)
    layout = Layout(plot_bgcolor="#f0f0f0",
                    paper_bgcolor="#f0f0f0",
                    font_size=14,
                    xaxis=axis,
                    yaxis=axis,
                    legend=attr(borderwidth=1.0,
                                bgcolor="f0f0f0", bordercolor="f0f0f0"),
                    titlefont_size=14,
		    margin=attr(t=65),
		    autosize=true)
    colors = ["#008fd5", "#fc4f30", "#e5ae38", "#6d904f",
              "#8b8b8b", "#810f7c"]
    gta = attr(marker_color=Cycler(colors))
    Style(layout=layout, trace=ta, global_trace=gta)
end

function seaborn_style()
    ta = Dict(:heatmap=>attr(colorscale="Greys"),
              :scatter=>attr(marker=attr(size=7, line_width=0.0),
                             line_width=1.75))
    axis = attr(showgrid=true, gridcolor="white",
                linewidth=1.0, linecolor="white",
                ticklen=0.0, tickcolor="#555555", ticks="outside", tickfont_size=10,
                titlefont_size=12, titlefont_color="#555555", automargin=true)
    # TODO: no concept of major vs minor ticks...
    layout = Layout(plot_bgcolor="EAEAF2",
                    paper_bgcolor="white",
                    width=800,
                    height=550, # TODO: what does font_color=0.15 mean??
                    font=attr(family="Arial", size=14, color=0.15),
                    xaxis=axis,
                    yaxis=axis,
                    legend=attr(font_size=10,
                                bgcolor="white", bordercolor="white"),
                    titlefont_size=14,
		    margin=attr(t=65),
		    autosize=true)
    colors = ["#4C72B0", "#55A868", "#C44E52", "#8172B2", "#CCB974", "#64B5CD"]
    gta = attr(marker_color=Cycler(colors))
    Style(trace=ta, layout=layout, global_trace=gta)
end

# This theme was taken from here:
# https://github.com/dcjones/Gadfly.jl/blob/cb28d6aca6b031d01e44146799e520b8bb0d349b/src/theme.jl#L342-L409
function gadfly_dark_style()
    label_color = "#a1a1a1"
    bgcolor = "#222831"
    grid_color = "#575757"

    color_cycle = ["#FE4365", "#ECA25C", "#3F9778", "#EEDBFF", "#236EAD",
                   "#60F6FF", "#D4EC9F", "#7E674B", "#9E7EC1", "#7CB5FB"]

    axis = attr(showgrid=true, gridcolor=grid_color, gridwidth=0.35,
                linecolor=grid_color, titlefont_color=label_color,
                linewidth=1.2, titlefont_size=14, tickcolor=label_color,
		automargin=true)

    layout = Layout(plot_bgcolor=bgcolor,
                    paper_bgcolor=bgcolor,
                    font_size=10,
                    xaxis=axis,
                    yaxis=axis,
                    font_color=label_color,
                    titlefont_size=14,
                    margin=attr(l=40, r=65, t=65, b=30),
		    autosize=true)

    gta = attr(marker_color=Cycler(color_cycle))
    Style(layout=layout, global_trace=gta)
end

function tomorrow_night_eighties_style()

    bgcolor = "#2d2d2d"  # Background
    grid_color = "#515151"  # Selection
    label_color = "#cccccc"  # Comment
    color_cycle = [
                    "#cc99cc",
                    "#66cccc",
                    "#f2777a",
                    "#ffcc66",
                    "#99cc99",
                    "#f99157",
                    "#6699cc",
                   ]

    axis = attr(showgrid=true, gridcolor=grid_color, gridwidth=0.35,
                linecolor=grid_color, titlefont_color=label_color,
                linewidth=1.2, titlefont_size=14, tickcolor=label_color,
		automargin=true)

    layout = Layout(plot_bgcolor=bgcolor,
                    paper_bgcolor=bgcolor,
                    font_size=10,
                    xaxis=axis,
                    yaxis=axis,
                    font_color=label_color,
                    titlefont_size=14,
                    margin=attr(l=65, r=65, t=65, b=65),
		    autosize=true)

    gta = attr(marker_color=Cycler(color_cycle))
    Style(layout=layout, global_trace=gta)
end

function style(sty::Symbol)
    sty == :ggplot ? ggplot_style() :
    sty == :fivethirtyeight ? fivethirtyeight_style() :
    sty == :seaborn ? seaborn_style() :
    sty == :gadfly_dark ? gadfly_dark_style() :
    sty == :tomorrow_night_eighties ? tomorrow_night_eighties_style() :
    sty == :default ? DEFAULT_STYLE[] :
    error("Unknown style $sty")
end

const STYLES = [
    :default, :ggplot, :fivethirtyeight, :seaborn, :gadfly_dark,
    :tomorrow_night_eighties
]

# NOTE: DEFAULT_STYLE is reset in __init__() based on environment vars
const DEFAULT_STYLE = Ref{Style}(Style())
const CURRENT_STYLE = Ref{Style}(DEFAULT_STYLE[])
reset_style!() = CURRENT_STYLE[] = DEFAULT_STYLE[]
use_style!(sty::Symbol) = CURRENT_STYLE[] = style(sty)
use_style!(s::Style) = CURRENT_STYLE[] = s
