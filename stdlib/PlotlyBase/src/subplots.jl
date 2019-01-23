"""
Given the number of rows and columns, return an NTuple{4,Float64} containing
`(width, height, vspace, hspace)`, where `width` and `height` are the
width and height of each subplot and `vspace` and `hspace` are the vertical
and horizonal spacing between subplots, respectively.
"""
function sizes(nr::Int, nc::Int, subplot_titles::Bool=false)
    # NOTE: the logic of this function was mostly borrowed from plotly.py
    dx = 0.2 / nc
    dy = subplot_titles ? 0.55 / nr : 0.3 / nr
    width = (1. - dx * (nc - 1)) / nc
    height = (1. - dy * (nr - 1)) / nr
    vspace = nr == 1 ? 0.0 : (1 - height*nr)/(nr-1)
    hspace = nc == 1 ? 0.0 : (1 - width*nc)/(nc-1)
    width, height, vspace, hspace
end

function gen_layout(nr, nc, subplot_titles::Bool=false)
    w, h, dy, dx = sizes(nr, nc, subplot_titles)

    x = 0.0  # start from left
    y = 1.0  # start from top

    out = Layout()
    for col in 1:nc

        y = 1.0 # reset y as we start a new col
        for row in 1:nr
            subplot = LinearIndices((nc, nr))[col, row]

            out["xaxis$subplot"] = Dict{Any,Any}(:domain=>[x, x+w],
                                                 :anchor=> "y$subplot")
            out["yaxis$subplot"] = Dict{Any,Any}(:domain=>[y-h, y],
                                                 :anchor=> "x$subplot")

            y -= nr == 1 ? 0.0 : h + dy
         end

         x += nc == 1 ? 0.0 : w + dx
    end

    out

end

function handle_titles!(big_layout, sub_layout, ix::Int)
    # don't worry about it if the sub_layout doesn't have a title
    if !haskey(sub_layout.fields, "title") && !haskey(sub_layout.fields, :title)
        return big_layout
    end

    # check for symbol or string
    nm = haskey(sub_layout.fields, "title") ? "title" : :title

    ann = Dict{Any,Any}(:font => Dict{Any,Any}(:size => 16),
                        :showarrow => false,
                        :text => pop!(sub_layout.fields, nm),
                        :x => mean(big_layout["xaxis$(ix).domain"]),
                        :xanchor => "center",
                        :xref => "paper",
                        :y => big_layout["yaxis$(ix).domain"][2],
                        :yanchor => "bottom",
                        :yref => "paper")
    anns = get(big_layout.fields, :annotations, Dict{Any,Any}[])
    push!(anns, ann)
    big_layout[:annotations] = anns
    big_layout
end

# plots are 3d if any of their traces have a 3d type. This should flow down
# the methods as ordered here
_is3d(p::Plot) = any(_is3d, p.data)
_is3d(t::GenericTrace) = _is3d(t[:type])
_is3d(t::Symbol) = _is3d(string(t))
_is3d(s::AbstractString) = s in ["surface", "mesh3d", "scatter3d"]

# else (maybe if trace didn't have a :type field set)
_is3d(x::Any)= false

function _cat(nr::Int, nc::Int, ps::Plot...)
    copied_plots = Plot[copy(p) for p in ps]
    subplot_titles = any(map(x -> haskey(x.layout.fields, :title) ||
                                  haskey(x.layout.fields, "title"), ps))
    layout = gen_layout(nr, nc, subplot_titles)

    for col in 1:nc, row in 1:nr
        ix = LinearIndices((nc, nr))[col, row]
        handle_titles!(layout, copied_plots[ix].layout, ix)
        layout["xaxis$ix"] = merge(copied_plots[ix].layout["xaxis"], layout["xaxis$ix"])
        layout["yaxis$ix"] = merge(copied_plots[ix].layout["yaxis"], layout["yaxis$ix"])

        if _is3d(copied_plots[ix])
            # need to move (x|y)axis$ix into scene$ix here
            layout["scene$ix"] = attr(
                xaxis=pop!(layout, "xaxis$(ix)"),
                yaxis=pop!(layout, "yaxis$(ix)")
            )
            for trace in copied_plots[ix].data
                trace["scene"] = "scene$ix"
            end
        else
            for trace in copied_plots[ix].data
                trace["xaxis"] = "x$ix"
                trace["yaxis"] = "y$ix"
            end
        end

    end

    Plot(vcat([p.data for p in copied_plots]...), layout)
end

Base.hcat(ps::Plot...) = _cat(1, length(ps), ps...)
Base.vcat(ps::Plot...) = _cat(length(ps), 1,  ps...)
Base.vect(ps::Plot...) = vcat(ps...)

function Base.hvcat(rows::Tuple{Vararg{Int}}, ps::Plot...)
    nr = length(rows)
    nc = rows[1]

    for (i, c) in enumerate(rows[2:end])
        c == nc || error("Saw $c columns in row $(i+1), expected $nc")
    end
    _cat(nr, nc, ps...)
end
