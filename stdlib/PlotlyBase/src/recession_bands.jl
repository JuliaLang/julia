# start with empty, it is easier to enumerate the true cases
to_date(x) = nothing

function to_date(x::AbstractString)
    # maybe it is yyyy
    if all(isdigit, x)
        return to_date(parse(Int, x))
    end

    # maybe it is yyyy-mm or yyyy-mm-dd
    parts = split(x, '-')
    nparts = length(parts)
    if nparts == 3
        try
            return Date(x, "y-m-d")
        catch
            return nothing
        end
    elseif nparts == 2
        try
            return Date(x, "y-m")
        catch
            return nothing
        end
    end

    # don't know how to handle anything else.
    return nothing
end

to_date(x::Union{Integer,Dates.TimeType}) = Date(x)

function to_date(x::AbstractArray{T,N}) where {T,N}
    out_arr = Date[]

    for i in x
        maybe_date = to_date(i)
        if maybe_date === nothing
            return nothing
        else
            push!(out_arr, maybe_date)
        end
    end
    reshape(out_arr, size(x))
end

to_date(x::AbstractArray{T,N}) where {T<:Dates.TimeType,N} = Date.(x)

#=
I populated recession_dates.tsv with the following (ugly!!) code

```
rec = get_data(f, "USREC", observation_start="1776-07-04").df[[:date, :value]]
did_start = fill(false, size(rec, 1))
did_end = copy(did_start)
did_start[1] = rec[1, :value] == 1.0
continuing_recession = did_start[1]
for i in 2:length(did_start)
    if rec[i, :value] == 1.0 && !continuing_recession
        did_start[i] = true
        continuing_recession = true
    end

    if rec[i, :value] == 0.0 && continuing_recession
        did_end[i] = true
        continuing_recession = false
    end
end

start_dates = rec[did_start, :date]
end_dates = rec[did_end, :date]

out_path = Pkg.dir("PlotlyBase", "recession_dates.tsv")
writedlm(out_path, [start_dates end_dates])
```
=#
function _recession_band_shapes(p::Plot; kwargs...)
    # data is 2 column matrix, first column is start date, second column
    # is end_date
    recession_data = map(Date, readdlm(
        joinpath(dirname(dirname(@__FILE__)), "recession_dates.tsv")
    ))::Matrix{Date}

    # keep track of which traces have dates for x
    n_trace = length(p.data)
    x_has_dates = fill(false, n_trace)

    # need to over the x attribute of each trace and get the most extreme dates
    # for each trace
    min_date = fill(Date(500_000), n_trace)
    max_date = fill(Date(-500_000), n_trace)
    dates_changed = fill(false, n_trace)
    for (i, t) in enumerate(p.data)
        t_x = t[:x]
        if !isempty(t_x)
            dates = to_date(t_x)
            dates === nothing && continue
            if typeof(dates) <: AbstractArray
                min_date[i] = min(min_date[i], minimum(dates))
                max_date[i] = max(max_date[i], maximum(dates))
                dates_changed[i] = true
                x_has_dates[i] = true
            end
        end
    end
    if !any(dates_changed)
        return nothing
    end

    # map traces into xaxis and yaxis
    xmap = trace_map(p, :x)
    ymap = trace_map(p, :y)

    # WANT: 1 set of shapes for each xaxis with dates
    n_xaxis = length(unique(xmap))

    band_kwargs = attr(
        fillcolor="#E6E6E6", opacity=0.4, line_width=0, layer="below",
        yref="paper", kwargs...
    )

    # if we only have one xaxis, only add one set of shapes
    if n_xaxis == 1
        ix1 = minimum(searchsortedfirst(recession_data[:, 1], m) for m in min_date)
        ix2 = maximum(searchsortedlast(recession_data[:, 2], m) for m in max_date)
        if isempty(ix1:ix2)
            return nothing
        end

        out = rect(
            recession_data[ix1:ix2, 1],
            recession_data[ix1:ix2, 2],
            0,
            1,
            band_kwargs.fields;
            xref="x"
        )
        return out
    end

    # otherwise we need to loop over the  traces and add one set of shapes
    # for each x axis
    out = Vector{Shape}()

    # first determine the min_date and max_date for each axis, based on that
    # info for each trace and the xmap.
    min_ax_date = fill(Date(500_000), n_xaxis)
    max_ax_date = fill(Date(-500_000), n_xaxis)
    for i in 1:n_trace
        ix = xmap[i]
        min_ax_date[ix] = min(min_ax_date[ix], min_date[ix])
        max_ax_date[ix] = max(max_ax_date[ix], max_date[ix])
    end

    # now loop through the traces and add one set of shapes per axis that
    # appears. We loop over traces so that we can be sure to get the correct
    # yaxis domain for each trace
    already_added = fill(false, n_xaxis)
    for i in 1:n_trace
        if x_has_dates[i]
            # move on if we have bands on this xax
            ix = xmap[i]
            already_added[ix] && continue

            # get starting and ending date
            ix1 = searchsortedfirst(recession_data[:, 1], min_ax_date[ix])
            ix2 = searchsortedlast(recession_data[:, 2], max_ax_date[ix])
            isempty(ix1:ix2) && continue  # range was bogus, move on

            # otherwise extract the yaxis details
            yax = p.layout[Symbol("yaxis", ymap[i])]

            isempty(yax) && continue  # don't have this axis somehow... move on

            # don't have info on where this axis lives ... move on
            !haskey(yax, :domain) && continue
            ybounds = yax[:domain]

            # we are in business, build the shapes and append them
            append!(out,
                rect(recession_data[ix1:ix2, 1],
                     recession_data[ix1:ix2, 2],
                     ybounds[1],
                     ybounds[2],
                     band_kwargs.fields;
                     xref=string("x", xmap[i]))
            )
            already_added[xmap[i]] = true
        end
    end
    out
end

function add_recession_bands!(p::Plot; kwargs...)
    bands = _recession_band_shapes(p; kwargs...)
    if bands === nothing
        return
    end

    # now we have some bands that we need to apply to the layout
    old_shapes = p.layout[:shapes]
    new_shapes = isempty(old_shapes) ? bands : vcat(old_shapes, bands)
    relayout!(p, shapes=new_shapes)
    new_shapes
end
