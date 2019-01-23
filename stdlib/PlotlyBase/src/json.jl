# -------------------------------- #
# Custom JSON output for our types #
# -------------------------------- #
JSON.lower(a::HasFields) = a.fields

function _apply_style_axis!(p::Plot, ax, force::Bool=false)
    if haskey(p.style.layout.fields, Symbol(ax, "axis")) || force
        ax_names = Iterators.filter(
            _x-> startswith(string(_x), "$(ax)axis"),
            keys(p.layout.fields)
        )

        for ax_name in ax_names
            cur = p.layout.fields[ax_name]
            cur = merge(p.style.layout[Symbol(ax, "axis")], cur)
            p.layout.fields[ax_name] = cur
        end

        if isempty(ax_names)
            nm = Symbol(ax, "axis")
            p.layout.fields[nm] = deepcopy(p.style.layout[nm])
        end
    end

end

_maybe_set_attr!(hf::HasFields, k::Symbol, v::Any) =
    get(hf, k, nothing) == nothing && setindex!(hf, v, k)

# special case for associative to get nested application
function _maybe_set_attr!(hf::HasFields, k1::Symbol, v::AbstractDict)
    for (k2, v2) in v
        _maybe_set_attr!(hf, Symbol(k1, "_", k2), v2)
    end
end

function _maybe_set_attr!(p::Plot, k1::Symbol, v::AbstractDict)
    for (k2, v2) in v
        _maybe_set_attr!(p, Symbol(k1, "_", k2), v2)
    end
end

function _maybe_set_attr!(p::Plot, k::Symbol, v)
    foreach(t -> _maybe_set_attr!(t, k, v), p.data)
end

function _maybe_set_attr!(p::Plot, k::Symbol, v::Cycler)
    ix = 0
    for t in p.data
        if t[k] == Dict()  # was empty
            t[k] = v[ix+=1]
        end
    end
end

function JSON.lower(p::Plot)
    _is3d = any(_x -> occursin("3d", string(_x[:type])), p.data)

    # apply layout attrs
    if !isempty(p.style.layout)
        # force xaxis and yaxis if plot is 2d
        _apply_style_axis!(p, "x", !_is3d)
        _apply_style_axis!(p, "y", !_is3d)
        _apply_style_axis!(p, "z")

        # extract this so we can pop! off xaxis and yaxis so they aren't
        # applied again
        la = deepcopy(p.style.layout)
        pop!(la.fields, :xaxis, nothing)
        pop!(la.fields, :yaxis, nothing)
        pop!(la.fields, :zaxis, nothing)
        p.layout = merge(la, p.layout)
    end

    # apply global trace attrs
    if !isempty(p.style.global_trace)
        for (k, v) in p.style.global_trace.fields
            _maybe_set_attr!(p, k, v)
        end
    end

    # apply trace specific attrs
    if !isempty(p.style.trace)
        for t in p.data
            t_type = Symbol(get(t, :type, :scatter))
            for (k, v) in get(p.style.trace, t_type, Dict())
                _maybe_set_attr!(p, k, v)
            end
        end
    end
    Dict(:data => p.data, :layout => p.layout)
end

# Let string interpolation stringify to JSON format
Base.print(io::IO, a::Union{Shape,GenericTrace,PlotlyAttribute,Layout,Plot}) = print(io, JSON.json(a))
Base.print(io::IO, a::Vector{T}) where {T<:GenericTrace} = print(io, JSON.json(a))

GenericTrace(d::AbstractDict{Symbol}) = GenericTrace(pop!(d, :type, "scatter"), d)
GenericTrace(d::AbstractDict{T}) where {T<:AbstractString} = GenericTrace(_symbol_dict(d))
Layout(d::AbstractDict{T}) where {T<:AbstractString} = Layout(_symbol_dict(d))

function JSON.parse(::Type{Plot}, str::AbstractString)
    d = JSON.parse(str)
    data = GenericTrace[GenericTrace(tr) for tr in d["data"]]
    layout = Layout(d["layout"])
    Plot(data, layout)
end

JSON.parsefile(::Type{Plot}, fn) =
    open(fn, "r") do f; JSON.parse(Plot, String(read(f))) end
