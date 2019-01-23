abstract type AbstractTrace end
abstract type AbstractLayout end

mutable struct GenericTrace{T<:AbstractDict{Symbol,Any}} <: AbstractTrace
    fields::T
end

function GenericTrace(kind::Union{AbstractString,Symbol},
                      fields=Dict{Symbol,Any}(); kwargs...)
    # use setindex! methods below to handle `_` substitution
    fields[:type] = kind
    gt = GenericTrace(fields)
    foreach(x->setindex!(gt, x[2], x[1]), kwargs)
    gt
end

const _layout_defaults = Dict{Symbol,Any}(:margin => Dict(:l=>50, :r=>50, :t=>60, :b=>50))

mutable struct Layout{T<:AbstractDict{Symbol,Any}} <: AbstractLayout
    fields::T

    function Layout{T}(fields::T; kwargs...) where T
        l = new{T}(merge(_layout_defaults, fields))
        foreach(x->setindex!(l, x[2], x[1]), kwargs)
        l
    end
end

Layout(fields::T=Dict{Symbol,Any}(); kwargs...) where {T<:AbstractDict{Symbol,Any}} =
    Layout{T}(fields; kwargs...)

kind(gt::GenericTrace) = get(gt, :type, "scatter")
kind(l::Layout) = "layout"

# -------------------------------------------- #
# Specific types of trace or layout attributes #
# -------------------------------------------- #
abstract type AbstractPlotlyAttribute end

mutable struct PlotlyAttribute{T<:AbstractDict{Symbol,Any}} <: AbstractPlotlyAttribute
    fields::T
end

function attr(fields=Dict{Symbol,Any}(); kwargs...)
    # use setindex! methods below to handle `_` substitution
    s = PlotlyAttribute(fields)
    for (k, v) in kwargs
        s[k] = v
    end
    s
end

abstract type AbstractLayoutAttribute <: AbstractPlotlyAttribute end
abstract type AbstractShape <: AbstractLayoutAttribute end

kind(::AbstractPlotlyAttribute) = "PlotlyAttribute"

# TODO: maybe loosen some day
const _Scalar = Union{Date,Number,AbstractString,Symbol}

# ------ #
# Shapes #
# ------ #

mutable struct Shape <: AbstractLayoutAttribute
    fields::AbstractDict{Symbol}
end

function Shape(kind::AbstractString, fields=Dict{Symbol,Any}(); kwargs...)
    # use setindex! methods below to handle `_` substitution
    fields[:type] = kind
    s = Shape(fields)
    foreach(x->setindex!(s, x[2], x[1]), kwargs)
    s
end

# helper method needed below
_rep(x, n) = take(cycle(x), n)

# line, circle, and rect share same x0, x1, y0, y1 args. Define methods for
# them here
for t in [:line, :circle, :rect]
    str_t = string(t)
    @eval $t(d::AbstractDict=Dict{Symbol,Any}(), ;kwargs...) =
        Shape($str_t, d; kwargs...)
    eval(Expr(:export, t))

    @eval function $(t)(x0::_Scalar, x1::_Scalar, y0::_Scalar, y1::_Scalar,
                        fields::AbstractDict=Dict{Symbol,Any}(); kwargs...)
        $(t)(fields; x0=x0, x1=x1, y0=y0, y1=y1, kwargs...)
    end

    @eval function $(t)(x0::Union{AbstractVector,_Scalar},
                        x1::Union{AbstractVector,_Scalar},
                        y0::Union{AbstractVector,_Scalar},
                        y1::Union{AbstractVector,_Scalar},
                        fields::AbstractDict=Dict{Symbol,Any}(); kwargs...)
        n = reduce(max, map(length, (x0, x1, y0, y1)))
        f(_x0, _x1, _y0, _y1) = $(t)(_x0, _x1, _y0, _y1, copy(fields); kwargs...)
        map(f, _rep(x0, n), _rep(x1, n), _rep(y0, n), _rep(y1, n))
    end
end

@doc "Draw a line through the points (x0, y0) and (x1, y2)" line

@doc """
Draw a circle from ((`x0`+`x1`)/2, (`y0`+`y1`)/2)) with radius
 (|(`x0`+`x1`)/2 - `x0`|, |(`y0`+`y1`)/2 -`y0`)|) """ circle

@doc """
Draw a rectangle linking (`x0`,`y0`), (`x1`,`y0`),
(`x1`,`y1`), (`x0`,`y1`), (`x0`,`y0`)""" rect


"Draw an arbitrary svg path"
path(p::AbstractString; kwargs...) = Shape("path"; path=p, kwargs...)

export path

# derived shapes

vline(x, ymin, ymax, fields::AbstractDict=Dict{Symbol,Any}(); kwargs...) =
    line(x, x, ymin, ymax, fields; kwargs...)

"""
`vline(x, fields::AbstractDict=Dict{Symbol,Any}(); kwargs...)`

Draw vertical lines at each point in `x` that span the height of the plot
"""
vline(x, fields::AbstractDict=Dict{Symbol,Any}(); kwargs...) =
    vline(x, 0, 1, fields; xref="x", yref="paper", kwargs...)

hline(y, xmin, xmax, fields::AbstractDict=Dict{Symbol,Any}(); kwargs...) =
    line(xmin, xmax, y, y, fields; kwargs...)

"""
`hline(y, fields::AbstractDict=Dict{Symbol,Any}(); kwargs...)`

Draw horizontal lines at each point in `y` that span the width of the plot
"""
hline(y, fields::AbstractDict=Dict{Symbol,Any}(); kwargs...) =
    hline(y, 0, 1, fields; xref="paper", yref="y", kwargs...)

# ---------------------------------------- #
# Implementation of getindex and setindex! #
# ---------------------------------------- #

const HasFields = Union{GenericTrace,Layout,Shape,PlotlyAttribute}
const _LikeAssociative = Union{PlotlyAttribute,AbstractDict}

#= NOTE: Generate this list with the following code
using JSON, PlotlyJS
d = JSON.parsefile(Pkg.dir("PlotlyJS", "deps", "plotschema.json"))
d = PlotlyJS._symbol_dict(d)

nms = Set{Symbol}()
function add_to_names!(d::AbstractDict)
    map(add_to_names!, keys(d))
    map(add_to_names!, values(d))
    nothing
end
add_to_names!(s::Symbol) = push!(nms, s)
add_to_names!(x) = nothing

add_to_names!(d[:schema][:layout][:layoutAttributes])
for (_, v) in d[:schema][:traces]
    add_to_names!(v)
end

_UNDERSCORE_ATTRS = collect(
    filter(
        x-> contains(string(x), "_") && !startswith(string(x), "_"),
        nms
    )
)

=#
const _UNDERSCORE_ATTRS = [:error_x, :copy_ystyle, :error_z, :plot_bgcolor,
                           :paper_bgcolor, :copy_zstyle, :error_y]

function Base.merge(hf::HasFields, d::Dict)
    out = deepcopy(hf)
    for (k, v) in d
        out[k] = d
    end
    out
end

function Base.merge!(hf1::HasFields, hf2::HasFields)
    for (k, v) in hf2.fields
        hf1[k] = v
    end
    hf1
end

Base.haskey(hf::HasFields, k::Symbol) = haskey(hf.fields, k)

Base.merge(hf1::T, hf2::T) where {T<:HasFields} =
    merge!(deepcopy(hf1), hf2)

Base.isempty(hf::HasFields) = isempty(hf.fields)

function Base.get(hf::HasFields, k::Symbol, default)
    out = getindex(hf, k)
    (out == Dict()) ? default : out
end

Base.iterate(hf::HasFields) = iterate(hf.fields)
Base.iterate(hf::HasFields, x) = iterate(hf.fields, x)

==(hf1::T, hf2::T) where {T<:HasFields} = hf1.fields == hf2.fields

# methods that allow you to do `obj["first.second.third"] = val`
function Base.setindex!(gt::HasFields, val, key::String)
    if in(Symbol(key), _UNDERSCORE_ATTRS)
        return gt.fields[Symbol(key)] = val
    else
        return setindex!(gt, val, map(Symbol, split(key, ['.', '_']))...)
    end
end

Base.setindex!(gt::HasFields, val, keys::String...) =
    setindex!(gt, val, map(Symbol, keys)...)

# Now for deep setindex. The deepest the json schema ever goes is 4 levels deep
# so we will simply write out the setindex calls for 4 levels by hand. If the
# schema gets deeper in the future we can @generate them with @nexpr
function Base.setindex!(gt::HasFields, val, key::Symbol)
    # check if single key has underscores, if so split at str and call above
    # unless it is one of the special attribute names with an underscore
    if occursin("_", string(key))

        if !in(key, _UNDERSCORE_ATTRS)
            return setindex!(gt, val, string(key))
        end
    end
    gt.fields[key] = val
end

function Base.setindex!(gt::HasFields, val, k1::Symbol, k2::Symbol)
    d1 = get(gt.fields, k1, Dict())
    d1[k2] = val
    gt.fields[k1] = d1
    val
end

function Base.setindex!(gt::HasFields, val, k1::Symbol, k2::Symbol, k3::Symbol)
    d1 = get(gt.fields, k1, Dict())
    d2 = get(d1, k2, Dict())
    d2[k3] = val
    d1[k2] = d2
    gt.fields[k1] = d1
    val
end

function Base.setindex!(gt::HasFields, val, k1::Symbol, k2::Symbol,
                        k3::Symbol, k4::Symbol)
    d1 = get(gt.fields, k1, Dict())
    d2 = get(d1, k2, Dict())
    d3 = get(d2, k3, Dict())
    d3[k4] = val
    d2[k3] = d3
    d1[k2] = d2
    gt.fields[k1] = d1
    val
end

#= NOTE: I need to special case instances when `val` is Associatve like so that
         I can partially update something that already exists.

Example:

hf = Layout(font_size=10)
val = Layout(font_family="Helvetica")

=#
function Base.setindex!(gt::HasFields, val::_LikeAssociative, key::Symbol)
    if occursin("_", string(key))

        if !in(key, _UNDERSCORE_ATTRS)
            return setindex!(gt, val, string(key))
        end
    end

    for (k, v) in val
        setindex!(gt, v, key, k)
    end
end

function Base.setindex!(gt::HasFields, val::_LikeAssociative, k1::Symbol,
                        k2::Symbol)
    for (k, v) in val
        setindex!(gt, v, k1, k2, k)
    end
end

function Base.setindex!(gt::HasFields, val::_LikeAssociative, k1::Symbol,
                        k2::Symbol, k3::Symbol)
    for (k, v) in val
        setindex!(gt, v, k1, k2, k3, k)
    end
end


# now on to the simpler getindex methods. They will try to get the desired
# key, but if it doesn't exist an empty dict is returned
function Base.getindex(gt::HasFields, key::String)
    if in(Symbol(key), _UNDERSCORE_ATTRS)
        gt.fields[Symbol(key)]
    else
        getindex(gt, map(Symbol, split(key, ['.', '_']))...)
    end
end

Base.getindex(gt::HasFields, keys::String...) =
    getindex(gt, map(Symbol, keys)...)

function Base.getindex(gt::HasFields, key::Symbol)
    if occursin("_", string(key))
        if !in(key, _UNDERSCORE_ATTRS)
            return getindex(gt, string(key))
        end
    end
    get(gt.fields, key, Dict())
end

function Base.getindex(gt::HasFields, k1::Symbol, k2::Symbol)
    d1 = get(gt.fields, k1, Dict())
    get(d1, k2, Dict())
end

function Base.getindex(gt::HasFields, k1::Symbol, k2::Symbol, k3::Symbol)
    d1 = get(gt.fields, k1, Dict())
    d2 = get(d1, k2, Dict())
    get(d2, k3, Dict())
end

function Base.getindex(gt::HasFields, k1::Symbol, k2::Symbol,
                       k3::Symbol, k4::Symbol)
    d1 = get(gt.fields, k1, Dict())
    d2 = get(d1, k2, Dict())
    d3 = get(d2, k3, Dict())
    get(d3, k4, Dict())
end

# Now to the pop! methods
function Base.pop!(gt::HasFields, key::String)
    if in(Symbol(key), _UNDERSCORE_ATTRS)
        pop!(gt.fields, Symbol(key))
    else
        pop!(gt, map(Symbol, split(key, ['.', '_']))...)
    end
end

Base.pop!(gt::HasFields, keys::String...) =
    pop!(gt, map(Symbol, keys)...)

function Base.pop!(gt::HasFields, key::Symbol)
    if occursin("_", string(key))
        if !in(key, _UNDERSCORE_ATTRS)
            return pop!(gt, string(key))
        end
    end
    pop!(gt.fields, key, Dict())
end

function Base.pop!(gt::HasFields, k1::Symbol, k2::Symbol)
    d1 = get(gt.fields, k1, Dict())
    pop!(d1, k2, Dict())
end

function Base.pop!(gt::HasFields, k1::Symbol, k2::Symbol, k3::Symbol)
    d1 = get(gt.fields, k1, Dict())
    d2 = get(d1, k2, Dict())
    pop!(d2, k3, Dict())
end

function Base.pop!(gt::HasFields, k1::Symbol, k2::Symbol,
                       k3::Symbol, k4::Symbol)
    d1 = get(gt.fields, k1, Dict())
    d2 = get(d1, k2, Dict())
    d3 = get(d2, k3, Dict())
    pop!(d3, k4, Dict())
end

# Function used to have meaningful display of traces and layouts
function _describe(x::HasFields)
    fields = sort(map(String, collect(keys(x.fields))))
    n_fields = length(fields)
    if n_fields == 0
        return "$(kind(x)) with no fields"
    elseif n_fields == 1
        return "$(kind(x)) with field $(fields[1])"
    elseif n_fields == 2
        return "$(kind(x)) with fields $(fields[1]) and $(fields[2])"
    else
        return "$(kind(x)) with fields $(join(fields, ", ", ", and "))"
    end
end

Base.show(io::IO, ::MIME"text/plain", g::HasFields) =
    println(io, _describe(g))
