# an IO subtype that has user-configurable precision for floating-point
# output, designed to be used by `display` methods that want this,
# while not affecting output to other IO streams.  (Since this is
# for interactive displays only, performance is less of a concern.)
#
# To avoid method ambiguities for show(::IO, ::SomeFloatType),
# floating-point types defined by external packages will have
# to opt-in to supporting PrecisionIO by defining a
# show(::InteractiveUtils.PrecisionIO, ::SomeFloatType) method
# similar to the ones below.

using Printf

########################################################################

export set_display_precision!, unset_display_precision!

# we define global defaults for float display, so that they can be set
# independent of the interactive display (REPL, IJulia, etc.)
const DISPLAY_PRECISION = Dict{Type,Union{Int,String}}()
const DISPLAY_COMPACT_PRECISION = Dict{Type,Union{Int,String}}()

# ensure a valid precision argument
checkprecision(::Type{T}, precision::Integer) where {T<:AbstractFloat} =
    precision > 0 || throw(ArgumentError("significant digits = $precision must be positive"))
checkprecision(::Type{T}, precision::AbstractString) where {T<:AbstractFloat} =
    Printf.format(Printf.Format(precision), zero(T)) isa AbstractString || throw(ArgumentError("Printf.format failed to return a string"))

function set_display_precision!(::Type{T}, precision::Union{Integer,AbstractString}; compact::Bool=false) where {T<:AbstractFloat}
    checkprecision(T, precision)
    foreach(Base.Multimedia.displays) do d
        set_display_precision!(d, T, precision; compact)
    end
    (compact ? DISPLAY_COMPACT_PRECISION : DISPLAY_PRECISION)[T] = precision
    return
end

function unset_display_precision!(::Type{T}; compact::Bool=false) where {T<:AbstractFloat}
    foreach(Base.Multimedia.displays) do d
        unset_display_precision!(d, T; compact)
    end
    delete!(compact ? DISPLAY_COMPACT_PRECISION : DISPLAY_PRECISION, T)
    return
end

# type defaults to AbstractFloat:
set_display_precision!(precision::Union{Integer,AbstractString}; compact::Bool=false) =
    set_display_precision!(AbstractFloat, precision; compact)
unset_display_precision!(; compact::Bool=false) =
    unset_display_precision!(AbstractFloat; compact)
set_display_precision!(d::AbstractDisplay, precision::Union{Integer,AbstractString}; compact::Bool=false) =
    set_display_precision!(d, AbstractFloat, precision; compact)
unset_display_precision!(d::AbstractDisplay, compact::Bool=false) =
    unset_display_precision!(d, AbstractFloat; compact)

# display subtypes should override these if they store their own precision settings:
set_display_precision!(d::AbstractDisplay, ::Type, precision::Union{Integer,AbstractString}; compact::Bool=false) = nothing
unset_display_precision!(d::AbstractDisplay, ::Type; compact::Bool=false) = nothing

########################################################################

struct PrecisionIO{IO_T<:IO} <: Base.AbstractPipe
    io::IO_T # an underlying IO stream

    precision::Dict{Type,Union{Int,String}} # map from float types to # of sig. digits / format string
    compact_precision::Dict{Type,Union{Int,String}} # overrides for :compact=>true contexts

    PrecisionIO(io::IO, precision::AbstractDict, compact_precision::AbstractDict) =
        new{typeof(io)}(io, precision, compact_precision)
end

# extract default precision dict for an IO object
for d in (:precision, :compact_precision)
    DD = Symbol(uppercase(string("DISPLAY_", d)))
    _dd = Symbol("_default_", d)
    @eval begin
        $_dd(io::IO) = $DD
        $_dd(io::PrecisionIO) = io.$d
        $_dd(io::IOContext) = $_dd(io.io)
    end
end

# canonicalized PrecisionIO constructor
precisionio(io::IO, precision::AbstractDict=_default_precision(io),
         compact_precision::AbstractDict=_default_compact_precision(io)) =
    PrecisionIO(io, precision, compact_precision)

# combine nested PrecisionIO wrappers
_merge(d1, d2) = d1 === d2 ? d1 : merge(d1, d2)
precisionio(io::PrecisionIO, precision::AbstractDict=io.precision,
         compact_precision::AbstractDict=io.compact_precision) =
    precisionio(io.io, _merge(io.precision, precision),
             _merge(io.compact_precision, compact_precision))

# canonicalize precisionio(::IOContext) to IOContext{PrecisionIO}
precisionio(io::IOContext, precision::AbstractDict=_default_precision(io),
         compact_precision::AbstractDict=_default_compact_precision(io)) =
    IOContext(precisionio(io.io, precision, compact_precision), io.dict)

# delegate most calls to the underlying I/O stream, like IOContext:
Base.pipe_reader(io::PrecisionIO) = io.io
Base.pipe_writer(io::PrecisionIO) = io.io
Base.lock(io::PrecisionIO) = lock(io.io)
Base.unlock(io::PrecisionIO) = unlock(io.io)
Base.in(key_value::Pair, io::PrecisionIO) = in(key_value, io.io)
Base.haskey(io::PrecisionIO, key) = haskey(io.io, key)
Base.getindex(io::PrecisionIO, key) = getindex(io.io, key)
Base.get(io::PrecisionIO, key, default) = get(io.io, key, default)
Base.keys(io::PrecisionIO) = keys(io.io)
Base.displaysize(io::PrecisionIO) = displaysize(io.io)
Base.show_circular(io::PrecisionIO, @nospecialize(x)) = Base.show_circular(io.io, x)

const DIO = Union{PrecisionIO, IOContext{<:PrecisionIO}}

Base.unwrapcontext(io::PrecisionIO) = io, Base.unwrapcontext(io.io)[2]

# when a context is copied to a new io stream,
# the presence of a PrecisionIO wrapper should be copied too.
Base.IOContext(io::IO, context::DIO) =
    IOContext(precisionio(Base.unwrapcontext(io)[1]), Base.unwrapcontext(context)[2])

########################################################################

# override the method in Ryu.jl
Base.show(io::DIO, x::Base.IEEEFloat, forceuntyped::Bool=false, fromprint::Bool=false) =
    _show(io, x, forceuntyped, fromprint)

# override the method in mpfr.jl
Base.show(io::DIO, x::BigFloat) = _show(io, x)

########################################################################

function displayprecision(io::PrecisionIO, ::Type{T}, compact::Bool=false) where {T}
    if compact # compact_precision overrides precision if present
        d = _displayprecision(io.compact_precision, T)
        !isnothing(d) && return d
    end
    return _displayprecision(io.precision, T)
end
displayprecision(io::IOContext{<:PrecisionIO}, ::Type{T}, compact::Bool=false) where {T} =
    displayprecision(io.io, T, compact)

_displayprecision(precision, ::Type{Any}) = nothing # base case
_displayprecision(precision, ::Type{T}) where {T} =
    get(() -> _displayprecision(precision, supertype(T)), precision, T)

_string(x::Real, precision::Int) = @sprintf("%.*g", precision, x)
_string(x::BigFloat, precision::Int) = Base.MPFR._string(x, precision)
_string(x::Real, precision::String) = Printf.format(Printf.Format(precision), x)

function _show(io::DIO, x::T, forceuntyped::Bool=false, fromprint::Bool=false) where {T <: AbstractFloat}
    compact = get(io, :compact, false)::Bool
    precision = displayprecision(io, T, compact)
    if !isnothing(precision)
        typed = !forceuntyped && !compact && get(io, :typeinfo, Any) != typeof(x)
        s = _string(x, precision)
        if x isa Float32 && !fromprint
            if contains(s, 'e')
                print(io.io, replace(s, 'e'=>'f'))
            elseif typed
                print(io.io, s, "f0")
            else
                print(io.io, s)
            end
        elseif x isa Float16 && typed
            print(io.io, "Float16(", s, ')')
        else
            print(io.io, s)
        end
    else
        if forceuntyped || fromprint # need 4-argument show
            show(io.io, x, forceuntyped, fromprint)
        else
            show(io.io, x)
        end
    end
    return
end
