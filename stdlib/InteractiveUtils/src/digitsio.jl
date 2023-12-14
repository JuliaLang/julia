# an IO subtype that has user-configurable digits for floating-point
# output, designed to be used by `display` methods that want this,
# while not affecting output to other IO streams.  (Since this is
# for interactive displays only, performance is less of a concern.)
#
# To avoid method ambiguities for show(::IO, ::SomeFloatType),
# floating-point types defined by external packages will have
# to opt-in to supporting DigitsIO by defining a
# show(::InteractiveUtils.DigitsIO, ::SomeFloatType) method
# similar to the ones below.


########################################################################
# we define global defaults for float display, so that they can be set
# independent of the interactive display (REPL, IJulia, etc.)

export get_display_digits, set_display_digits, unset_display_digits

const DISPLAY_DIGITS = Dict{Type,Int}()
const DISPLAY_COMPACT_DIGITS = Dict{Type,Int}()

function get_display_digits(::Type{T}; compact::Bool=true) where {T<:AbstractFloat}
    if compact # compact_digits overrides digits if present
        d = _numdigits(DISPLAY_COMPACT_DIGITS, T)
        d > 0 && return d
    end
    d = _numdigits(DISPLAY_DIGITS, T)
    return d > 0 ? d : nothing
end

set_display_digits(::Type{T}, digits::Integer; compact::Bool=true) where {T<:AbstractFloat} =
    (compact ? DISPLAY_COMPACT_DIGITS : DISPLAY_DIGITS)[T] = digits

function unset_display_digits(::Type{T}; compact::Bool=true) where {T<:AbstractFloat} =
    delete!(compact ? DISPLAY_COMPACT_DIGITS : DISPLAY_DIGITS, T)
    return
end

########################################################################

struct DigitsIO{IO_T<:IO} <: IO
    io::IO_T # an underlying IO stream

    digits::Dict{Type,Int} # map from float types to # of sig. digits
    compact_digits::Dict{Type,Int} # overrides for :compact=>true contexts

    DigitsIO(io::IO, digits::AbstractDict, compact_digits::AbstractDict) =
        new{typeof(io)}(io, digits, compact_digits)
end

# extract default digits dict for an IO object
for d in (:digits, :compact_digits)
    DD = Symbol(uppercase(string("default_", d)))
    _dd = Symbol("_default_", d)
    @eval begin
        $_dd(io::IO) = $DD
        $_dd(io::DigitsIO) = io.$d
        $_dd(io::IOContext) = $_dd(io.io)
    end
end

# canonicalized DigitsIO constructor
digitsio(io::IO, digits::AbstractDict=_display_digits(io),
         compact_digits::AbstractDict=_display_compact_digits(io)) =
    DigitsIO(io, digits, compact_digits)

# combine nested DigitsIO wrappers
_merge(d1, d2) = d1 === d2 ? d1 : merge(d1, d2)
digitsio(io::DigitsIO, digits::AbstractDict=io.digits,
         compact_digits::AbstractDict=io.compact_digits) =
    digitsio(io.io, _merge(io.digits, digits),
             merge(io.compact_digits, compact_digits))

# canonicalize digitsio(::IOContext) to IOContext{DigitsIO}
digitsio(io::IOContext, digits::AbstractDict=_display_digits(io),
         compact_digits::AbstractDict=_display_compact_digits(io)) =
    IOContext(digitsio(io.io, digits, compact_digits), io.dict)

# delegate most calls to the underlying I/O stream, like IOContext:
Base.pipe_reader(io::DigitsIO) = Base.pipe_reader(io.io)
Base.pipe_writer(io::DigitsIO) = Base.pipe_writer(io.io)
Base.lock(io::DigitsIO) = lock(io.io)
Base.unlock(io::DigitsIO) = unlock(io.io)
Base.in(key_value::Pair, io::DigitsIO) = in(key_value, io.io)
Base.haskey(io::DigitsIO, key) = haskey(io.io, key)
Base.getindex(io::DigitsIO, key) = getindex(io.io, key)
Base.get(io::DigitsIO, key, default) = get(io.io, key, default)
Base.keys(io::DigitsIO) = keys(io.io)
Base.displaysize(io::DigitsIO) = displaysize(io.io)
Base.show_circular(io::DigitsIO, @nospecialize(x)) = Base.show_circular(io.io, x)

const DIO = Union{DigitsIO, IOContext{<:DigitsIO}}

Base.unwrapcontext(io::DigitsIO) = io, Base.unwrapcontext(io.io)[2]

# when a context is copied to a new io stream,
# the presence of a DigitsIO wrapper should be copied too.
Base.IOContext(io::IO, context::DIO) =
    IOContext(digitsio(unwrapcontext(io)[1]), unwrapcontext(context)[2])

########################################################################

# override the method in Ryu.jl
Base.show(io::DIO, x::Base.IEEEFloat, forceuntyped::Bool=false, fromprint::Bool=false) =
    _show(io, x, forceuntyped, fromprint)

# override the method in mpfr.jl
Base.show(io::DIO, x::BigFloat) = _show(io, x)

########################################################################

function numdigits(io::DigitsIO, ::Type{T}, compact::Bool=false) where {T}
    if compact # compact_digits overrides digits if present
        d = _numdigits(io.compact_digits, T)
        d > 0 && return d
    end
    return _numdigits(io.digits, T)
end
numdigits(io::IOContext{<:DigitsIO}, ::Type{T}, compact::Bool=false) where {T} =
    numdigits(io.io, T, compact)

_numdigits(digits, ::Type{Any}) = 0 # base case
_numdigits(digits, ::Type{T}) where {T} =
    get(() -> _numdigits(digits, supertype(T)), digits, T)

_string(x::Real, digits::Int) = @sprintf("%.*g", digits, x)
_string(x::BigFloat, digits::Int) = Base.MPFR._string(x, digits)

function _show(io::DIO, x::T, forceuntyped::Bool=false, fromprint::Bool=false) where {T <: AbstractFloat}
    compact = get(io, :compact, false)::Bool
    digits = numdigits(io, T, compact)
    if digits > 0
        typed = !forceuntyped && !compact && get(io, :typeinfo, Any) != typeof(x)
        s = _string(x, digits)
        if x isa Float32
            if contains(s, 'e')
                print(io.io, replace(s, 'e'=>'f'))
            elseif typed
                print(io.io, s, "f0")
                s = s * "f0"
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
