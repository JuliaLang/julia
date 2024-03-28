
"""
    StaticInt(N::Int) -> StaticInt{N}()

A statically sized `Int`. Use `StaticInt(N)` instead of `Val(N)` when you want it to behave like a number.
"""
struct StaticInt{N} <: Integer
    StaticInt{N}() where {N} = new{N::Int}()
    StaticInt(N::Int) = StaticInt{N}()
    StaticInt(N::StaticInt) = N
    StaticInt(::Val{N}) where {N} = StaticInt(N)
    StaticInt(N) = StaticInt(Int(N))
end

_dynamic_int(@nospecialize(x::StaticInt))::Int = _dynamic_int(typeof(x))
_dynamic_int(@nospecialize(x::Type{<:StaticInt}))::Int = x.parameters[1]

const Zero = StaticInt{0}
const One = StaticInt{1}
const IntType = Union{Int,StaticInt}
IntType(x::Integer) = Int(x)
IntType(x::IntType) = x

convert(::Type{T}, @nospecialize(N::StaticInt)) where {T<:Number} = convert(T, Int(N))
Bool(x::StaticInt{0}) = false
Bool(x::StaticInt{1}) = true

BigInt(@nospecialize(x::StaticInt)) = BigInt(Int(x))
Integer(x::StaticInt) = x
(::Type{T})(@nospecialize(x::StaticInt)) where {T<:Integer} = T(_dynamic_int(x))
(::Type{T})(x::Int) where {T<:StaticInt} = StaticInt(x)
convert(::Type{StaticInt{N}}, ::StaticInt{N}) where {N} = StaticInt{N}()

promote_rule(@nospecialize(T1::Type{<:StaticInt}), ::Type{T2}) where {T2<:Number} = promote_type(Int, T2)
promote_rule(@nospecialize(T1::Type{<:StaticInt}), ::Type{T2}) where {T2<:AbstractIrrational} = promote_type(Int, T2)
for (S, T) in [(:Complex, :Real), (:Rational, :Integer), (:(TwicePrecision), :Any)]
    @eval function promote_rule(::Type{$S{T}}, @nospecialize(SI::Type{<:StaticInt})) where {T<:$T}
        promote_type($S{T}, Int)
    end
end
promote_rule(::Type{Union{Nothing,Missing}}, @nospecialize(T::Type{<:StaticInt})) = Union{Nothing,Missing,Int}
promote_rule(::Type{T1}, @nospecialize(T2::Type{<:StaticInt})) where {T1>:Union{Missing,Nothing}} = promote_type(T1, Int)
promote_rule(::Type{T1}, @nospecialize(T2::Type{<:StaticInt})) where {T1>:Nothing} = promote_type(T1, Int)
promote_rule(::Type{T1}, @nospecialize(T2::Type{<:StaticInt})) where {T1>:Missing} = promote_type(T1, Int)
for T in [:Bool, :Missing, :BigFloat, :BigInt, :Nothing, :Any]
    # let S = :Any
    @eval begin
        promote_rule(@nospecialize(S::Type{<:StaticInt}), ::Type{$T}) = promote_type(Int, $T)
        promote_rule(::Type{$T}, @nospecialize(S::Type{<:StaticInt})) = promote_type($T, Int)
    end
end
promote_rule(@nospecialize(T1::Type{<:StaticInt}), @nospecialize(T2::Type{<:StaticInt})) = Int

%(@nospecialize(n::StaticInt), ::Type{Integer}) = Int(n)

eltype(@nospecialize(T::Type{<:StaticInt})) = Int
iszero(::Zero) = true
iszero(@nospecialize(x::StaticInt)) = false
isone(::One) = true
isone(@nospecialize(x::StaticInt)) = false
zero(@nospecialize(x::Type{<:StaticInt})) = Zero()
one(@nospecialize(x::Type{<:StaticInt})) = One()

for T in [:Real, :Rational, :Integer]
    for f in [:(-), :(+), :(*)]
        @eval begin
            $(f)(x::$T, @nospecialize(y::StaticInt)) = $(f)(x, Int(y))
            $(f)(@nospecialize(x::StaticInt), y::$T) = $(f)(Int(x), y)
        end
    end
end
@inline -(::StaticInt{M}) where {M} = StaticInt{-M}()

for f in [:(+), :(-), :(*), :(÷), :(%), :(<<), :(>>), :(>>>), :(&), :(|), :(⊻)]
    @eval begin
        @inline $f(::StaticInt{M}, ::StaticInt{N}) where {M,N} = StaticInt{$f(M,N)}()
    end
end
for f in [:(<<), :(>>), :(>>>)]
    @eval begin
        $f(@nospecialize(x::StaticInt), y::UInt) where {M} = $f(Int(x), y)
        $f(x::Integer, @nospecialize(y::StaticInt)) = $f(x, Int(y))
    end
end
for f in [:(==), :(!=), :(<), :(≤), :(>), :(≥)]
    @eval begin
        $f(::StaticInt{M}, ::StaticInt{N}) where {M,N} = $f(M, N)
        $f(@nospecialize(x::StaticInt), y::Int) where {M} = $f(Int(x), y)
        $f(x::Int, @nospecialize(y::StaticInt)) = $f(x, Int(y))
    end
end

widen(@nospecialize(x::StaticInt)) = Int(x)

UnitRange{T}(@nospecialize(start::StaticInt), stop) where {T<:Real} = UnitRange{T}(T(start), T(stop))
UnitRange{T}(start, @nospecialize(stop::StaticInt)) where {T<:Real} = UnitRange{T}(T(start), T(stop))
function UnitRange{T}(@nospecialize(start::StaticInt), @nospecialize(stop::StaticInt)) where {T<:Real}
    UnitRange{T}(T(start), T(stop))
end
UnitRange(@nospecialize(start::StaticInt), stop) = UnitRange(Int(start), stop)
UnitRange(start, @nospecialize(stop::StaticInt)) = UnitRange(start, Int(stop))
function UnitRange(@nospecialize(start::StaticInt), @nospecialize(stop::StaticInt))
    UnitRange(Int(start), Int(stop))
end

## ranges

"""
    OptionallyStaticUnitRange(start, stop) <: AbstractUnitRange{Int}

Similar to `UnitRange` except each field may be an `Int` or `StaticInt`. An
`OptionallyStaticUnitRange` is intended to be constructed internally from other valid
indices. Therefore, users should not expect the same checks are used to ensure construction
of a valid `OptionallyStaticUnitRange` as a `UnitRange`.
"""
struct OptionallyStaticUnitRange{F<:IntType,L<:IntType} <: AbstractUnitRange{Int}
    start::F
    stop::L

    function OptionallyStaticUnitRange(start::IntType, stop::IntType)
        new{typeof(start),typeof(stop)}(start, stop)
    end
   function OptionallyStaticUnitRange(start::Integer, stop::Integer)
        OptionallyStaticUnitRange(IntType(start), IntType(stop))
    end
    function OptionallyStaticUnitRange(x::AbstractRange)
        step(x) == 1 && return OptionallyStaticUnitRange(maybe_static_first(x), maybe_static_last(x))

        errmsg(x) = throw(ArgumentError("step must be 1, got $(step(x))")) # avoid GC frame
        errmsg(x)
    end
    OptionallyStaticUnitRange{F,L}(x::AbstractRange) where {F,L} = OptionallyStaticUnitRange(x)
    function OptionallyStaticUnitRange{StaticInt{F},StaticInt{L}}() where {F,L}
        new{StaticInt{F},StaticInt{L}}()
    end
end

"""
    OptionallyStaticStepRange(start, step, stop) <: OrdinalRange{Int,Int}

Similarly to [`OptionallyStaticUnitRange`](@ref), `OptionallyStaticStepRange` permits
a combination of static and standard primitive `Int`s to construct a range. It
specifically enables the use of ranges without a step size of 1. It may be constructed
through the use of `OptionallyStaticStepRange` directly or using static integers with
the range operator (i.e., `:`).
"""
struct OptionallyStaticStepRange{F<:IntType,S<:IntType,L<:IntType} <: OrdinalRange{Int,Int}
    start::F
    step::S
    stop::L

    function OptionallyStaticStepRange(start::IntType, step::IntType, stop::IntType)
        lst = _steprange_last(start, step, stop)
        new{typeof(start),typeof(step),typeof(lst)}(start, step, lst)
    end
    function OptionallyStaticStepRange(start::Integer, step::Integer, stop::Integer)
        OptionallyStaticStepRange(IntType(start), IntType(step), IntType(stop))
    end
    function OptionallyStaticStepRange(x::AbstractRange)
        OptionallyStaticStepRange(maybe_static_first(x), maybe_static_step(x), maybe_static_last(x))
    end
end

# to make StepRange constructor inlineable, so optimizer can see `step` value
@inline function _steprange_last(start::StaticInt, step::StaticInt, stop::StaticInt)
    StaticInt(_steprange_last(Int(start), Int(step), Int(stop)))
end
@inline function _steprange_last(start::Integer, step::StaticInt, stop::StaticInt)
    if step === one(step)
        # we don't need to check the `stop` if we know it acts like a unit range
        return stop
    else
        return _steprange_last(start, Int(step), Int(stop))
    end
end
@inline function _steprange_last(start::Integer, step::Integer, stop::Integer)
    z = zero(step)
    if step === z
        throw(ArgumentError("step cannot be zero"))
    else
        if stop == start
            return Int(stop)
        else
            if step > z
                if stop > start
                    return stop - Int(unsigned(stop - start) % step)
                else
                    return Int(start - one(start))
                end
            else
                if stop > start
                    return Int(start + one(start))
                else
                    return stop + Int(unsigned(start - stop) % -step)
                end
            end
        end
    end
end

const OptionallyStaticRange = Union{<:OptionallyStaticUnitRange,<:OptionallyStaticStepRange}

first(r::OptionallyStaticRange) = Int(r.start)

step(r::OptionallyStaticStepRange)= Int(r.step)

last(r::OptionallyStaticRange) = Int(r.stop)

lastindex(x::OptionallyStaticRange) = length(x)
function length(r::OptionallyStaticUnitRange)
    if isempty(r)
        return 0
    else
        return Int(r.stop - (r.start + One()))
    end
end
length(r::OptionallyStaticStepRange) = _range_length(r.start, r.step, r.stop)
_range_length(start, s, stop) = nothing
@inline function _range_length(start::IntType, s::IntType, stop::IntType)
   if s > 0
        if stop < start  # isempty
            return 0
        else
            return Int(div(stop - start, s)) + 1
        end
    else
        if stop > start  # isempty
            return 0
        else
            return Int(div(start - stop, -s)) + 1
        end
    end
end

AbstractUnitRange{Int}(r::OptionallyStaticUnitRange) = r
function AbstractUnitRange{T}(r::OptionallyStaticUnitRange) where {T}
    if known_first(r) === 1 && T <: Integer
        return OneTo{T}(last(r))
    else
        return UnitRange{T}(first(r), last(r))
    end
end

(:)(start::Integer, stop::StaticInt) = OptionallyStaticUnitRange(start, stop)
(:)(start::StaticInt, stop::Integer)  = OptionallyStaticUnitRange(start, stop)
(:)(start::StaticInt, stop::StaticInt) = OptionallyStaticUnitRange(start, stop)
function (:)(start::StaticInt, step::StaticInt, stop::StaticInt)
    OptionallyStaticStepRange(start, step, stop)
end
function (:)(start::Integer, step::StaticInt, stop::StaticInt)
    OptionallyStaticStepRange(start, step, stop)
end
function (:)(start::StaticInt, step::StaticInt, stop::Integer)
    OptionallyStaticStepRange(start, step, stop)
end
function (:)(start::StaticInt, step::Integer, stop::StaticInt)
    OptionallyStaticStepRange(start, step, stop)
end
function (:)(start::Integer, step::Integer, stop::StaticInt) 
    OptionallyStaticStepRange(start, step, stop)
end
function (:)(start::Integer, step::StaticInt, stop::Integer)
    OptionallyStaticStepRange(start, step, stop)
end
function (:)(start::StaticInt, step::Integer, stop::Integer)
    OptionallyStaticStepRange(start, step, stop)
end
(:)(start::StaticInt, ::StaticInt{1}, stop::StaticInt) = start:stop
(:)(start::Integer, ::StaticInt{1}, stop::StaticInt) = start:stop
(:)(start::StaticInt, ::StaticInt{1}, stop::Integer) = start:stop
function (:)(start::Integer, step::StaticInt{1}, stop::Integer)
    OptionallyStaticUnitRange(start, stop)
end
isempty(r::OptionallyStaticUnitRange) = r.start > r.stop
function isempty(r::OptionallyStaticStepRange)
    (r.start != r.stop) & ((r.step > 0) != (r.stop > r.start))
end

function getindex(r::OptionallyStaticUnitRange, s::AbstractUnitRange{<:Integer})
    @boundscheck checkbounds(r, s)
    f = r.start
    fnew = f - one(f)
    return (fnew+maybe_static_first(s)):(fnew+maybe_static_last(s))
end

function getindex(x::OptionallyStaticUnitRange, i::Int)::Int
    val = (x.start - One()) + i
    @boundscheck ((i < 1) || val > last(x)) && throw(BoundsError(x, i))
    val
end

## traits
"""
    known_first(::Type{T}) -> Union{Int,Nothing}

If `first` of an instance of type `T` is known at compile time, return it.
Otherwise, return `nothing`.
"""
known_first(x) = known_first(typeof(x))
known_first(::Type) = nothing
known_first(::Type{OneTo{T}}) where {T} = T(1)
known_first(::Type{<:OptionallyStaticUnitRange{StaticInt{F}}}) where {F} = F::Int
known_first(::Type{<:OptionallyStaticStepRange{StaticInt{F}}}) where {F} = F::Int
known_first(::Type{Slice{T}}) where {T} = known_first(T)
known_first(::Type{IdentityUnitRange{T}}) where {T} = known_first(T)

"""
    known_step(::Type{T}) -> Union{Int,Nothing}

If `step` of an instance of type `T` is known at compile time, return it.
Otherwise, return `nothing`.
"""
known_step(x) = known_step(typeof(x))
known_step(::Type) = nothing
known_step(T::Type{<:AbstractUnitRange}) = eltype(T)(1)
known_step(::Type{<:OptionallyStaticStepRange{<:Any,StaticInt{S}}}) where {S} = S::Int

"""
    known_last(::Type{T}) -> Union{Int,Nothing}

If `last` of an instance of type `T` is known at compile time, return it.
Otherwise, return `nothing`.
"""
known_last(x) = known_last(typeof(x))
known_last(::Type) = nothing
known_last(::Type{<:OptionallyStaticUnitRange{<:Any,StaticInt{L}}}) where {L} = L::Int
known_last(::Type{<:OptionallyStaticStepRange{<:Any,<:Any,StaticInt{L}}}) where {L} = L::Int
known_last(::Type{Slice{T}}) where {T} = known_last(T)
known_last(::Type{IdentityUnitRange{T}}) where {T} = known_last(T)

"""
    known_length(::Type{T}) -> Union{Int,Nothing}

If `length` of an instance of type `T` is known at compile time, return it.
Otherwise, return `nothing`.
"""
known_length(x) = known_length(typeof(x))
known_length(@nospecialize T::Type{<:Tuple}) = nfields(T)
known_length(@nospecialize T::Type{<:NamedTuple}) = fieldcount(T)
known_length(::Type{<:AbstractCartesianIndex{N}}) where {N} = N
function known_length(T::Type{<:AbstractRange})
    _range_length(known_first(T), known_step(T), known_last(T))
end

@inline maybe_static_length(x) = maybe_static(known_length, length, x)
@inline maybe_static_first(x) = maybe_static(known_first, first, x)
@inline maybe_static_last(x) = maybe_static(known_last, last, x)
@inline maybe_static_step(x) = maybe_static(known_step, step, x)
@inline function maybe_static(f::F, g::G, x) where {F,G}
    L = f(x)
    if L === nothing
        return g(x)
    else
        return StaticInt(L)
    end
end

