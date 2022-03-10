
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

:(%)(@nospecialize(n::StaticInt), ::Type{Integer}) = Int(n)

eltype(@nospecialize(T::Type{<:StaticInt})) = Int
iszero(::Zero) = true
iszero(@nospecialize(x::StaticInt)) = false
isone(::One) = true
isone(@nospecialize(x::StaticInt)) = false
zero(@nospecialize(x::Type{<:StaticInt})) = Zero()
one(@nospecialize(x::Type{<:StaticInt})) = One()

for T in [:Real, :Rational, :Integer]
    @eval begin
        +(x::$T, @nospecialize(y::StaticInt)) = x + Int(y)
        +(@nospecialize(x::StaticInt), y::$T) = Int(x) + y
        -(x::$T, @nospecialize(y::StaticInt)) = x - Int(y)
        -(@nospecialize(x::StaticInt), - y::$T) = Int(x) - y
        *(x::$T, @nospecialize(y::StaticInt)) = x * Int(y)
        *(@nospecialize(x::StaticInt), y::$T) = Int(x) * y
    end
end
@inline :(-)(::StaticInt{M}) where {M} = StaticInt{-M}()

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
UnitRange(@nospecialize(start::StaticInt), @nospecialize(stop::StaticInt))
    UnitRange(Int(start), Int(stop))
end

