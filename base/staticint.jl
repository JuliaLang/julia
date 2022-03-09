
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

Base.convert(::Type{T}, @nospecialize(N::StaticInt)) where {T<:Number} = convert(T, Int(N))
Base.Bool(x::StaticInt{0}) = false
Base.Bool(x::StaticInt{1}) = true

Base.BigInt(@nospecialize(x::StaticInt)) = BigInt(Int(x))
Base.Integer(x::StaticInt) = x
(::Type{T})(@nospecialize(x::StaticInt)) where {T<:Integer} = T(_dynamic_int(x))
(::Type{T})(x::Int) where {T<:StaticInt} = StaticInt(x)
Base.convert(::Type{StaticInt{N}}, ::StaticInt{N}) where {N} = StaticInt{N}()

Base.promote_rule(@nospecialize(T1::Type{<:StaticInt}), ::Type{T2}) where {T2<:Number} = promote_type(Int, T2)
Base.promote_rule(@nospecialize(T1::Type{<:StaticInt}), ::Type{T2}) where {T2<:AbstractIrrational} = promote_type(Int, T2)
for (S, T) in [(:Complex, :Real), (:Rational, :Integer), (:(Base.TwicePrecision), :Any)]
    @eval function Base.promote_rule(::Type{$S{T}}, @nospecialize(SI::Type{<:StaticInt})) where {T<:$T}
        promote_type($S{T}, Int)
    end
end
Base.promote_rule(::Type{Union{Nothing,Missing}}, @nospecialize(T::Type{<:StaticInt})) = Union{Nothing,Missing,Int}
Base.promote_rule(::Type{T1}, @nospecialize(T2::Type{<:StaticInt})) where {T1>:Union{Missing,Nothing}} = promote_type(T1, Int)
Base.promote_rule(::Type{T1}, @nospecialize(T2::Type{<:StaticInt})) where {T1>:Nothing} = promote_type(T1, Int)
Base.promote_rule(::Type{T1}, @nospecialize(T2::Type{<:StaticInt})) where {T1>:Missing} = promote_type(T1, Int)
for T in [:Bool, :Missing, :BigFloat, :BigInt, :Nothing, :Any]
    # let S = :Any
    @eval begin
        Base.promote_rule(@nospecialize(S::Type{<:StaticInt}), ::Type{$T}) = promote_type(Int, $T)
        Base.promote_rule(::Type{$T}, @nospecialize(S::Type{<:StaticInt})) = promote_type($T, Int)
    end
end
Base.promote_rule(@nospecialize(T1::Type{<:StaticInt}), @nospecialize(T2::Type{<:StaticInt})) = Int

Base.:(%)(@nospecialize(n::StaticInt), ::Type{Integer}) = Int(n)

Base.eltype(@nospecialize(T::Type{<:StaticInt})) = Int
Base.iszero(::Zero) = true
Base.iszero(@nospecialize(x::StaticInt)) = false
Base.isone(::One) = true
Base.isone(@nospecialize(x::StaticInt)) = false
Base.zero(@nospecialize(x::Type{<:StaticInt})) = Zero()
Base.one(@nospecialize(x::Type{<:StaticInt})) = One()

for T in [:Real, :Rational, :Integer]
    @eval begin
        @inline Base.:(+)(x::$T, y::Zero) = x
        @inline Base.:(+)(x::$T, @nospecialize(y::StaticInt)) = x + Int(y)
        @inline Base.:(+)(x::Zero, y::$T) = x
        @inline Base.:(+)(@nospecialize(x::StaticInt), y::$T) = Int(x) + y
        @inline Base.:(-)(x::$T, y::Zero) = x
        @inline Base.:(-)(x::$T, @nospecialize(y::StaticInt)) = x - Int(y)
        @inline Base.:(*)(x::$T, y::Zero) = Zero()
        @inline Base.:(*)(x::$T, y::One) = x
        @inline Base.:(*)(x::$T, @nospecialize(y::StaticInt)) = x * Int(y)
        @inline Base.:(*)(x::Zero, y::$T) = Zero()
        @inline Base.:(*)(x::One, y::$T) = y
        @inline Base.:(*)(@nospecialize(x::StaticInt), y::$T) = Int(x) * y
    end
end
@inline Base.:(+)(::Zero, ::Zero) = Zero()
@inline Base.:(+)(::Zero, ::StaticInt{M}) where {M} = StaticInt{M}()
@inline Base.:(+)(::StaticInt{M}, ::Zero) where {M} = StaticInt{M}()

@inline Base.:(-)(::StaticInt{M}) where {M} = StaticInt{-M}()
@inline Base.:(-)(::StaticInt{M}, ::Zero) where {M} = StaticInt{M}()

@inline Base.:(*)(::Zero, ::Zero) = Zero()
@inline Base.:(*)(::One, ::Zero) = Zero()
@inline Base.:(*)(::Zero, ::One) = Zero()
@inline Base.:(*)(::One, ::One) = One()
@inline Base.:(*)(::StaticInt{M}, ::Zero) where {M} = Zero()
@inline Base.:(*)(::Zero, ::StaticInt{M}) where {M} = Zero()
@inline Base.:(*)(::StaticInt{M}, ::One) where {M} = StaticInt{M}()
@inline Base.:(*)(::One, ::StaticInt{M}) where {M} = StaticInt{M}()
for f in [:(+), :(-), :(*), :(/), :(÷), :(%), :(<<), :(>>), :(>>>), :(&), :(|), :(⊻)]
    @eval @generated function Base.$f(::StaticInt{M}, ::StaticInt{N}) where {M,N}
        return Expr(:call, Expr(:curly, :StaticInt, $f(M, N)))
    end
end
for f in [:(<<), :(>>), :(>>>)]
    @eval begin
        @inline Base.$f(@nospecialize(x::StaticInt), y::UInt) where {M} = $f(Int(x), y)
        @inline Base.$f(x::Integer, @nospecialize(y::StaticInt)) = $f(x, Int(y))
    end
end
for f in [:(==), :(!=), :(<), :(≤), :(>), :(≥)]
    @eval begin
        @inline Base.$f(::StaticInt{M}, ::StaticInt{N}) where {M,N} = $f(M, N)
        @inline Base.$f(@nospecialize(x::StaticInt), y::Int) where {M} = $f(Int(x), y)
        @inline Base.$f(x::Int, @nospecialize(y::StaticInt)) = $f(x, Int(y))
    end
end

@inline function maybe_static(f::F, g::G, x) where {F,G}
    L = f(x)
    if L === nothing
        return g(x)
    else
        return static(L)
    end
end

@inline Base.widen(::StaticInt{N}) where {N} = widen(N)

Base.UnitRange{T}(@nospecialize(start::StaticInt), stop) where {T<:Real} = UnitRange{T}(T(start), T(stop))
Base.UnitRange{T}(start, @nospecialize(stop::StaticInt)) where {T<:Real} = UnitRange{T}(T(start), T(stop))
function Base.UnitRange{T}(@nospecialize(start::StaticInt), @nospecialize(stop::StaticInt)) where {T<:Real}
    UnitRange{T}(T(start), T(stop))
end

Base.UnitRange(@nospecialize(start::StaticInt), stop) = UnitRange(Int(start), stop)
Base.UnitRange(start, @nospecialize(stop::StaticInt)) = UnitRange(start, Int(stop))
function Base.UnitRange(@nospecialize(start::StaticInt), @nospecialize(stop::StaticInt))
    UnitRange(Int(start), Int(stop))
end

