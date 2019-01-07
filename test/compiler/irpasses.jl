# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

# Issue #29983
# This one is a bit hard to trigger, but the key is to create a case
# where SROA needs to introduce an intermediate type-unstable phi node
struct Foo29983{T}
    x::Tuple{T}
end
struct Bar29983{S}
    x::S
end
Base.:+(a::T, b::Bar29983{S}) where {T, S} = Bar29983(a + b.x)
Base.:+(a::Bar29983{S}, b::T) where {T, S} = b + a
Base.:+(a::Bar29983{S}, b::Bar29983{T}) where {T, S} = Bar29983(a.x + b.x)
Base.:+(a::Foo29983, b::Foo29983) = Foo29983((a.x[1] + b.x[1],))

function f(x::Vector{T}) where {T}
    x1 = Foo29983((x[1],))
    la1 = Foo29983((x[1],))
    f1 = Foo29983((0,))
    for _ in 1:2
        f1 += la1
    end
    return f1
end

@test f([Bar29983(1.0)]).x[1].x == 2.0
