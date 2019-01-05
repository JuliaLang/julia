# Tests for SROA

mutable struct Foo30594; x::Float64; end
Base.copy(x::Foo30594) = Foo30594(x.x)
function add!(p::Foo30594, off::Foo30594)
    p.x += off.x
    return p
end
Base.:(+)(a::Foo30594, b::Foo30594) = add!(copy(a), b)

let results = Float64[]
    @noinline use30594(x) = push!(results, x.x); nothing
    function foo30594(cnt::Int, dx::Int)
        step = Foo30594(dx)
        curr = step + Foo30594(1)
        for i in 1:cnt
            use30594(curr)
            curr = curr + step
        end
        nothing
    end

    foo30594(4, -1)
    @test results == [0.0, -1.0, -2.0, -3.0]
end
