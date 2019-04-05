using Base.Experimental: ImmutableArray
function simple()
    a = Vector{Float64}(undef, 5)
    for i = 1:5
        a[i] = i
    end
    ImmutableArray(a)
end
let
    @allocated(simple())
    @test @allocated(simple()) < 100
end
