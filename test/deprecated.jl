# Once we drop 0.4 support, we want the following to return depwarns
# info("Beginning of tests with deprecation warnings")

v = 1
d = Dict{Int,Int}()
d[1] = 1
@test Compat.@Dict(1 => 1) == d
@test Compat.@Dict(1 => v) == d

@test typeof(@compat(Dict(1 => 1))) == Dict{Int,Int}
@test @compat(Dict(1 => 1)) == d
@test @compat(Dict(1 => v)) == d

ad = Dict{Any,Any}()
ad[1] = 1
@test Compat.@AnyDict(1 => 1) == ad
@test Compat.@AnyDict(1 => v) == ad

@test typeof(@compat(Dict{Any,Any}(1 => 1))) == Dict{Any,Any}
@test @compat(Dict{Any,Any}(1 => 1)) == ad
@test @compat(Dict{Any,Any}(1 => v)) == ad

td = Dict{Int,Float64}()
td[1] = 1.0

@test typeof(@compat(Dict{Int,Float64}(1 => 1))) == Dict{Int,Float64}
@test @compat(Dict{Int,Float64}(1 => 1)) == td
@test @compat(Dict{Int,Float64}(1 => v)) == td

@test @compat(Dict()) == Dict()
@test @compat(Dict{Any,Any}()) == Dict{Any,Any}()
@test @compat(Dict([(1, 1)])) == d
@test @compat(Dict(:Void => :Nothing)) == Dict([(:Void, :Nothing)])

d2 = Dict{Symbol,Dict{Symbol,Int}}()
d2[:a] = Dict{Symbol,Int}()
d2[:a][:b] = 1
@test @compat(Dict(:a => Dict(:b => 1))) == d2

d = Dict(zip([1, 2], [3, 4]))
@test d == @compat Dict(1=>3, 2=>4)

@compat function f()
    a = :a
    b = Dict(:b => 1)
    Dict(a => b)
end
@test f() == d2

module CartesianTest
    using Base.Cartesian, Compat
    @ngenerate N NTuple{N,Int} function f(X::NTuple{N,Int}...)
        @ncall N tuple X
    end
end

@test CartesianTest.f(1) == (1,)
@test CartesianTest.f(1,2) == (1,2)
@test CartesianTest.f(1,2,3) == (1,2,3)
@test CartesianTest.f(1,2,3,4) == (1,2,3,4)
@test CartesianTest.f(1,2,3,4,5) == (1,2,3,4,5)

@test Compat.KERNEL == Sys.KERNEL

@test String == @compat(Union{Compat.UTF8String,Compat.ASCIIString})

let x = fill!(StringVector(5), 0x61)
    @test pointer(x) == pointer(Compat.UTF8String(x))
end

foostring(::String) = 1
@test foostring("hello") == 1
@test foostring("λ") == 1
@test isa("hello", Compat.ASCIIString)
@test isa("λ", Compat.UTF8String)
