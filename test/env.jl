# This file is a part of Julia. License is MIT: http://julialang.org/license

@test !("f=a=k=e=n=a=m=e" ∈ keys(ENV))

# issue #10994
@test_throws ArgumentError ENV["bad\0name"] = "ok"
@test_throws ArgumentError ENV["okname"] = "bad\0val"
@test_throws ArgumentError Sys.set_process_title("bad\0title")

withenv("bad"=>"dog") do
    @test_throws ArgumentError ENV["bad\0cat"]
end

# issue #11170
withenv("TEST"=>"nonempty") do
    @test ENV["TEST"] == "nonempty"
end
withenv("TEST"=>"") do
    @test ENV["TEST"] == ""
end

let c = collect(ENV)
    @test isa(c, Vector)
    @test length(ENV) == length(c)
    @test isempty(ENV) || first(ENV) in c
end

# test for non-existent keys
key = randstring(25)
@test !haskey(ENV,key)
@test_throws KeyError ENV[key]
@test get(ENV,key,"default") == "default"

# Test for #17956
@test length(ENV) > 1
k1, k2 = "__test__", "__test1__"
withenv(k1=>k1, k2=>k2) do
    b_k1, b_k2 = false, false
    for (k, v) in ENV
        if k==k1
            b_k1=true
        elseif k==k2
            b_k2=true
        end
    end
    @test b_k1 && b_k2
    io = IOBuffer()
    show(io, ENV)
    s = String(take!(io))
    @test contains(s, "$k1=$k1")
    @test contains(s, "$k2=$k2")

    @test pop!(ENV, k1) == k1
    @test !haskey(ENV, k1)
    ENV[k1] = k1
    @test pop!(ENV, k1) == k1
    @test pop!(ENV, k1, "not_there") == "not_there"

    ENV[k1] = k1
    @test delete!(ENV, k1) == ENV
    @test !haskey(ENV, k1)
end

# Test for #10853
@test withenv(Dict{Any,Any}()...) do; true; end

# Test for #18141
for (k, v) in ENV
    if length(v) > 0
        @test v[end] != '\0'
    end
end
