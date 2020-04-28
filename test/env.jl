# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random

@test !("f=a=k=e=n=a=m=e" ∈ keys(ENV))

@testset "issue #10994" begin
    @test_throws ArgumentError ENV["bad\0name"] = "ok"
    @test_throws ArgumentError ENV["okname"] = "bad\0val"
    @test_throws ArgumentError Sys.set_process_title("bad\0title")

    withenv("bad"=>"dog") do
        @test_throws ArgumentError ENV["bad\0cat"]
    end
end
@testset "issue #11170" begin
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
end
@testset "non-existent keys" begin
    key = randstring(25)
    @test !haskey(ENV,key)
    @test_throws KeyError ENV[key]
    @test get(ENV,key,"default") == "default"
    @test get(() -> "default", ENV, key) == "default"

    key = randstring(25)
    @test !haskey(ENV, key)
    @test get!(ENV, key, "default") == "default"
    @test haskey(ENV, key)
    @test ENV[key] == "default"
end
@testset "#17956" begin
    @test length(ENV) > 1
    k1, k2 = "__TEST__", "__TEST1__"
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
        @test occursin("$k1=$k1", s)
        @test occursin("$k2=$k2", s)

        @test pop!(ENV, k1) == k1
        @test !haskey(ENV, k1)
        ENV[k1] = k1
        @test pop!(ENV, k1) == k1
        @test pop!(ENV, k1, "not_there") == "not_there"

        ENV[k1] = k1
        @test delete!(ENV, k1) == ENV
        @test !haskey(ENV, k1)
    end
end
# Test for #10853
@test withenv(Dict{Any,Any}()...) do; true; end

# Test for #18141
for (k, v) in ENV
    if length(v) > 0
        @test v[end] != '\0'
    end
end

@testset "push" begin
    @test !haskey(ENV, "testing_envdict")
    push!(ENV, "testing_envdict" => "tested")
    @test haskey(ENV, "testing_envdict")
    @test ENV["testing_envdict"] == "tested"
    delete!(ENV, "testing_envdict")
end

if Sys.iswindows()
    @testset "windows case-insensitivity" begin
        for k in ("testing_envdict", "testing_envdict_\u00ee")
            K = uppercase(k)
            v = "tested $k"
            ENV[k] = v
            @test haskey(ENV, K)
            @test ENV[K] == v
            @test K in keys(ENV)
            @test K in collect(keys(ENV))
            @test k ∉ collect(keys(ENV))
            env = copy(ENV)
            @test haskey(env, K)
            @test env[K] == v
            @test !haskey(env, k)
            delete!(ENV, k)
            @test !haskey(ENV, K)
        end
    end
end
