# This file is a part of Julia. License is MIT: https://julialang.org/license

# Make a copy of the original environment
original_env = copy(ENV)

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

# issue #43486
struct Obj43486 end
(::Obj43486)() = ENV["KEY"] == "VALUE"
let
    f = Obj43486()
    @test !(f isa Function)
    @test withenv(f, "KEY" => "VALUE")
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

    key = randstring(25)
    @test !haskey(ENV, key)
    @test get!(ENV, key, 0) == 0
    @test ENV[key] == "0"
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

@testset "get_bool_env" begin
    @testset "truthy" begin
        for v in ("t", "true", "y", "yes", "1")
            for _v in (v, uppercasefirst(v), uppercase(v))
                ENV["testing_gbe"] = _v
                @test Base.get_bool_env("testing_gbe", false) == true
                @test Base.get_bool_env(() -> false, "testing_gbe") == true
                @test Base.get_bool_env("testing_gbe", true) == true
                @test Base.get_bool_env(() -> true, "testing_gbe") == true
            end
        end
    end
    @testset "falsy" begin
        for v in ("f", "false", "n", "no", "0")
            for _v in (v, uppercasefirst(v), uppercase(v))
                ENV["testing_gbe"] = _v
                @test Base.get_bool_env("testing_gbe", true) == false
                @test Base.get_bool_env(() -> true, "testing_gbe") == false
                @test Base.get_bool_env("testing_gbe", false) == false
                @test Base.get_bool_env(() -> false, "testing_gbe") == false
            end
        end
    end
    @testset "empty" begin
        ENV["testing_gbe"] = ""
        @test Base.get_bool_env("testing_gbe", true) == true
        @test Base.get_bool_env(() -> true, "testing_gbe") == true
        @test Base.get_bool_env("testing_gbe", false) == false
        @test Base.get_bool_env(() -> false, "testing_gbe") == false
    end
    @testset "undefined" begin
        delete!(ENV, "testing_gbe")
        @test !haskey(ENV, "testing_gbe")
        @test Base.get_bool_env("testing_gbe", true) == true
        @test Base.get_bool_env(() -> true, "testing_gbe") == true
        @test Base.get_bool_env("testing_gbe", false) == false
        @test Base.get_bool_env(() -> false, "testing_gbe") == false
    end
    @testset "unrecognized" begin
        for v in ("truw", "falls")
            ENV["testing_gbe"] = v
            @test Base.get_bool_env("testing_gbe", true) === nothing
            @test_throws ArgumentError Base.get_bool_env("testing_gbe", true, throw=true)
            @test Base.get_bool_env("testing_gbe", false) === nothing
            @test_throws ArgumentError Base.get_bool_env("testing_gbe", false, throw=true)
        end
    end

    # the "default" arg shouldn't have a default val, for clarity.
    @test_throws MethodError Base.get_bool_env("testing_gbe")

    delete!(ENV, "testing_gbe")
    @test !haskey(ENV, "testing_gbe")
end

# Restore the original environment
for k in collect(keys(ENV))
    if !haskey(original_env, k)
        delete!(ENV, k)
    end
end
for (k, v) in pairs(original_env)
    ENV[k] = v
end
