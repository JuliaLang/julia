# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "sysinfo" begin
# We can't guarantee that these are correct, but we can at least check
# that they run
@test length(Base.Sys.cpu_info()) > 0
sprint(Base.Sys.cpu_summary)
@test Base.Sys.uptime() > 0
Base.Sys.loadavg()

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

end
