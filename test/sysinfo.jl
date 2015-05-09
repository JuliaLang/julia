# We can't guarantee that these are correct, but we can at least check
# that they run
@test length(Base.Sys.cpu_info()) > 0
sprint(Base.Sys.cpu_summary)
@test Base.Sys.uptime() > 0
Base.Sys.loadavg()

# issue #11170
Base.with_env("TEST", "nonempty") do
    @test ENV["TEST"] == "nonempty"
end
Base.with_env("TEST", "") do
    @test ENV["TEST"] == ""
end
