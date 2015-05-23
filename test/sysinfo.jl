# This file is a part of Julia. License is MIT: http://julialang.org/license

# We can't guarantee that these are correct, but we can at least check
# that they run
@test length(Base.Sys.cpu_info()) > 0
sprint(Base.Sys.cpu_summary)
@test Base.Sys.uptime() > 0
Base.Sys.loadavg()

# issue #10994
@test_throws ArgumentError ENV["bad\0name"] = "ok"
@test_throws ArgumentError ENV["okname"] = "bad\0val"
@test_throws ArgumentError Sys.set_process_title("bad\0title")

# issue #11170
withenv("TEST"=>"nonempty") do
    @test ENV["TEST"] == "nonempty"
end
withenv("TEST"=>"") do
    @test ENV["TEST"] == ""
end
