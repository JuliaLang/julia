# This file is a part of Julia. License is MIT: https://julialang.org/license

# We can't guarantee that these are correct, but we can at least check
# that they run
@test length(Base.Sys.cpu_info()) > 0
sprint(Base.Sys.cpu_summary)
@test Base.Sys.uptime() > 0
Base.Sys.loadavg()
