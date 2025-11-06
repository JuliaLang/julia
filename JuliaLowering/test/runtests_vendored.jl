if !isdefined(Base, :JuliaLowering)
    # JuliaLowering is not yet vendored in the sysimage by default, but we
    # can pretend it is for testing with a quick `eval` into Base
    Base.include(Base, joinpath(@__DIR__, "..", "src", "JuliaLowering.jl"))
end

# restore error hints (emptied by `testdefs.jl`) so that errors print as
# JuliaLowering expects them to
Base.Experimental.register_error_hint(Base.UndefVarError_hint, UndefVarError)

using Base.JuliaSyntax
using Base.JuliaLowering

# TODO: Does this still need to run in Main?
include(joinpath(@__DIR__, "runtests.jl")) # run the actual tests
