# This file is a part of Julia. License is MIT: http://julialang.org/license

bt = backtrace()
have_backtrace = false
for l in bt
    lkup = ccall(:jl_lookup_code_address, Any, (Ptr{Void},), l)
    if lkup[1][1] == :backtrace
        @test lkup[1][5] == false # fromC
        have_backtrace = true
        break
    end
end

@test have_backtrace

#issue 12977: line numbers for kwarg methods.
linenum = @__LINE__; f12977(; args...) = ()
loc = functionloc(f12977)
@test endswith(loc[1], "backtrace.jl")
@test loc[2] == linenum

# issue #922: SimplifyCFG pass merges throws
code_loc(p, skipC=true) = ccall(:jl_lookup_code_address, Any, (Ptr{Void},Cint), p-1, skipC)

@noinline function test_throw_commoning(x)
    if x==1; throw(AssertionError()); end
    if x==2; throw(AssertionError()); end
end

let
    local b1, b2
    try
        test_throw_commoning(1)
    catch
        b1 = stacktrace(catch_backtrace())
    end
    try
        test_throw_commoning(2)
    catch
        b2 = stacktrace(catch_backtrace())
    end
    i1 = findfirst(frame -> frame.func === :test_throw_commoning, b1)
    i2 = findfirst(frame -> frame.func === :test_throw_commoning, b2)
    @test i1 > 0 && i2 > 0
    @test b1[i1].line != b2[i2].line
end
