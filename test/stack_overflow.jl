# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

# helper function for returning stderr and stdout
# from running a command (ignoring failure status)
function readchomperrors(exename::Cmd)
    out = Base.PipeEndpoint()
    err = Base.PipeEndpoint()
    p = run(exename, devnull, out, err, wait=false)
    o = @async(readchomp(out))
    e = @async(readchomp(err))
    return (success(p), fetch(o), fetch(e))
end

let exename = Base.julia_cmd()
    @show readchomperrors(`$exename -e "f() = f(); f()"`)
    @show readchomperrors(`$exename -e "f() = f(); fetch(@async f())"`)
end

# Issue #49507: stackoverflow in type inference caused by close(::Channel, ::Exception)
@testset "close(::Channel, ::StackOverflowError)" begin
    ch = let result = Channel()
        foo() = try
            foo()
        catch e;
            close(result, e)
        end

        foo()  # This shouldn't fail with an internal stackoverflow error in inference.

        result
    end

    @test (try take!(ch) catch e; e; end) isa StackOverflowError
end
