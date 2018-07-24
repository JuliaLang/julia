# This file is a part of Julia. License is MIT: https://julialang.org/license

# test for proper handling of FD exhaustion
if Sys.isunix()
    let ps = Pipe[]
        ulimit_n = tryparse(Int, readchomp(`sh -c 'ulimit -n'`))
        try
            for i = 1:100*something(ulimit_n, 1000)
                p = Pipe()
                Base.link_pipe!(p)
                push!(ps, p)
            end
            if ulimit_n === nothing
                @warn "`ulimit -n` is set to unlimited, fd exhaustion cannot be tested"
                @test_broken false
            else
                @test false
            end
        catch ex
            isa(ex, Base.UVError) || rethrow(ex)
            @test ex.code in (Base.UV_EMFILE, Base.UV_ENFILE)
        finally
            foreach(close, ps)
        end
    end
end
