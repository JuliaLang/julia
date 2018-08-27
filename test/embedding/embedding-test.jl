# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests that the output of the embedding examples are correct
using Test
using Libdl

if Sys.iswindows()
    # libjulia needs to be in the same directory as the embedding executable or in path
    ENV["PATH"] = string(Sys.BINDIR, ";", ENV["PATH"])
end

@test length(ARGS) == 2
@testset "embedding example" begin
    out = Pipe()
    err = Pipe()
    p = run(pipeline(Cmd(ARGS[1:1]), stdin=devnull, stdout=out, stderr=err), wait=false)
    close(out.in)
    close(err.in)
    out_task = @async readlines(out)
    err = read(err, String)
    @test err == "MethodError: no method matching this_function_has_no_methods()\n"
    @test success(p)
    lines = fetch(out_task)
    @test length(lines) == 10
    @test parse(Float64, lines[1]) ≈ sqrt(2)
    @test lines[8] == "called bar"
    @test lines[9] == "calling new bar"
    @test lines[10] == "      From worker 2:\tTaking over the world..."
end

if !Sys.iswindows()
    # libjulia needs to be in LD_LIBRARY_PATH in order to dlopen it.
    # On Windows it needs to be in the same directory as the embedding
    # executable or in PATH, but that was arranged earlier.
    libdir = dirname(abspath(Libdl.dlpath("libjulia")))
    if haskey(ENV, "LD_LIBRARY_PATH")
        ENV["LD_LIBRARY_PATH"] = string(libdir, ":", ENV["LD_LIBRARY_PATH"])
    else
        ENV["LD_LIBRARY_PATH"] = libdir
    end
end

@testset "embedding dl" begin
    out = Pipe()
    err = Pipe()
    p = run(pipeline(Cmd(ARGS[2:2]), stdin=devnull, stdout=out, stderr=err), wait=false)
    close(out.in)
    close(err.in)
    out_task = @async readlines(out)
    err = read(err, String)
    @test err == "Intentional error: MethodError: no method matching this_function_has_no_methods()\n"
    @test success(p)
    lines = fetch(out_task)
    @test length(lines) == 1
    @test parse(Float64, lines[1]) == 1.414214
end
