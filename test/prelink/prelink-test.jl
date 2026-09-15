# This file is a part of Julia. License is MIT: https://julialang.org/license

# Drive the two programs that `Makefile` built: one that holds a system image
# with the room for the list of a pre-relocation, and one that moves at every
# start.

using Test

bin = ARGS[1]
program = joinpath(bin, "prelink")
prelinked = joinpath(bin, "prelinked")
pie = joinpath(bin, "prelink-pie")
answer = "prelinked 42"

# Run `cmd` and give back its exit code, its output and its errors.
function run_program(cmd)
    out, err = IOBuffer(), IOBuffer()
    p = run(pipeline(ignorestatus(cmd), stdout = out, stderr = err), wait = true)
    return p.exitcode, String(take!(out)), String(take!(err))
end

@testset "a program that holds a system image" begin
    code, out, err = run_program(`$program`)
    @test code == 0
    @test chomp(out) == answer
end

@testset "the program writes a pre-relocated copy of itself" begin
    rm(prelinked, force = true)
    code, out, err = run_program(`$program --julia-args --output-prelinked=$prelinked`)
    @test code == 0
    @test isfile(prelinked)
    # The report says how much of the relocation the writer could not do. What
    # is left is a pointer that the file cannot hold, and there are very few of
    # them; if the count runs away, the image lost its own `nothing`, its
    # symbols, or the thunks of the entry points of the runtime.
    matched = match(r"pre-relocated: (\d+) of (\d+) pointers on the list", err)
    @test matched !== nothing
    if matched !== nothing
        left, total = parse(Int, matched[1]), parse(Int, matched[2])
        @test total > 100_000
        @test left <= 64
    end
end

@testset "the pre-relocated program does the same" begin
    if isfile(prelinked)
        @test Sys.isexecutable(prelinked)
        code, out, err = run_program(`$prelinked`)
        @test code == 0
        @test chomp(out) == answer
    end
end

@testset "what the runtime refuses" begin
    # Each of these restores the image and stops before it writes anything.
    # The runtime refuses two more that this harness cannot build: an image
    # pre-relocated for another address, and an image with several code
    # variants, which needs a base image that carries the same variants.
    for (what, cmd, message) in (
            ("an image that is pre-relocated already", `$prelinked`, "pre-relocated already"),
            ("a program that moves at every start", `$pie`, "moves at every start"))
        @testset "$what" begin
            output = tempname()
            code, out, err = run_program(`$cmd --julia-args --output-prelinked=$output`)
            @test code == 1
            @test occursin(message, err)
            @test !isfile(output)
            rm(output, force = true)
        end
    end
end
