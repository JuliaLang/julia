# This file is a part of Julia. License is MIT: https://julialang.org/license

# The codegen assertions require default bounds-checking semantics
# (`--check-bounds=auto`), while the test harness forces `--check-bounds=yes`
# on its workers, so run them in a subprocess (mirroring test/boundscheck.jl).

using Test

let cmd = `$(Base.julia_cmd()) --check-bounds=auto --startup-file=no $(joinpath(@__DIR__, "inbounds_codegen_exec.jl"))`
    @test success(pipeline(cmd; stdout=stdout, stderr=stderr))
end
