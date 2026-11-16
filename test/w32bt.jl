# Fresh-process part of the JuliaLang/julia#55900 diagnostic (not for merging).
# The in-process part runs inside errorshow.jl. The dropped frames vary between
# runs, so check many processes.
using Test
const NPROC = parse(Int, get(ENV, "W32BT_NPROC", "40"))
const script = joinpath(@__DIR__, "w32bt", "diag.jl")
@testset "w32 backtrace frame chain" begin
    nbad = 0
    for i in 1:NPROC
        out = read(`$(Base.julia_cmd()) --startup-file=no $script 5`, String)
        bad = !occursin(r"W32BT pid=\d+ bad=0 ", out)
        nbad += bad
        (bad || i == 1) && print("---- W32BT process $i\n", out)
    end
    println("W32BT summary: $nbad of $NPROC fresh processes saw dropped frames or chain mismatches")
    Sys.iswindows() && Sys.WORD_SIZE == 32 && @test nbad == 0
end
