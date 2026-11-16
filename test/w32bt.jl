# Diagnostic for JuliaLang/julia#55900 (not for merging): run the frame-chain
# check in many fresh processes, since the dropped frames vary between runs.
using Test
const NPROC = parse(Int, get(ENV, "W32BT_NPROC", "60"))
const script = joinpath(@__DIR__, "w32bt", "diag.jl")
@testset "w32 backtrace frame chain" begin
    nbad = 0
    for i in 1:NPROC
        out = read(`$(Base.julia_cmd()) --startup-file=no $script 5`, String)
        bad = occursin(r"bad=[1-9]", out) || occursin("gap after", out)
        nbad += bad
        (bad || i == 1) && print("---- W32BT process $i\n", out)
    end
    println("W32BT summary: $nbad of $NPROC processes saw skipped frames")
    @test nbad == 0
end
