# Tests for @Base_show macro
include("../show_type.jl")

using Test

@testset "@Base_show macro" begin
    fname = tempname()
    try
        open(fname, "w") do fout
            redirect_stdout(fout) do
                @Base_show typeof(42)
            end
        end
        @test read(fname, String) == "typeof(42) = Int64\n"
    finally
        rm(fname, force=true)
    end
end

@testset "@Base_show for various types" begin
    fname = tempname()
    try
        open(fname, "w") do fout
            redirect_stdout(fout) do
                @Base_show typeof([1, 2, 3])
            end
        end
        @test read(fname, String) == "typeof([1, 2, 3]) = Vector{Int64}\n"
    finally
        rm(fname, force=true)
    end
end
