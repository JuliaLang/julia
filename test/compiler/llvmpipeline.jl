using Test

bounds_check = ("yes", "no", "auto")
opt_level = (0, 1, 2, 3)

for opt in opt_level
    for bc in bounds_check
        @testset "LLVM pipeline check with boundscheck=$bc and opt=-O$opt" begin
            cmd = `$(Base.julia_cmd()) --check-bounds=$(bc) -O$(opt) --depwarn=error --startup-file=no compiler/llvmpipeline_opt_$(opt).jl`
            if !success(pipeline(cmd; stdout=stdout, stderr=stderr))
                # Just to mark it as a fail
                @test "llvmpipeline fails for cmd : $cmd" == ""
            else
                # Just to mark it as a pass
                @test "llvmpipeline passes for -O$opt bounds=$bc" != ""
            end
        end
    end
end
