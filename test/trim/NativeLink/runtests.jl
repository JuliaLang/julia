# Verify the trimmed `NativeLink` executable binds an identified library natively
using Test

outdir = ARGS[1]

@testset "NativeLink" begin
    exe = joinpath(outdir, "bin", "nativelink" * splitext(Base.julia_exename())[2])
    lines = readlines(addenv(`$exe`, "JULIA_LOAD_CODEGEN_LIB" => "0"))
    @test lines[1] == "ver patch: $(VERSION.patch)"

    # a natively bound symbol is an ordinary undefined symbol of the executable,
    # rather than something looked up at run time
    nmprog = Sys.which("llvm-nm")
    nmprog === nothing && (nmprog = Sys.which("nm"))
    if nmprog !== nothing
        syms = read(`$nmprog -u $exe`, String)
        @test occursin(r"\b_?jl_ver_patch\b", syms)
    end
end
