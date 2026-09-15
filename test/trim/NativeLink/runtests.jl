# Verify the trimmed `NativeLink` executable binds an identified library natively and
# that the build recorded the foreign symbols it uses
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

    manifest = joinpath(@__DIR__, "used-foreign-symbols.json")
    @test isfile(manifest)
    json = read(manifest, String)
    @test occursin("\"package_uuid\": \"6a2ab6db-2a0d-40e1-8b5e-7a9e0d2f4c11\"", json)
    @test occursin("\"library\": \"libjulia\"", json)
    @test !occursin("<libjulia", json)
    @test occursin("{\"symbol\": \"jl_ver_patch\", \"kind\": \"ccall\", \"linkage\": \"native\"}", json)
    # everything else stays at run-time lookup
    @test occursin("\"linkage\": \"lazy\"", json)
end
