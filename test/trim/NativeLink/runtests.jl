# Verify the trimmed `NativeLink` executable links a library with a `LibraryID` natively
# and that the build wrote the foreign symbol record
using Test

outdir = ARGS[1]

@testset "NativeLink" begin
    exe = joinpath(outdir, "bin", "nativelink" * splitext(Base.julia_exename())[2])
    lines = readlines(addenv(`$exe`, "JULIA_LOAD_CODEGEN_LIB" => "0"))
    @test lines[1] == "ver patch: $(VERSION.patch)"
    @test lines[2] == "ver patch ptr: true"

    # a natively bound symbol is an ordinary undefined symbol of the executable,
    # rather than something looked up at run time
    nmprog = Sys.which("llvm-nm")
    nmprog === nothing && (nmprog = Sys.which("nm"))
    if nmprog !== nothing
        syms = read(`$nmprog -u $exe`, String)
        @test occursin(r"\b_?jl_ver_patch\b", syms)
    end

    record = joinpath(@__DIR__, "foreign-symbol-record.json")
    @test isfile(record)
    json = read(record, String)
    @test occursin("\"package_uuid\": \"6a2ab6db-2a0d-40e1-8b5e-7a9e0d2f4c11\"", json)
    @test occursin("\"library\": \"libjulia\"", json)
    @test !occursin("<libjulia", json)
    # a library of the runtime has the UUID of `julia`
    @test occursin("\"package_uuid\": \"1222c4b2-2114-5bfd-aeef-88e4692bbb3e\",\n      \"library\": \"libjulia-internal\"", json)
    # a library string is not a `LibraryID`
    @test occursin("\"library_file\": \"libnativelink_missing\"", json)
    @test !occursin("\"library\": \"libnativelink_missing\"", json)
    @test occursin("{\"symbol\": \"jl_ver_patch\", \"kind\": \"ccall\", \"linkage\": \"native\"}", json)
    @test occursin("{\"symbol\": \"jl_ver_patch\", \"kind\": \"cglobal\", \"linkage\": \"native\"}", json)
    @test occursin("{\"symbol\": \"jl_get_pgcstack_static\", \"kind\": \"ccall\", \"linkage\": \"lazy\"}", json)
    # everything else stays at run-time lookup
    @test occursin("\"linkage\": \"lazy\"", json)
end
