# This file is a part of Julia. License is MIT: https://julialang.org/license
import Base: SHA1

using Artifacts, Test, Base.BinaryPlatforms
using Artifacts: with_artifacts_directory, pack_platform!, unpack_platform, load_overrides
using TOML

# prepare for the package tests by ensuring the required artifacts are downloaded now
artifacts_dir = mktempdir()
run(addenv(`$(Base.julia_cmd()) --color=no $(joinpath(@__DIR__, "refresh_artifacts.jl")) $(artifacts_dir)`, "TERM"=>"dumb"))

@testset "Load Overrides" begin
    """
        create_test_overrides_toml(temp_dir::String)

    Create "Overrides.toml" in the given `temp_dir`.
    """
    function create_test_overrides_toml(temp_dir::String)
        # Define the overrides
        overrides = Dict(
            "78f35e74ff113f02274ce60dab6e92b4546ef806" => "/path/to/replacement",
            "c76f8cda85f83a06d17de6c57aabf9e294eb2537" => "fb886e813a4aed4147d5979fcdf27457d20aa35d",
            "d57dbccd-ca19-4d82-b9b8-9d660942965b" => Dict(
                "c_simple" => "/path/to/c_simple_dir",
                "libfoo" => "fb886e813a4aed4147d5979fcdf27457d20aa35d"
            )
        )

        # Get the artifacts directory
        artifacts_dir = joinpath(temp_dir, "artifacts")

        # Ensure the artifacts directory exists
        isdir(artifacts_dir) || mkdir(artifacts_dir)

        # Get the path to the Overrides.toml file
        overrides_path = joinpath(artifacts_dir, "Overrides.toml")

        # Create the Overrides.toml file
        open(overrides_path, "w") do io
            TOML.print(io, overrides)
        end
    end

    # Specify the expected test result when depot path does not exist or no overriding happened
    empty_output = Dict{Symbol, Any}(
        :UUID => Dict{Base.UUID, Dict{String, Union{SHA1, String}}}(),
        :hash => Dict{SHA1, Union{SHA1, String}}()
    )

    # Specify the expected test result when overriding happened
    expected_output = Dict{Symbol, Any}(
        :UUID => Dict{Base.UUID, Dict{String, Union{SHA1, String}}}(Base.UUID("d57dbccd-ca19-4d82-b9b8-9d660942965b") => Dict("c_simple" => "/path/to/c_simple_dir", "libfoo" => SHA1("fb886e813a4aed4147d5979fcdf27457d20aa35d"))),
        :hash => Dict{SHA1, Union{SHA1, String}}(SHA1("78f35e74ff113f02274ce60dab6e92b4546ef806") => "/path/to/replacement", SHA1("c76f8cda85f83a06d17de6c57aabf9e294eb2537") => SHA1("fb886e813a4aed4147d5979fcdf27457d20aa35d"))
    )

    # Test `load_overrides()` works with *no* "Overrides.toml" file
    @test load_overrides() == empty_output

    # Create a temporary directory
    mktempdir() do temp_dir
        # Back up the old `DEPOT_PATH``
        old_depot_path = copy(Base.DEPOT_PATH)

        # Set `DEPOT_PATH` to that directory
        empty!(Base.DEPOT_PATH)
        push!(Base.DEPOT_PATH, temp_dir)

        try
            # Create "Overrides.toml" for the test
            create_test_overrides_toml(temp_dir)

            # Test `load_overrides()` works *with* "Overrides.toml" file but non-nothing ARTIFACT_OVERRIDES[]
            @test load_overrides() == empty_output

            # Test `load_overrides()` works *with* "Overrides.toml" file with force parameter, which overrides even when `ARTIFACT_OVERRIDES[] !== nothing``
            @test load_overrides(force=true) == expected_output
        finally # Make sure `DEPOT_PATH` will be restored to the status quo in the event of a bug
            # Restore the old `DEPOT_PATH` to avoid messing with any other code
            empty!(Base.DEPOT_PATH)
            append!(Base.DEPOT_PATH, old_depot_path)
        end
    end
    # Temporary directory and test "Overrides.toml" file will be automatically deleted when out of scope
    # This means after this block, the system *should* behave like this test never happened.

    # Test the "Overrides.toml" file is cleared back to the status quo
    @test load_overrides(force=true) == empty_output
end

@testset "Artifact Paths" begin
    mktempdir() do tempdir
        with_artifacts_directory(tempdir) do
            hash = Base.SHA1("0"^40)
            paths = Artifacts.artifact_paths(hash)
            @test length(paths) == 1
            @test startswith(first(paths), tempdir)

            @test !artifact_exists(hash)
            mkpath(first(paths))
            @test artifact_exists(hash)
            @test artifact_path(hash) == first(paths)
        end
    end
end

@testset "Serialization Tools" begin
    # First, some basic tests
    meta = Dict()
    pack_platform!(meta, Platform("i686", "linux"))
    @test meta["os"] == "linux"
    @test meta["arch"] == "i686"
    @test meta["libc"] == "glibc"

    meta = Dict()
    pack_platform!(meta, Platform("armv7l", "linux"; libc="musl"))
    @test meta["os"] == "linux"
    @test meta["arch"] == "armv7l"
    @test meta["libc"] == "musl"

    meta = Dict()
    pack_platform!(meta, Platform("x86_64", "windows"; libgfortran_version=v"3"))
    @test meta["os"] == "windows"
    @test meta["arch"] == "x86_64"
    @test meta["libgfortran_version"] == "3.0.0"

    meta = Dict()
    pack_platform!(meta, Platform("aarch64", "macOS"))
    @test meta == Dict("os" => "macos", "arch" => "aarch64")

    # Next, fuzz it out!  Ensure that we exactly reconstruct our platforms!
    platforms = Platform[]
    for libgfortran_version in (v"3", v"4", v"5", nothing),
        libstdcxx_version in (v"3.4.11", v"3.4.19", nothing),
        cxxstring_abi in ("cxx03", "cxx11", nothing)

        for arch in ("x86_64", "i686", "aarch64", "armv7l"),
            libc in ("glibc", "musl")

            push!(platforms, Platform(arch, "linux"; libc, libgfortran_version, libstdcxx_version, cxxstring_abi))
        end
        push!(platforms, Platform("x86_64", "windows"; libgfortran_version, libstdcxx_version, cxxstring_abi))
        push!(platforms, Platform("i686", "windows"; libgfortran_version, libstdcxx_version, cxxstring_abi))
        push!(platforms, Platform("x86_64", "macOS"; libgfortran_version, libstdcxx_version, cxxstring_abi))
        push!(platforms, Platform("aarch64", "macOS"; libgfortran_version, libstdcxx_version, cxxstring_abi))
        push!(platforms, Platform("x86_64", "FreeBSD"; libgfortran_version, libstdcxx_version, cxxstring_abi))
    end

    for p in platforms
        meta = Dict()
        pack_platform!(meta, p)
        @test unpack_platform(meta, "foo", "<in-memory-Artifacts.toml>") == p

        # Test that some things raise warnings
        bad_meta = copy(meta)
        delete!(bad_meta, "os")
        @test_logs (:error, r"Invalid artifacts file") unpack_platform(bad_meta, "foo", "")

        bad_meta = copy(meta)
        delete!(bad_meta, "arch")
        @test_logs (:error, r"Invalid artifacts file") unpack_platform(bad_meta, "foo", "")
    end
end

@testset "Artifact Slash-indexing" begin
    with_artifacts_directory(artifacts_dir) do
        exeext = Sys.iswindows() ? ".exe" : ""

        # simple lookup, gives us the directory for `HelloWorldC` for the current architecture
        HelloWorldC_dir = artifact"HelloWorldC"
        @test isdir(HelloWorldC_dir)
        HelloWorldC_exe_path = joinpath(HelloWorldC_dir, "bin", "hello_world$(exeext)")
        @test isfile(HelloWorldC_exe_path)

        # Simple slash-indexed lookup
        HelloWorldC_bin_path = artifact"HelloWorldC/bin"
        @test isdir(HelloWorldC_bin_path)
        # Test that forward and backward slash are equivalent
        @test artifact"HelloWorldC\\bin" == artifact"HelloWorldC/bin"

        # Dynamically-computed lookup; not done at compile-time
        generate_artifact_name() = "HelloWorldC"
        HelloWorldC_dir = @artifact_str(generate_artifact_name())
        @test isdir(HelloWorldC_dir)
        HelloWorldC_exe_path = joinpath(HelloWorldC_dir, "bin", "hello_world$(exeext)")
        @test isfile(HelloWorldC_exe_path)

        # Dynamically-computed slash-indexing:
        generate_bin_path(pathsep) = "HelloWorldC$(pathsep)bin$(pathsep)hello_world$(exeext)"
        @test isfile(@artifact_str(generate_bin_path("/")))
        @test isfile(@artifact_str(generate_bin_path("\\")))
    end
end

@testset "@artifact_str Platform passing" begin
    with_artifacts_directory(artifacts_dir) do
        win64 = Platform("x86_64", "windows")
        mac64 = Platform("x86_64", "macos")
        @test basename(@artifact_str("HelloWorldC", win64)) == "6e1eb164b0651aa44621eac4dfa340d6e60295ef"
        @test basename(@artifact_str("HelloWorldC", mac64)) == "2e1742c9c0addd693b0b025f7a1e7aa4c50a0e6c"
    end
end

@testset "artifact_hash()" begin
    # Use the Linus OS on an ARMv7L architecture for the tests to make tests reproducible
    armv7l_linux = Platform("armv7l", "linux")

    # Check the first key in Artifacts.toml is hashed correctly
    @test artifact_hash("HelloWorldC", joinpath(@__DIR__, "Artifacts.toml"); platform=armv7l_linux) ==
            SHA1("0a8e7b523ef6be31311aefe9983a488616e58201")

    # Check the second key in Artifacts.toml is hashed correctly
    @test artifact_hash("socrates", joinpath(@__DIR__, "Artifacts.toml"); platform=armv7l_linux) ==
            SHA1("43563e7631a7eafae1f9f8d9d332e3de44ad7239")

    # Check artifact_hash() works for any AbstractString
    @test artifact_hash(SubString("HelloWorldC0", 1, 11), joinpath(@__DIR__, "Artifacts.toml"); platform=armv7l_linux) ==
            SHA1("0a8e7b523ef6be31311aefe9983a488616e58201")
end

@testset "select_downloadable_artifacts()" begin
    armv7l_linux = Platform("armv7l", "linux")
    artifacts = select_downloadable_artifacts(joinpath(@__DIR__, "Artifacts.toml"); platform=armv7l_linux)
    @test length(keys(artifacts)) == 1
    @test artifacts["HelloWorldC"]["git-tree-sha1"] == "0a8e7b523ef6be31311aefe9983a488616e58201"

    artifacts = select_downloadable_artifacts(joinpath(@__DIR__, "Artifacts.toml"); platform=armv7l_linux, include_lazy=true)
    @test length(keys(artifacts)) == 2
    @test artifacts["HelloWorldC"]["git-tree-sha1"] == "0a8e7b523ef6be31311aefe9983a488616e58201"
    @test artifacts["socrates"]["git-tree-sha1"] == "43563e7631a7eafae1f9f8d9d332e3de44ad7239"
end

@testset "@artifact_str install errors" begin
    for imports in ("Artifacts, Pkg", "Pkg, Pkg.Artifacts", "Pkg.Artifacts")
        mktempdir() do tempdir
            with_artifacts_directory(tempdir) do
                ex = @test_throws ErrorException artifact"HelloWorldC"
                @test startswith(ex.value.msg, "Artifact \"HelloWorldC\" was not found ")
                ex = @test_throws ErrorException artifact"socrates"
                @test startswith(ex.value.msg, "Artifact \"socrates\" is a lazy artifact; ")

                # Can install if we load `Pkg` or `Pkg.Artifacts`
                anon = Module(:__anon__)
                Core.eval(anon, Meta.parse("using $(imports), Test"))
                # Ensure that we get the expected exception, since this test runs with --depwarn=error
                Core.eval(anon, quote
                    try
                        artifact"socrates"
                        @assert false "this @artifact_str macro invocation should have failed!"
                    catch e
                        @test startswith("using Pkg instead of using LazyArtifacts is deprecated", e.msg)
                    end
                end)
            end
        end
    end
end

@testset "`Artifacts.artifact_names` and friends" begin
    n = length(Artifacts.artifact_names)
    @test length(Base.project_names) == n
    @test length(Base.manifest_names) == 2n # there are two manifest names per project name
    @test length(Base.preferences_names) == n
end

@testset "Docstrings" begin
    @test isempty(Docs.undocumented_names(Artifacts))
end
