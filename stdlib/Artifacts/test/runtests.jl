# This file is a part of Julia. License is MIT: https://julialang.org/license
import Base: SHA1

using Artifacts, Test, Base.BinaryPlatforms
using Artifacts: with_artifacts_directory, pack_platform!, unpack_platform, load_overrides
using TOML

# prepare for the package tests by ensuring the required artifacts are downloaded now
artifacts_dir = mktempdir()
run(addenv(`$(Base.julia_cmd()) --color=no $(joinpath(@__DIR__, "refresh_artifacts.jl")) $(artifacts_dir)`, "TERM"=>"dumb"))

@testset "Load Overrides()" begin
    # -- helper functions to create the test `Overrides.toml` and restore the status quo afterwards
    """
        create_test_overrides_toml()::Tuple{Bool, Bool}

    Create "Overrides.toml" in the "~/.julia/artifacts" dir if it is not already there.
    Otherwise, do nothing.

    Return two parameters:
    The first parameter returns true if the artifacts directory did not exist and was created.
    Otherwise return false.

    The second parameter returns true if "Overrides.toml" did not exist and was created.
    Otherwise return false.
    """
    function create_test_overrides_toml()::Tuple{Bool, Bool}
        # Define the overrides
        overrides = Dict(
            "78f35e74ff113f02274ce60dab6e92b4546ef806" => "/path/to/replacement",
            "c76f8cda85f83a06d17de6c57aabf9e294eb2537" => "fb886e813a4aed4147d5979fcdf27457d20aa35d",
            "d57dbccd-ca19-4d82-b9b8-9d660942965b" => Dict(
                "c_simple" => "/path/to/c_simple_dir",
                "libfoo" => "fb886e813a4aed4147d5979fcdf27457d20aa35d"
            )
        )

        # Get the path to the default depot
        depot_path = DEPOT_PATH[1]

        # Get the artifacts directory
        artifacts_dir = joinpath(depot_path, "artifacts")

        # Check if the directory already exists
        created_artifacts_dir = !isdir(artifacts_dir)

        # Ensure the artifacts directory exists
        isdir(artifacts_dir) || mkdir(artifacts_dir)

        # Get the path to the Overrides.toml file
        overrides_path = joinpath(artifacts_dir, "Overrides.toml")

        # Create the Overrides.toml file if it doesn't exist
        if !isfile(overrides_path)
            open(overrides_path, "w") do io
                TOML.print(io, overrides)
            end
            return created_artifacts_dir, true
        else
            return created_artifacts_dir, false
        end
    end

    """
        clear_test_overrides_toml(created_artifacts_dir::Bool, created_overrides_toml::Bool)

    Delete "Overrides.toml" in the "~/.julia/artifacts" dir and the artifacts directory
    if they were created by `create_test_overrides_toml()`.
    Otherwise, do nothing.
    """
    function clear_test_overrides_toml(created_artifacts_dir::Bool, created_overrides_toml::Bool)
        # Get the path to the default depot
        depot_path = DEPOT_PATH[1]

        # Get the artifacts directory
        artifacts_dir = joinpath(depot_path, "artifacts")

        # Get the path to the Overrides.toml file
        overrides_path = joinpath(artifacts_dir, "Overrides.toml")

        # Delete the Overrides.toml file if it exists
        if created_overrides_toml
            if isfile(overrides_path)
                # Overrides.toml file deleted.
                rm(overrides_path)
            else
                # Overrides.toml file does not exist.
            end
        end

        # Delete the artifacts directory if it exists and is empty
        if created_artifacts_dir
            if isdir(artifacts_dir)
                if isempty(readdir(artifacts_dir))
                    # Artifacts directory deleted.
                    rm(artifacts_dir)
                else
                    # Artifacts directory is not empty. Not deleting.
                end
            else
                # Artifacts directory does not exist.
            end
        end
    end

    # -- Overrides.toml in the artifacts directory    
    # Create "Overrides.toml" for the test
    created_artifacts_dir, created_overrides_toml = create_test_overrides_toml()

    # Run test
    expected_output = Dict{Symbol, Any}(
        :UUID => Dict{Base.UUID, Dict{String, Union{SHA1, String}}}(Base.UUID("d57dbccd-ca19-4d82-b9b8-9d660942965b") => Dict("c_simple" => "/path/to/c_simple_dir", "libfoo" => SHA1("fb886e813a4aed4147d5979fcdf27457d20aa35d"))), 
        :hash => Dict{SHA1, Union{SHA1, String}}(SHA1("78f35e74ff113f02274ce60dab6e92b4546ef806") => "/path/to/replacement", SHA1("c76f8cda85f83a06d17de6c57aabf9e294eb2537") => SHA1("fb886e813a4aed4147d5979fcdf27457d20aa35d"))
    )
    @test load_overrides() == expected_output

    # -- Overrides.toml not in the artifacts directory
    # Delete test "Overrides.toml"
    clear_test_overrides_toml(created_artifacts_dir, created_overrides_toml)

    # Run test
    @test load_overrides() == expected_output
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
        @test basename(@artifact_str("HelloWorldC", win64)) == "2f1a6d4f82cd1eea785a5141b992423c09491f1b"
        @test basename(@artifact_str("HelloWorldC", mac64)) == "f8ab5a03697f9afc82210d8a2be1d94509aea8bc"
    end
end

@testset "artifact_hash()" begin
    # Use the Linus OS on an ARMv7L architecture for the tests to make tests reproducible
    armv7l_linux = Platform("armv7l", "linux")

    # Check the first key in Artifacts.toml is hashed correctly
    @test artifact_hash("HelloWorldC", joinpath(@__DIR__, "Artifacts.toml"); platform=armv7l_linux) ==
            SHA1("5a8288c8a30578c0d0f24a9cded29579517ce7a8")

    # Check the second key in Artifacts.toml is hashed correctly
    @test artifact_hash("socrates", joinpath(@__DIR__, "Artifacts.toml"); platform=armv7l_linux) ==
            SHA1("43563e7631a7eafae1f9f8d9d332e3de44ad7239")

    # Check artifact_hash() works for any AbstractString
    @test artifact_hash(SubString("HelloWorldC0", 1, 11), joinpath(@__DIR__, "Artifacts.toml"); platform=armv7l_linux) ==
            SHA1("5a8288c8a30578c0d0f24a9cded29579517ce7a8")
end

@testset "select_downloadable_artifacts()" begin
    armv7l_linux = Platform("armv7l", "linux")
    artifacts = select_downloadable_artifacts(joinpath(@__DIR__, "Artifacts.toml"); platform=armv7l_linux)
    @test length(keys(artifacts)) == 1
    @test artifacts["HelloWorldC"]["git-tree-sha1"] == "5a8288c8a30578c0d0f24a9cded29579517ce7a8"

    artifacts = select_downloadable_artifacts(joinpath(@__DIR__, "Artifacts.toml"); platform=armv7l_linux, include_lazy=true)
    @test length(keys(artifacts)) == 2
    @test artifacts["HelloWorldC"]["git-tree-sha1"] == "5a8288c8a30578c0d0f24a9cded29579517ce7a8"
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
    @test length(Base.manifest_names) == n
    @test length(Base.preferences_names) == n
end
