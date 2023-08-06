# This file is a part of Julia. License is MIT: https://julialang.org/license

using Artifacts, Test, Base.BinaryPlatforms
using Artifacts: with_artifacts_directory, pack_platform!, unpack_platform

# prepare for the package tests by ensuring the required artifacts are downloaded now
artifacts_dir = mktempdir()
run(addenv(`$(Base.julia_cmd()) --color=no $(joinpath(@__DIR__, "refresh_artifacts.jl")) $(artifacts_dir)`, "TERM"=>"dumb"))

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
