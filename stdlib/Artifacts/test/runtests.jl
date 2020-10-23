# This file is a part of Julia. License is MIT: https://julialang.org/license

using Artifacts, Test, Base.BinaryPlatforms
using Artifacts: with_artifacts_directory, pack_platform!, unpack_platform

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
    mktempdir() do tempdir
        with_artifacts_directory(tempdir) do
            exeext = Sys.iswindows() ? ".exe" : ""

            # simple lookup, gives us the directory for `c_simple` for the current architecture
            c_simple_dir = artifact"c_simple"
            @test isdir(c_simple_dir)
            c_simple_exe_path = joinpath(c_simple_dir, "bin", "c_simple$(exeext)")
            @test isfile(c_simple_exe_path)

            # Simple slash-indexed lookup
            c_simple_bin_path = artifact"c_simple/bin"
            @test isdir(c_simple_bin_path)
            # Test that forward and backward slash are equivalent
            @test artifact"c_simple\\bin" == artifact"c_simple/bin"

            # Dynamically-computed lookup; not done at compile-time
            generate_artifact_name() = "c_simple"
            c_simple_dir = @artifact_str(generate_artifact_name())
            @test isdir(c_simple_dir)
            c_simple_exe_path = joinpath(c_simple_dir, "bin", "c_simple$(exeext)")
            @test isfile(c_simple_exe_path)

            # Dynamically-computed slash-indexing:
            generate_bin_path(pathsep) = "c_simple$(pathsep)bin$(pathsep)c_simple$(exeext)"
            @test isfile(@artifact_str(generate_bin_path("/")))
            @test isfile(@artifact_str(generate_bin_path("\\")))
        end
    end
end

@testset "@artifact_str Platform passing" begin
    mktempdir() do tempdir
        with_artifacts_directory(tempdir) do
            win64 = Platform("x86_64", "windows")
            mac64 = Platform("x86_64", "macos")
            @test basename(@artifact_str("c_simple", win64)) == "444cecb70ff39e8961dd33e230e151775d959f37"
            @test basename(@artifact_str("c_simple", mac64)) == "7ba74e239348ea6c060f994c083260be3abe3095"
        end
    end
end