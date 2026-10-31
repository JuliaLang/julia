# This file is a part of Julia. License is MIT: https://julialang.org/license
import Base: SHA1

using Artifacts, Test, Base.BinaryPlatforms
using Artifacts: with_artifacts_directory, pack_platform!, unpack_platform, load_overrides
using TOML

@test isempty(Test.detect_closure_boxes(Artifacts))

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

function with_test_depots(f, depots...)
    old_depot_path = copy(Base.DEPOT_PATH)
    empty!(Base.DEPOT_PATH)
    append!(Base.DEPOT_PATH, depots)
    try
        f()
    finally
        empty!(Base.DEPOT_PATH)
        append!(Base.DEPOT_PATH, old_depot_path)
        load_overrides(force=true)
    end
end

function write_overrides_toml(depot, overrides)
    mkpath(joinpath(depot, "artifacts"))
    open(joinpath(depot, "artifacts", "Overrides.toml"), "w") do io
        TOML.print(io, overrides)
    end
end

@testset "Overrides.toml layering and invalid entries" begin
    uuid = Base.UUID("d57dbccd-ca19-4d82-b9b8-9d660942965b")
    h1, h2, h3, h4, h5 = ("$(i)"^40 for i in 1:5)
    mktempdir() do dir
        high, low = joinpath(dir, "high"), joinpath(dir, "low")
        p1, p2 = joinpath(dir, "p1"), joinpath(dir, "p2")
        write_overrides_toml(low, Dict(
            h1 => p1,
            h2 => p2,
            string(uuid) => Dict("a" => p1, "b" => h3),
        ))
        # The first depot wins, and an empty string removes an override from a later depot
        write_overrides_toml(high, Dict(
            h2 => "",
            string(uuid) => Dict("a" => h3, "b" => "", "bad" => "relative/path", "num" => 1, "nested" => Dict("x" => p1)),
            "not-a-hash" => p1,
            h4 => "relative/path",
            "not-a-uuid" => Dict("a" => p1),
            h5 => 1,
        ))
        with_test_depots(high, low) do
            overrides = @test_logs(
                (:error, r"Invalid SHA1 hash 'not-a-hash'"),
                (:error, r"entry '4{40}' must map to an absolute path or SHA1 hash"),
                (:error, r"failed to parse entry `4{40}`"),
                (:error, r"Invalid UUID 'not-a-uuid'"),
                (:error, r"failed to parse entry `5{40}`"),
                (:error, r"entry 'bad' must map to an absolute path or SHA1 hash"),
                (:error, r"failed to parse entry `bad` for UUID 'd57dbccd-ca19-4d82-b9b8-9d660942965b'"),
                (:error, r"failed to parse entry `num` for UUID"),
                (:error, r"failed to parse entry `nested` for UUID"),
                match_mode=:any,
                load_overrides(force=true))
            @test overrides[:hash] == Dict(SHA1(h1) => p1)
            @test overrides[:UUID] == Dict(uuid => Dict("a" => SHA1(h3)))

            @test Artifacts.query_override(SHA1(h1)) == p1
            @test Artifacts.query_override(SHA1(h2)) === nothing
            @test artifact_path(SHA1(h1)) == p1
            @test artifact_path(SHA1(h1); honor_overrides=false) == joinpath(high, "artifacts", h1)
            @test Artifacts.query_override(uuid, "a") == joinpath(high, "artifacts", h3)
            @test Artifacts.query_override(uuid, SubString("a")) == joinpath(high, "artifacts", h3)
            @test Artifacts.query_override(uuid, "b") === nothing
            @test Artifacts.query_override(Base.UUID(0), "a") === nothing
        end
    end
end

@testset "Overrides.toml by package UUID" begin
    uuid = Base.UUID("d57dbccd-ca19-4d82-b9b8-9d660942965b")
    ha, hb, hc, hd, he = ("$(c)"^40 for c in "abcde")
    mktempdir() do depot
        artifacts_toml = joinpath(depot, "Artifacts.toml")
        write(artifacts_toml, """
            [[plat]]
            arch = "x86_64"
            os = "linux"
            git-tree-sha1 = "$ha"

            [[plat]]
            arch = "x86_64"
            os = "windows"
            git-tree-sha1 = "$hb"

            [single]
            git-tree-sha1 = "$hc"

            [untouched]
            git-tree-sha1 = "$hd"
            """)
        override_dir = joinpath(depot, "override")
        write_overrides_toml(depot, Dict(string(uuid) => Dict("plat" => override_dir, "single" => he)))
        with_test_depots(depot) do
            load_overrides(force=true)
            @test Artifacts.query_override(SHA1(ha)) === nothing

            # Overrides by name only take effect once the package's Artifacts.toml is loaded
            Artifacts.load_artifacts_toml(SubString(artifacts_toml, 1); pkg_uuid=uuid)
            @test Artifacts.query_override(SHA1(ha)) == override_dir
            @test Artifacts.query_override(SHA1(hb)) == override_dir
            @test Artifacts.query_override(SHA1(hc)) == joinpath(depot, "artifacts", he)
            @test Artifacts.query_override(SHA1(hd)) === nothing

            meta = artifact_meta("plat", artifacts_toml; pkg_uuid=uuid, platform=Platform("x86_64", "windows"))
            @test artifact_path(SHA1(meta["git-tree-sha1"])) == override_dir

            load_overrides(force=true)
            @test Artifacts.query_override(SHA1(hc)) === nothing
            @test artifact_hash("single", artifacts_toml; pkg_uuid=uuid) == SHA1(hc)
            @test Artifacts.query_override(SHA1(hc)) == joinpath(depot, "artifacts", he)
        end
    end
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
        cxxlib in ("libstdcxx",),
        cxxlib_version in (v"3.4.11", v"3.4.19", nothing),
        cxxstring_abi in ("cxx03", "cxx11", nothing)

        for arch in ("x86_64", "i686", "aarch64", "armv7l"),
            libc in ("glibc", "musl")

            push!(platforms, Platform(arch, "linux"; libc, libgfortran_version, cxxlib, cxxlib_version, cxxstring_abi))
        end
        push!(platforms, Platform("x86_64", "windows"; libgfortran_version, cxxlib, cxxlib_version, cxxstring_abi))
        push!(platforms, Platform("i686", "windows"; libgfortran_version, cxxlib, cxxlib_version, cxxstring_abi))
        push!(platforms, Platform("x86_64", "macOS"; libgfortran_version, cxxlib, cxxlib_version, cxxstring_abi))
        push!(platforms, Platform("aarch64", "macOS"; libgfortran_version, cxxlib, cxxlib_version, cxxstring_abi))
        push!(platforms, Platform("x86_64", "FreeBSD"; libgfortran_version, cxxlib, cxxlib_version, cxxstring_abi))
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
    # Use the Linux OS on an ARMv7L architecture for the tests to make tests reproducible
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

    @test select_downloadable_artifacts(joinpath(@__DIR__, "nonexistent.toml")) == Dict{String,Any}()

    artifacts_toml = SubString(joinpath(@__DIR__, "Artifacts.toml"), 1)
    artifacts = select_downloadable_artifacts(artifacts_toml; platform=armv7l_linux, include_lazy=true)
    @test sort(collect(keys(artifacts))) == ["HelloWorldC", "socrates"]
    @test artifacts["HelloWorldC"]["git-tree-sha1"] == "0a8e7b523ef6be31311aefe9983a488616e58201"
    artifact_dict = Artifacts.load_artifacts_toml(artifacts_toml)
    artifacts = select_downloadable_artifacts(artifact_dict, artifacts_toml; platform=armv7l_linux, include_lazy=true)
    @test sort(collect(keys(artifacts))) == ["HelloWorldC", "socrates"]
end

@testset "artifact_meta()" begin
    linux64 = Platform("x86_64", "linux")
    win64 = Platform("x86_64", "windows")
    mac64 = Platform("x86_64", "macos")
    mktempdir() do dir
        artifacts_toml = joinpath(dir, "Artifacts.toml")
        write(artifacts_toml, """
            malformed = "oops"

            [[plat]]
            arch = "x86_64"
            os = "linux"
            git-tree-sha1 = "$("a"^40)"

            [[plat]]
            arch = "x86_64"
            os = "windows"
            git-tree-sha1 = "$("b"^40)"

            [single]
            git-tree-sha1 = "$("c"^40)"

            [nohash]
            lazy = true
            """)
        @test artifact_meta("plat", artifacts_toml; platform=linux64)["git-tree-sha1"] == "a"^40
        @test artifact_meta("plat", artifacts_toml; platform=win64)["git-tree-sha1"] == "b"^40
        @test artifact_meta("plat", artifacts_toml; platform=mac64) === nothing
        @test artifact_hash("plat", artifacts_toml; platform=mac64) === nothing
        @test artifact_meta(SubString("single"), SubString(artifacts_toml, 1); platform=mac64)["git-tree-sha1"] == "c"^40
        @test artifact_meta("missing", artifacts_toml) === nothing
        @test artifact_meta("single", joinpath(dir, "nonexistent.toml")) === nothing
        @test (@test_logs (:error, r"artifact 'malformed' malformed, must be array or dict") artifact_meta("malformed", artifacts_toml)) === nothing
        @test (@test_logs (:error, r"artifact 'nohash' contains no `git-tree-sha1`") artifact_meta("nohash", artifacts_toml)) === nothing

        artifact_dict = Artifacts.load_artifacts_toml(artifacts_toml)
        @test Artifacts.artifact_slash_lookup("plat/bin/foo", artifact_dict, artifacts_toml, linux64) ==
            ("plat", joinpath("bin", "foo"), SHA1("a"^40))
        @test Artifacts.artifact_slash_lookup(SubString("plat/bin"), artifact_dict, SubString(artifacts_toml, 1), win64) ==
            ("plat", "bin", SHA1("b"^40))
        @test_throws "Cannot locate artifact 'plat/bin' for x86_64-apple-darwin" Artifacts.artifact_slash_lookup("plat/bin", artifact_dict, artifacts_toml, mac64)
        @test_throws "Cannot locate artifact 'missing'" Artifacts.artifact_slash_lookup("missing", artifact_dict, artifacts_toml, linux64)
    end
end

@testset "find_artifacts_toml()" begin
    mktempdir() do dir
        pkg = joinpath(dir, "Pkg")
        mkpath(joinpath(pkg, "src", "sub"))
        write(joinpath(pkg, "Project.toml"), "")
        srcfile = joinpath(pkg, "src", "Pkg.jl")
        write(srcfile, "")
        # The search stops at the package's project file
        @test find_artifacts_toml(srcfile) === nothing

        write(joinpath(pkg, "Artifacts.toml"), "")
        @test find_artifacts_toml(srcfile) == joinpath(pkg, "Artifacts.toml")
        @test find_artifacts_toml(joinpath(pkg, "src", "sub")) == joinpath(pkg, "Artifacts.toml")
        @test find_artifacts_toml(SubString(srcfile, 1)) == joinpath(pkg, "Artifacts.toml")
        write(joinpath(pkg, "JuliaArtifacts.toml"), "")
        @test find_artifacts_toml(srcfile) == joinpath(pkg, "JuliaArtifacts.toml")

        loose = joinpath(dir, "loose")
        mkpath(joinpath(loose, "a", "b"))
        write(joinpath(loose, "Artifacts.toml"), "")
        @test find_artifacts_toml(joinpath(loose, "a", "b", "script.jl")) == joinpath(loose, "Artifacts.toml")

        root = dir
        while dirname(root) != root
            root = dirname(root)
        end
        @test find_artifacts_toml(root) === nothing

        rm(joinpath(pkg, "Artifacts.toml"))
        rm(joinpath(pkg, "JuliaArtifacts.toml"))
        ex = Expr(:macrocall, GlobalRef(Artifacts, Symbol("@artifact_str")), LineNumberNode(1, Symbol(srcfile)), "foo")
        @test_throws "Cannot locate '(Julia)Artifacts.toml' file when attempting to use artifact 'foo'" macroexpand(Module(), ex)
    end
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
                depwarn_flag = Base.JLOptions().depwarn
                # 0: --depwarn=no
                # 1: --depwarn=yes
                # 2: --depwarn=error
                if depwarn_flag == 0
                    @warn "Skipping one test, because we are running with --depwarn=no"
                    @test_skip false
                elseif depwarn_flag == 1
                    expected_msg = "using Pkg instead of using LazyArtifacts is deprecated"
                    @test_logs (:warn,expected_msg) Core.eval(anon, :(artifact"socrates"))
                elseif depwarn_flag == 2
                    Core.eval(anon, quote
                        try
                            artifact"socrates"
                            # The previous line should have thrown, so we should not reach the next line:
                            error("this @artifact_str macro invocation should have failed!")
                        catch e
                            @test startswith("using Pkg instead of using LazyArtifacts is deprecated", e.msg)
                        end
                    end)
                else
                    error("Unexpected value for Base.JLOptions().depwarn: $(depwarn_flag)")
                end
            end
        end
    end

    mktempdir() do tempdir
        hash = artifact_hash("HelloWorldC", joinpath(@__DIR__, "Artifacts.toml"))
        open(joinpath(tempdir, "Overrides.toml"), "w") do io
            TOML.print(io, Dict(bytes2hex(hash.bytes) => joinpath(tempdir, "missing")))
        end
        try
            with_artifacts_directory(SubString(tempdir, 1)) do
                load_overrides(force=true)
                ex = @test_throws ErrorException artifact"HelloWorldC"
                @test occursin(joinpath(tempdir, "missing"), ex.value.msg)
                @test occursin("Check that your `Overrides.toml` file is correct", ex.value.msg)
            end
        finally
            load_overrides(force=true)
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
