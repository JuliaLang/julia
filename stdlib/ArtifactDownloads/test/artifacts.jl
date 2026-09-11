# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Random, TOML
using ArtifactDownloads, Artifacts, Base.BinaryPlatforms
using ArtifactDownloads.PlatformEngines: download_verify_unpack
import Artifacts: with_artifacts_directory
import Base: SHA1
using ..ArtifactDownloadsTests: list_tarball_files

const fixtures_dir = joinpath(@__DIR__, "artifacts")

# Helper function to create an artifact, then chmod() the whole thing to 0o644.  This is
# important to keep hashes stable across platforms that have different umasks, changing
# the permissions within a tree hash, breaking our tests.
function create_artifact_chmod(f::Function)
    return create_artifact() do path
        f(path)

        # Change all files to have 644 permissions, leave directories alone
        for (root, dirs, files) in walkdir(path)
            for f in files
                f = joinpath(root, f)
                islink(f) || chmod(f, 0o644)
            end
        end
    end
end

@testset "pkg_server" begin
    withenv("JULIA_PKG_SERVER" => nothing) do
        @test ArtifactDownloads.pkg_server() == "https://pkg.julialang.org"
    end
    withenv("JULIA_PKG_SERVER" => "") do
        @test ArtifactDownloads.pkg_server() === nothing
    end
    withenv("JULIA_PKG_SERVER" => "foo.bar/") do
        @test ArtifactDownloads.pkg_server() == "https://foo.bar"
    end
    withenv("JULIA_PKG_SERVER" => "http://localhost:8000") do
        @test ArtifactDownloads.pkg_server() == "http://localhost:8000"
    end
end

@testset "Artifact Creation" begin
    # We're going to ensure that our artifact creation does in fact give git-tree-sha1's.
    creators = [
        # First test the empty artifact
        (
            path -> begin
                # add no contents
            end, "4b825dc642cb6eb9a060e54bf8d69288fbee4904",
        ),

        # Next test creating a single file
        (
            path -> begin
                open(joinpath(path, "foo"), "w") do io
                    print(io, "Hello, world!")
                end
            end, "339aad93c0f854604248ea3b7c5b7edea20625a9",
        ),

        # Next we will test creating multiple files
        (
            path -> begin
                open(joinpath(path, "foo1"), "w") do io
                    print(io, "Hello")
                end
                open(joinpath(path, "foo2"), "w") do io
                    print(io, "world!")
                end
            end, "98cda294312216b19e2a973e9c291c0f5181c98c",
        ),

        # Finally, we will have nested directories and all that good stuff
        (
            path -> begin
                mkpath(joinpath(path, "bar", "bar"))
                open(joinpath(path, "bar", "bar", "foo1"), "w") do io
                    print(io, "Hello")
                end
                open(joinpath(path, "bar", "foo2"), "w") do io
                    print(io, "world!")
                end
                open(joinpath(path, "foo3"), "w") do io
                    print(io, "baz!")
                end

                # Empty directories do nothing to effect the hash, so we create one with a
                # random name to prove that it does not get hashed into the rest.  Also, it
                # turns out that life is complex enough that we need to test the nested
                # empty directories case as well.
                rand_dir = joinpath(path, Random.randstring(8), "inner")
                mkpath(rand_dir)

                # Symlinks are not followed, even if they point to directories
                symlink("foo3", joinpath(path, "foo3_link"))
                symlink("../bar", joinpath(path, "bar", "infinite_link"))
            end, "86a1ce580587d5851fdfa841aeb3c8d55663f6f9",
        ),
    ]

    for (creator, known_hash) in creators
        # Create artifact
        hash = create_artifact_chmod(creator)

        # Ensure it hashes to the correct gitsha:
        @test all(hash.bytes .== hex2bytes(known_hash))

        # Test that we can look it up and that it sits in the right place
        @test basename(dirname(artifact_path(hash))) == "artifacts"
        @test basename(artifact_path(hash)) == known_hash
        @test artifact_exists(hash)

        # Test that the artifact verifies
        @test verify_artifact(hash)
    end

    @testset "File permissions" begin
        mktempdir() do artifacts_dir
            with_artifacts_directory(artifacts_dir) do
                subdir = "subdir"
                file1 = "file1"
                file2 = "file2"
                dir_link = "dir_link"
                file_link = "file_link"
                hash = create_artifact() do dir
                    # Create files, links, and directories
                    mkpath(joinpath(dir, subdir))
                    touch(joinpath(dir, subdir, file1))
                    touch(joinpath(dir, subdir, file2))
                    symlink(basename(subdir), joinpath(dir, dir_link))
                    symlink(basename(file1), joinpath(dir, subdir, file_link))
                end
                artifact_dir = artifact_path(hash)
                # Make sure only files are read-only
                @test iszero(filemode(joinpath(artifact_dir, file1)) & 0o222)
                @test iszero(filemode(joinpath(artifact_dir, file2)) & 0o222)
                @test iszero(filemode(joinpath(artifact_dir, file_link)) & 0o222)
                @test !iszero(filemode(joinpath(artifact_dir, subdir)) & 0o222)
                @test !iszero(filemode(joinpath(artifact_dir, dir_link)) & 0o222)
                # Make sure we can delete the artifact directory without having
                # to manually change permissions
                rm(artifact_dir; recursive = true)
            end
        end
    end
end

@testset "with_artifacts_directory()" begin
    mktempdir() do art_dir
        with_artifacts_directory(art_dir) do
            hash = create_artifact() do path
                touch(joinpath(path, "foo"))
            end
            @test startswith(artifact_path(hash), art_dir)
        end
    end
end

@testset "Artifacts.toml Utilities" begin
    artifacts_toml = joinpath(fixtures_dir, "Artifacts.toml")
    arty_hash = SHA1("43563e7631a7eafae1f9f8d9d332e3de44ad7239")
    @test artifact_hash("arty", artifacts_toml) == arty_hash
    @test arty_hash in extract_all_hashes(artifacts_toml)
    # `socrates` is lazy, so it is only listed on request
    @test count(==(arty_hash), extract_all_hashes(artifacts_toml)) == 1
    @test count(==(arty_hash), extract_all_hashes(artifacts_toml; include_lazy = true)) == 2

    mktempdir() do dir
        with_artifacts_directory(dir) do
            # Ensure it's installable (we uninstall first, to make sure)
            @test !artifact_exists(arty_hash)

            @test ensure_artifact_installed("arty", artifacts_toml) == artifact_path(arty_hash)
            @test verify_artifact(arty_hash)

            # Make sure doing it twice "just works"
            @test ensure_artifact_installed("arty", artifacts_toml) == artifact_path(arty_hash)

            # clean up after thyself
            remove_artifact(arty_hash)
            @test !verify_artifact(arty_hash)
        end
    end

    # Test binding/unbinding
    mktempdir() do path
        hash = create_artifact() do path
            open(joinpath(path, "foo.txt"), "w") do io
                print(io, "hello, world!")
            end
        end

        # Bind this artifact to something
        artifacts_toml = joinpath(path, "Artifacts.toml")
        @test artifact_hash("foo_txt", artifacts_toml) == nothing
        bind_artifact!(artifacts_toml, "foo_txt", hash)

        # Test that this binding worked
        @test artifact_hash("foo_txt", artifacts_toml) == hash
        @test ensure_artifact_installed("foo_txt", artifacts_toml) == artifact_path(hash)

        # Test that binding caused an entry in the artifact_usage.toml
        usage = TOML.parsefile(joinpath(ArtifactDownloads.logdir(), "artifact_usage.toml"))
        @test any(x -> startswith(x, artifacts_toml), keys(usage))

        # Test that we can overwrite bindings
        hash2 = create_artifact() do path
            open(joinpath(path, "foo.txt"), "w") do io
                print(io, "goodbye, world!")
            end
        end
        @test_throws ErrorException bind_artifact!(artifacts_toml, "foo_txt", hash2)
        @test artifact_hash("foo_txt", artifacts_toml) == hash
        bind_artifact!(artifacts_toml, "foo_txt", hash2; force = true)
        @test artifact_hash("foo_txt", artifacts_toml) == hash2

        # Test that we can un-bind
        unbind_artifact!(artifacts_toml, "foo_txt")
        @test artifact_hash("foo_txt", artifacts_toml) == nothing

        # Test platform-specific binding and providing download_info
        download_info = [
            ArtifactDownloadInfo("http://google.com/hello_world", "0"^64),
            ArtifactDownloadInfo("http://microsoft.com/hello_world", "a"^64, 1),
        ]

        # First, test the binding of things with various platforms and overwriting and such works properly
        linux64 = Platform("x86_64", "linux")
        win32 = Platform("i686", "windows")
        bind_artifact!(artifacts_toml, "foo_txt", hash; download_info = download_info, platform = linux64)
        @test artifact_hash("foo_txt", artifacts_toml; platform = linux64) == hash
        @test artifact_hash("foo_txt", artifacts_toml; platform = Platform("x86_64", "macos")) == nothing
        @test_throws ErrorException bind_artifact!(artifacts_toml, "foo_txt", hash2; download_info = download_info, platform = linux64)
        bind_artifact!(artifacts_toml, "foo_txt", hash; download_info = download_info, platform = win32)
        bind_artifact!(artifacts_toml, "foo_txt", hash2; download_info = download_info, platform = linux64, force = true)
        @test artifact_hash("foo_txt", artifacts_toml; platform = linux64) == hash2
        @test artifact_hash("foo_txt", artifacts_toml; platform = win32) == hash
        @test ensure_artifact_installed("foo_txt", artifacts_toml; platform = linux64) == artifact_path(hash2)
        @test ensure_artifact_installed("foo_txt", artifacts_toml; platform = win32) == artifact_path(hash)

        # Default HostPlatform() adds a compare_strategy key that doesn't get picked up from
        # the Artifacts.toml
        testhost = Platform("x86_64", "linux", Dict("libstdcxx_version" => "1.2.3"))
        # Newer Julia translates the `libstdcxx_version` tag into `cxxlib_version`
        # (with `cxxlib=libstdcxx`), so set the compare strategy on whichever version
        # tag the platform actually ended up with.
        version_key = haskey(tags(testhost), "libstdcxx_version") ? "libstdcxx_version" : "cxxlib_version"
        BinaryPlatforms.set_compare_strategy!(testhost, version_key, BinaryPlatforms.compare_version_cap)
        @test_throws ErrorException bind_artifact!(artifacts_toml, "foo_txt", hash; download_info = download_info, platform = testhost)

        # Next, check that we can get the download_info properly:
        meta = artifact_meta("foo_txt", artifacts_toml; platform = win32)
        @test meta["download"][1]["url"] == "http://google.com/hello_world"
        @test !haskey(meta["download"][1], "size")
        @test meta["download"][2]["sha256"] == "a"^64
        @test meta["download"][2]["size"] == 1

        rm(artifacts_toml)

        # test relative Artifacts.toml paths (https://github.com/simeonschaub/ArtifactUtils.jl/issues/19)
        cd(path) do
            hash3 = create_artifact() do path
                open(joinpath(path, "foo.txt"), "w") do io
                    print(io, "bla bla")
                end
            end

            # Bind this artifact to something
            artifacts_toml = "Artifacts.toml" # no parent dir specified
            @test artifact_hash("foo_txt", artifacts_toml) == nothing
            bind_artifact!(artifacts_toml, "foo_txt", hash3)

            # Test that this binding worked
            @test artifact_hash("foo_txt", artifacts_toml) == hash3
            @test ensure_artifact_installed("foo_txt", artifacts_toml) == artifact_path(hash3)
        end
    end

    # Let's test some known-bad Artifacts.toml files
    badifact_dir = joinpath(fixtures_dir, "bad")

    # First, parsing errors
    @test_logs (:error, r"contains no `git-tree-sha1`") artifact_meta("broken_artifact", joinpath(badifact_dir, "no_gitsha.toml"))
    @test_logs (:error, r"malformed, must be array or dict!") artifact_meta("broken_artifact", joinpath(badifact_dir, "not_a_table.toml"))

    # Next, test incorrect download errors
    for ignore_hash in (false, true)
        withenv("JULIA_PKG_IGNORE_HASHES" => ignore_hash ? "1" : nothing) do
            mktempdir() do dir
                with_artifacts_directory(dir) do
                    @test artifact_meta("broken_artifact", joinpath(badifact_dir, "incorrect_gitsha.toml")) != nothing
                    if !ignore_hash
                        @test_throws ErrorException ensure_artifact_installed("broken_artifact", joinpath(badifact_dir, "incorrect_gitsha.toml"))
                    else
                        @test_logs (:error, r"Tree Hash Mismatch!") match_mode = :any begin
                            path = ensure_artifact_installed("broken_artifact", joinpath(badifact_dir, "incorrect_gitsha.toml"))
                            @test endswith(path, "0000000000000000000000000000000000000000")
                            @test isdir(path)
                        end
                    end
                end
            end
        end
    end

    mktempdir() do dir
        with_artifacts_directory(dir) do
            @test artifact_meta("broken_artifact", joinpath(badifact_dir, "incorrect_sha256.toml")) != nothing
            @test_throws r"Hash Mismatch!" ensure_artifact_installed("broken_artifact", joinpath(badifact_dir, "incorrect_sha256.toml"))

            artifact_toml = joinpath(badifact_dir, "doesnotexist.toml")
            @test_throws ErrorException ensure_artifact_installed("does_not_exist", artifact_toml)
        end
    end
end

@testset "download_artifact reports failure as a value" begin
    mktempdir() do dir
        with_artifacts_directory(dir) do
            # nothing listens here, so this fails fast without touching the network
            ret = download_artifact(SHA1("0"^40), "http://127.0.0.1:1/nothing.tar.gz", "0"^64; quiet_download = true)
            @test ret !== true
            @test ret isa Exception
            @test !artifact_exists(SHA1("0"^40))
        end
    end
end

@testset "Artifact archival" begin
    mktempdir() do art_dir
        with_artifacts_directory(art_dir) do
            hash = create_artifact(p -> touch(joinpath(p, "foo")))
            tarball_path = joinpath(art_dir, "foo.tar.gz")
            archive_artifact(hash, tarball_path)
            @test "foo" in list_tarball_files(tarball_path)

            # Test archiving something that doesn't exist fails
            remove_artifact(hash)
            @test_throws ErrorException archive_artifact(hash, tarball_path)
        end
    end
end

@testset "installing artifacts when symlinks are copied" begin
    # copy symlinks to simulate the typical Microsoft Windows user experience where
    # developer mode is not enabled (no admin rights)
    withenv("BINARYPROVIDER_COPYDEREF" => "true", "JULIA_PKG_IGNORE_HASHES" => "true") do
        mktempdir() do dir
            with_artifacts_directory(dir) do
                artifacts_toml = joinpath(fixtures_dir, "Artifacts.toml")
                cts_real_hash = create_artifact() do dir
                    local meta = artifact_meta("collapse_the_symlink", artifacts_toml)
                    local collapse_url = meta["download"][1]["url"]
                    local collapse_hash = meta["download"][1]["sha256"]
                    # Because "BINARYPROVIDER_COPYDEREF"=>"true", this will copy symlinks.
                    download_verify_unpack(collapse_url, collapse_hash, dir; verbose = true, ignore_existence = true)
                end
                cts_hash = artifact_hash("collapse_the_symlink", artifacts_toml)
                @test !artifact_exists(cts_hash)
                @test artifact_exists(cts_real_hash)
                @test_logs (:error, r"Tree Hash Mismatch!") match_mode = :any ensure_artifact_installed("collapse_the_symlink", artifacts_toml; pkg_server_eligible = false)
                @test artifact_exists(cts_hash)
                # Make sure existing artifacts don't get deleted.
                @test artifact_exists(cts_real_hash)
            end
        end
    end
end
