using BinaryProvider
using Compat
using Compat.Test
using SHA

# The platform we're running on
const platform = platform_key()

# Useful command to launch `sh` on any platform
const sh = gen_sh_cmd

# Output of a few scripts we are going to run
const simple_out = "1\n2\n3\n4\n"
const long_out = join(["$(idx)\n" for idx in 1:100], "")
const newlines_out = join(["marco$d\npolo$d\n" for d in ("","\r","\r\n")], "")

# Explicitly probe platform engines in verbose mode to get coverage and make
# CI debugging easier
BinaryProvider.probe_platform_engines!(;verbose=true)

@testset "OutputCollector" begin
    cd("output_tests") do
        # Collect the output of `simple.sh``
        oc = OutputCollector(sh(`./simple.sh`))

        # Ensure we can wait on it and it exited properly
        @test wait(oc)

        # Ensure further waits are fast and still return 0
        let
            tstart = time()
            @test wait(oc)
            @test time() - tstart < 0.1
        end

        # Test that we can merge properly
        @test merge(oc) == simple_out

        # Test that merging twice works
        @test merge(oc) == simple_out

        # Test that `tail()` gives the same output as well
        @test tail(oc) == simple_out

        # Test that colorization works
        let
            red = Base.text_colors[:red]
            def = Base.text_colors[:default]
            gt = "1\n$(red)2\n$(def)3\n4\n"
            @test merge(oc; colored=true) == gt
            @test tail(oc; colored=true) == gt
        end

        # Test that we can grab stdout and stderr separately
        @test stdout(oc) == "1\n3\n4\n"
        @test stderr(oc) == "2\n"
    end

    # Next test a much longer output program
    cd("output_tests") do
        oc = OutputCollector(sh(`./long.sh`))

        # Test that it worked, we can read it, and tail() works
        @test wait(oc)
        @test merge(oc) == long_out
        @test tail(oc; len=10) == join(["$(idx)\n" for idx in 91:100], "")
    end

    # Next, test a command that fails
    cd("output_tests") do
        oc = OutputCollector(sh(`./fail.sh`))

        @test !wait(oc)
        @test merge(oc) == "1\n2\n"
    end

    # Next, test a command that kills itself (NOTE: This doesn't work on windows.  sigh.)
    @static if !is_windows()
        cd("output_tests") do
            oc = OutputCollector(sh(`./kill.sh`))

            @test !wait(oc)
            @test stdout(oc) == "1\n2\n"
        end
    end

    # Next, test reading the output of a pipeline()
    grepline = pipeline(sh(`-c 'printf "Hello\nWorld\nJulia"'`), `grep ul`)
    oc = OutputCollector(grepline)

    @test wait(oc)
    @test merge(oc) == "Julia\n"

    # Next, test that \r and \r\n are treated like \n
    cd("output_tests") do
        oc = OutputCollector(sh(`./newlines.sh`))

        @test wait(oc)
        @test stdout(oc) == newlines_out
    end

    # Next, test that tee'ing to a stream works
    cd("output_tests") do
        ios = IOBuffer()
        oc = OutputCollector(sh(`./simple.sh`); tee_stream=ios, verbose=true)
        @test wait(oc)
        @test merge(oc) == simple_out
        seekstart(ios)
        println(readstring(ios))
    end
end

@testset "PlatformNames" begin
    # Ensure the platform type constructors are well behaved
    @test_throws ArgumentError Linux(:not_a_platform)
    @test_throws ArgumentError MacOS(:i686)
    @test_throws ArgumentError Windows(:armv7l)
    @test_throws ArgumentError Linux(:x86_64, :crazy_libc)

    # Test that our platform_dlext stuff works
    @test platform_dlext(Linux(:x86_64)) == platform_dlext(Linux(:i686))
    @test platform_dlext(Windows(:x86_64)) == platform_dlext(Windows(:i686))
    @test platform_dlext(MacOS()) != platform_dlext(Linux(:armv7l))

    # Test some valid dynamic library paths
    @test valid_dl_path("libfoo.so.1.2.3", Linux(:x86_64))
    @test valid_dl_path("libfoo-1.dll", Windows(:x86_64))
    @test valid_dl_path("libfoo.1.2.3.dylib", MacOS())
    @test !valid_dl_path("libfoo.dylib", Linux(:x86_64))
    @test !valid_dl_path("libfoo.so", Windows(:x86_64))

    # Make sure the platform_key() with explicit triplet works or doesn't
    @test platform_key("x86_64-linux-gnu") == Linux(:x86_64)
    @test platform_key("i686-unknown-linux-gnu") == Linux(:i686)
    @test platform_key("x86_64-apple-darwin14") == MacOS()
    @test platform_key("armv7l-pc-linux-gnueabihf") == Linux(:armv7l)
    @test platform_key("aarch64-unknown-linux-gnu") == Linux(:aarch64)
    @test platform_key("powerpc64le-linux-gnu") == Linux(:ppc64le)
    @test platform_key("x86_64-w64-mingw32") == Windows(:x86_64)
    @test platform_key("i686-w64-mingw32") == Windows(:i686)
    @test_throws ArgumentError platform_key("invalid-triplet-yo")
    @test_throws ArgumentError platform_key("aarch64-unknown-gnueabihf")
    @test_throws ArgumentError platform_key("x86_64-w32-mingw64")

    # Test that we can indeed ask if something is linux or windows, etc...
    @test Compat.Sys.islinux(Linux(:aarch64))
    @test !Compat.Sys.islinux(Windows(:x86_64))
    @test Compat.Sys.iswindows(Windows(:i686))
    @test !Compat.Sys.iswindows(Linux(:x86_64))
    @test Compat.Sys.isapple(MacOS())
    @test !Compat.Sys.isapple(Linux(:ppc64le))

    # Test that every supported platform is _something_
    if isdefined(Base.Sys, :isapple)
        isbasesomething(p) = Sys.islinux(p) || Sys.iswindows(p) || Sys.isapple(p)
        @test all(isbasesomething, supported_platforms())
    end
    issomething(p) = Compat.Sys.islinux(p) || Compat.Sys.iswindows(p) ||
                     Compat.Sys.isapple(p)
    @test all(issomething, supported_platforms())

    @test wordsize(Linux(:i686)) == wordsize(Linux(:armv7l)) == 32
    @test wordsize(MacOS()) == wordsize(Linux(:aarch64)) == 64

    @test triplet(Windows(:i686)) == "i686-w64-mingw32"
    @test triplet(Linux(:x86_64, :musl)) == "x86_64-linux-musl"
    @test triplet(Linux(:armv7l, :musl)) == "arm-linux-musleabihf"
    @test triplet(Linux(:x86_64)) == "x86_64-linux-gnu"
    @test triplet(Linux(:armv7l)) == "arm-linux-gnueabihf"
    @test triplet(MacOS()) == "x86_64-apple-darwin14"
end

@testset "Prefix" begin
    mktempdir() do temp_dir
        prefix = Prefix(temp_dir)

        # Test that it's taking the absolute path
        @test prefix.path == abspath(temp_dir)

        # Test that `bindir()`, `libdir()` and `includedir()` all work
        for dir in unique([bindir(prefix), libdir(prefix), includedir(prefix)])
            @test !isdir(dir)
            mkpath(dir)
        end

        # Create a little script within the bindir to ensure we can run it
        ppt_path = joinpath(bindir(prefix), "prefix_path_test.sh")
        open(ppt_path, "w") do f
            write(f, "#!/bin/sh\n")
            write(f, "echo yolo\n")
        end
        chmod(ppt_path, 0o775)

        # Test that activation adds certain paths to our environment variables
        activate(prefix)

        # PATH[1] should be "<prefix>/bin" now
        @test BinaryProvider.split_PATH()[1] == bindir(prefix)
        @test Libdl.DL_LOAD_PATH[1] == libdir(prefix)

        # Test we can run the script we dropped within this prefix.  Once again,
        # something about Windows | busybox | Julia won't pick this up even though
        # the path clearly points to the file.  :(
        @static if !is_windows()
            @test success(sh(`$(ppt_path)`))
            @test success(sh(`prefix_path_test.sh`))
        end

        # Now deactivate and make sure that all traces are gone
        deactivate(prefix)
        @test BinaryProvider.split_PATH()[1] != bindir(prefix)
        @test Libdl.DL_LOAD_PATH[1] != libdir(prefix)
    end
end

@testset "Products" begin
    temp_prefix() do prefix
        # Test that basic satisfication is not guaranteed
        e_path = joinpath(bindir(prefix), "fooifier")
        l_path = joinpath(libdir(prefix), "libfoo.$(Libdl.dlext)")
        e = ExecutableProduct(prefix, "fooifier")
        ef = FileProduct(e_path)
        l = LibraryProduct(prefix, "libfoo")
        lf = FileProduct(l_path)

        @test !satisfied(e; verbose=true)
        @test !satisfied(ef; verbose=true)
        @test !satisfied(l, verbose=true)
        @test !satisfied(lf, verbose=true)

        # Test that simply creating a file that is not executable doesn't
        # satisfy an Executable Product (and say it's on Linux so it doesn't
        # complain about the lack of an .exe extension)
        mkpath(bindir(prefix))
        touch(e_path)
        @test satisfied(ef, verbose=true)
        @static if !is_windows()
            # Windows doesn't care about executable bit, grumble grumble
            @test !satisfied(e, verbose=true, platform=Linux(:x86_64))
        end

        # Make it executable and ensure this does satisfy the Executable
        chmod(e_path, 0o777)
        @test satisfied(e, verbose=true, platform=Linux(:x86_64))

        # Remove it and add a `$(path).exe` version to check again, this
        # time saying it's a Windows executable
        rm(e_path; force=true)
        touch("$(e_path).exe")
        chmod("$(e_path).exe", 0o777)
        @test locate(e, platform=Windows(:x86_64)) == "$(e_path).exe"

        # Test that simply creating a library file doesn't satisfy it if we are
        # testing something that matches the current platform's dynamic library
        # naming scheme, because it must be `dlopen()`able.
        mkpath(libdir(prefix))
        touch(l_path)
        @test satisfied(lf, verbose=true)
        @test !satisfied(l, verbose=true)

        # But if it is from a different platform, simple existence will be
        # enough to satisfy a LibraryProduct
        @static if is_windows()
            l_path = joinpath(libdir(prefix), "libfoo.so")
            touch(l_path)
            @test satisfied(l, verbose=true, platform=Linux(:x86_64))
        else
            l_path = joinpath(libdir(prefix), "libfoo.dll")
            touch(l_path)
            @test satisfied(l, verbose=true, platform=Windows(:x86_64))
        end
    end

    # Ensure that the test suite thinks that these libraries are foreign
    # so that it doesn't try to `dlopen()` them:
    foreign_platform = @static if platform_key() == Linux(:aarch64)
        # Arbitrary architecture that is not dlopen()'able
        Linux(:ppc64le)
    else
        # If we're not Linux(:aarch64), then say the libraries are
        Linux(:aarch64)
    end

    # Test for valid library name permutations
    for ext in ["1.so", "so", "so.1", "so.1.2", "so.1.2.3"]
        temp_prefix() do prefix
            l_path = joinpath(libdir(prefix), "libfoo.$ext")
            l = LibraryProduct(prefix, "libfoo")
            mkdir(dirname(l_path))
            touch(l_path)
            @test satisfied(l; verbose=true, platform=foreign_platform)
        end
    end

    # Test for invalid library name permutations
    for ext in ["so.1.2.3a", "so.1.a"]
        temp_prefix() do prefix
            l_path = joinpath(libdir(prefix), "libfoo.$ext")
            l = LibraryProduct(prefix, "libfoo")
            mkdir(dirname(l_path))
            touch(l_path)
            @test !satisfied(l; verbose=true, platform=foreign_platform)
        end
    end
end

@testset "Packaging" begin
    # Clear out previous build products
    for f in readdir(".")
        if !endswith(f, ".tar.gz") || !endswith(f, ".sha256")
            continue
        end
        rm(f; force=true)
    end

    # Gotta set this guy up beforehand
    tarball_path = nothing
    tarball_hash = nothing

    temp_prefix() do prefix
        # Create random files
        mkpath(bindir(prefix))
        mkpath(libdir(prefix))
        bar_path = joinpath(bindir(prefix), "bar.sh")
        open(bar_path, "w") do f
            write(f, "#!/bin/sh\n")
            write(f, "echo yolo\n")
        end
        baz_path = joinpath(libdir(prefix), "baz.so")
        open(baz_path, "w") do f
            write(f, "this is not an actual .so\n")
        end

        # Next, package it up as a .tar.gz file
        tarball_path, tarball_hash = package(prefix, "./libfoo"; verbose=true)
        @test isfile(tarball_path)

        # Check that we are calculating the hash properly
        tarball_hash_check = open(tarball_path, "r") do f
            bytes2hex(sha256(f))
        end
        @test tarball_hash_check == tarball_hash

        # Test that packaging into a file that already exists fails
        @test_throws ErrorException package(prefix, "./libfoo")
    end

    # Test that we can inspect the contents of the tarball
    contents = list_tarball_files(tarball_path)
    const libdir_name = is_windows() ? "bin" : "lib"
    @test joinpath("bin", "bar.sh") in contents
    @test joinpath(libdir_name, "baz.so") in contents

    # Install it within a new Prefix
    temp_prefix() do prefix
        # Install the thing
        @test install(tarball_path, tarball_hash; prefix=prefix, verbose=true)

        # Ensure we can use it
        bar_path = joinpath(bindir(prefix), "bar.sh")
        baz_path = joinpath(libdir(prefix), "baz.so")

        # Ask for the manifest that contains these files to ensure it works
        manifest_path = manifest_for_file(bar_path; prefix=prefix)
        @test isfile(manifest_path)
        manifest_path = manifest_for_file(baz_path; prefix=prefix)
        @test isfile(manifest_path)

        # Ensure that manifest_for_file doesn't work on nonexistant files
        @test_throws ErrorException manifest_for_file("nonexistant"; prefix=prefix)

        # Ensure that manifest_for_file doesn't work on orphan files
        orphan_path = joinpath(bindir(prefix), "orphan_file")
        touch(orphan_path)
        @test isfile(orphan_path)
        @test_throws ErrorException manifest_for_file(orphan_path; prefix=prefix)

        # Ensure that trying to install again over our existing files is an error
        @test_throws ErrorException install(tarball_path, tarball_path; prefix=prefix)

        # Ensure we can uninstall this tarball
        @test uninstall(manifest_path; verbose=true)
        @test !isfile(bar_path)
        @test !isfile(baz_path)
        @test !isfile(manifest_path)

        # Ensure that we don't want to install tarballs from other platforms
        cp(tarball_path, "./libfoo_juliaos64.tar.gz")
        @test_throws ArgumentError install("./libfoo_juliaos64.tar.gz", tarball_hash; prefix=prefix)
        rm("./libfoo_juliaos64.tar.gz"; force=true)

        # Ensure that hash mismatches throw errors
        fake_hash = reverse(tarball_hash)
        @test_throws ErrorException install(tarball_path, fake_hash; prefix=prefix)
    end

    rm(tarball_path; force=true)
    rm("$(tarball_path).sha256"; force=true)
end

@testset "Verification" begin
    temp_prefix() do prefix
        foo_path = joinpath(prefix, "foo")
        open(foo_path, "w") do file
            write(file, "test")
        end
        foo_hash = bytes2hex(sha256("test"))

        # Check that verifying with the right hash works
        info("This should say; no hash cache found")
        @test verify(foo_path, foo_hash; verbose=true)

        # Check that it created a .sha256 file
        @test isfile("$(foo_path).sha256")

        # Check that it verifies the second time around properly
        info("This should say; hash cache is consistent")
        @test verify(foo_path, foo_hash; verbose=true)

        # Sleep for imprecise filesystems
        sleep(2)

        # Get coverage of messing with different parts of the verification chain
        touch(foo_path)
        info("This should say; file has been modified")
        @test verify(foo_path, foo_hash; verbose=true)
        @test_throws ErrorException verify(foo_path, "0"^32; verbose=true)
        touch(foo_path)
        @test verify(foo_path, foo_hash; verbose=true)
        open("$(foo_path).sha256", "w") do file
            write(file, "this is not the right hash")
        end
        info("This should say; hash has changed")
        @test verify(foo_path, foo_hash; verbose=true)
    end
end

# Use `build_libfoo_tarball.jl` in the BinDeps2.jl repository to generate more of these
const bin_prefix = "https://github.com/staticfloat/small_bin/raw/74b7fd81e3fbc8963b14b0ebbe5421e270d8bdcf"
const libfoo_downloads = Dict(
    Linux(:i686) =>     ("$bin_prefix/libfoo.i686-linux-gnu.tar.gz", "1398353bcbbd88338189ece9c1d6e7c508df120bc4f93afbaed362a9f91358ff"),
    Linux(:x86_64) =>   ("$bin_prefix/libfoo.x86_64-linux-gnu.tar.gz", "b9d57a6e032a56b1f8641771fa707523caa72f1a2e322ab99eeeb011f13ad9f3"),
    Linux(:aarch64) =>  ("$bin_prefix/libfoo.aarch64-linux-gnu.tar.gz", "19d9da0e6e7fb506bf4889eb91e936fda43493a39cd4fd7bd5d65506cede6f95"),
    Linux(:armv7l) =>   ("$bin_prefix/libfoo.arm-linux-gnueabihf.tar.gz", "8e33c1a0e091e6e5b8fcb902e5d45329791bb57763ee9cbcde49c1ec9bd8532a"),
    Linux(:ppc64le) =>  ("$bin_prefix/libfoo.powerpc64le-linux-gnu.tar.gz", "b48a64d48be994ec99b1a9fb60e0af7f4415a57596518cb90a340987b79fad81"),
    MacOS() =>          ("$bin_prefix/libfoo.x86_64-apple-darwin14.tar.gz", "661b71edb433ab334b0fef70db3b5c45d35f2b3bee0d244f54875f1ec899c10f"),
    Windows(:i686) =>   ("$bin_prefix/libfoo.i686-w64-mingw32.tar.gz", "3d4a8d4bf0169007a42d809a1d560083635b1540a1bc4a42108841dcb6d2aaea"),
    Windows(:x86_64) => ("$bin_prefix/libfoo.x86_64-w64-mingw32.tar.gz", "2d08fbc9a534cd021f36b6bbe86ddabb2dafbedeb589581240aa4a8c5b896055"),
)

# Test manually downloading and using libfoo
@testset "Downloading" begin
    temp_prefix() do prefix
        if !haskey(libfoo_downloads, platform)
            warn("Platform $platform does not have a libfoo download, skipping download tests")
        else
            # Test a good download works
            url, hash = libfoo_downloads[platform]
            @test install(url, hash; prefix=prefix, verbose=true)

            fooifier = ExecutableProduct(prefix, "fooifier")
            libfoo = LibraryProduct(prefix, "libfoo")

            @test satisfied(fooifier; verbose=true)
            @test satisfied(libfoo; verbose=true)

            fooifier_path = locate(fooifier)
            libfoo_path = locate(libfoo)


            # We know that foo(a, b) returns 2*a^2 - b
            result = 2*2.2^2 - 1.1

            # Test that we can invoke fooifier
            @test !success(`$fooifier_path`)
            @test success(`$fooifier_path 1.5 2.0`)
            @test parse(Float64,readchomp(`$fooifier_path 2.2 1.1`)) ≈ result

            # Test that we can dlopen() libfoo and invoke it directly
            hdl = Libdl.dlopen_e(libfoo_path)
            @test hdl != C_NULL
            foo = Libdl.dlsym_e(hdl, :foo)
            @test foo != C_NULL
            @test ccall(foo, Cdouble, (Cdouble, Cdouble), 2.2, 1.1) ≈ result
            Libdl.dlclose(hdl)

            # Test uninstallation
            @test uninstall(manifest_from_url(url; prefix=prefix); verbose=true)

            # Test that download_verify_unpack() works
            download_verify_unpack(url, hash, prefix.path)
            @test satisfied(fooifier; verbose=true)
            @test satisfied(libfoo; verbose=true)

            # Test that download_verify twice in a row works, and that mucking
            # with the file causes a redownload if `force` is true:
            tmpfile = joinpath(prefix, "libfoo.tar.gz")
            @test download_verify(url, hash, tmpfile; verbose=true)
            @test download_verify(url, hash, tmpfile; verbose=true)

            # We sleep for at least a second here so that filesystems with low
            # precision in their mtime implementations don't get confused
            sleep(2)

            open(tmpfile, "w") do f
                write(f, "hehehehe")
            end

            @test_throws ErrorException download_verify(url, hash, tmpfile; verbose=true)
            @test download_verify(url, hash, tmpfile; verbose=true, force=true)

        end

        # Test a bad download fails properly
        bad_url = "http://localhost:1/this_is_not_a_file.x86_64-linux-gnu.tar.gz"
        bad_hash = "0"^64
        @test_throws ErrorException install(bad_url, bad_hash; prefix=prefix, verbose=true)
    end
end

# Test the same as the above, but using BinaryPackage abstraction
@testset "BinaryPackage" begin
    temp_prefix() do prefix
        if !haskey(libfoo_downloads, platform)
            warn("Platform $platform does not have a libfoo download, skipping download tests")
        else
            url, hash = libfoo_downloads[platform]
            fooifier = ExecutableProduct(prefix, "fooifier")
            libfoo = LibraryProduct(prefix, "libfoo")
            binpkg = BinaryPackage(url, hash, platform, [fooifier, libfoo])

            # Test installation and uninstallation
            @test install(binpkg; prefix=prefix, verbose=true)
            @test uninstall(binpkg; prefix=prefix, verbose=true)

            # Now test that we can uninstall even if we don't have the right `url`:
            @test install(binpkg; prefix=prefix, verbose=true)
            binpkg2 = BinaryPackage("fakeurl", hash, platform, [fooifier, libfoo])
            @test uninstall(binpkg2; prefix=prefix, verbose=true)

            # Test that we can't uninstall from the wrong prefix
            temp_prefix() do wrong_prefix
                @test_throws ErrorException uninstall(binpkg; prefix=wrong_prefix, verbose=true)
            end

            # Test that we can't guess a manifest path from a package with the wrong
            # url and no products:
            binpkg3 = BinaryPackage("fakeurl", hash, platform)
            @test_throws ErrorException uninstall(binpkg3; prefix=prefix, verbose=true)
        end
    end
end

# Test installation and failure modes of the bundled LibFoo.jl
@testset "LibFoo.jl" begin
    const color="--color=$(Base.have_color ? "yes" : "no")"
    cd("LibFoo.jl") do
        rm("./deps/deps.jl"; force=true)
        rm("./deps/usr"; force=true, recursive=true)

        # Install `libfoo` and build the `deps.jl` file for `LibFoo.jl`
        run(`$(Base.julia_cmd()) $(color) deps/build.jl`)

        # Ensure `deps.jl` was actually created
        @test isfile("deps/deps.jl")
    end

    cd("LibFoo.jl/test") do
        # Now, run `LibFoo.jl`'s tests, adding `LibFoo.jl` to the LOAD_PATH
        # so that the tests can pick up the `LibFoo` module
        withenv("JULIA_LOAD_PATH"=>joinpath(pwd(),"..","src")) do
            run(`$(Base.julia_cmd()) $(color) runtests.jl`)
        end
    end
end
