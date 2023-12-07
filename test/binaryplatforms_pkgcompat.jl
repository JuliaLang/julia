module BinaryPlatformPkgCompatTests
# This test module was ported over from Pkg.jl/test/binaryplatforms.jl

using Test, Base.BinaryPlatforms.PkgCompat
import Base.BinaryPlatforms.PkgCompat: platform_name

# The platform we're running on
const platform = @inferred Platform platform_key_abi()

# This is a compatibility test; once we've fully migrated away from Pkg.BinaryPlatforms
# to the new Base.BinaryPlatforms module, we can throw away the shim definitions in
# `BinaryPlatforms_compat.jl` and drop these tests.
@testset "Compat - PlatformNames" begin
    # Ensure the platform type constructors are well behaved
    @testset "Platform constructors" begin
        @test_throws ArgumentError Linux(:not_a_platform)
        @test_throws ArgumentError Linux(:x86_64; libc=:crazy_libc)
        @test_throws ArgumentError Linux(:x86_64; libc=:glibc, call_abi=:crazy_abi)
        @test_throws ArgumentError Linux(:x86_64; libc=:glibc, call_abi=:eabihf)
        @test_throws ArgumentError Linux(:armv7l; libc=:glibc, call_abi=:kekeke)
        @test_throws ArgumentError MacOS(:i686)
        @test_throws ArgumentError MacOS(:x86_64; libc=:glibc)
        @test_throws ArgumentError MacOS(:x86_64; call_abi=:eabihf)
        @test_throws ArgumentError Windows(:x86_64; libc=:glibc)
        @test_throws ArgumentError Windows(:x86_64; call_abi=:eabihf)
        @test_throws ArgumentError FreeBSD(:not_a_platform)
        @test_throws ArgumentError FreeBSD(:x86_64; libc=:crazy_libc)
        @test_throws ArgumentError FreeBSD(:x86_64; call_abi=:crazy_abi)
        @test_throws ArgumentError FreeBSD(:x86_64; call_abi=:eabihf)

        # Test copy constructor
        cabi = CompilerABI(;
            libgfortran_version=v"3",
            libstdcxx_version=v"3.4.18",
            cxxstring_abi=:cxx03,
        )
        cabi2 = CompilerABI(cabi; cxxstring_abi=:cxx11)
        @test libgfortran_version(cabi) == libgfortran_version(cabi2)
        @test libstdcxx_version(cabi) == libstdcxx_version(cabi2)
        @test cxxstring_abi(cabi) != cxxstring_abi(cabi2)

        # Explicitly test that we can pass arguments to UnknownPlatform,
        # and it doesn't do anything.
        @test UnknownPlatform(:riscv; libc=:fuchsia_libc) == UnknownPlatform()
    end

    @testset "Platform properties" begin
        # Test that we can get the name of various platforms
        for T in (Linux, MacOS, Windows, FreeBSD)
            @test endswith(lowercase(string(T)), lowercase(platform_name(T(:x86_64))))
        end

        # Test that we can get the arch of various platforms
        @test arch(Linux(:aarch64; libc=:musl)) == :aarch64
        @test arch(Windows(:i686)) == :i686
        @test arch(FreeBSD(:amd64)) == :x86_64
        @test arch(FreeBSD(:i386)) == :i686
        @test arch(UnknownPlatform(:ppc64le)) == nothing

        # Test that our platform_dlext stuff works
        @test platform_dlext(Linux(:x86_64)) == platform_dlext(Linux(:i686))
        @test platform_dlext(Windows(:x86_64)) == platform_dlext(Windows(:i686))
        @test platform_dlext(MacOS()) != platform_dlext(Linux(:armv7l))
        @test platform_dlext(FreeBSD(:x86_64)) == platform_dlext(Linux(:x86_64))
        @test platform_dlext() == platform_dlext(platform)

        @test wordsize(Linux(:i686)) == wordsize(Linux(:armv7l)) == 32
        @test wordsize(MacOS()) == wordsize(Linux(:aarch64)) == 64
        @test wordsize(FreeBSD(:x86_64)) == wordsize(Linux(:powerpc64le)) == 64

        @test call_abi(Linux(:x86_64)) === nothing
        @test call_abi(Linux(:armv6l)) == :eabihf
        @test call_abi(Linux(:armv7l; call_abi=:eabihf)) == :eabihf
        @test call_abi(UnknownPlatform(;call_abi=:eabihf)) === nothing

        @test triplet(Windows(:i686)) == "i686-w64-mingw32"
        @test triplet(Linux(:x86_64; libc=:musl)) == "x86_64-linux-musl"
        @test triplet(Linux(:armv7l; libc=:musl)) == "armv7l-linux-musleabihf"
        @test triplet(Linux(:armv6l; libc=:musl, call_abi=:eabihf)) == "armv6l-linux-musleabihf"
        @test triplet(Linux(:x86_64)) == "x86_64-linux-gnu"
        @test triplet(Linux(:armv6l)) == "armv6l-linux-gnueabihf"
        @test triplet(MacOS()) == "x86_64-apple-darwin14"
        @test triplet(FreeBSD(:x86_64)) == "x86_64-unknown-freebsd11.1"
        @test triplet(FreeBSD(:i686)) == "i686-unknown-freebsd11.1"
    end

    @testset "Valid DL paths" begin
        # Test some valid dynamic library paths
        @test valid_dl_path("libfoo.so.1.2.3", Linux(:x86_64))
        @test valid_dl_path("libfoo.1.2.3.so", Linux(:x86_64))
        @test valid_dl_path("libfoo-1.2.3.dll", Windows(:x86_64))
        @test valid_dl_path("libfoo.1.2.3.dylib", MacOS())
        @test !valid_dl_path("libfoo.dylib", Linux(:x86_64))
        @test !valid_dl_path("libfoo.so", Windows(:x86_64))
        @test !valid_dl_path("libfoo.dll", MacOS())
        @test !valid_dl_path("libfoo.so.1.2.3.", Linux(:x86_64))
        @test !valid_dl_path("libfoo.so.1.2a.3", Linux(:x86_64))
    end

    @testset "platforms_match()" begin
        # Just do a quick combinatorial sweep for completeness' sake for platform matching
        for libgfortran_version in (nothing, v"3", v"5"),
            libstdcxx_version in (nothing, v"3.4.18", v"3.4.26"),
            cxxstring_abi in (nothing, :cxx03, :cxx11)

            cabi = CompilerABI(;
                libgfortran_version=libgfortran_version,
                libstdcxx_version=libstdcxx_version,
                cxxstring_abi=cxxstring_abi,
            )
            @test platforms_match(Linux(:x86_64), Linux(:x86_64, compiler_abi=cabi))
            @test platforms_match(Linux(:x86_64, compiler_abi=cabi), Linux(:x86_64))

            # Also test auto-string-parsing
            @test platforms_match(triplet(Linux(:x86_64)), Linux(:x86_64, compiler_abi=cabi))
            @test platforms_match(Linux(:x86_64), triplet(Linux(:x86_64, compiler_abi=cabi)))
        end

        # Ensure many of these things do NOT match
        @test !platforms_match(Linux(:x86_64), Linux(:i686))
        @test !platforms_match(Linux(:x86_64), Windows(:x86_64))
        @test !platforms_match(Linux(:x86_64), MacOS())
        @test !platforms_match(Linux(:x86_64), UnknownPlatform())

        # Make some explicitly non-matching cabi's
        base_cabi = CompilerABI(;
            libgfortran_version=v"5",
            cxxstring_abi=:cxx11,
        )
        for arch in (:x86_64, :i686, :aarch64, :armv6l, :armv7l),
            cabi in (
                CompilerABI(libgfortran_version=v"3"),
                CompilerABI(cxxstring_abi=:cxx03),
                CompilerABI(libgfortran_version=v"4", cxxstring_abi=:cxx11),
                CompilerABI(libgfortran_version=v"3", cxxstring_abi=:cxx03),
            )

            @test !platforms_match(Linux(arch, compiler_abi=base_cabi), Linux(arch, compiler_abi=cabi))
        end
    end

    @testset "Sys.is* overloading" begin
        # Test that we can indeed ask if something is linux or windows, etc...
        @test Sys.islinux(Linux(:aarch64))
        @test !Sys.islinux(Windows(:x86_64))
        @test Sys.iswindows(Windows(:i686))
        @test !Sys.iswindows(Linux(:x86_64))
        @test Sys.isapple(MacOS())
        @test !Sys.isapple(Linux(:powerpc64le))
        @test Sys.isbsd(MacOS())
        @test Sys.isbsd(FreeBSD(:x86_64))
        @test !Sys.isbsd(Linux(:powerpc64le; libc=:musl))
    end
end

end # module
