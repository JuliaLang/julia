# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Base.BinaryPlatforms, Base.BinaryPlatforms.CPUID

@testset "CPUID" begin
    @test CPUID.cpu_isa() isa CPUID.ISA

    get_x86_64(n) = (CPUID.ISAs_by_family["x86_64"][n].second)
    @test get_x86_64(2) <  get_x86_64(4)
    @test get_x86_64(5) <= get_x86_64(5)
    @test get_x86_64(3) >= get_x86_64(3)
    @test get_x86_64(7) >= get_x86_64(1)
    @test sort([get_x86_64(6), get_x86_64(4), get_x86_64(2), get_x86_64(4)]) ==
        [get_x86_64(2), get_x86_64(4), get_x86_64(4), get_x86_64(6)]
end

# Helper constructor to create a Platform with `validate_strict` set to `true`.
P(args...; kwargs...) = Platform(args...; validate_strict=true, kwargs...)

# Ensure the platform type constructors are well behaved
@testset "Platform validation" begin
    for os_name in ("Linux", "linux", "FREEBSD", "wiNDows", "MacOS")
        p = P("x86_64", os_name)
        @test isa(p, Platform)
        @test os(p) == lowercase(os_name)
    end
    for arch_name in ("x86_64", "ARMv7l", "armv6l", "I686", "pOWerpc64le")
        p = P(arch_name, "linux")
        @test isa(p, Platform)
        @test arch(p) == lowercase(arch_name)
    end
    # Test some normalization
    @test arch(P("amd64", "freebsd")) == "x86_64"
    @test arch(P("ARM", "linux")) == "armv7l"
    @test arch(P("ppc64le", "linux")) == "powerpc64le"

    # test some error cases
    @test_throws ArgumentError P("x64_86", "linux")
    @test_throws ArgumentError P("x86_64", "not_an_os")
    @test_throws ArgumentError P("x86_64", "linux"; libc="crazy_libc")
    @test_throws ArgumentError P("x86_64", "linux"; libc="glibc", call_abi="crazy_abi")
    @test_throws ArgumentError P("x86_64", "linux"; libc="glibc", call_abi="eabihf")
    @test_throws ArgumentError P("i686", "linux"; libc="musl", call_abi="eabi")
    @test_throws ArgumentError P("arm", "linux"; call_abi="")
    @test_throws ArgumentError P("armv7l", "linux"; call_abi="kekeke")
    @test_throws ArgumentError P("armv6l", "linux"; call_abi="kekeke")
    @test_throws ArgumentError P("armv6l", "linux"; libgfortran_version="lel")
    @test_throws ArgumentError P("x86_64", "linux"; cxxstring_abi="lel")
    @test_throws ArgumentError P("x86_64", "windows"; libstdcxx_version="lel")
    @test_throws ArgumentError P("i686", "macos")
    @test_throws ArgumentError P("x86_64", "macos"; libc="glibc")
    @test_throws ArgumentError P("x86_64", "macos"; call_abi="eabihf")
    @test_throws ArgumentError P("powerpc64le", "windows")
    @test_throws ArgumentError P("x86_64", "windows"; call_abi="eabihf")
    @test_throws ArgumentError P("x86_64", "freebsd"; libc="crazy_libc")
    @test_throws ArgumentError P("x86_64", "freebsd"; call_abi="crazy_abi")
    @test_throws ArgumentError P("x86_64", "freebsd"; call_abi="eabihf")
    @test_throws ArgumentError P("x86_64", "linux"; arch="i686")
    @test_throws ArgumentError P("x86_64", "linux"; ARCH="i686")
    @test_throws ArgumentError P("x86_64", "linux"; os="windows")
end

@testset "Platform properties" begin
    # Test that `platform_name()` works
    platforms = ("Linux", "macOS", "Windows", "FreeBSD")
    for platform in platforms
        @test platform_name(P("x86_64", platform)) == platform
    end

    # Test `arch()`
    arch_names = ("x86_64", "i686", "powerpc64le", "armv7l", "armv6l", "aarch64")
    for arch_name in arch_names
        @test arch(P(arch_name, "linux")) == arch_name
    end

    # Test that if we aren't using strict validation, we can actually use new names too:
    @test arch(Platform("jpu", "linux")) == "jpu"

    # platform_dlext()
    @test platform_dlext(P("x86_64", "linux")) == "so"
    @test platform_dlext(P("armv7l", "windows")) == "dll"
    @test platform_dlext(P("x86_64", "freebsd")) == "so"
    @test platform_dlext(P("aarch64", "macos")) == "dylib"
    @test platform_dlext() == platform_dlext(HostPlatform())

    # wordsize()
    @test wordsize(P("i686", "linux")) == wordsize(P("armv7l", "windows")) == 32
    @test wordsize(P("aarch64", "macos")) == wordsize(P("x86_64", "freebsd")) == 64
    @test wordsize(P("x86_64", "windows")) == wordsize(P("powerpc64le", "linux")) == 64

    # call_abi()
    for platform in platforms
        @test call_abi(P("x86_64", platform)) === nothing
    end
    @test call_abi(P("armv7l", "linux")) == "eabihf"
    @test call_abi(P("armv7l", "linux"; call_abi="eabihf")) == "eabihf"
    @test call_abi(P("armv7l", "linux"; call_abi="eabi")) == "eabi"
    @test call_abi(P("armv6l", "linux")) == "eabihf"
    # Test that we can at least set an `eabi` call ABI, not that Julia actually supports it...
    @test call_abi(P("armv7l", "linux"; call_abi="eabi")) == "eabi"

    # Test some different OS's and libc/call ABIs
    @test triplet(P("i686", "Windows")) == "i686-w64-mingw32"
    @test triplet(P("x86_64", "linux"; libc="musl")) == "x86_64-linux-musl"
    @test triplet(P("armv7l", "linux"; libc="musl")) == "armv7l-linux-musleabihf"
    @test triplet(P("armv6l", "linux"; libc="musl", call_abi="eabihf")) == "armv6l-linux-musleabihf"
    @test triplet(P("armv6l", "linux"; call_abi="eabi")) == "armv6l-linux-gnueabi"
    @test triplet(P("x86_64", "linux")) == "x86_64-linux-gnu"
    @test triplet(P("armv6l", "linux")) == "armv6l-linux-gnueabihf"
    @test triplet(P("x86_64", "macos")) == "x86_64-apple-darwin"
    @test triplet(P("x86_64", "macos"; os_version=v"16")) == "x86_64-apple-darwin16"
    @test triplet(P("x86_64", "freebsd")) == "x86_64-unknown-freebsd"
    @test triplet(P("i686", "freebsd")) == "i686-unknown-freebsd"

    # Now test libgfortran/cxxstring ABIs
    @test triplet(P("x86_64", "linux"; libgfortran_version=v"3", cxxstring_abi="cxx11")) == "x86_64-linux-gnu-libgfortran3-cxx11"
    @test triplet(P("armv7l", "linux"; libc="musl", cxxstring_abi="cxx03")) == "armv7l-linux-musleabihf-cxx03"
    if !isnothing(detect_libgfortran_version())
        # When `libgfortran` can be detected at runtime, make sure
        # `HostPlatform` has the appropriate key.
        @test tags(HostPlatform())["libgfortran_version"] == string(detect_libgfortran_version())
    end

    # Test tags()
    t = tags(P("x86_64", "linux"))
    @test all(haskey.(Ref(t), ("arch", "os", "libc")))
    @test haskey(tags(P("x86_64", "linux"; customtag="foo")), "customtag")
    @test tags(HostPlatform())["julia_version"] == string(VERSION.major, ".", VERSION.minor, ".", VERSION.patch)

    # Test that we can modify tags at will using the dict-like interface:
    p = P("x86_64", "linux")
    p["foo"] = "bar"
    @test tags(p)["foo"] == "bar"
    @test p["foo"] == "bar"
    @test p["os"] == "linux"
    p["os"] = "JuliaOS"
    @test p["os"] == "juliaos"

    # Test that trying to set illegal tags fails
    @test_throws ArgumentError p["os"] = "a+b"

    # Test that our `hash()` is stable
    @test hash(HostPlatform()) == hash(HostPlatform())

    # Test that round-tripping through `triplet` for a does not
    # maintain equality, as we end up losing the `compare_strategies`:
    p = Platform("x86_64", "linux"; cuda = v"11")
    Base.BinaryPlatforms.set_compare_strategy!(p, "cuda", Base.BinaryPlatforms.compare_version_cap)
    q = parse(Platform, triplet(p))
    @test q != p
end

@testset "Triplet parsing" begin
    # Make sure the Platform() with explicit triplet works
    R(str) = parse(Platform, str; validate_strict=true)
    @test R("x86_64-linux-gnu") == P("x86_64", "linux")
    @test R("x86_64-linux-musl") == P("x86_64", "linux"; libc="musl")
    @test R("i686-unknown-linux-gnu") == P("i686", "linux")
    @test R("x86_64-apple-darwin") == P("x86_64", "macos")
    @test R("x86_64-apple-darwin14") == P("x86_64", "macos"; os_version="14")
    @test R("x86_64-apple-darwin17.0.0") == P("x86_64", "macos"; os_version="17")
    @test R("armv7l-pc-linux-gnueabihf") == P("armv7l", "linux")
    @test R("armv7l-linux-musleabihf") == P("armv7l", "linux"; libc="musl")
    @test R("armv6l-linux-gnueabi") == P("armv6l", "linux"; call_abi="eabi")
    # Test that the short name "arm" goes to `armv7l`
    @test R("arm-linux-gnueabihf") == P("armv7l", "linux")
    @test R("aarch64-unknown-linux-gnu") == P("aarch64", "linux")
    @test R("powerpc64le-linux-gnu") == P("powerpc64le", "linux")
    @test R("ppc64le-linux-gnu") == P("powerpc64le", "linux")
    @test R("x86_64-w64-mingw32") == P("x86_64", "windows")
    @test R("i686-w64-mingw32") == P("i686", "windows")

    # FreeBSD has lots of arch names that don't match elsewhere
    @test R("x86_64-unknown-freebsd11.1") == P("x86_64", "freebsd"; os_version=v"11.1")
    @test R("i686-unknown-freebsd11.1") == P("i686", "freebsd"; os_version=v"11.1")
    @test R("amd64-unknown-freebsd12.0") == P("x86_64", "freebsd"; os_version=v"12.0")
    @test R("i386-unknown-freebsd10.3") == P("i686", "freebsd"; os_version=v"10.3")
    @test R("aarch64-apple-darwin18.7") == P("aarch64", "macos"; os_version=v"18.7")
    @test R("arm64-apple-darwin20") == P("aarch64", "macos"; os_version=v"20")

    # Test inclusion of ABI stuff, both old-style and new-style
    @test R("x86_64-linux-gnu-gcc7") == P("x86_64", "linux"; libgfortran_version=v"4")
    @test R("x86_64-linux-gnu-gcc4-cxx11") == P("x86_64", "linux"; libgfortran_version=v"3", cxxstring_abi="cxx11")
    @test R("x86_64-linux-gnu-cxx11") == P("x86_64", "linux"; cxxstring_abi="cxx11")
    @test R("x86_64-linux-gnu-libgfortran3-cxx03") == P("x86_64", "linux"; libgfortran_version=v"3", cxxstring_abi="cxx03")
    @test R("x86_64-linux-gnu-libstdcxx26") ==  P("x86_64", "linux"; libstdcxx_version=v"3.4.26")

    @test_throws ArgumentError R("totally FREEFORM text!!1!!!1!")
    @test_throws ArgumentError R("invalid-triplet-here")
    @test parse(Platform, "aarch64-linux-gnueabihf") == Platform("aarch64", "linux"; call_abi="eabihf")
    @test_throws ArgumentError R("aarch64-linux-gnueabihf")
    @test_throws ArgumentError R("x86_64-w32-mingw64")

    # Test extended attributes
    @test R("x86_64-linux-gnu-march+avx2") == P("x86_64", "linux"; march="avx2")
    @test R("x86_64-linux-gnu-march+x86_64-cuda+10.1") == P("x86_64", "linux"; march="x86_64", cuda="10.1")

    # Round-trip our little homie through `triplet()`, with some bending
    # of the rules for MacOS and FreeBSD, who have incomplete `os_version`
    # numbers embedded within their triplets.
    p = Platform("x86_64", "linux")
    @test parse(Platform, triplet(p)) == p

    # Also test round-tripping through `repr()`:
    p = Platform("aarch64", "macos"; os_version=v"20", march="armv8_4_crypto_sve")
    @test eval(Meta.parse(repr(p))) == p
end

@testset "platforms_match()" begin
    # Just do a quick combinatorial sweep for completeness' sake for platform matching
    linux = P("x86_64", "linux")
    for libgfortran_version in (nothing, v"3", v"5"),
        libstdcxx_version in (nothing, v"3.4.18", v"3.4.26"),
        cxxstring_abi in (nothing, :cxx03, :cxx11)

        p = P("x86_64", "linux"; libgfortran_version, libstdcxx_version, cxxstring_abi)
        @test platforms_match(linux, p)
        @test platforms_match(p, linux)

        # Also test auto-string-parsing
        @test platforms_match(triplet(linux), p)
        @test platforms_match(linux, triplet(p))
    end

    # Test that Julia version is matched only on major.minor by default
    @test platforms_match(P("x86_64", "linux"; julia_version=v"1.5.0"),
                          P("x86_64", "linux"; julia_version=v"1.5.1"))
    @test !platforms_match(P("x86_64", "linux"; julia_version=v"1.5.0"),
                           P("x86_64", "linux"; julia_version=v"1.6.0"))

    # Ensure many of these things do NOT match
    @test !platforms_match(linux, P("i686", "linux"))
    @test !platforms_match(linux, P("x86_64", "windows"))
    @test !platforms_match(linux, P("x86_64", "macos"))

    # Make some explicitly non-matching compiler ABI platforms
    host = P("x86_64", "linux"; libgfortran_version=v"5", cxxstring_abi="cxx11")
    for arch in ("x86_64", "i686", "aarch64", "armv6l", "armv7l", "powerpc64le"),
        kwargs in ((:libgfortran_version => v"3",), (:cxxstring_abi => "cxx03",),
                   (:libgfortran_version => v"4", :cxxstring_abi => "cxx11"),
                   (:libgfortran_version => v"3", :cxxstring_abi => "cxx03"))
        a = P(arch, "linux"; libgfortran_version=v"5", cxxstring_abi="cxx11")
        b = P(arch, "linux"; kwargs...)
        @test !platforms_match(a, b)
    end

    # Test version bounds with HostPlatform()
    host = HostPlatform(P("x86_64", "macos"; os_version="14", libstdcxx_version=v"3.4.26"))
    @test platforms_match(host, P("x86_64", "macos"))
    @test platforms_match(host, P("x86_64", "macos"; os_version="14"))
    @test platforms_match(host, P("x86_64", "macos"; os_version="13"))
    @test !platforms_match(host, P("x86_64", "macos"; os_version="15"))
    @test platforms_match(host, P("x86_64", "macos"; libstdcxx_version="3.4.18"))
    @test platforms_match(host, P("x86_64", "macos"; os_version=v"10", libstdcxx_version="3.4.18"))
    @test !platforms_match(host, P("x86_64", "macos"; os_version=v"10", libstdcxx_version="3.4.27"))
    @test !platforms_match(host, P("x86_64", "macos"; os_version=v"14", libstdcxx_version=v"4"))
end

@testset "DL name/version parsing" begin
    # Make sure our version parsing code is working
    @test parse_dl_name_version("libgfortran.so", "linux") == ("libgfortran", nothing)
    @test parse_dl_name_version("libgfortran.so.3", "linux") == ("libgfortran", v"3")
    @test parse_dl_name_version("libgfortran.so.3.4", "linux") == ("libgfortran", v"3.4")
    @test_throws ArgumentError parse_dl_name_version("libgfortran.so.3.4a", "linux")
    @test_throws ArgumentError parse_dl_name_version("libgfortran", "linux")
    @test_throws ArgumentError parse_dl_name_version("libgfortranso", "linux")
    @test parse_dl_name_version("libgfortran.so", "freebsd") == ("libgfortran", nothing)
    @test parse_dl_name_version("libgfortran.so.3", "freebsd") == ("libgfortran", v"3")
    @test parse_dl_name_version("libgfortran.so.3.4", "freebsd") == ("libgfortran", v"3.4")
    @test_throws ArgumentError parse_dl_name_version("libgfortran.so.3.4a", "freebsd")
    @test_throws ArgumentError parse_dl_name_version("libgfortran", "freebsd")
    @test_throws ArgumentError parse_dl_name_version("libgfortranso", "freebsd")
    @test parse_dl_name_version("libgfortran.dylib", "macos") == ("libgfortran", nothing)
    @test parse_dl_name_version("libgfortran.3.dylib", "macos") == ("libgfortran", v"3")
    @test parse_dl_name_version("libgfortran.3.4.dylib", "macos") == ("libgfortran", v"3.4")
    @test parse_dl_name_version("libgfortran.3.4a.dylib", "macos") == ("libgfortran.3.4a", nothing)
    @test_throws ArgumentError parse_dl_name_version("libgfortran", "macos")
    @test_throws ArgumentError parse_dl_name_version("libgfortrandylib", "macos")
    @test parse_dl_name_version("libgfortran.dll", "windows") == ("libgfortran", nothing)
    @test parse_dl_name_version("libgfortran-3.dll", "windows") == ("libgfortran", v"3")
    @test parse_dl_name_version("libgfortran-3.4.dll", "windows") == ("libgfortran", v"3.4")
    @test parse_dl_name_version("libgfortran-3.4a.dll", "windows") == ("libgfortran-3.4a", nothing)
    @test_throws ArgumentError parse_dl_name_version("libgfortran", "windows")
    @test_throws ArgumentError parse_dl_name_version("libgfortrandll", "windows")
end

@testset "Sys.is* overloading" begin
    # Test that we can indeed ask if something is linux or windows, etc...
    @test Sys.islinux(P("aarch64", "linux"))
    @test !Sys.islinux(P("x86_64", "windows"))
    @test Sys.iswindows(P("i686", "windows"))
    @test !Sys.iswindows(P("powerpc64le", "linux"))
    @test Sys.isapple(P("x86_64", "macos"))
    @test !Sys.isapple(P("armv7l", "windows"))
    @test Sys.isbsd(P("aarch64", "macos"))
    @test Sys.isbsd(P("x86_64", "freebsd"))
    @test !Sys.isbsd(P("x86_64", "linux"; libc="musl"))
end

@testset "Compiler ABI detection" begin
    # Let's check and ensure that we can autodetect the currently-running Julia process
    @test detect_libgfortran_version() !== nothing

    # We run these to get coverage, but we can't test anything, because we could be built
    # with `clang`, which wouldn't have any `libstdc++` constraints at all
    detect_libstdcxx_version()
    detect_cxxstring_abi()
end

@testset "select_platform" begin
    platforms = Dict(
        # Typical binning test
        P("x86_64", "linux"; libgfortran_version=v"3") => "linux4",
        P("x86_64", "linux"; libgfortran_version=v"4") => "linux7",
        P("x86_64", "linux"; libgfortran_version=v"5") => "linux8",

        # Ambiguity test
        P("aarch64", "linux"; libgfortran_version=v"3") => "linux4",
        P("aarch64", "linux"; libgfortran_version=v"3", libstdcxx_version=v"3.4.18") => "linux5",

        # OS test
        P("x86_64", "macos"; libgfortran_version=v"3") => "mac4",
        P("x86_64", "windows"; cxxstring_abi=:cxx11) => "win",
    )

    @test select_platform(platforms, P("x86_64", "linux")) == "linux8"
    @test select_platform(platforms, P("x86_64", "linux"; libgfortran_version=v"4")) == "linux7"

    # Ambiguity test
    @test select_platform(platforms, P("aarch64", "linux")) == "linux5"
    @test select_platform(platforms, P("aarch64", "linux"; libgfortran_version=v"3")) == "linux5"
    @test select_platform(platforms, P("aarch64", "linux"; libgfortran_version=v"4")) === nothing

    @test select_platform(platforms, P("x86_64", "macos")) == "mac4"
    @test select_platform(platforms, P("x86_64", "macos"; libgfortran_version=v"4")) === nothing

    @test select_platform(platforms, P("x86_64", "windows"; cxxstring_abi="cxx11")) == "win"
    @test select_platform(platforms, P("x86_64", "windows"; cxxstring_abi="cxx03")) === nothing

    # Sorry, Alex. ;)
    @test select_platform(platforms, P("x86_64", "freebsd")) === nothing
end

@testset "Custom comparators" begin
    # We're going to define here some custom comparators for Platform objects to ensure they work.
    # First, a symmetric one, which doesn't care which `Platform` object requested this comparison:
    function matches_oddness(a::String, b::String, a_requested::Bool, b_requested::Bool)
        return (parse(Int, a) % 2) == (parse(Int, b) % 2)
    end

    comp_strat = Dict("vally" => matches_oddness)

    # First, test that these two do not match, because it's using equality to test the `vally` tag
    a = Platform("x86_64", "linux"; vally="2")
    b = Platform("x86_64", "linux"; vally="4")
    @test !platforms_match(a, b)

    # Now, test that setting one or both `Platform`'s to use the `matches_oddness()` comparator works:
    ac = Platform("x86_64", "linux"; vally="2", compare_strategies=comp_strat)
    bc = Platform("x86_64", "linux"; vally="4", compare_strategies=comp_strat)
    @test platforms_match(ac, b)
    @test platforms_match(a, bc)
    @test platforms_match(ac, bc)

    # Test that even with the comparison strat, we don't match if they're not both even:
    bfc = Platform("x86_64", "linux"; vally="3", compare_strategies=comp_strat)
    @test !platforms_match(ac, bfc)


    # Next, an asymmetric comparison strategy.  We'll create a "less than or equal to" constraint
    # that uses the `{a,b}_requested` paramters to determine which number represents the limit.
    function less_than_constraint(a::String, b::String, a_requested::Bool, b_requested::Bool)
        a = parse(Int, a)
        b = parse(Int, b)
        if a_requested && !b_requested
            return b < a
        end
        if b_requested && !a_requested
            return a < b
        end
        # If two constraints have been requested, return true if they are the same constraint.
        return a == b
    end

    comp_strat = Dict("vally" => less_than_constraint)
    a = Platform("x86_64", "linux"; vally="2")
    b = Platform("x86_64", "linux"; vally="4")

    ac = Platform("x86_64", "linux"; vally="2", compare_strategies=comp_strat)
    bc = Platform("x86_64", "linux"; vally="4", compare_strategies=comp_strat)

    # Vanilla comparison doesn't work
    @test !platforms_match(a, b)

    # a and bc match, but not ac and b.  Also test reciprocity.
    @test platforms_match(a, bc)
    @test platforms_match(bc, a)
    @test !platforms_match(ac, b)
    @test !platforms_match(b, ac)

    # ac and bc do not match, but ac and ac do
    @test !platforms_match(ac, bc)
    @test platforms_match(ac, ac)
    @test platforms_match(bc, bc)
end
