# This file is a part of Julia. License is MIT: https://julialang.org/license

# parsing tests
@test v"2" == VersionNumber(2)
@test v"3.2" == VersionNumber(3, 2)
@test v"4.3.2" == VersionNumber(4, 3, 2)

@test v"2-" == VersionNumber(2, 0, 0, ("",), ())
@test v"3.2-" == VersionNumber(3, 2, 0, ("",), ())
@test v"4.3.2-" == VersionNumber(4, 3, 2, ("",), ())

@test v"2-1" == VersionNumber(2, 0, 0, (1,), ())
@test v"3.2-1" == VersionNumber(3, 2, 0, (1,), ())
@test v"4.3.2-1" == VersionNumber(4, 3, 2, (1,), ())

@test v"2-a" == VersionNumber(2, 0, 0, ("a",), ()) == v"2a"
@test v"3.2-a" == VersionNumber(3, 2, 0, ("a",), ()) == v"3.2a"
@test v"4.3.2-a" == VersionNumber(4, 3, 2, ("a",), ()) == v"4.3.2a"

@test v"2-a1" == VersionNumber(2, 0, 0, ("a1",), ()) == v"2a1"
@test v"3.2-a1" == VersionNumber(3, 2, 0, ("a1",), ()) == v"3.2a1"
@test v"4.3.2-a1" == VersionNumber(4, 3, 2, ("a1",), ()) == v"4.3.2a1"

@test v"2-1a" == VersionNumber(2, 0, 0, ("1a",), ())
@test v"3.2-1a" == VersionNumber(3, 2, 0, ("1a",), ())
@test v"4.3.2-1a" == VersionNumber(4, 3, 2, ("1a",), ())

@test v"2-a.1" == VersionNumber(2, 0, 0, ("a", 1), ()) == v"2a.1"
@test v"3.2-a.1" == VersionNumber(3, 2, 0, ("a", 1), ()) == v"3.2a.1"
@test v"4.3.2-a.1" == VersionNumber(4, 3, 2, ("a", 1), ()) == v"4.3.2a.1"

@test v"2-1.a" == VersionNumber(2, 0, 0, (1, "a"), ())
@test v"3.2-1.a" == VersionNumber(3, 2, 0, (1, "a"), ())
@test v"4.3.2-1.a" == VersionNumber(4, 3, 2, (1, "a"), ())

@test v"2+" == VersionNumber(2, 0, 0, (), ("",))
@test v"3.2+" == VersionNumber(3, 2, 0, (), ("",))
@test v"4.3.2+" == VersionNumber(4, 3, 2, (), ("",))

@test v"2+1" == VersionNumber(2, 0, 0, (), (1,))
@test v"3.2+1" == VersionNumber(3, 2, 0, (), (1,))
@test v"4.3.2+1" == VersionNumber(4, 3, 2, (), (1,))

@test v"2+a" == VersionNumber(2, 0, 0, (), ("a",))
@test v"3.2+a" == VersionNumber(3, 2, 0, (), ("a",))
@test v"4.3.2+a" == VersionNumber(4, 3, 2, (), ("a",))

@test v"2+a1" == VersionNumber(2, 0, 0, (), ("a1",))
@test v"3.2+a1" == VersionNumber(3, 2, 0, (), ("a1",))
@test v"4.3.2+a1" == VersionNumber(4, 3, 2, (), ("a1",))

@test v"2+1a" == VersionNumber(2, 0, 0, (), ("1a",))
@test v"3.2+1a" == VersionNumber(3, 2, 0, (), ("1a",))
@test v"4.3.2+1a" == VersionNumber(4, 3, 2, (), ("1a",))

@test v"2+a.1" == VersionNumber(2, 0, 0, (), ("a", 1))
@test v"3.2+a.1" == VersionNumber(3, 2, 0, (), ("a", 1))
@test v"4.3.2+a.1" == VersionNumber(4, 3, 2, (), ("a", 1))

@test v"2+1.a" == VersionNumber(2, 0, 0, (), (1, "a"))
@test v"3.2+1.a" == VersionNumber(3, 2, 0, (), (1, "a"))
@test v"4.3.2+1.a" == VersionNumber(4, 3, 2, (), (1, "a"))

# ArgumentErrors in constructor
@test_throws ArgumentError VersionNumber(4, 3, 2, ("nonalphanumeric!",), ())
@test_throws ArgumentError VersionNumber(4, 3, 2, ("nonalphanumeric!", 1), ())
@test_throws ArgumentError VersionNumber(4, 3, 2, (1, "nonalphanumeric!"), ())

@test_throws ArgumentError VersionNumber(4, 3, 2, ("", 1), ())
@test_throws ArgumentError VersionNumber(4, 3, 2, (1, ""), ())
@test_throws ArgumentError VersionNumber(4, 3, 2, ("",), ("",))
@test_throws ArgumentError VersionNumber(4, 3, 2, ("",), ("nonempty", 1))

@test_throws ArgumentError VersionNumber(4, 3, 2, (), ("nonalphanumeric!",))
@test_throws ArgumentError VersionNumber(4, 3, 2, (), ("nonalphanumeric!", 1))
@test_throws ArgumentError VersionNumber(4, 3, 2, (), (1, "nonalphanumeric!"))

@test_throws ArgumentError VersionNumber(4, 3, 2, (), ("", 1))

# show
io = IOBuffer()
show(io,v"4.3.2+1.a")
@test length(String(take!(io))) == 12

# construction from Int
@test VersionNumber(2) == v"2.0.0"

# construction from Tuple
@test VersionNumber((2,)) == v"2.0.0"
@test VersionNumber((3, 2)) == v"3.2.0"

# construction from AbstractString
@test VersionNumber("4.3.2+1.a") == v"4.3.2+1.a"

# typemin and typemax
@test typemin(VersionNumber) == v"0-"
@test typemax(VersionNumber) == v"∞"
let ∞ = typemax(UInt32)
    @test typemin(VersionNumber) == VersionNumber(0, 0, 0, ("",), ())
    @test typemax(VersionNumber) == VersionNumber(∞, ∞, ∞, (), ("",))
end

# issupbuild
import Base.issupbuild
@test issupbuild(v"4.3.2+")
@test ~issupbuild(v"4.3.2")
@test ~issupbuild(v"4.3.2-")
@test ~issupbuild(v"4.3.2-a")
@test ~issupbuild(v"4.3.2-a1")
@test ~issupbuild(v"4.3.2-1")
@test ~issupbuild(v"4.3.2-a.1")
@test ~issupbuild(v"4.3.2-1a")
@test ~issupbuild(v"4.3.2+a")
@test ~issupbuild(v"4.3.2+a1")
@test ~issupbuild(v"4.3.2+1")
@test ~issupbuild(v"4.3.2+a.1")
@test ~issupbuild(v"4.3.2+1a")

# basic comparison
VersionNumber(2, 3, 1) == VersionNumber(Int8(2), UInt32(3), Int32(1)) == v"2.3.1"
@test v"2.3.0" < v"2.3.1" < v"2.4.8" < v"3.7.2"
@test v"0.6.0-" < v"0.6.0-dev" < v"0.6.0-dev.123" < v"0.6.0-dev.unknown" < v"0.6.0-pre" < v"0.6.0"

#lowerbound and upperbound
import Base: lowerbound, upperbound
@test lowerbound(v"4.3.2") == v"4.3.2-"
@test lowerbound(v"4.3.2-") == v"4.3.2-"
@test lowerbound(v"4.3.2+") == v"4.3.2-"
@test upperbound(v"4.3.2") == v"4.3.2+"
@test upperbound(v"4.3.2-") == v"4.3.2+"
@test upperbound(v"4.3.2+") == v"4.3.2+"

# advanced comparison & manipulation
import Base: thispatch, thisminor, thismajor,
             nextpatch, nextminor, nextmajor, check_new_version
@test v"1.2.3" == thispatch(v"1.2.3-")
@test v"1.2.3" == thispatch(v"1.2.3-pre")
@test v"1.2.3" == thispatch(v"1.2.3")
@test v"1.2.3" == thispatch(v"1.2.3+post")
@test v"1.2.3" == thispatch(v"1.2.3+")

@test v"1.2" == thisminor(v"1.2.3-")
@test v"1.2" == thisminor(v"1.2.3-pre")
@test v"1.2" == thisminor(v"1.2.3")
@test v"1.2" == thisminor(v"1.2.3+post")
@test v"1.2" == thisminor(v"1.2.3+")

@test v"1" == thismajor(v"1.2.3-")
@test v"1" == thismajor(v"1.2.3-pre")
@test v"1" == thismajor(v"1.2.3")
@test v"1" == thismajor(v"1.2.3+post")
@test v"1" == thismajor(v"1.2.3+")

@test v"1.2.3" == nextpatch(v"1.2.3-")
@test v"1.2.3" == nextpatch(v"1.2.3-pre")
@test v"1.2.4" == nextpatch(v"1.2.3")
@test v"1.2.4" == nextpatch(v"1.2.3+post")
@test v"1.2.4" == nextpatch(v"1.2.3+")

@test v"1.2" == nextminor(v"1.2-")
@test v"1.2" == nextminor(v"1.2-pre")
@test v"1.3" == nextminor(v"1.2")
@test v"1.3" == nextminor(v"1.2+post")
@test v"1.3" == nextminor(v"1.2+")

@test v"1.3" == nextminor(v"1.2.3-")
@test v"1.3" == nextminor(v"1.2.3-pre")
@test v"1.3" == nextminor(v"1.2.3")
@test v"1.3" == nextminor(v"1.2.3+post")
@test v"1.3" == nextminor(v"1.2.3+")

@test v"1" == nextmajor(v"1-")
@test v"1" == nextmajor(v"1-pre")
@test v"2" == nextmajor(v"1")
@test v"2" == nextmajor(v"1+post")
@test v"2" == nextmajor(v"1+")

@test v"2" == nextmajor(v"1.2-")
@test v"2" == nextmajor(v"1.2-pre")
@test v"2" == nextmajor(v"1.2")
@test v"2" == nextmajor(v"1.2+post")
@test v"2" == nextmajor(v"1.2+")

@test v"2" == nextmajor(v"1.2.3-")
@test v"2" == nextmajor(v"1.2.3-pre")
@test v"2" == nextmajor(v"1.2.3")
@test v"2" == nextmajor(v"1.2.3+post")
@test v"2" == nextmajor(v"1.2.3+")

for major=0:3, minor=0:3, patch=0:3
    a = VersionNumber(major,minor,patch,("",),())
    b = VersionNumber(major,minor,patch,("pre",),())
    c = VersionNumber(major,minor,patch,(),())
    d = VersionNumber(major,minor,patch,(),("post",))
    e = VersionNumber(major,minor,patch,(),("",))
    @test a < b < c < d < e
    for x in [a,b,c,d,e]
        @test thispatch(x) == VersionNumber(major,minor,patch)
        @test thisminor(x) == VersionNumber(major,minor,0)
        @test thismajor(x) == VersionNumber(major,0,0)
        @test x < nextpatch(x) <= nextminor(x) <= nextmajor(x)
        @test x < thispatch(x) ? nextpatch(x) == thispatch(x) : thispatch(x) < nextpatch(x)
        @test x < thisminor(x) ? nextminor(x) == thisminor(x) : thisminor(x) < nextminor(x)
        @test x < thismajor(x) ? nextmajor(x) == thismajor(x) : thismajor(x) < nextmajor(x)
    end
end

# check_new_version
import Base.check_new_version
@test check_new_version([v"1", v"2"], v"3") === nothing
@test_throws AssertionError check_new_version([v"2", v"1"], v"3")
@test_throws ErrorException check_new_version([v"1", v"2"], v"2")
@test check_new_version(VersionNumber[], v"0") === nothing
@test check_new_version(VersionNumber[], v"0.0.1") === nothing
@test_throws ErrorException check_new_version(VersionNumber[], v"0.0.2")
@test check_new_version(VersionNumber[], v"0.1") === nothing
@test_throws ErrorException check_new_version(VersionNumber[], v"0.2")
@test check_new_version(VersionNumber[], v"1") === nothing
@test_throws ErrorException check_new_version(VersionNumber[], v"2")
@test_throws ErrorException check_new_version(VersionNumber[v"1", v"2", v"3"], v"2")
@test_throws ErrorException check_new_version([v"1", v"2"], v"4")
@test_throws ErrorException check_new_version([v"1", v"2"], v"2-rc")
@test check_new_version([v"1", v"2"], v"2.0.1") === nothing
@test check_new_version([v"1", v"2"], v"2.1") === nothing
@test check_new_version([v"1", v"2"], v"3") === nothing

# banner
import Base.banner
io = IOBuffer()
@test banner(io) === nothing
@test length(String(take!(io))) > 50

# julia_version.h version test
@test VERSION.major == ccall(:jl_ver_major, Cint, ())
@test VERSION.minor == ccall(:jl_ver_minor, Cint, ())
@test VERSION.patch == ccall(:jl_ver_patch, Cint, ())

# test construction with non-Int and non-String components
@test_throws MethodError VersionNumber()
@test VersionNumber(true) == v"1"
@test VersionNumber(true, 0x2) == v"1.2"
@test VersionNumber(true, 0x2, Int128(3)) == v"1.2.3"
@test VersionNumber(true, 0x2, Int128(3)) == v"1.2.3"
@test VersionNumber(true, 0x2, Int128(3), (GenericString("rc"), 0x1)) == v"1.2.3-rc.1"
@test VersionNumber(true, 0x2, Int128(3), (GenericString("rc"), 0x1)) == v"1.2.3-rc.1"
@test VersionNumber(true, 0x2, Int128(3), (), (GenericString("sp"), 0x2)) == v"1.2.3+sp.2"

# VersionSet tests

import Base.Pkg.Types: VersionInterval, VersionSet

function chkint(a::VersionSet)
    ints = a.intervals
    for k = 1:length(ints)
        ints[k].lower < ints[k].upper || return false
        k < length(ints) && (ints[k].upper < ints[k+1].lower || return false)
    end
    return true
end

const empty_versionset = VersionSet(VersionInterval[])
@test isempty(empty_versionset)

# VersionSet intersections and unions
@test empty_versionset ∩ empty_versionset == empty_versionset
@test empty_versionset ∪ empty_versionset == empty_versionset
for t = 1:1_000
    a = VersionSet(sort!(map(v->VersionNumber(v...), [(rand(0:8),rand(0:3)) for i = 1:rand(0:10)]))...)
    b = VersionSet(sort!(map(v->VersionNumber(v...), [(rand(0:8),rand(0:3)) for i = 1:rand(0:10)]))...)
    @assert chkint(a)
    @assert chkint(b)
    u = a ∪ b
    @test chkint(u)
    i = a ∩ b
    @test chkint(i)
    for vM = 0:9, vm = 0:5
        v = VersionNumber(vM, vm)
        @test (v ∈ a || v ∈ b) ? (v ∈ u) : (v ∉ u)
        @test (v ∈ a && v ∈ b) ? (v ∈ i) : (v ∉ i)
    end
end

# PR #23075
@testset "versioninfo" begin
    # check that versioninfo(io; verbose=true) doesn't error, produces some output
    # and doesn't invoke Pkg.status which will error if JULIA_PKGDIR is set
    mktempdir() do dir
        withenv("JULIA_PKGDIR" => dir) do
            buf = PipeBuffer()
            versioninfo(buf, verbose=true)
            ver = read(buf, String)
            @test startswith(ver, "Julia Version $VERSION")
            @test contains(ver, "Environment:")
            @test contains(ver, "Package Status:")
            @test contains(ver, "no packages installed")
            @test isempty(readdir(dir))
        end
    end
end
