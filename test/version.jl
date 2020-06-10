# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random

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
             nextpatch, nextminor, nextmajor
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

# julia_versionnumber
@test julia_versionnumber("nightly") ==
      VersionNumber(replace(Base.read("VERSION", String), "\n" => ""))
@test thispatch(julia_versionnumber("nightly")) == thispatch(VERSION)
@test julia_versionnumber("1.2.3") == v"1.2.3"
@test julia_versionnumber(v"1.2.3") == v"1.2.3"
