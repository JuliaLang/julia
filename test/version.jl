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

# parse()/tryparse()
@test parse(VersionNumber, "1.2.3") == v"1.2.3"
@test_throws ArgumentError parse(VersionNumber, "not a version")
@test tryparse(VersionNumber, "3.2.1") == v"3.2.1"
@test tryparse(VersionNumber, "not a version") === nothing

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

# construct from VersionNumber
let
    v = VersionNumber("1.2.3")
    @test VersionNumber(v) == v
end

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

# VersionNumber has the promised fields
let v = v"4.2.1-1.x+a.9"
    @test v.major isa Integer
    @test v.minor isa Integer
    @test v.patch isa Integer
    @test v.prerelease isa Tuple{Vararg{Union{Integer, AbstractString}}}
    @test v.build isa Tuple{Vararg{Union{Integer, AbstractString}}}
end

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

# VersionRange printing keeps the upper bound of a "*" lower bound range
let r = Base.VersionRange("*-2")
    @test string(r) == "0 - 2"
    @test v"2.5.0" in Base.VersionRange(string(r))
end

# Semver compat specs, as used for `[compat]` entries and syntax versions
@testset "VersionSpec and semver_spec" begin
    using Base: VersionBound, VersionRange, VersionSpec, semver_spec, isjoinable,
        matches_spec_range!

    @test semver_spec("^1.2.3") == VersionSpec("1.2.3-1")
    @test semver_spec("^1.2") == VersionSpec("1.2.0-1")
    @test semver_spec("^1") == VersionSpec("1.0.0-1")
    @test semver_spec("^0.2.3") == VersionSpec("0.2.3-0.2")
    @test semver_spec("^0.0.3") == VersionSpec("0.0.3-0.0.3")
    @test semver_spec("^0.0") == VersionSpec("0.0.0-0.0")
    @test semver_spec("^0") == VersionSpec("0.0.0-0")
    @test semver_spec("1.2.3") == semver_spec("^1.2.3")
    @test semver_spec("0.0.3") == semver_spec("^0.0.3")
    @test semver_spec("~1.2.3") == VersionSpec("1.2.3-1.2")
    @test semver_spec("~1.2") == VersionSpec("1.2.0-1.2")
    @test semver_spec("~1") == VersionSpec("1.0.0-1")
    @test semver_spec("~0") == VersionSpec("0.0.0-0")
    @test semver_spec("<1.2.3") == VersionSpec("0.0.0 - 1.2.2")
    @test semver_spec("<1.2") == VersionSpec("0.0.0 - 1.1")
    @test semver_spec("<2") == VersionSpec("0.0.0 - 1")
    @test semver_spec("=1.2") == VersionSpec("1.2.0")
    @test semver_spec(">=1.2  ") == VersionSpec("1.2.0-*")
    @test semver_spec("≥ 1") == semver_spec(">=1") == VersionSpec("1.0.0-*")
    @test semver_spec("1.2 - 2") == VersionSpec("1.2 - 2")
    @test semver_spec("1 - 2.3.4") == VersionSpec("1 - 2.3.4")
    @test semver_spec("0.1.0 - 0.2.2, 1.2") == VersionSpec(["0.1.0 - 0.2.2", "1.2.0-1"])
    @test semver_spec("1, 1.5") == VersionSpec("1")
    @test semver_spec("1.2 - 1.5, 1.6 - 1.8") == VersionSpec("1.2 - 1.8")

    @test_throws ErrorException semver_spec("0.0.0")
    @test_throws ErrorException semver_spec(">=0.0.0")
    @test_throws ErrorException semver_spec("0.7 1.0")
    @test_throws ErrorException semver_spec("1.2.3.4")
    @test semver_spec("1.2.3.4"; throw = false) === nothing

    @test v"1.5.2" in semver_spec("1.2.3")
    @test !(v"2.0.0" in semver_spec("1.2.3"))
    @test !(v"1.2.2" in semver_spec("1.2.3"))
    @test v"1.2.99" in semver_spec("~1.2.3")
    @test !(v"1.3.0" in semver_spec("~1.2.3"))
    @test v"0.0.3" in semver_spec("^0.0.3")
    @test !(v"0.0.4" in semver_spec("^0.0.3"))
    @test v"0.2.3" in semver_spec("<0.2.4")
    @test !(v"0.2.4" in semver_spec("<0.2.4"))
    @test v"0.2.99" in semver_spec("0.1 - 0.2")
    @test !(v"0.3.0" in semver_spec("0.1 - 0.2"))
    @test v"100.0.0" in semver_spec(">=1.2")
    @test v"2.3.4" in semver_spec("1.2 - 2.3.4")
    @test !(v"2.3.5" in semver_spec("1.2 - 2.3.4"))
    @test v"1.4.0" in VersionSpec()
    @test !(v"1.4.0" in VersionSpec(VersionRange[]))

    @test VersionBound(" v1.2 ") == VersionBound(1, 2) == VersionBound((0x1, 0x2))
    @test VersionBound("*") == VersionBound()
    @test VersionBound(v"1.2.3") == VersionBound(1, 2, 3)
    @test hash(VersionBound(1, 2)) == hash(VersionBound("1.2"))
    @test_throws ErrorException VersionBound("1.2.3.4")
    @test_throws ArgumentError VersionBound(1, 2, 3, 4)
    @test_throws ArgumentError VersionRange("1-2-3")

    @test isjoinable(VersionBound(), VersionBound())
    @test isjoinable(VersionBound(1, 5), VersionBound(1, 6))
    @test !isjoinable(VersionBound(1, 2), VersionBound(1, 4))
    @test isjoinable(VersionBound(2), VersionBound(1, 5))
    @test !isjoinable(VersionBound(1, 2, 3), VersionBound(1, 3))
    @test !isjoinable(VersionBound(1, 5), VersionBound(1, 6, 0))

    @test VersionRange() == VersionRange("*")
    @test VersionRange(1, 2) == VersionRange("1.2")
    @test VersionRange(v"1.2.3") == VersionRange("1.2.3")
    @test VersionRange(v"1.0.0", v"1.2.1") == VersionRange("1.0.0 - 1.2.1")
    @test VersionRange("1.2-1.2.0") == VersionRange("1.2.0")
    @test VersionRange("1.2.0-1.2") == VersionRange("1.2")
    @test isempty(VersionRange("2-1"))
    @test isempty(VersionRange("1.3-1.2"))
    @test !isempty(VersionRange("1-1.2"))
    @test !isempty(VersionRange("*"))

    @test isempty(union!(VersionRange[]))
    @test isempty(union!([VersionRange("2-1"), VersionRange("3-2")]))
    @test union!([VersionRange("1.5-2.8"), VersionRange("2.5-3")]) == [VersionRange("1.5-3")]
    @test union!([VersionRange("2.5-3"), VersionRange("1.5-2.8")]) == [VersionRange("1.5-3")]
    @test union!([VersionRange("1.5-2.2"), VersionRange("2.5-3")]) ==
        [VersionRange("1.5-2.2"), VersionRange("2.5-3")]
    @test union!([VersionRange("1.5-2.2"), VersionRange("2-1"), VersionRange("2.5-3"), VersionRange("2.3-2.4")]) ==
        [VersionRange("1.5-3")]

    @test intersect(VersionSpec("1.2-2"), VersionSpec("1.5-3")) == VersionSpec("1.5-2")
    @test isempty(intersect(VersionSpec("1"), VersionSpec("2")))
    @test isempty(intersect(VersionSpec(VersionRange[]), VersionSpec("2")))
    @test intersect(semver_spec("0.1, 1"), semver_spec("0.1.5 - 1.2")) == VersionSpec(["0.1.5 - 0.1", "1 - 1.2"])
    @test intersect(v"1.3.0", VersionSpec("1")) == VersionSpec("1.3.0")
    @test isempty(intersect(VersionSpec("1"), v"2.0.0"))
    @test union(VersionSpec("1"), VersionSpec("1")) == VersionSpec("1")
    @test union(VersionSpec("1"), VersionSpec("3")) == VersionSpec(["1", "3"])
    @test union(VersionSpec("1.0-1.4"), VersionSpec("1.5")) == VersionSpec("1.0-1.5")

    let s = VersionSpec("1.2-3")
        @test copy(s) === s
        @test VersionSpec(s) === s
        @test hash(s) == hash(VersionSpec(["1.2-2", "2.1-3"]))
        @test minimum(s) == VersionBound(1, 2)
    end
    @test VersionSpec(v"1.2.3") == VersionSpec("1.2.3")
    @test VersionSpec(VersionRange("1.2")) == VersionSpec("1.2")

    let versions = [v"0.9.0", v"1.0.0", v"1.2.0", v"1.9.9", v"2.0.0"], dest = trues(6)
        @test matches_spec_range!(dest, versions, semver_spec("1.2, 0.9"), 5) == [1, 0, 1, 1, 0, 1]
        @test matches_spec_range!(dest, versions, semver_spec("=1.0.0"), 5) == [0, 1, 0, 0, 0, 1]
        @test matches_spec_range!(dest, versions, VersionSpec(VersionRange[]), 5) == [0, 0, 0, 0, 0, 1]
    end

    @test string(VersionSpec()) == "*"
    @test string(VersionSpec(VersionRange[])) == "∅"
    @test string(VersionSpec("1.2-2")) == "1.2 - 2"
    @test string(semver_spec("0.4, 1.2")) == "[0.4, 1.2.0 - 1]"
    @test string(VersionRange("1.2")) == "1.2"
    @test string(VersionRange("1.2-*")) == "1.2.0 - *"
    @test string(VersionRange("0-0.3.2")) == "0 - 0.3.2"
    @test repr(VersionRange("1.2-1.3")) == "VersionRange(\"1.2 - 1.3\")"
    @test repr(VersionSpec("1.2-1.3")) == "VersionSpec(\"1.2 - 1.3\")"
    @test repr(semver_spec("0.4, 1.2")) == "VersionSpec([\"0.4\", \"1.2.0 - 1\"])"
    @test repr(VersionSpec(VersionRange[])) == "VersionSpec([])"
    for s in ["1.2", "1.2.3 - 2", "0 - 0.3.2", "1.2.0 - *", "*"]
        r = VersionRange(s)
        @test VersionRange(string(r)) == r
    end
    for s in ["^0.2.3", "~1.2", "1.2 - 2.3.4", "<1.2"]
        spec = semver_spec(s)
        @test VersionSpec(string(spec)) == spec
    end
end

@test v"1.0.0-1" < v"1.0.0-a"
@test !(v"1.0.0-a" < v"1.0.0-1")
