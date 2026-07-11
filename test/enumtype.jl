# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for the `enum` keyword surface syntax and its Base-level API,
# exercising the default parse/lowering pipeline via modules whose syntax
# version is set to 1.14.

using Test

module EnumTypeTestMod end
Base.set_syntax_version(EnumTypeTestMod, v"1.14")
module EnumTypeExtA end
Base.set_syntax_version(EnumTypeExtA, v"1.14")
module EnumTypeExtB end
Base.set_syntax_version(EnumTypeExtB, v"1.14")

# make the defining module reachable from the extending modules
Core.eval(EnumTypeExtA, :(const CMod = $EnumTypeTestMod))
Core.eval(EnumTypeExtB, :(const CMod = $EnumTypeTestMod))

include_string(EnumTypeTestMod, """
enum Color::UInt8
    Red
    Green = 5
    ...
end
""")

@testset "enum declaration" begin
    T = EnumTypeTestMod.Color
    @test Base.isenumtype(T)
    @test !Base.isenumtype(Int32)
    @test !Base.isenumtype(1)
    @test isprimitivetype(T)
    @test isbitstype(T)
    @test sizeof(T) == 1
    @test Base.enumstoragetype(T) == UInt8
    @test Base.isopenenum(T)
    @test supertype(T) == Any
    @test reinterpret(UInt8, EnumTypeTestMod.Red) == 0x00
    @test reinterpret(UInt8, EnumTypeTestMod.Green) == 0x05
    @test reinterpret(T, 0x05) === EnumTypeTestMod.Green
    @test instances(T) === (EnumTypeTestMod.Red, EnumTypeTestMod.Green)
    @test_throws MethodError instances(Int32)
end

include_string(EnumTypeTestMod, """
enum Fruit <: Integer
    Apple
end
""")

@testset "declared supertype and default storage" begin
    T = EnumTypeTestMod.Fruit
    @test supertype(T) == Integer
    @test Base.enumstoragetype(T) == Int32
    @test !Base.isopenenum(T)
    @test sizeof(T) == 4
end

include_string(EnumTypeExtA, """
enum CMod.Color::UInt8
    ...
    Blue
end
""")
include_string(EnumTypeExtB, """
enum CMod.Color::UInt8
    ...
    Blue
end
""")

@testset "extension" begin
    T = EnumTypeTestMod.Color
    @test EnumTypeExtA.Blue isa T
    @test EnumTypeExtB.Blue isa T
    # same member name in two modules gives distinct members
    @test EnumTypeExtA.Blue !== EnumTypeExtB.Blue
    @test reinterpret(UInt8, EnumTypeExtA.Blue) != reinterpret(UInt8, EnumTypeExtB.Blue)
    @test length(instances(T)) == 4
    @test EnumTypeExtA.Blue in instances(T)
    # closed enums cannot be extended
    @test_throws Exception include_string(EnumTypeExtA, """
    enum CMod.Fruit::Int32
        ...
        Rotten
    end
    """)
    # storage type must match
    @test_throws Exception include_string(EnumTypeExtA, """
    enum CMod.Color::UInt16
        ...
        Purple
    end
    """)
end

@testset "surface syntax errors" begin
    # qualified name requires a leading `...`
    @test_throws Exception include_string(EnumTypeExtA, """
    enum CMod.Color::UInt8
        Purple
    end
    """)
    # extension cannot re-declare openness
    @test_throws Exception include_string(EnumTypeExtA, """
    enum CMod.Color::UInt8
        ...
        Purple
        ...
    end
    """)
    # extension cannot declare a supertype
    @test_throws Exception include_string(EnumTypeExtA, """
    enum CMod.Color::UInt8 <: Any
        ...
        Purple
    end
    """)
    # extension must repeat the storage type
    @test_throws Exception include_string(EnumTypeExtA, """
    enum CMod.Color
        ...
        Purple
    end
    """)
    # duplicate explicit values
    @test_throws Exception include_string(EnumTypeTestMod, """
    enum Dup
        D1 = 1
        D2 = 1
    end
    """)
    # non-representable explicit value
    @test_throws Exception include_string(EnumTypeTestMod, """
    enum Narrow::UInt8
        N1 = 256
    end
    """)
end

include_string(EnumTypeTestMod, """
enum Registry
    ...
end
""")

@testset "empty open enum" begin
    T = EnumTypeTestMod.Registry
    @test Base.isopenenum(T)
    @test instances(T) === ()
    include_string(EnumTypeExtA, """
    enum CMod.Registry::Int32
        ...
        FirstEntry
    end
    """)
    @test length(instances(T)) == 1
    @test instances(T)[1] === EnumTypeExtA.FirstEntry
end

@testset "member value expressions see earlier members" begin
    include_string(EnumTypeTestMod, """
    enum Chain::Int64
        C1 = 10
        C2 = reinterpret(Int64, C1) * 2
    end
    """)
    @test reinterpret(Int64, EnumTypeTestMod.C2) == 20
end

@testset "show" begin
    T = EnumTypeTestMod.Color
    red = EnumTypeTestMod.Red
    ctx = :module => EnumTypeTestMod
    @test sprint(show, red; context=ctx) == "Red"
    @test sprint(print, red; context=ctx) == "Red"
    @test sprint(show, MIME("text/plain"), red; context=ctx) == "Red::Color = 0x00"
    # members are qualified by their owning module when not visible
    strA = sprint(show, EnumTypeExtA.Blue; context=(:module => EnumTypeTestMod))
    @test endswith(strA, "EnumTypeExtA.Blue")
    # compact mode drops the qualification
    @test sprint(show, EnumTypeExtA.Blue; context=(:compact => true)) == "Blue"
    # bit patterns without a registered member
    stray = reinterpret(T, 0x77)
    @test sprint(show, stray; context=ctx) == "reinterpret(Color, 0x77)"
    # containers of enum values
    @test sprint(show, [red, EnumTypeTestMod.Green]; context=ctx) == "Color[Red, Green]"
end

@testset "redefinition creates a fresh type" begin
    include_string(EnumTypeTestMod, """
    enum Redef
        R1
    end
    """)
    T1 = EnumTypeTestMod.Redef
    r1 = EnumTypeTestMod.R1
    include_string(EnumTypeTestMod, """
    enum Redef
        R1
        R2
    end
    """)
    T2 = EnumTypeTestMod.Redef
    @test T2 !== T1
    @test r1 isa T1
    @test EnumTypeTestMod.R1 isa T2
    @test length(instances(T2)) == 2
end

@testset "no implicit integer conversions" begin
    red = EnumTypeTestMod.Red
    @test_throws MethodError convert(UInt8, red)
    @test_throws MethodError UInt8(red)
    @test_throws MethodError red + 1
    @test_throws MethodError isless(red, EnumTypeTestMod.Green)
end

@testset "identity-based hashing" begin
    T = EnumTypeTestMod.Color
    red = EnumTypeTestMod.Red
    @test hash(red) == hash(reinterpret(T, 0x00))
    @test hash(red) != hash(EnumTypeTestMod.Green)
    # same-named members of different modules have distinct identities
    @test hash(EnumTypeExtA.Blue) != hash(EnumTypeExtB.Blue)
    d = Dict(red => 1, EnumTypeExtA.Blue => 2)
    @test d[reinterpret(T, 0x00)] == 1
    @test d[EnumTypeExtA.Blue] == 2
end
