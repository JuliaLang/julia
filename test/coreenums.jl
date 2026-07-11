# This file is a part of Julia. License is MIT: https://julialang.org/license

# Runtime-level tests for extensible enum types, exercising the Core builtins
# directly (the surface `enum` syntax is tested separately).

using Test

module CoreEnumTestA end
module CoreEnumTestB end

abstract type CEAbstract end

# Member registration declares constants whose bindings only become visible to
# code in a newer world age, so all registrations happen at toplevel here and
# the testsets below (closures running in a later world) reference them.

const CEBasic = Core._enumtype(@__MODULE__, :CEBasic, Any, UInt8, false)
const CESuper = Core._enumtype(@__MODULE__, :CESuper, CEAbstract, Int16, true)

@testset "enum type creation" begin
    @test CEBasic isa DataType
    @test isprimitivetype(CEBasic)
    @test isbitstype(CEBasic)
    @test sizeof(CEBasic) == 1
    @test supertype(CEBasic) == Any
    @test isempty(CEBasic.parameters)

    @test supertype(CESuper) == CEAbstract
    @test sizeof(CESuper) == 2

    # invalid storage types
    @test_throws ErrorException Core._enumtype(@__MODULE__, :CEBad, Any, Float64, false)
    @test_throws ErrorException Core._enumtype(@__MODULE__, :CEBad, Any, String, false)
    @test_throws ErrorException Core._enumtype(@__MODULE__, :CEBad, Any, Integer, false)
    # an enum type is not a valid storage type
    @test_throws ErrorException Core._enumtype(@__MODULE__, :CEBad, Any, CEBasic, false)
    # invalid supertype
    @test_throws ErrorException Core._enumtype(@__MODULE__, :CEBad, Int, UInt8, false)
end

const CEAuto = Core._enumtype(@__MODULE__, :CEAuto, Any, Int32, false)
const ceauto_a = Core._enum_add_member(CEAuto, @__MODULE__, :CEAutoA, nothing)
const ceauto_b = Core._enum_add_member(CEAuto, @__MODULE__, :CEAutoB, nothing)
const ceauto_e = Core._enum_add_member(CEAuto, @__MODULE__, :CEAutoE, Int32(5))
const ceauto_c = Core._enum_add_member(CEAuto, @__MODULE__, :CEAutoC, nothing)

@testset "member registration and auto assignment" begin
    @test ceauto_a isa CEAuto && ceauto_b isa CEAuto && ceauto_e isa CEAuto && ceauto_c isa CEAuto
    @test reinterpret(Int32, ceauto_a) == 0
    @test reinterpret(Int32, ceauto_b) == 1
    @test reinterpret(Int32, ceauto_e) == 5
    @test reinterpret(Int32, ceauto_c) == 2
    # auto assignment skips values taken explicitly
    Core._enum_add_member(CEAuto, @__MODULE__, :CEAutoD, Int32(3))
    d2 = Core._enum_add_member(CEAuto, @__MODULE__, :CEAutoD2, nothing)
    @test reinterpret(Int32, d2) == 4
    d3 = Core._enum_add_member(CEAuto, @__MODULE__, :CEAutoD3, nothing)
    @test reinterpret(Int32, d3) == 6

    # members are declared as constants in the owning module
    @test isconst(@__MODULE__, :CEAutoA)
    @test CEAutoA === ceauto_a
    @test CEAutoB === ceauto_b

    # reinterpret round-trips both ways
    @test reinterpret(CEAuto, Int32(5)) === ceauto_e
    @test reinterpret(Int32, reinterpret(CEAuto, Int32(42))) == 42
end

const CEIdem = Core._enumtype(@__MODULE__, :CEIdem, Any, UInt16, false)
const ceidem_a = Core._enum_add_member(CEIdem, @__MODULE__, :CEIdemA, nothing)
const ceidem_x = Core._enum_add_member(CEIdem, @__MODULE__, :CEIdemX, UInt16(100))

@testset "idempotent re-registration and conflicts" begin
    # re-registering is idempotent and returns the identical instance
    @test Core._enum_add_member(CEIdem, @__MODULE__, :CEIdemA, nothing) === ceidem_a
    @test Core._enum_add_member(CEIdem, @__MODULE__, :CEIdemX, UInt16(100)) === ceidem_x
    # explicit re-registration with different bits errors
    @test_throws ErrorException Core._enum_add_member(CEIdem, @__MODULE__, :CEIdemX, UInt16(101))
    # aliasing (a second name for a taken value) errors
    @test_throws ErrorException Core._enum_add_member(CEIdem, @__MODULE__, :CEIdemY, UInt16(100))
    @test_throws ErrorException Core._enum_add_member(CEIdem, @__MODULE__, :CEIdemZ, UInt16(0)) # taken by auto CEIdemA
    # wrong value type errors
    @test_throws ErrorException Core._enum_add_member(CEIdem, @__MODULE__, :CEIdemW, Int16(7))
end

const CEClosed = Core._enumtype(@__MODULE__, :CEClosed, Any, Int32, false)
const CEOpen = Core._enumtype(@__MODULE__, :CEOpen, Any, Int32, true)
const ceopen_exta = Core._enum_add_member(CEOpen, CoreEnumTestA, :Ext, nothing)
const ceopen_extb = Core._enum_add_member(CEOpen, CoreEnumTestB, :Ext, nothing)

@testset "openness and extension" begin
    # the defining module can always add members
    Core._enum_add_member(CEClosed, @__MODULE__, :CEClosedA, nothing)
    # other modules cannot add members to a closed enum
    @test_throws ErrorException Core._enum_add_member(CEClosed, CoreEnumTestA, :Intruder, nothing)
    @test_throws ErrorException Core._enum_extend(CEClosed, CoreEnumTestA, Int32)
    # extension of an open enum
    @test Core._enum_extend(CEOpen, CoreEnumTestA, Int32) === nothing
    # extension must redeclare the matching storage type
    @test_throws ErrorException Core._enum_extend(CEOpen, CoreEnumTestA, UInt32)

    # same member name in two modules gives distinct members with distinct values
    @test ceopen_exta !== ceopen_extb
    @test reinterpret(Int32, ceopen_exta) != reinterpret(Int32, ceopen_extb)
    @test isconst(CoreEnumTestA, :Ext) && isconst(CoreEnumTestB, :Ext)
    @test CoreEnumTestA.Ext === ceopen_exta
    @test CoreEnumTestB.Ext === ceopen_extb
end

const CEIntro = Core._enumtype(@__MODULE__, :CEIntro, Any, UInt8, true)
const ceintro_a = Core._enum_add_member(CEIntro, @__MODULE__, :CEIntroA, nothing)
const ceintro_b = Core._enum_add_member(CEIntro, @__MODULE__, :CEIntroB, UInt8(9))

@testset "member table introspection" begin
    tab = Core._enum_members(CEIntro)
    @test tab isa Core.SimpleVector
    # 3-slot header: storage type, isopen, next-auto hint
    @test tab[1] == UInt8
    @test tab[2] === true
    nmembers = (length(tab) - 3) ÷ 4
    @test nmembers == 2
    names = [tab[3 + 4*(i-1) + 1] for i in 1:nmembers]
    mods  = [tab[3 + 4*(i-1) + 2] for i in 1:nmembers]
    insts = [tab[3 + 4*(i-1) + 3] for i in 1:nmembers]
    expls = [tab[3 + 4*(i-1) + 4] for i in 1:nmembers]
    @test names == [:CEIntroA, :CEIntroB]
    @test all(==(@__MODULE__), mods)
    @test insts[1] === ceintro_a && insts[2] === ceintro_b
    @test insts[1] === CEIntroA && insts[2] === CEIntroB
    @test expls == [false, true]
    # the table is a snapshot: adding a member does not mutate it
    Core._enum_add_member(CEIntro, @__MODULE__, :CEIntroC, nothing)
    @test length(tab) == 3 + 2*4
    @test length(Core._enum_members(CEIntro)) == 3 + 3*4

    # non-enum types have no member table
    @test_throws ErrorException Core._enum_members(Int32)
    @test_throws ErrorException Core._enum_add_member(Int32, @__MODULE__, :Nope, nothing)
end

# NOTE: enum types must stay rooted (normally via their const binding, which the
# surface syntax always creates) while instances of them exist, so these tests
# bind every type that receives members at toplevel.
const CEFull = Core._enumtype(@__MODULE__, :CEFull, Any, UInt8, false)
const CEWrap = Core._enumtype(@__MODULE__, :CEWrap, Any, UInt8, false)

@testset "enum-full" begin
    T = CEFull
    for i in 0:255
        Core._enum_add_member(T, @__MODULE__, Symbol(:CEFullM, i), nothing)
    end
    @test_throws ErrorException Core._enum_add_member(T, @__MODULE__, :CEFullOverflow, nothing)
    # auto assignment wraps around and finds holes before declaring fullness
    T2 = CEWrap
    for i in 1:255
        Core._enum_add_member(T2, @__MODULE__, Symbol(:CEWrapM, i), UInt8(i))
    end
    hole = Core._enum_add_member(T2, @__MODULE__, :CEWrapHole, nothing)
    @test reinterpret(UInt8, hole) == 0x00
    @test_throws ErrorException Core._enum_add_member(T2, @__MODULE__, :CEWrapOverflow, nothing)
end

const CEEgal = Core._enumtype(@__MODULE__, :CEEgal, Any, Int64, false)

@testset "egal and hashing of enum values" begin
    T = CEEgal
    a = Core._enum_add_member(T, @__MODULE__, :CEEgalA, nothing)
    b = Core._enum_add_member(T, @__MODULE__, :CEEgalB, nothing)
    @test a === a
    @test a !== b
    @test a == a
    @test a != b
    @test objectid(a) == objectid(reinterpret(T, Int64(0)))
    d = Dict{Any,Int}(a => 1, b => 2)
    @test d[reinterpret(T, Int64(0))] == 1
end

const CEGC = Core._enumtype(@__MODULE__, :CEGC, Any, Int32, false)

@testset "GC stress over table growth" begin
    T = CEGC
    insts = []
    for i in 1:100
        push!(insts, Core._enum_add_member(T, @__MODULE__, Symbol(:CEGCM, i), nothing))
        GC.gc(false)
    end
    GC.gc()
    tab = Core._enum_members(T)
    @test (length(tab) - 3) ÷ 4 == 100
    for i in 1:100
        @test reinterpret(Int32, insts[i]::T) == i - 1
    end
end
