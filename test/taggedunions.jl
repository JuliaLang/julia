# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tagged union layout: on 64-bit platforms, a Union mixing reference members
# with 1..4 small primitive members is stored as one tagged word. See
# doc/src/devdocs/taggedunions.md for the encoding.

using Test

const TAGGED_ENABLED = Sys.WORD_SIZE == 64 &&
    Base.istaggedunion(Union{Int32, String})

primitive type UInt63 63 end
UInt63(x::UInt64) = Core.Intrinsics.trunc_int(UInt63, x)
Base.UInt64(x::UInt63) = Core.Intrinsics.zext_int(UInt64, x)
primitive type UInt62 62 end
UInt62(x::UInt64) = Core.Intrinsics.trunc_int(UInt62, x)
primitive type UInt61 61 end
UInt61(x::UInt64) = Core.Intrinsics.trunc_int(UInt61, x)

istagged(u) = Base.istaggedunion(u)

@testset "classification" begin
    if !TAGGED_ENABLED
        @test !istagged(Union{Int32, String})
        return
    end
    # reference + small primitive => tagged
    @test istagged(Union{Int32, String})
    @test istagged(Union{UInt63, String})
    @test istagged(Union{Bool, String})
    @test istagged(Union{Char, String})
    @test istagged(Union{Float32, String})
    @test !istagged(Union{Int32, Nothing})           # all-inline: selector layout wins
    @test istagged(Union{Int32, Nothing, String})    # singleton rides along as a reference
    @test istagged(Union{Int32, AbstractString})     # abstract members are references
    @test istagged(Union{Int8, Int16, String})       # k = 2
    @test istagged(Union{Int8, Int16, Int32, String})  # k = 3, 3 immediates
    @test istagged(Union{Int8, Int16, Int32, UInt32, String}) # k = 3, 4 immediates
    @test istagged(Union{UInt62, UInt61, String})    # 62 <= 64 - 2

    # not tagged: no immediate fits / no reference member / too many immediates
    @test !istagged(Union{Int64, String})            # 64 > 63
    @test !istagged(Union{Float64, String})
    @test !istagged(Union{Ptr{Cvoid}, String})
    @test !istagged(Union{Int8, Int16})              # all-isbits stays selector-based
    @test !istagged(Union{Int8, Int16, Int32, UInt32, UInt16, String}) # 5 immediates
    @test !istagged(Union{UInt63, UInt62, String})   # 63 > 64 - 2, all-or-nothing
    @test !istagged(Union{String, Nothing})          # no immediate member
    @test Base.isbitsunion(Union{Int8, Int16})       # unchanged

    # layout introspection
    @test sizeof(Union{Int32, String}) == 8
    struct TU1; x::Union{Int32, String}; end
    @test Base.datatype_ntaggedptrs(TU1) == 1
    @test Base.datatype_npointers(TU1) == 0
    @test !Base.datatype_pointerfree(TU1)
    @test sizeof(TU1) == 8
    @test Base.datatype_arrayelem(Memory{Union{Int32, String}}) == 3
    struct TUNest; a::Int8; b::TU1; c::String; end
    @test Base.datatype_ntaggedptrs(TUNest) == 1
    @test Base.datatype_npointers(TUNest) == 1
    @test fieldoffset(TUNest, 2) == 8
end

if TAGGED_ENABLED

mutable struct MTU
    x::Union{Int32, String}
    MTU() = new()
    MTU(x) = new(x)
end

struct STU
    x::Union{UInt63, String}
end

@testset "field semantics" begin
    m = MTU()
    @test !isdefined(m, :x)
    @test_throws UndefRefError m.x
    m.x = Int32(17)
    @test isdefined(m, :x)
    @test m.x === Int32(17)
    s = "hello" * "!"  # non-interned
    m.x = s
    @test m.x === s
    m.x = Int32(-1)
    @test m.x === Int32(-1)

    # negative payloads keep their bits
    for v in (Int32(0), Int32(-1), typemax(Int32), typemin(Int32))
        m.x = v
        @test m.x === v
    end

    # 63-bit boundary values
    st = STU(UInt63(0x7fff_ffff_ffff_ffff))
    @test UInt64(st.x) == 0x7fff_ffff_ffff_ffff
    st = STU(UInt63(UInt64(0)))
    @test UInt64(st.x) == 0

    # egal and objectid
    @test STU(UInt63(UInt64(5))) === STU(UInt63(UInt64(5)))
    @test STU(UInt63(UInt64(5))) !== STU(UInt63(UInt64(6)))
    @test STU("abc" * "d") === STU("abc" * "d")  # contents-egal references
    @test STU("abcd") !== STU(UInt63(UInt64(5)))
    @test objectid(STU(UInt63(UInt64(5)))) == objectid(STU(UInt63(UInt64(5))))
    @test objectid(STU("abc" * "d")) == objectid(STU("abc" * "d"))

    # a pre-boxed immediate value canonicalizes on store
    v = Base.inferencebarrier(Int32(42))::Union{Int32, String}
    m.x = v
    @test m.x === Int32(42)

    # hash consistency through Dict
    d = Dict{STU, Int}()
    d[STU(UInt63(UInt64(9)))] = 1
    d[STU("xyz" * "w")] = 2
    @test d[STU(UInt63(UInt64(9)))] == 1
    @test d[STU("xyzw")] == 2
end

@testset "swap/modify/replace/setonce" begin
    m = MTU(Int32(1))
    old = @atomicswap :not_atomic m.x = "s" * "1"
    @test old === Int32(1)
    @test m.x == "s1"
    got = @atomicreplace :not_atomic m.x "s1" => Int32(7)
    @test got.success && got.old == "s1"
    @test m.x === Int32(7)
    got = @atomicreplace :not_atomic m.x "nope" => Int32(8)
    @test !got.success && got.old === Int32(7)
    pair = @atomic :not_atomic m.x += Int32(3)
    @test pair === Int32(10)
    @test m.x === Int32(10)

    m2 = MTU()
    @test_throws UndefRefError @atomicswap :not_atomic m2.x = Int32(1)

    m3 = MTU()
    @test setfieldonce!(m3, :x, Int32(5), :not_atomic, :not_atomic)
    @test m3.x === Int32(5)
    @test !setfieldonce!(m3, :x, Int32(6), :not_atomic, :not_atomic)
    @test m3.x === Int32(5)
end

mutable struct ATU
    @atomic x::Union{Int32, String}
    ATU(x) = new(x)
end

@testset "atomic fields" begin
    a = ATU(Int32(1))
    @test (@atomic a.x) === Int32(1)
    @atomic a.x = "s" * "2"
    @test (@atomic a.x) == "s2"
    old = @atomicswap a.x = Int32(3)
    @test old == "s2"
    got = @atomicreplace a.x Int32(3) => Int32(4)
    @test got.success
    @test (@atomic a.x) === Int32(4)
end

@testset "Memory and Array" begin
    mem = Memory{Union{UInt63, String}}(undef, 8)
    @test !isassigned(mem, 1)
    @test_throws UndefRefError mem[1]
    mem[1] = UInt63(UInt64(11))
    mem[2] = "a" * "b"
    @test mem[1] === UInt63(UInt64(11))
    @test mem[2] == "ab"
    @test isassigned(mem, 1) && isassigned(mem, 2) && !isassigned(mem, 3)
    Base.unsetindex!(mem, 1)
    @test !isassigned(mem, 1)

    mem2 = copy(mem)
    @test !isassigned(mem2, 1)
    @test mem2[2] == "ab"

    v = Vector{Union{UInt63, String}}(undef, 2)
    v[1] = UInt63(UInt64(1))
    v[2] = "x" * "y"
    push!(v, UInt63(UInt64(3)))
    append!(v, [UInt63(UInt64(4)), "zz" * ""])
    @test v[1] === UInt63(UInt64(1))
    @test v[2] == "xy"
    @test v[3] === UInt63(UInt64(3))
    @test v[5] == "zz"
    @test length(v) == 5
    deleteat!(v, 2)
    @test v[2] === UInt63(UInt64(3))

    # copyto! within and between memories, overlapping both directions
    src = Memory{Union{UInt63, String}}(undef, 6)
    for i in 1:6
        src[i] = isodd(i) ? UInt63(UInt64(i)) : "s" * string(i)
    end
    dst = Memory{Union{UInt63, String}}(undef, 6)
    copyto!(dst, src)
    @test dst[1] === UInt63(UInt64(1)) && dst[2] == "s2" && dst[6] == "s6"
    a = collect(Union{UInt63, String}, src)
    copyto!(a, 2, a, 1, 4)
    @test a[2] === UInt63(UInt64(1)) && a[3] == "s2"

    @test_throws ArgumentError Base.unsafe_wrap(Memory{Union{UInt63, String}},
                                                convert(Ptr{Union{UInt63, String}}, C_NULL), 0)

    # struct elements containing tagged slots
    memS = Memory{STU}(undef, 3)
    memS[1] = STU(UInt63(UInt64(21)))
    memS[2] = STU("el" * "2")
    @test memS[1].x === UInt63(UInt64(21))
    @test memS[2].x == "el2"
end

@testset "atomic Memory" begin
    amem = AtomicMemory{Union{Int32, String}}(undef, 2)
    @atomic amem[1] = Int32(5)
    @atomic amem[2] = "at" * "om"
    @test (@atomic amem[1]) === Int32(5)
    @test (@atomic amem[2]) == "atom"
    old = @atomicswap amem[1] = "sw" * "ap"
    @test old === Int32(5)
    @test (@atomic amem[1]) == "swap"
end

@testset "GC stress" begin
    n = 1000
    mem = Memory{Union{UInt63, String}}(undef, n)
    for i in 1:n
        mem[i] = isodd(i) ? UInt63(UInt64(i)) : "str" * string(i)
    end
    GC.gc(true); GC.gc(true); GC.gc(false)
    for i in 1:n
        if isodd(i)
            @test mem[i] === UInt63(UInt64(i))
        else
            @test mem[i] == "str" * string(i)
        end
    end

    # old parent, young child: remembered set through the tagged slot
    holder = MTU(Int32(0))
    GC.gc(true); GC.gc(true)  # promote holder
    holder.x = "young" * string(rand(1:10))
    ys = holder.x
    GC.gc(false)
    @test holder.x === ys

    # multi_wb path: store an immutable struct containing a tagged slot
    memS = Memory{STU}(undef, 4)
    GC.gc(true); GC.gc(true)
    memS[1] = STU("young" * "1")
    GC.gc(false)
    @test memS[1].x == "young1"

    # same, but into an old mutable parent's inline field: the barrier has to
    # see the conditional reference inside the stored value, not just the
    # unconditional pointers. Needs a heap of its own, since a young sweep is
    # what exposes a missing remset entry.
    let script = """
        primitive type U63 63 end
        U63(x::UInt64) = Core.Intrinsics.trunc_int(U63, x)
        struct S; x::Union{U63, String}; end
        mutable struct MI; s::S; end                   # tagged word inside an inline field
        mutable struct MD; s::Union{U63, String}; end  # tagged word as the field itself
        @noinline setinline!(m::MI, s::S) = (m.s = s)
        @noinline setdirect!(m::MD, s::Union{U63, String}) = (m.s = s)
        function probe(n, rounds)
            ps = [MI(S(U63(UInt64(0)))) for _ in 1:n]
            qs = [MD(U63(UInt64(0))) for _ in 1:n]
            GC.gc(true); GC.gc(true)  # promote the parents
            bad = 0
            for r in 1:rounds
                for i in 1:n
                    setinline!(ps[i], S(string("inline", r, "_", i)))
                    setdirect!(qs[i], string("direct", r, "_", i))
                end
                GC.gc(false)  # young sweep: reaches the strings only via the remset
                for _ in 1:20_000; Ref(rand(UInt64)); end  # reuse whatever was freed
                for i in 1:n
                    ps[i].s.x == string("inline", r, "_", i) || (bad += 1)
                    qs[i].s == string("direct", r, "_", i) || (bad += 1)
                end
            end
            return bad
        end
        exit(probe(500, 20) == 0 ? 0 : 1)
        """
        cmd = `$(Base.julia_cmd()) --startup-file=no -e $script`
        @test success(pipeline(cmd; stdout=devnull, stderr=devnull))
    end

    # finalizer liveness: overwriting the only reference lets the object die
    died = Ref(false)
    m = MTU(Int32(0))
    @noinline function attach_and_drop(m, died)
        obj = MTU(Int32(1))  # dummy mutable to attach a finalizer to
        finalizer(x -> (died[] = true), obj)
        m.x = string(objectid(obj))  # a String; obj dies with this frame
        nothing
    end
    attach_and_drop(m, died)
    GC.gc(true); GC.gc(true)
    @test died[]
end

@testset "tuples and nested structs" begin
    t = (Int32(1), STU(UInt63(UInt64(2))), "s")
    @test t[2].x === UInt63(UInt64(2))
    tt = Tuple{Union{Int32, String}, Int}((Int32(3), 4))
    @test tt[1] === Int32(3)
    tt2 = Tuple{Union{Int32, String}, Int}(("a" * "b", 4))
    @test tt2[1] == "ab"

    struct Outer
        pre::Int8
        inner::STU
        post::Union{Int32, String}
    end
    o = Outer(Int8(1), STU(UInt63(UInt64(5))), Int32(6))
    @test o.inner.x === UInt63(UInt64(5))
    @test o.post === Int32(6)
    o2 = Outer(Int8(1), STU("in" * "ner"), "po" * "st")
    @test o2.inner.x == "inner"
    @test o2.post == "post"
    @test o2 === Outer(Int8(1), STU("inner"), "post")

    # closures capturing tagged-union-typed values
    function makeclosure(u::Union{Int32, String})
        () -> u
    end
    @test makeclosure(Int32(9))() === Int32(9)
    @test makeclosure("cl" * "os")() == "clos"
end

function summarysize_roundtrips()
    m = MTU("payload" * "!")
    sz = Base.summarysize(m)
    m2 = MTU(Int32(1))
    sz2 = Base.summarysize(m2)
    sz > sz2  # the string is charged to the containing object
end

@testset "show and summarysize" begin
    m = MTU(Int32(7))
    @test occursin("7", sprint(show, m.x))
    buf = IOBuffer()
    Base.show(buf, MIME"text/plain"(), Memory{Union{Int32, String}}(undef, 2))
    @test summarysize_roundtrips()
end

struct STI
    x::Union{Int32, String}
end

@testset "Serialization round-trip" begin
    # n.b. stdlib Serialization has no `write` methods for custom odd-bit
    # primitives (a pre-existing gap), so this uses Int32 immediates
    using Serialization
    mem = Memory{Union{Int32, String}}(undef, 3)
    mem[1] = Int32(77)
    mem[2] = "ser" * "ial"
    io = IOBuffer()
    serialize(io, (mem, STI(Int32(3)), STI("s" * "2")))
    seekstart(io)
    (mem2, s1, s2) = deserialize(io)
    @test mem2[1] === Int32(77)
    @test mem2[2] == "serial"
    @test !isassigned(mem2, 3)
    @test s1.x === Int32(3)
    @test s2.x == "s2"
end

@testset "C API word encoding (the ABI guarantee)" begin
    # read the raw word: odd => immediate with tag 2i+1 and payload << k
    m = MTU(Int32(21))
    GC.@preserve m begin
        w = unsafe_load(convert(Ptr{UInt64}, pointer_from_objref(m)))
        @test w & 1 == 1              # immediate
        @test w >> 1 == UInt64(21)    # payload for the single immediate member
    end
    s = "abc" * "d"
    m.x = s
    GC.@preserve m s begin
        w = unsafe_load(convert(Ptr{UInt64}, pointer_from_objref(m)))
        @test w & 1 == 0              # reference, stored untagged
        @test w == UInt64(reinterpret(UInt, pointer_from_objref(s)))
    end
end

@testset "no allocation on the immediate path" begin
    function sum_imm(mem::Memory{Union{UInt63, String}})
        s = UInt64(0)
        for i in eachindex(mem)
            x = mem[i]
            if x isa UInt63
                s += UInt64(x)
            end
        end
        s
    end
    mem = Memory{Union{UInt63, String}}(undef, 100)
    for i in 1:100
        mem[i] = UInt63(UInt64(i))
    end
    @test sum_imm(mem) == UInt64(5050)
    # note: allocation-freedom of this loop arrives with the codegen fast
    # paths; for now only correctness is asserted
end

end # TAGGED_ENABLED
