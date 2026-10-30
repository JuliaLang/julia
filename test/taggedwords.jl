# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tagged words: a Union mixing reference components with 1..4 small primitive
# components is stored as one tagged word. See doc/src/devdocs/taggedwords.md
# for the encoding.

using Test
using InteractiveUtils: code_llvm

const W = Sys.WORD_SIZE

# the widest immediate components at tag width k = 1, 2, 3
primitive type Widest1 (W - 1) end
Widest1(x::UInt) = Core.Intrinsics.trunc_int(Widest1, x)
Base.UInt(x::Widest1) = Core.Intrinsics.zext_int(UInt, x)
primitive type Widest2 (W - 2) end
primitive type Widest3 (W - 3) end

istagged(u) = Base.istaggedunion(u)

@testset "classification" begin
    # reference + small primitive => tagged
    @test istagged(Union{Int16, String})
    @test istagged(Union{Widest1, String})
    @test istagged(Union{Bool, String})
    @test !istagged(Union{Int16, Nothing})           # all-inline: selector layout wins
    @test istagged(Union{Int16, Nothing, String})    # singleton rides along as a reference
    @test istagged(Union{Int16, AbstractString})     # abstract components are references
    @test istagged(Union{Int8, Int16, String})       # k = 2
    @test istagged(Union{Int8, Int16, UInt8, String})  # k = 3, 3 immediates
    @test istagged(Union{Int8, Int16, UInt8, UInt16, String}) # k = 3, 4 immediates
    @test istagged(Union{Widest2, Widest3, String})  # W - 2 <= W - 2
    @test istagged(Union{Int8, Int, String})         # Int is a reference component

    # 32-bit primitives are immediate components only on 64-bit platforms
    @test istagged(Union{Int32, String}) == (W == 64)
    @test istagged(Union{Char, String}) == (W == 64)
    @test istagged(Union{Float32, String}) == (W == 64)

    # not tagged: no immediate fits / no reference component / too many immediates
    @test !istagged(Union{Int, String})              # W > W - 1
    @test !istagged(Union{Float64, String})
    @test !istagged(Union{Ptr{Cvoid}, String})
    @test !istagged(Union{Int8, Int16})              # all-isbits stays selector-based
    @test !istagged(Union{Int8, Int16, UInt8, UInt16, Bool, String}) # 5 immediates
    @test !istagged(Union{Widest1, Widest2, String}) # W - 1 > W - 2, all-or-nothing
    @test !istagged(Union{String, Nothing})          # no immediate component
    @test Base.isbitsunion(Union{Int8, Int16})       # unchanged
    small_bits_union = Union{(Val{i} for i in 1:125)..., Int16}
    large_bits_union = Union{(Val{i} for i in 1:126)..., Int16}
    @test Base.isbitsunion(small_bits_union)
    @test !istagged(small_bits_union)
    @test all(isbitstype, Base.uniontypes(large_bits_union))
    @test istagged(large_bits_union)

    # layout introspection
    # as for any union with a reference component, at runtime and in inference
    @test_throws ErrorException sizeof(Union{Int16, String})
    @test Base.infer_return_type(() -> sizeof(Union{Int16, String})) === Union{}
    struct TU1; x::Union{Int16, String}; end
    @test Base.datatype_ntaggedwords(TU1) == 1
    @test Base.datatype_npointers(TU1) == 1      # tagged words are pointers in the layout
    @test !Base.datatype_pointerfree(TU1)
    @test sizeof(TU1) == sizeof(Int)
    @test Base.datatype_arrayelem(Memory{Union{Int16, String}}) == 3
    struct TUNest; a::Int8; b::TU1; c::String; end
    @test Base.datatype_ntaggedwords(TUNest) == 1
    @test Base.datatype_npointers(TUNest) == 2
    @test fieldoffset(TUNest, 2) == sizeof(Int)
end

mutable struct MTU
    x::Union{Int16, String}
    MTU() = new()
    MTU(x) = new(x)
end

struct STU
    x::Union{Widest1, String}
end

mutable struct MPrim
    x::Union{Int8, Int32, Int, String}
end

struct TOnly  # its only reference slot is a tagged word
    x::Union{Int16, String}
end

mutable struct HoldsTOnly
    t::TOnly
    HoldsTOnly() = new()
end

@testset "field semantics" begin
    m = MTU()
    @test !isdefined(m, :x)
    @test_throws UndefRefError m.x
    m.x = Int16(17)
    @test isdefined(m, :x)
    @test m.x === Int16(17)
    s = "hello" * "!"  # non-interned
    m.x = s
    @test m.x === s
    m.x = Int16(-1)
    @test m.x === Int16(-1)

    # negative payloads keep their bits
    for v in (Int16(0), Int16(-1), typemax(Int16), typemin(Int16))
        m.x = v
        @test m.x === v
    end

    # widest payload boundary values
    st = STU(Widest1(typemax(UInt) >> 1))
    @test UInt(st.x) == typemax(UInt) >> 1
    st = STU(Widest1(UInt(0)))
    @test UInt(st.x) == 0

    # primitive components of other widths: Int8 and, on 64-bit platforms,
    # Int32 are immediates; Int is a reference component
    p = MPrim(Int8(0))
    for v in (Int8(-2), typemin(Int32), typemax(Int32), typemin(Int), typemax(Int))
        p.x = v
        @test p.x === v
    end

    # egal and objectid
    @test STU(Widest1(UInt(5))) === STU(Widest1(UInt(5)))
    @test STU(Widest1(UInt(5))) !== STU(Widest1(UInt(6)))
    @test STU("abc" * "d") === STU("abc" * "d")  # contents-egal references
    @test STU("abcd") !== STU(Widest1(UInt(5)))
    @test objectid(STU(Widest1(UInt(5)))) == objectid(STU(Widest1(UInt(5))))
    @test objectid(STU("abc" * "d")) == objectid(STU("abc" * "d"))

    # a pre-boxed immediate value canonicalizes on store
    v = Base.inferencebarrier(Int16(42))::Union{Int16, String}
    m.x = v
    @test m.x === Int16(42)

    # hash consistency through Dict
    d = Dict{STU, Int}()
    d[STU(Widest1(UInt(9)))] = 1
    d[STU("xyz" * "w")] = 2
    @test d[STU(Widest1(UInt(9)))] == 1
    @test d[STU("xyzw")] == 2
end

@testset "#undef inline field whose only reference is tagged" begin
    # the tagged word marks the inline value #undef, as the first pointer does
    # for an inline struct holding references
    h = HoldsTOnly()
    isdef(h) = isdefined(h, :t)
    getf(h) = h.t
    @test !isdefined(h, :t)
    @test !isdef(h)
    @test_throws UndefRefError h.t
    @test_throws UndefRefError getf(h)
    h.t = TOnly(Int16(3))
    @test isdef(h)
    @test getf(h).x === Int16(3)
end

@testset "swap/modify/replace/setonce" begin
    m = MTU(Int16(1))
    old = @atomicswap :not_atomic m.x = "s" * "1"
    @test old === Int16(1)
    @test m.x == "s1"
    got = @atomicreplace :not_atomic m.x "s1" => Int16(7)
    @test got.success && got.old == "s1"
    @test m.x === Int16(7)
    got = @atomicreplace :not_atomic m.x "nope" => Int16(8)
    @test !got.success && got.old === Int16(7)
    pair = @atomic :not_atomic m.x += Int16(3)
    @test pair === Int16(10)
    @test m.x === Int16(10)

    m2 = MTU()
    @test_throws UndefRefError @atomicswap :not_atomic m2.x = Int16(1)

    m3 = MTU()
    @test setfieldonce!(m3, :x, Int16(5), :not_atomic, :not_atomic)
    @test m3.x === Int16(5)
    @test !setfieldonce!(m3, :x, Int16(6), :not_atomic, :not_atomic)
    @test m3.x === Int16(5)
end

mutable struct ATU
    @atomic x::Union{Int16, String}
    ATU(x) = new(x)
end

@testset "atomic fields" begin
    a = ATU(Int16(1))
    @test (@atomic a.x) === Int16(1)
    @atomic a.x = "s" * "2"
    @test (@atomic a.x) == "s2"
    old = @atomicswap a.x = Int16(3)
    @test old == "s2"
    got = @atomicreplace a.x Int16(3) => Int16(4)
    @test got.success
    @test (@atomic a.x) === Int16(4)

    # compiled
    getat(a) = @atomic a.x
    setat!(a, v) = (@atomic a.x = v; nothing)
    setat!(a, "at" * "om")
    @test getat(a) == "atom"
    setat!(a, Int16(-3))
    @test getat(a) === Int16(-3)
end

@testset "Memory and Array" begin
    mem = Memory{Union{Widest1, String}}(undef, 8)
    @test !isassigned(mem, 1)
    @test_throws UndefRefError mem[1]
    mem[1] = Widest1(UInt(11))
    mem[2] = "a" * "b"
    @test mem[1] === Widest1(UInt(11))
    @test mem[2] == "ab"
    @test isassigned(mem, 1) && isassigned(mem, 2) && !isassigned(mem, 3)
    Base.unsetindex!(mem, 1)
    @test !isassigned(mem, 1)

    mem2 = copy(mem)
    @test !isassigned(mem2, 1)
    @test mem2[2] == "ab"

    v = Vector{Union{Widest1, String}}(undef, 2)
    v[1] = Widest1(UInt(1))
    v[2] = "x" * "y"
    push!(v, Widest1(UInt(3)))
    append!(v, [Widest1(UInt(4)), "zz" * ""])
    @test v[1] === Widest1(UInt(1))
    @test v[2] == "xy"
    @test v[3] === Widest1(UInt(3))
    @test v[5] == "zz"
    @test length(v) == 5
    deleteat!(v, 2)
    @test v[2] === Widest1(UInt(3))

    # copyto! within and between memories, overlapping both directions
    src = Memory{Union{Widest1, String}}(undef, 6)
    for i in 1:6
        src[i] = isodd(i) ? Widest1(UInt(i)) : "s" * string(i)
    end
    dst = Memory{Union{Widest1, String}}(undef, 6)
    copyto!(dst, src)
    @test dst[1] === Widest1(UInt(1)) && dst[2] == "s2" && dst[6] == "s6"
    a = collect(Union{Widest1, String}, src)
    copyto!(a, 2, a, 1, 4)
    @test a[2] === Widest1(UInt(1)) && a[3] == "s2"

    @test_throws ArgumentError Base.unsafe_wrap(Memory{Union{Widest1, String}},
                                                convert(Ptr{Union{Widest1, String}}, C_NULL), 0)

    # struct elements containing tagged words
    memS = Memory{STU}(undef, 3)
    memS[1] = STU(Widest1(UInt(21)))
    memS[2] = STU("el" * "2")
    @test memS[1].x === Widest1(UInt(21))
    @test memS[2].x == "el2"
end

@testset "atomic Memory" begin
    amem = AtomicMemory{Union{Int16, String}}(undef, 2)
    @atomic amem[1] = Int16(5)
    @atomic amem[2] = "at" * "om"
    @test (@atomic amem[1]) === Int16(5)
    @test (@atomic amem[2]) == "atom"
    old = @atomicswap amem[1] = "sw" * "ap"
    @test old === Int16(5)
    @test (@atomic amem[1]) == "swap"
end

@testset "GC stress" begin
    n = 1000
    mem = Memory{Union{Widest1, String}}(undef, n)
    for i in 1:n
        mem[i] = isodd(i) ? Widest1(UInt(i)) : "str" * string(i)
    end
    GC.gc(true); GC.gc(true); GC.gc(false)
    for i in 1:n
        if isodd(i)
            @test mem[i] === Widest1(UInt(i))
        else
            @test mem[i] == "str" * string(i)
        end
    end

    # old parent, young child: remembered set through the tagged word
    holder = MTU(Int16(0))
    GC.gc(true); GC.gc(true)  # promote holder
    holder.x = "young" * string(rand(1:10))
    ys = holder.x
    GC.gc(false)
    @test holder.x === ys

    # multi_wb path: store an immutable struct containing a tagged word
    memS = Memory{STU}(undef, 4)
    GC.gc(true); GC.gc(true)
    memS[1] = STU("young" * "1")
    GC.gc(false)
    @test memS[1].x == "young1"

    # same, but into an old mutable parent's inline field: the barrier has to
    # see the reference in the tagged word inside the stored value, not just
    # the plain pointers. Needs a heap of its own, since a young sweep is
    # what exposes a missing remset entry.
    let script = """
        primitive type UW (Sys.WORD_SIZE - 1) end
        UW(x::UInt) = Core.Intrinsics.trunc_int(UW, x)
        struct S; x::Union{UW, String}; end
        mutable struct MI; s::S; end                   # tagged word inside an inline field
        mutable struct MD; s::Union{UW, String}; end   # tagged word as the field itself
        @noinline setinline!(m::MI, s::S) = (m.s = s)
        @noinline setdirect!(m::MD, s::Union{UW, String}) = (m.s = s)
        function probe(n, rounds)
            ps = [MI(S(UW(UInt(0)))) for _ in 1:n]
            qs = [MD(UW(UInt(0))) for _ in 1:n]
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
    m = MTU(Int16(0))
    @noinline function attach_and_drop(m, died)
        obj = MTU(Int16(1))  # dummy mutable to attach a finalizer to
        finalizer(x -> (died[] = true), obj)
        m.x = string(objectid(obj))  # a String; obj dies with this frame
        nothing
    end
    attach_and_drop(m, died)
    GC.gc(true); GC.gc(true)
    @test died[]
end

@testset "tuples and nested structs" begin
    t = (Int16(1), STU(Widest1(UInt(2))), "s")
    @test t[2].x === Widest1(UInt(2))
    tt = Tuple{Union{Int16, String}, Int}((Int16(3), 4))
    @test tt[1] === Int16(3)
    tt2 = Tuple{Union{Int16, String}, Int}(("a" * "b", 4))
    @test tt2[1] == "ab"

    struct Outer
        pre::Int8
        inner::STU
        post::Union{Int16, String}
    end
    o = Outer(Int8(1), STU(Widest1(UInt(5))), Int16(6))
    @test o.inner.x === Widest1(UInt(5))
    @test o.post === Int16(6)
    o2 = Outer(Int8(1), STU("in" * "ner"), "po" * "st")
    @test o2.inner.x == "inner"
    @test o2.post == "post"
    @test o2 === Outer(Int8(1), STU("inner"), "post")

    # closures capturing values of a union stored as a tagged word
    function makeclosure(u::Union{Int16, String})
        () -> u
    end
    @test makeclosure(Int16(9))() === Int16(9)
    @test makeclosure("cl" * "os")() == "clos"
end

function summarysize_roundtrips()
    m = MTU("payload" * "!")
    sz = Base.summarysize(m)
    m2 = MTU(Int16(1))
    sz2 = Base.summarysize(m2)
    sz > sz2  # the string is charged to the containing object
end

@testset "show and summarysize" begin
    m = MTU(Int16(7))
    @test occursin("7", sprint(show, m.x))
    buf = IOBuffer()
    Base.show(buf, MIME"text/plain"(), Memory{Union{Int16, String}}(undef, 2))
    @test summarysize_roundtrips()

    # an immediate element is no object of its own
    mem = Memory{Union{Int16, String}}(undef, 10)
    undefsize = (Base.summarysize(mem), Base.summarysize(mem; count=true))
    fill!(mem, Int16(1000))
    @test (Base.summarysize(mem), Base.summarysize(mem; count=true)) == undefsize
    s = "payload" * "!"
    mem[1] = s
    @test Base.summarysize(mem) == undefsize[1] + Base.summarysize(s)
end

@testset "Ref conversion" begin
    T = Union{Int16, String}
    s = "ab" * "c"
    for r in (Ref{T}(s), Ref(T[s], 1))
        GC.@preserve r @test Base.unsafe_convert(Ptr{Cvoid}, r) == pointer_from_objref(s)
    end
    # an immediate has no box to point to
    for r in (Ref{T}(Int16(5)), Ref(T[Int16(5)], 1))
        @test_throws ArgumentError Base.unsafe_convert(Ptr{Cvoid}, r)
    end
end

struct STI
    x::Union{Int16, String}
end

@testset "Serialization round-trip" begin
    # n.b. stdlib Serialization has no `write` methods for custom odd-bit
    # primitives (a pre-existing gap), so this uses Int16 immediates
    using Serialization
    mem = Memory{Union{Int16, String}}(undef, 3)
    mem[1] = Int16(77)
    mem[2] = "ser" * "ial"
    io = IOBuffer()
    serialize(io, (mem, STI(Int16(3)), STI("s" * "2")))
    seekstart(io)
    (mem2, s1, s2) = deserialize(io)
    @test mem2[1] === Int16(77)
    @test mem2[2] == "serial"
    @test !isassigned(mem2, 3)
    @test s1.x === Int16(3)
    @test s2.x == "s2"
end

@testset "C API word encoding (the ABI guarantee)" begin
    # read the raw word: odd => immediate with tag 2i+1 and payload << k
    m = MTU(Int16(21))
    GC.@preserve m begin
        w = unsafe_load(convert(Ptr{UInt}, pointer_from_objref(m)))
        @test w & 1 == 1              # immediate
        @test w >> 1 == UInt(21)      # payload for the single immediate component
    end
    s = "abc" * "d"
    m.x = s
    GC.@preserve m s begin
        w = unsafe_load(convert(Ptr{UInt}, pointer_from_objref(m)))
        @test w & 3 == 0              # reference, stored untagged
        @test w == reinterpret(UInt, pointer_from_objref(s))
    end
end

mutable struct MNI  # references to small components carry their union index
    x::Union{Nothing, Int16, Int, String}
    MNI() = new()
    MNI(x) = new(x)
end

mutable struct MK3  # tag width 3: four immediates of different kinds
    x::Union{Bool, Int8, Float16, UInt16, String}
end

getx(m) = m.x
setx!(m, v) = (m.x = v; nothing)
setx_any!(m, @nospecialize(v)) = (m.x = v; nothing)
copyx!(a, b) = (a.x = b.x; nothing)
rawx(m) = GC.@preserve m unsafe_load(convert(Ptr{UInt}, pointer_from_objref(m)))
llvm(f, types) = sprint(io -> code_llvm(io, f, types; debuginfo=:none))
getm(mem, i) = mem[i]
setm!(mem, i, v) = (mem[i] = v; nothing)
setm_any!(mem, i, @nospecialize(v)) = (mem[i] = v; nothing)
isas(mem, i) = isassigned(mem, i)

@testset "codegen: fields" begin
    @test !occursin("get_nth_field", llvm(getx, (MTU,)))
    @test !occursin("store_tagged_word", llvm(setx!, (MTU, Int16)))
    # `new` stores into a fresh object, which needs no write barrier unless
    # encoding allocates a box
    unopt(f, types) = sprint(io -> code_llvm(io, f, types; debuginfo=:none, optimize=false))
    @test !occursin("write_barrier", unopt(MTU, (String,)))
    mknni(c, i::Int) = MNI(c ? i : i % Int16)
    @test occursin("write_barrier", unopt(mknni, (Bool, Int)))

    m = MTU()
    @test_throws UndefRefError getx(m)
    s = "ab" * "c"
    for v in (Int16(0), Int16(-1), typemin(Int16), typemax(Int16), s)
        setx!(m, v)
        @test getx(m) === v
        setx_any!(m, v)
        @test getx(m) === v
        m2 = MTU()
        copyx!(m2, m)
        @test getx(m2) === v
    end
    # an immediate boxed by a dynamic caller is stored unboxed
    setx_any!(m, Int16(42))
    @test rawx(m) == UInt(42) << 1 | 1

    # isa branches on the decoded union index, for references too
    kind(m) = (x = m.x; x isa Nothing ? 0 : x isa Int16 ? 1 : x isa Int ? 2 : 3)
    n = MNI()
    @test_throws UndefRefError kind(n)
    for (v, k) in ((nothing, 0), (Int16(-5), 1), (typemin(Int), 2), ("s" * "t", 3))
        setx!(n, v)
        @test kind(n) == k
        @test getx(n) === v
        setx_any!(n, v)
        @test kind(n) == k
        n2 = MNI()
        copyx!(n2, n)
        @test getx(n2) === v
    end
    # a split union value whose small reference component still needs a box
    pick!(m, c, i::Int) = (m.x = c ? i : i % Int16; nothing)
    pick!(n, true, 7)
    @test getx(n) === 7
    pick!(n, false, 7)
    @test getx(n) === Int16(7)
    picknothing!(m, c, i::Int) = (m.x = c ? i : nothing; nothing)
    picknothing!(n, true, 8)
    @test getx(n) === 8
    picknothing!(n, false, 8)
    @test getx(n) === nothing

    # every kind of immediate keeps its bits
    k3 = MK3(false)
    for v in (true, false, Int8(-128), Float16(-0.0), reinterpret(Float16, 0x7e01), typemax(UInt16), "k" * "3")
        setx!(k3, v)
        @test getx(k3) === v
        setx_any!(k3, v)
        @test getx(k3) === v
    end
    p = MPrim(Int8(0))
    for v in (Int8(-2), typemin(Int32), typemax(Int32), typemin(Int))
        setx!(p, v)
        @test getx(p) === v
    end
    # the widest payload, through `new` and a load from an immutable struct
    mkstu(x) = STU(x)
    for u in (UInt(0), typemax(UInt) >> 1)
        @test UInt(getx(mkstu(Widest1(u)))) == u
    end
end

@testset "codegen: Memory" begin
    mem = Memory{Union{Nothing, Int16, Int, String}}(undef, 2)
    @test !occursin("memoryrefget", llvm(getm, (typeof(mem), Int)))
    @test !occursin("memoryrefset", llvm(setm!, (typeof(mem), Int, Int16)))
    @test_throws UndefRefError getm(mem, 1)
    @test !isas(mem, 1)
    for v in (nothing, Int16(3), typemax(Int), "m" * "em")
        setm!(mem, 1, v)
        @test isas(mem, 1)
        @test getm(mem, 1) === v
        setm_any!(mem, 2, v)
        @test getm(mem, 2) === v
    end

    amem = AtomicMemory{Union{Int16, String}}(undef, 1)
    agetm(m, i) = @atomic m[i]
    asetm!(m, i, v) = (@atomic m[i] = v; nothing)
    asetm!(amem, 1, Int16(5))
    @test agetm(amem, 1) === Int16(5)
    asetm!(amem, 1, "a" * "b")
    @test agetm(amem, 1) == "ab"
end

@testset "codegen: no allocation for immediates" begin
    function sumfield(ms)
        s = 0
        for m in ms
            x = m.x
            x isa Int16 && (s += x)
        end
        s
    end
    function setfield(ms)
        for (i, m) in enumerate(ms)
            m.x = i % Int16
        end
    end
    function summem(mem)
        s = 0
        for i in eachindex(mem)
            x = mem[i]
            x isa Int16 && (s += x)
        end
        s
    end
    function setmem(mem)
        for i in eachindex(mem)
            mem[i] = i % Int16
        end
    end
    function copymem(dst, src)
        for i in eachindex(src)
            dst[i] = src[i]
        end
    end
    ms = [MTU(i % Int16) for i in 1:1000]
    mem = Memory{Union{Int16, String}}(undef, 1000)
    setmem(mem)
    dst = similar(mem)
    setfield(ms)
    copymem(dst, mem)
    @test sumfield(ms) == summem(mem) == summem(dst) == 500500
    @test (@allocated sumfield(ms)) == 0
    @test (@allocated setfield(ms)) == 0
    @test (@allocated summem(mem)) == 0
    @test (@allocated setmem(mem)) == 0
    @test (@allocated copymem(dst, mem)) == 0

    # references to small components are copied, not boxed again
    ns = Memory{Union{Nothing, Int16, Int, String}}(undef, 4)
    ns[1] = nothing; ns[2] = Int16(1); ns[3] = typemax(Int); ns[4] = "x" * "y"
    nd = similar(ns)
    copymem(nd, ns)
    @test all(i -> nd[i] === ns[i], 1:4)
    @test (@allocated copymem(nd, ns)) == 0

    function sum_imm(mem::Memory{Union{Widest1, String}})
        s = UInt(0)
        for i in eachindex(mem)
            x = mem[i]
            if x isa Widest1
                s += UInt(x)
            end
        end
        s
    end
    wmem = Memory{Union{Widest1, String}}(undef, 100)
    for i in 1:100
        wmem[i] = Widest1(UInt(i))
    end
    @test sum_imm(wmem) == UInt(5050)
    @test (@allocated sum_imm(wmem)) == 0
end

@testset "codegen: write barrier" begin
    # fresh strings stored by compiled code into old parents survive a young
    # collection only through the remembered set
    function fillmem!(mem, r)
        for i in eachindex(mem)
            mem[i] = isodd(i) ? i % Int16 : string("m", r, "_", i)
        end
    end
    function fillfields!(ms, r)
        for i in eachindex(ms)
            ms[i].x = isodd(i) ? i % Int16 : string("f", r, "_", i)
        end
    end
    mem = Memory{Union{Int16, String}}(undef, 500)
    ms = [MTU() for _ in 1:500]
    GC.gc(true); GC.gc(true)  # promote the parents
    bad = 0
    for r in 1:10
        fillmem!(mem, r)
        fillfields!(ms, r)
        GC.gc(false)
        for _ in 1:20_000; Ref(rand(UInt64)); end  # reuse whatever was freed
        for i in eachindex(mem)
            mem[i] == (isodd(i) ? i % Int16 : string("m", r, "_", i)) || (bad += 1)
            ms[i].x == (isodd(i) ? i % Int16 : string("f", r, "_", i)) || (bad += 1)
        end
    end
    @test bad == 0
end

# Int16 sorts after the 127 small components codegen's union index can name
const BigU = Union{(Val{i} for i in 1:130)..., Int16, String}
mutable struct MBig
    x::BigU
end
getbig(m) = m.x
setbig!(m, v) = (m.x = v; nothing)
getbigm(mem) = mem[1]
setbigm!(mem, v) = (mem[1] = v; nothing)

@testset "codegen: more components than the union index" begin
    @test istagged(BigU)
    m = MBig(Int16(7))
    mem = Memory{BigU}(undef, 1)
    for v in (Int16(-3), "s", Val(129))
        setbig!(m, v)
        @test getbig(m) === v
        setbigm!(mem, v)
        @test getbigm(mem) === v
    end
end

# The 128th small component cannot be represented in a union index.
const LimitU = Union{(Val{i} for i in 1:126)..., Int16, String}
const EdgeU = Union{(Val{i} for i in 1:127)..., Int16, String}
mutable struct MLimit
    x::LimitU
end
mutable struct MEdge
    x::EdgeU
end
mutable struct AMEdge
    @atomic x::EdgeU
end
edgekind(m) = m.x isa Int16
edgekindmem(m) = m[1] isa Int16
edgecopy!(dst, src) = (dst.x = src.x; nothing)
edgecopymem!(dst, src) = (dst[1] = src[1]; nothing)
edgeatomiccopy!(dst, src) = (@atomic dst.x = (@atomic src.x); nothing)
edgeatomiccopymem!(dst, src) = (@atomic dst[1] = (@atomic src[1]); nothing)
limitkind(m) = m.x isa Int16

@testset "codegen: union index boundary" begin
    @test limitkind(MLimit(Int16(7)))
    @test !occursin("get_nth_field", llvm(limitkind, (MLimit,)))
    m = MEdge(Int16(7))
    mem = Memory{EdgeU}(undef, 1)
    mem[1] = Int16(7)
    @test edgekind(m)
    @test edgekindmem(mem)
    dst = MEdge("s")
    dstmem = Memory{EdgeU}(undef, 1)
    a = AMEdge(Int16(7))
    adst = AMEdge("s")
    amem = AtomicMemory{EdgeU}(undef, 1)
    adstmem = AtomicMemory{EdgeU}(undef, 1)
    for v in (Int16(-3), "s", Val(127))
        m.x = v
        mem[1] = v
        @atomic a.x = v
        @atomic amem[1] = v
        edgecopy!(dst, m)
        edgecopymem!(dstmem, mem)
        edgeatomiccopy!(adst, a)
        edgeatomiccopymem!(adstmem, amem)
        @test dst.x === v
        @test dstmem[1] === v
        @test (@atomic adst.x) === v
        @test (@atomic adstmem[1]) === v
    end
end
