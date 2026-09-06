# This file is a part of Julia. License is MIT: https://julialang.org/license

include(joinpath(@__DIR__, "regions_api.jl"))
using Serialization

# A quarantine is permanent, and a quarantined region stays live, which
# blocks its parent's reset. The clean cases use regions 1 to 3 first and
# reset them; the quarantine cases then burn one region each, from 7 down
# to 2, and the fresh-copy cases burn regions 8 to 11. The mask check at
# the end fixes the outcome of every case.

const SIM = 1
const EVENT = 2
const CLEAN = 3

# A window that allocates and drops its own objects, with no store into
# region 0, resets cleanly: nothing about it arms a quarantine.
@noinline function clean_window()
    region_set(CLEAN)
    local last
    for i in 1:1000
        last = Ref(i)
    end
    escape(last)
    region_set(0)
    check("the clean window is not quarantined", quarantined(CLEAN) == 0)
    check("a window with no store into region 0 resets", !refused(region_reset(CLEAN)))
end

mutable struct Holder
    f::Any
end
@noinline make_holder(n) = (region_set(n); h = Holder(nothing); region_set(0); h)
@noinline make_child(n)  = (region_set(n); c = Ref(1); region_set(0); c)
@noinline store!(h::Holder, c) = (h.f = c; nothing)

# A region-0 object is legal under any parent: it outlives every region,
# so the reference never dangles.
function region0_child_into_region1_parent()
    p = make_holder(SIM)
    c = make_child(0)
    store!(p, c)
    check("a region-0 child into a region-1 parent does not quarantine it", quarantined(SIM) == 0)
end

# A same-region store never crosses a reset boundary.
function region1_child_into_region1_parent()
    p = make_holder(SIM)
    c = make_child(SIM)
    store!(p, c)
    check("a region-1 child into a region-1 parent does not quarantine it", quarantined(SIM) == 0)
end

# In the default chain region 1 is the parent of region 2 and resets after
# it, so a region-1 child in a region-2 parent is legal.
function region1_child_into_region2_parent()
    p = make_holder(EVENT)
    c = make_child(SIM)
    store!(p, c)
    check("a region-1 child into a region-2 parent is legal", quarantined(EVENT) == 0)
end

# The child resets before the parent in the default chain.
function reset_clean_regions()
    check("region 2 is not quarantined before its reset", quarantined(EVENT) == 0)
    r2 = code(region_reset(EVENT))
    check("region 2 resets (got $r2)", r2 >= 0)
    check("region 1 is not quarantined before its reset", quarantined(SIM) == 0)
    r1 = code(region_reset(SIM))
    check("region 1 resets (got $r1)", r1 >= 0)
end

# The reverse store is the escape: a region-7 child in a region-6 parent
# would dangle when region 7 resets first. The barrier quarantines the
# child's region, the parent's region stays clean, and the live quarantined
# child blocks the parent's reset from now on.
function chain_escape_quarantines_region7()
    p = make_holder(6)
    c = make_child(7)
    store!(p, c)
    check("a region-7 child into a region-6 parent quarantines region 7", quarantined(7) == 1)
    check("the parent's region is not quarantined", quarantined(6) == 0)
    check("the quarantined reset refuses", code(region_reset(7)) == EQUARANTINED)
    check("the parent refuses its reset while the quarantined child lives", code(region_reset(6)) == ECHILD)
    check("the stored value still reads", p.f[] == 1)
end

# A Dict made in region 0 that grows inside a window allocates its new
# tables where the Dict lives, not where the window is: a replacement buffer
# takes the region of the container it belongs to. So the rehash is not an
# escape and the region stays clean. The buffer follows its container; an
# element does not, which regions_containers.jl fixes in both directions.
@noinline function grow_into!(d, n)
    region_set(n)
    for k in 1:10_000
        d[k] = k
    end
    region_set(0)
end

function dict_rehash_does_not_quarantine_region6()
    d = Dict{Int,Int}()
    grow_into!(d, 6)
    check("the Dict's rehash inside the window does not quarantine", quarantined(6) == 0)
    check("the Dict reads correctly", all(d[k] == k for k in 1:10_000))
end

# A constructor call is a store too: the holder is built in region 0 after
# the child's window closed, and its initializing store puts a region-5
# child into a region-0 object. The barrier catches that store as well.
@noinline make_ctor_child() = Ref{Int}(123456789)
@noinline construct_holder(c) = Holder(c)
@noinline function build_ctor_gap()
    region_set(5)
    c = make_ctor_child()
    region_set(0)
    return construct_holder(c)
end

function ctor_gap_quarantines_region5()
    p = build_ctor_gap()
    check("the constructor store quarantines the child's region", quarantined(5) == 1)
    check("the value reads intact", p.f[] == 123456789)
    check("the quarantined reset refuses", code(region_reset(5)) == EQUARANTINED)
end

# The barrier at construction is the region guard alone. A fresh parent is
# young, so vanilla emits no generational barrier for its boxed children,
# and the region check must not bring one back: the lowered constructor
# holds the guard's block, `region_wb`, and no `may_trigger_wb` block. A
# `setfield!` on an old parent holds both. An object with two boxed children
# pays one guard (one load of the armed flag) and one call per child.
mutable struct Two
    a::Any
    b::Any
end
@noinline construct_two(c, d) = Two(c, d)
using InteractiveUtils: code_llvm
lowered_ir(f, types) = sprint(io -> code_llvm(io, f, types; optimize=true, debuginfo=:none))
function ctor_barrier_is_region_only()
    ctor = lowered_ir(construct_holder, (Base.RefValue{Int},))
    check("the constructor holds the region guard", occursin("region_wb", ctor))
    check("the constructor holds no generational barrier", !occursin("may_trigger_wb", ctor))
    two = lowered_ir(construct_two, (Base.RefValue{Int}, Base.RefValue{Int}))
    check("two boxed children share one guard", count("load i8, ptr @jl_gc_region_barrier_on", two) == 1)
    check("two boxed children get one check each", count("jl_gc_region_wb", two) == 2)
    store = lowered_ir(store!, (Holder, Base.RefValue{Int}))
    check("the setfield! holds the region guard", occursin("region_wb", store))
    check("the setfield! holds the generational barrier", occursin("may_trigger_wb", store))
end

# A copy of an inline value with pointers is a store too, and one that
# stores no box: the pointer fields of the value land in a fresh object by
# memcpy. The barrier walks the pointer fields of the copied value at every
# such copy. Each case makes the child in its own window, then copies a
# value that holds it from region 0 through one of the copy paths.
struct Twin
    a::Base.RefValue{Int}
    b::Base.RefValue{Int}
end
mutable struct TwinHolder
    t::Twin                 # an inline field with two pointers
end
mutable struct Mutable
    a::Base.RefValue{Int}
end
@noinline make_twin_child(n) = (region_set(n); c = Ref{Int}(123456789); region_set(0); c)

# `new` with an inline field: the compiler copies the fields of the Twin
# into the fresh TwinHolder without a box.
@noinline construct_twin_holder(c) = TwinHolder(Twin(c, c))
function inline_field_copy_quarantines_region8()
    c = make_twin_child(8)
    h = construct_twin_holder(c)
    check("an inline field with pointers quarantines the child's region", quarantined(8) == 1)
    check("the value reads intact", h.t.a[] == 123456789)
    check("the quarantined reset refuses", code(region_reset(8)) == EQUARANTINED)
end

# An unboxed value stored into an `Any` slot: the compiler makes a fresh
# box and copies the fields of the value into it.
@noinline box_twin(c) = Ref{Any}(Twin(c, c))
function fresh_box_quarantines_region9()
    c = make_twin_child(9)
    r = box_twin(c)
    check("a fresh box of a value with pointers quarantines the child's region", quarantined(9) == 1)
    check("the value reads intact", (r[]::Twin).b[] == 123456789)
    check("the quarantined reset refuses", code(region_reset(9)) == EQUARANTINED)
end

# A `getfield` whose object type the compiler does not know runs in the
# runtime: `jl_new_bits` copies the inline field into a fresh box.
@noinline dynamic_field(x, i) = getfield(Base.inferencebarrier(x), i)
@noinline function make_twin_holder_in_region(n)
    region_set(n)
    c = Ref{Int}(123456789)
    h = TwinHolder(Twin(c, c))
    region_set(0)
    return h
end
function runtime_copy_quarantines_region10()
    h = make_twin_holder_in_region(10)
    check("the same-region construction does not quarantine", quarantined(10) == 0)
    t = dynamic_field(h, 1)
    check("a runtime copy of an inline field quarantines the child's region", quarantined(10) == 1)
    check("the value reads intact", (t::Twin).a[] == 123456789)
    check("the quarantined reset refuses", code(region_reset(10)) == EQUARANTINED)
end

# `unsafe_load` of a pointer to a mutable struct: the compiler copies the
# struct into a fresh box.
@noinline load_copy(p::Ptr{Mutable}) = unsafe_load(p)
@noinline make_mutable_in_region(n) = (region_set(n); m = Mutable(Ref{Int}(123456789)); region_set(0); m)
function pointer_load_copy_quarantines_region11()
    m = make_mutable_in_region(11)
    p = Ptr{Mutable}(pointer_from_objref(m))
    copy = GC.@preserve m load_copy(p)
    check("a pointer load of a struct with pointers quarantines the child's region", quarantined(11) == 1)
    check("the value reads intact", copy.a[] == 123456789)
    check("the quarantined reset refuses", code(region_reset(11)) == EQUARANTINED)
end

# The barrier at a fresh copy is the region guard alone, one check per
# pointer field of the copied value. The inline-field constructor holds the
# guard and two checks, one per pointer of the Twin, and no generational
# barrier. The fresh box adds two checks to the one that the `Ref{Any}`
# store already pays.
function fresh_copy_barrier_is_region_only()
    holder = lowered_ir(construct_twin_holder, (Base.RefValue{Int},))
    check("the inline-field constructor holds the region guard", occursin("region_wb", holder))
    check("the inline-field constructor holds no generational barrier", !occursin("may_trigger_wb", holder))
    check("the two pointers of the inline field get one check each", count("jl_gc_region_wb", holder) == 2)
    box = lowered_ir(box_twin, (Base.RefValue{Int},))
    check("the fresh box holds the region guard", occursin("region_wb", box))
    check("the fresh box gets one check per pointer plus the store's", count("jl_gc_region_wb", box) == 3)
end

# The barrier of a fresh object must not keep the object on the heap. In
# vanilla this function is `ret 1.0`: GVN forwards the size store of the
# Array to the `length` load, the test folds, and alloc-opt elides the Array
# and its Memory. The barrier of the Array's `MemoryRef` field passes the
# fresh Array to a call before the element stores, and BasicAA separates the
# element stores from the Array's fields only while the Array is not
# captured, so the intrinsic declares its parent `nocapture`.
@noinline small_literal_vector(i) = (v = [Float64(i), 2.0, 3.0]; length(v) < 16 ? 1.0 : sum(v))
function fresh_barrier_leaves_alloc_opt_intact()
    ir = lowered_ir(small_literal_vector, (Int,))
    check("the small literal vector allocates nothing", !occursin("ijl_gc_", ir))
    check("the length test folds", !occursin("mapreduce_impl", ir))
    check("no region check survives on the folded vector", !occursin("region_wb", ir))
end

mutable struct Box
    v::Int
end
@noinline make_in_region(n) = (region_set(n); x = Box(7); region_set(0); x)

# An IdDict stores its key into its own table, a region-0 object.
function iddict_key_quarantines_region4()
    x = make_in_region(4)
    d = IdDict{Any,Int}()
    d[x] = 1
    check("an IdDict key from region 4 quarantines it", quarantined(4) == 1)
    check("the lookup still finds the key", d[x] == 1)
    check("the quarantined reset refuses", code(region_reset(4)) == EQUARANTINED)
end

# The serializer records every object it writes in a region-0 table.
function serialize_quarantines_region3()
    x = make_in_region(3)
    buf = IOBuffer()
    serialize(buf, x)
    check("serializing a region-3 object quarantines it", quarantined(3) == 1)
    check("serialize still produced bytes", position(buf) > 0)
    check("the quarantined reset refuses", code(region_reset(3)) == EQUARANTINED)
end

# A task made inside a window is a region object, and its schedule stores it
# into the scheduler's queues, region-0 objects. Tasks are made outside the
# window; a task opens its own window inside.
@noinline function spawn_in_region(n)
    region_set(n)
    t = Threads.@spawn 1 + 1
    r = fetch(t)
    region_set(0)
    return r
end

function spawn_quarantines_region2()
    r = spawn_in_region(2)
    check("the task spawned inside the window still computes", r == 2)
    check("a task spawned inside a window quarantines its region", quarantined(2) == 1)
    check("the quarantined reset refuses", code(region_reset(2)) == EQUARANTINED)
end

function quarantine_mask_is_as_expected()
    check("region 1 is not quarantined", quarantined(1) == 0)
    # Region 6 held the Dict rehash, which is no longer an escape.
    check("region 6 is not quarantined", quarantined(6) == 0)
    for n in (2, 3, 4, 5, 7, 8, 9, 10, 11)
        check("region $n is quarantined", quarantined(n) == 1)
    end
end

clean_window()
region0_child_into_region1_parent()
region1_child_into_region1_parent()
region1_child_into_region2_parent()
reset_clean_regions()
chain_escape_quarantines_region7()
dict_rehash_does_not_quarantine_region6()
ctor_gap_quarantines_region5()
ctor_barrier_is_region_only()
inline_field_copy_quarantines_region8()
fresh_box_quarantines_region9()
runtime_copy_quarantines_region10()
pointer_load_copy_quarantines_region11()
fresh_copy_barrier_is_region_only()
fresh_barrier_leaves_alloc_opt_intact()
iddict_key_quarantines_region4()
serialize_quarantines_region3()
spawn_quarantines_region2()
quarantine_mask_is_as_expected()

finish("regions_escape")
