# This file is a part of Julia. License is MIT: https://julialang.org/license

include(joinpath(@__DIR__, "regions_api.jl"))

# The stores that the escape barrier did not see. Each one puts a region
# object into a region-0 parent through a path the write barrier of the
# stock collector skips, because the parent is fresh or because the copy
# moves many references at once. Each case burns its own region, from 7
# down, and the mask check at the end fixes the outcome of every case.
#
# Two groups of cases are the opposite. An argument vector of a splat call
# is a temporary of the call, and a check there would quarantine a region
# for ordinary correct code. It must stay unchecked. And a bulk copy out of
# a young container that holds only old elements moves no region reference
# anywhere: the copy barrier must look at the elements, not at the pair of
# containers.

# The default tree is the chain 0 <- 1 <- 2 <- ..., so a region that stays
# live blocks the reset of every region below it. The two cases that reset
# run first and share the lowest region, one after the other; the cases that
# burn a region run after, from 7 down.
const SPLAT = 1
const BULK = 1
const SLICER = 2
const TWINR = 3
const SVECR = 4
const COPYR = 5
const COPYTO = 6
const TASKR = 7

# A callable object made inside a window is a region object. The task is
# made outside the window, as the discipline demands, so the task is a
# region-0 object and its `start` field holds the region object.
#
# The callable is a mutable struct, not a closure. A closure with captured
# fields is immutable, so every pass as `Any` boxes it again and the box,
# not the closure, is what a region holds.
mutable struct Callable
    x::Int
end
(c::Callable)() = c.x + 1

@noinline function make_callable(n)
    region_set(n)
    c = Callable(41)
    region_set(0)
    escape(c)
    return c
end

function task_start_quarantines()
    c = make_callable(TASKR)
    check("the callable lives in its region", region_of(c) == TASKR)
    check("nothing escaped it yet", quarantined(TASKR) == 0)
    t = Task(c)
    escape(t)
    check("the task holds the region object", region_of(t) == 0)
    check("a region callable in a region-0 task quarantines its region", quarantined(TASKR) == 1)
    check("the callable still runs", c() == 42)
end

# A vector built inside a window holds region objects, legally: the parent
# and the children share the region. A bulk copy into a region-0 vector
# moves those references out, and the copy barrier must see it.
@noinline function make_source(n, len)
    region_set(n)
    s = Any[Ref(i) for i in 1:len]
    region_set(0)
    escape(s)
    return s
end

function copyto_quarantines()
    src = make_source(COPYTO, 16)
    sink = Vector{Any}(undef, 16)
    copyto!(sink, src)
    escape(sink)
    check("a bulk copy of region elements into a region-0 vector quarantines",
          quarantined(COPYTO) == 1)
    check("the copy is correct", sink[16][] == 16)
end

# `copy` allocates the new memory in the region that is current, which is 0
# here, and moves the element references into it with no barrier call at
# all.
function copy_quarantines()
    src = make_source(COPYR, 16)
    dup = copy(src)
    escape(dup)
    check("a copy of a region vector into region 0 quarantines", quarantined(COPYR) == 1)
    check("the copy is correct", dup[16][] == 16)
end

# The argument vector of a splat call lives for the call and dies with it.
# It never outlives the region, so it is not an escape, and the barrier must
# not fire on it. This case guards that exemption.
@noinline count_args(args...) = length(args)

function splat_does_not_quarantine()
    v = make_source(SPLAT, 3)
    check("a splat call passes region objects", count_args(v...) == 3)
    check("a splat call does not quarantine", quarantined(SPLAT) == 0)
    check("the region of the arguments still resets", !refused(reset_via_call(SPLAT)))
end

# A bulk copy checks the pair (destination, source container) first: when
# the pair passes, no element can escape. When the pair fails, the elements
# decide. A young vector that a window filled with old objects fails the
# pair, and a copy of it into an old vector is legal: every reference that
# moves points into region 0. The five runtime paths that copy many
# references at once get the same source, and none of them may quarantine.
#
# Each path runs in a frame of its own, so that the young source is not on
# any frame when the region resets. The old objects are made before any
# window opens.
const OLD = [Ref(i) for i in 1:16]

struct Twin
    a::Base.RefValue{Int}
    b::Base.RefValue{Int}
end

mutable struct TwinHolder
    t::Twin
end
const HOLDER = TwinHolder(Twin(OLD[1], OLD[1]))

@noinline function make_young_of_old(n)
    region_set(n)
    y = Any[OLD[i] for i in 1:2:16]
    region_set(0)
    escape(y)
    return y
end

# A Vector{Twin} stores its elements inline, two references each. The
# vector is young; the references point to old objects.
@noinline function make_young_twins_of_old(n)
    region_set(n)
    t = [Twin(OLD[i], OLD[i + 1]) for i in 1:2:16]
    region_set(0)
    escape(t)
    return t
end

# A Twin boxed inside a window is a region object with two old fields.
@noinline function make_young_twin_box(n)
    region_set(n)
    box = Any[Twin(OLD[3], OLD[4])]
    region_set(0)
    escape(box)
    return box[1]
end

@noinline function make_young_svec_of_old(n)
    region_set(n)
    s = Core.svec(OLD...)
    region_set(0)
    escape(s)
    return s
end

# jl_genericmemory_copyto, boxed elements: append! and copy of a Vector{Any}.
@noinline function append_young_of_old(n)
    y = make_young_of_old(n)
    sink = Any[]
    append!(sink, y)
    dup = copy(y)
    escape(sink)
    escape(dup)
    return length(sink) == 8 && sink[8] === OLD[15] && region_of(dup) == 0 && dup[1] === OLD[1]
end

# jl_genericmemory_copyto, inline elements with references: copyto! of a
# Vector{Twin}.
@noinline function copyto_young_twins_of_old(n)
    t = make_young_twins_of_old(n)
    sink = Vector{Twin}(undef, 8)
    copyto!(sink, t)
    escape(sink)
    return sink[8].b === OLD[16] && sink[1].a === OLD[1]
end

# jl_genericmemory_copy_slice, both element layouts: the C copy of an array.
@noinline function slice_young_of_old(n)
    y = make_young_of_old(n)
    t = make_young_twins_of_old(n)
    dupy = ccall(:jl_array_copy, Any, (Any,), y)::Vector{Any}
    dupt = ccall(:jl_array_copy, Any, (Any,), t)::Vector{Twin}
    escape(dupy)
    escape(dupt)
    return region_of(dupy) == 0 && dupy[8] === OLD[15] && region_of(dupt) == 0 && dupt[8].b === OLD[16]
end

# jl_svec_copy: the copy of a SimpleVector.
@noinline function copy_young_svec_of_old(n)
    s = make_young_svec_of_old(n)
    dup = ccall(:jl_svec_copy, Any, (Any,), s)::Core.SimpleVector
    escape(dup)
    return region_of(dup) == 0 && length(dup) == 16 && dup[16] === OLD[16]
end

# jl_gc_multi_wb: the runtime store of an inline immutable with references
# into a field, through the untyped setfield! of the runtime.
@noinline function set_young_twin_of_old(n)
    b = make_young_twin_box(n)
    check("the boxed Twin is a region object", region_of(b) == n)
    ccall(:jl_set_nth_field, Cvoid, (Any, Csize_t, Any), HOLDER, 0, b)
    return HOLDER.t.a === OLD[3] && HOLDER.t.b === OLD[4]
end

function bulk_copies_of_old_do_not_quarantine()
    check("append! and copy of a young vector of old elements are right", append_young_of_old(BULK))
    check("append! and copy of a young vector of old elements do not quarantine", quarantined(BULK) == 0)
    check("copyto! of young inline twins of old elements is right", copyto_young_twins_of_old(BULK))
    check("copyto! of young inline twins of old elements does not quarantine", quarantined(BULK) == 0)
    check("the C copy of young arrays of old elements is right", slice_young_of_old(BULK))
    check("the C copy of young arrays of old elements does not quarantine", quarantined(BULK) == 0)
    check("the copy of a young svec of old elements is right", copy_young_svec_of_old(BULK))
    check("the copy of a young svec of old elements does not quarantine", quarantined(BULK) == 0)
    check("the runtime store of a young boxed Twin of old fields is right", set_young_twin_of_old(BULK))
    check("the runtime store of a young boxed Twin of old fields does not quarantine", quarantined(BULK) == 0)
    check("the region of the young sources resets", !refused(reset_via_call(BULK)))
    check("the copies read after the reset",
          HOLDER.t.a[] == 3 && HOLDER.t.b[] == 4 && OLD[16][] == 16)
end

# The element check is not blind: a young Twin of young fields, and a young
# svec of young elements, still quarantine when a bulk copy moves them out.
@noinline function make_young_twins(n)
    region_set(n)
    t = [Twin(Ref(i), Ref(i + 1)) for i in 1:2:16]
    region_set(0)
    escape(t)
    return t
end

function copyto_of_twins_quarantines()
    t = make_young_twins(TWINR)
    sink = Vector{Twin}(undef, 8)
    copyto!(sink, t)
    escape(sink)
    check("a bulk copy of inline twins of region fields into a region-0 vector quarantines",
          quarantined(TWINR) == 1)
    check("the copy is correct", sink[8].b[] == 16)
end

@noinline function make_young_svec(n)
    region_set(n)
    s = Core.svec(Ref(1), Ref(2))
    region_set(0)
    escape(s)
    return s
end

function svec_copy_quarantines()
    s = make_young_svec(SVECR)
    dup = ccall(:jl_svec_copy, Any, (Any,), s)::Core.SimpleVector
    escape(dup)
    check("a copy of a region svec into region 0 quarantines", quarantined(SVECR) == 1)
    check("the copy is correct", dup[2][] == 2)
end

# The C copy of an array of boxed elements: the layout of a boxed memory
# lists no pointer, so a check that follows the layout misses it.
function slice_copy_quarantines()
    src = make_source(SLICER, 16)
    dup = ccall(:jl_array_copy, Any, (Any,), src)::Vector{Any}
    escape(dup)
    check("the C copy of a region vector into region 0 quarantines", quarantined(SLICER) == 1)
    check("the copy is correct", region_of(dup) == 0 && dup[16][] == 16)
end

splat_does_not_quarantine()
bulk_copies_of_old_do_not_quarantine()
task_start_quarantines()
copyto_quarantines()
copy_quarantines()
svec_copy_quarantines()
copyto_of_twins_quarantines()
slice_copy_quarantines()

check("exactly the six stores quarantined",
      quarantined(TASKR) == 1 && quarantined(COPYTO) == 1 && quarantined(COPYR) == 1 &&
      quarantined(SVECR) == 1 && quarantined(TWINR) == 1 && quarantined(SLICER) == 1 &&
      quarantined(SPLAT) == 0)

finish("regions_stores")
