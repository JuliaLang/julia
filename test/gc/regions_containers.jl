# This file is a part of Julia. License is MIT: https://julialang.org/license

include(joinpath(@__DIR__, "regions_api.jl"))

# A container that grows inside a window keeps its buffer where the
# container lives. The new buffer replaces the old one, so it takes the
# region of the old buffer, not the region of the open window. Without the
# rule, every one of these cases quarantines the window's region for an
# operation the program has every right to make.
#
# The rule covers the buffer. The elements stay the program's own data and
# keep the reference rule: the last case stores a region object into a
# region-0 vector, and that still quarantines.

const GROW = 1
const ELEM = 2
const RING = 3
const RINGBAD = 4

const LONG = Int[]
const LONGANY = Any[]

@noinline function push_inside_window(n, count)
    region_set(n)
    for i in 1:count
        push!(LONG, i)
    end
    region_set(0)
end

function a_vector_grows_inside_a_window()
    push_inside_window(GROW, 10_000)
    check("push! of a bits element inside a window does not quarantine", quarantined(GROW) == 0)
    check("the vector is right", length(LONG) == 10_000 && LONG[end] == 10_000)
    check("the region resets", !refused(reset_via_call(GROW)))
    check("the vector is right after the reset", LONG[end] == 10_000 && sum(LONG) == 50_005_000)
    # 10_000 Ints are 80 KB: the data is malloc'd and tracked where the borrow
    # put its header, so the sum above proves the reset did not free it. The
    # page chains of the window's region must be whole after the reset too.
    check("region verify finds no inconsistency after the reset", region_verify(GROW) == 0)
end

@noinline function grow_the_front_inside_window(n, count)
    region_set(n)
    for i in 1:count
        pushfirst!(LONG, -i)
    end
    resize!(LONG, length(LONG) + 100)
    region_set(0)
end

function a_vector_grows_at_both_ends()
    grow_the_front_inside_window(GROW, 2_000)
    check("pushfirst! and resize! inside a window do not quarantine", quarantined(GROW) == 0)
    check("the front of the vector is right", LONG[1] == -2_000)
    check("the region resets", !refused(reset_via_call(GROW)))
end

const D = Dict{Int,Int}()

@noinline function grow_dict_inside_window(n, count)
    region_set(n)
    for i in 1:count
        D[i] = i
    end
    region_set(0)
end

function a_dict_rehashes_inside_a_window()
    grow_dict_inside_window(GROW, 10_000)
    check("a Dict rehash inside a window does not quarantine", quarantined(GROW) == 0)
    check("the Dict is right", length(D) == 10_000 && all(D[k] == k for k in 1:10_000))
    check("the region resets", !refused(reset_via_call(GROW)))
    check("the Dict is right after the reset", D[10_000] == 10_000)
end

const ID = IdDict{Any,Any}()
const KEYS = Any[]
const VALS = Any[]

# The keys and the values are made before the window opens. An IdDict holds
# both as `Any`, so a value made inside the window would be boxed there and
# the box would be a region object in a region-0 table: an element escape,
# not a buffer one. This case is about the table the rehash replaces.
@noinline function grow_iddict_inside_window(n, count)
    region_set(n)
    for i in 1:count
        ID[KEYS[i]] = VALS[i]
    end
    region_set(0)
end

function an_iddict_rehashes_inside_a_window()
    for i in 1:4_000
        push!(KEYS, Ref(i))
        push!(VALS, Ref(-i))
    end
    grow_iddict_inside_window(GROW, 4_000)
    check("an IdDict rehash inside a window does not quarantine", quarantined(GROW) == 0)
    check("the IdDict is right", length(ID) == 4_000 && ID[KEYS[4_000]][] == -4_000)
    check("the region resets", !refused(reset_via_call(GROW)))
    check("the IdDict is right after the reset", ID[KEYS[1]][] == -1)
end

const BUF = IOBuffer()

@noinline function grow_iobuffer_inside_window(n, count)
    region_set(n)
    for i in 1:count
        write(BUF, UInt8(i & 0xff))
    end
    region_set(0)
end

function an_iobuffer_grows_inside_a_window()
    grow_iobuffer_inside_window(GROW, 100_000)
    check("an IOBuffer growth inside a window does not quarantine", quarantined(GROW) == 0)
    check("the buffer is right", position(BUF) == 100_000)
    check("the region resets", !refused(reset_via_call(GROW)))
    check("the buffer reads after the reset", length(take!(BUF)) == 100_000)
end

# `take!` hands the data out and marks the buffer for a reinit: the next
# write, or a truncate, makes new data. The new data replaces the data the
# buffer held, so it takes the region of the buffer, not the region of the
# open window. A cached buffer that is emptied with `take!` and written to
# inside a window is the everyday shape of this case.
const TAKEN = IOBuffer()

@noinline function write_after_take_inside_window(n)
    region_set(n)
    write(TAKEN, "after the take")
    region_set(0)
end

function an_iobuffer_reinits_inside_a_window()
    write(TAKEN, "before the take")
    take!(TAKEN)
    write_after_take_inside_window(GROW)
    check("a write after take! inside a window does not quarantine", quarantined(GROW) == 0)
    check("the new data of the buffer is in region 0", region_of(TAKEN.data) == 0)
    check("the buffer reads", String(take!(TAKEN)) == "after the take")
    check("the region resets", !refused(reset_via_call(GROW)))
end

@noinline function truncate_after_take_inside_window(n)
    region_set(n)
    truncate(TAKEN, 16)
    region_set(0)
end

function an_iobuffer_truncates_inside_a_window()
    write(TAKEN, "before the take")
    take!(TAKEN)
    truncate_after_take_inside_window(GROW)
    check("a truncate after take! inside a window does not quarantine", quarantined(GROW) == 0)
    check("the truncated data of the buffer is in region 0", region_of(TAKEN.data) == 0)
    check("the buffer holds the zeros", take!(TAKEN) == zeros(UInt8, 16))
    check("the region resets", !refused(reset_via_call(GROW)))
end

# `empty!` gives an IdDict a fresh table; the table replaces the one the
# dictionary held and takes the dictionary's region.
@noinline function empty_iddict_inside_window(n)
    region_set(n)
    empty!(ID)
    region_set(0)
end

function an_iddict_empties_inside_a_window()
    empty_iddict_inside_window(GROW)
    check("empty! of an IdDict inside a window does not quarantine", quarantined(GROW) == 0)
    check("the fresh table of the IdDict is in region 0", region_of(ID.ht) == 0)
    check("the IdDict is empty", isempty(ID))
    ID[KEYS[1]] = VALS[1]
    check("the IdDict takes a key after the empty!", ID[KEYS[1]][] == -1)
    check("the region resets", !refused(reset_via_call(GROW)))
end

# An IdSet grows two buffers: the key list (jl_idset_put_key) and the index
# table (jl_idset_put_idx, a smallintset rehash). Both replace the ones the
# set held and take the set's region. The keys are made before the window
# opens, as for the IdDict above.
const IDS = Base.IdSet{Any}()

@noinline function grow_idset_inside_window(n, count)
    region_set(n)
    for i in 1:count
        push!(IDS, KEYS[i])
    end
    region_set(0)
end

function an_idset_grows_inside_a_window()
    grow_idset_inside_window(GROW, 4_000)
    check("an IdSet growth inside a window does not quarantine", quarantined(GROW) == 0)
    check("the key list of the IdSet is in region 0", region_of(IDS.list) == 0)
    check("the index table of the IdSet is in region 0", region_of(IDS.idxs) == 0)
    check("the IdSet is right", length(IDS) == 4_000 && KEYS[4_000] in IDS && !(VALS[1] in IDS))
    check("the region resets", !refused(reset_via_call(GROW)))
    check("the IdSet is right after the reset", KEYS[1] in IDS)
end

# The runtime grows its own region-0 vectors through jl_array_grow_end, the
# C growth path, while a task holds a window (a backedge list, a method
# table's entries). The grown buffer takes the region of the array.
const RUNTIME_GROWN = Any[]

@noinline function c_push_inside_window(n, count)
    region_set(n)
    for i in 1:count
        ccall(:jl_array_ptr_1d_push, Cvoid, (Any, Any), RUNTIME_GROWN, KEYS[i])
    end
    region_set(0)
end

function a_c_growth_inside_a_window()
    c_push_inside_window(GROW, 4_000)
    check("jl_array_grow_end inside a window does not quarantine", quarantined(GROW) == 0)
    check("the buffer grown by C is in region 0", region_of(RUNTIME_GROWN.ref.mem) == 0)
    check("the vector is right", length(RUNTIME_GROWN) == 4_000 && RUNTIME_GROWN[4_000] === KEYS[4_000])
    check("the region resets", !refused(reset_via_call(GROW)))
end

# The buffer follows its container; an element does not. A region object
# stored into a region-0 vector outlives its region, and the barrier is
# right to quarantine it.
@noinline function push_a_region_element(n)
    region_set(n)
    r = Ref(1)
    region_set(0)
    push!(LONGANY, r)
    return nothing
end

function an_element_still_quarantines()
    push_a_region_element(ELEM)
    check("a region element in a region-0 vector still quarantines", quarantined(ELEM) == 1)
    check("the element still reads", LONGANY[1][] == 1)
end


# A container of the program's own. Base carries the rule for its
# containers; a package carries it for its own, with the borrow that
# contrib/memory-regions/regions.jl exports as `@in_region_of`. The two
# cases below are the same growth with the borrow and without it.
mutable struct Ring
    data::Memory{Float64}
end
const RING_GOOD = Ring(Memory{Float64}(undef, 4))
const RING_BAD  = Ring(Memory{Float64}(undef, 4))

@noinline function grow_ring!(r::Ring)
    new = Memory{Float64}(undef, 2 * length(r.data))
    copyto!(new, 1, r.data, 1, length(r.data))
    new[length(r.data) + 1] = 1.0
    r.data = new
    return nothing
end

@noinline function grow_ring_borrowed!(r::Ring)
    @in_region_of r begin
        new = Memory{Float64}(undef, 2 * length(r.data))
        copyto!(new, 1, r.data, 1, length(r.data))
        new[length(r.data) + 1] = 1.0
        r.data = new
    end
    return nothing
end

@noinline function grow_ring_inside_window(n, r, borrowed)
    region_set(n)
    borrowed ? grow_ring_borrowed!(r) : grow_ring!(r)
    region_set(0)
    return nothing
end

function a_program_container_grows_inside_a_window()
    grow_ring_inside_window(RING, RING_GOOD, true)
    check("a borrowed replacement does not quarantine", quarantined(RING) == 0)
    check("the borrowed buffer is in the region of its container",
          region_of(RING_GOOD.data) == region_of(RING_GOOD))
    check("the borrowed buffer reads", length(RING_GOOD.data) == 8 && RING_GOOD.data[5] == 1.0)
    check("the window's region resets", !refused(reset_via_call(RING)))
    check("the buffer still reads after the reset", RING_GOOD.data[5] == 1.0)
end

# The same growth without the borrow: the new buffer lands in the window's
# region, an older container takes it, and the barrier is right to
# quarantine. This is the case the macro exists for.
function a_program_container_without_the_borrow_quarantines()
    grow_ring_inside_window(RINGBAD, RING_BAD, false)
    check("an unborrowed replacement quarantines the window's region",
          quarantined(RINGBAD) == 1)
    check("the unborrowed buffer is in the window's region",
          region_of(RING_BAD.data) == RINGBAD)
end

a_vector_grows_inside_a_window()
a_vector_grows_at_both_ends()
a_dict_rehashes_inside_a_window()
an_iddict_rehashes_inside_a_window()
an_iobuffer_grows_inside_a_window()
an_iobuffer_reinits_inside_a_window()
an_iobuffer_truncates_inside_a_window()
an_iddict_empties_inside_a_window()
an_idset_grows_inside_a_window()
a_c_growth_inside_a_window()
an_element_still_quarantines()
a_program_container_grows_inside_a_window()
a_program_container_without_the_borrow_quarantines()

finish("regions_containers")
