# This file is a part of Julia. License is MIT: https://julialang.org/license

include(joinpath(@__DIR__, "regions_api.jl"))

# The region count. Region 0 is the stock heap; regions 1 to MAX_REGIONS - 1
# are the regions a program can open. The masks of the tree and of the
# quarantine are 64-bit words, one bit per region, so the top region tests
# their top bit and region 32 tests the bit a 32-bit mask does not have.
# The quarantines come last: a quarantined region stays live for the life
# of the process, and a live region refuses a tree declaration.

const TOP = MAX_REGIONS - 1

mutable struct ManyNode
    f::Any
end

@noinline function many_fill(n, k)
    region_set(n)
    last = ManyNode(nothing)
    for _ in 1:k
        last = ManyNode(last)
    end
    region_set(0)
    return last
end

@noinline many_make(n) = (region_set(n); b = ManyNode(nothing); region_set(0); b)
@noinline many_store!(p, c) = (p.f = c; nothing)

# ---- every region opens, and the one past the last refuses ----

@noinline function many_open_all()
    for n in 1:TOP
        obj = many_fill(n, 200)
        check("region $n holds the object", region_of(obj) == n)
        check("region $n holds pages", region_pages(n) > 0)
    end
end

function every_region_opens()
    check("set($MAX_REGIONS) refuses with EINVAL", region_set(MAX_REGIONS) == EINVAL)
    check("set($MAX_REGIONS) leaves region 0 current", region_current() == 0)
    check("quarantined($MAX_REGIONS) is 0", quarantined(MAX_REGIONS) == 0)
    many_open_all()
    check("region $TOP is not quarantined", quarantined(TOP) == 0)
end

# ---- the default chain reaches the top region ----

# In the chain 0 <- 1 <- ... <- TOP, the parent of TOP is TOP - 1. A store
# that points toward the root is legal: an object of TOP that references an
# object of TOP - 1, or one of its own region. Both read the top word of the
# uptree table, and the second reads its top bit.
@noinline function many_legal_stores_at_the_top()
    p = many_make(TOP)
    c = many_make(TOP - 1)
    many_store!(p, c)
    check("a store toward the root from the top region is legal", quarantined(TOP) == 0)
    check("the parent region of the store is untouched", quarantined(TOP - 1) == 0)
    q = many_make(TOP)
    r = many_make(TOP)
    many_store!(q, r)
    check("a store inside the top region is legal", quarantined(TOP) == 0)
    return nothing
end

function the_chain_reaches_the_top()
    check("the parent of $TOP is $(TOP - 1)", parent_of(TOP) == TOP - 1)
    check("the parent of 1 is 0", parent_of(1) == 0)
    many_legal_stores_at_the_top()
    check("the reset of $(TOP - 1) refuses while $TOP is live (ECHILD)",
          code(reset_via_call(TOP - 1)) == ECHILD)
    check("the reset of 1 refuses while 2 is live (ECHILD)", code(reset_via_call(1)) == ECHILD)
    for n in TOP:-1:1
        check("region $n resets once its child is gone", !refused(reset_via_call(n)))
        check("region $n holds no pages after its reset", region_pages(n) == 0)
    end
end

# ---- a declared tree with 62 leaves under one trunk ----

const MANY_TRUNK = 1

# The fills and the stores run in their own frame: the frame that resets the
# leaves must hold no leaf object, or the root scan refuses the last leaf.
@noinline function many_fill_leaves()
    trunk_obj = many_fill(MANY_TRUNK, 200)
    for leaf in 2:TOP
        leaf_obj = many_fill(leaf, 200)
        many_store!(leaf_obj, trunk_obj)           # leaf -> trunk: legal
    end
    return nothing
end

@noinline function many_leaves_round()
    many_fill_leaves()
    check("no leaf quarantined after the stores into the trunk",
          all(quarantined(leaf) == 0 for leaf in 2:TOP))
    check("the trunk refuses while its leaves are live (ECHILD)",
          code(reset_via_call(MANY_TRUNK)) == ECHILD)
    for leaf in 2:TOP
        check("leaf $leaf resets while the other leaves are live", !refused(reset_via_call(leaf)))
    end
    return nothing
end

function a_tree_with_many_leaves()
    for leaf in 2:TOP
        check("declare $leaf <- $MANY_TRUNK", declare_parent(leaf, MANY_TRUNK) == 0)
    end
    check("every leaf has the trunk as its parent", all(parent_of(leaf) == MANY_TRUNK for leaf in 2:TOP))
    many_leaves_round()
    check("the trunk resets once every leaf is gone", !refused(reset_via_call(MANY_TRUNK)))
end

# ---- the quarantine mask has a bit for every region ----

const HOLD = Any[]

@noinline function many_escape(n)
    obj = many_make(n)
    push!(HOLD, obj)                               # a region object into a region-0 vector
    return nothing
end

function the_mask_has_every_bit()
    many_escape(32)
    check("an escape from region 32 quarantines region 32", quarantined(32) == 1)
    check("the escape from 32 leaves region 0 alone", quarantined(0) == 0)
    check("the escape from 32 leaves region 31 alone", quarantined(31) == 0)
    check("the escape from 32 leaves region 33 alone", quarantined(33) == 0)
    many_escape(TOP)
    check("an escape from region $TOP quarantines region $TOP", quarantined(TOP) == 1)
    check("the escape from $TOP leaves region $(TOP - 1) alone", quarantined(TOP - 1) == 0)
    check("the escape from $TOP leaves region 1 alone", quarantined(1) == 0)
    check("region 32 stays quarantined", quarantined(32) == 1)
    check("the reset of $TOP refuses (EQUARANTINED)", code(reset_via_call(TOP)) == EQUARANTINED)
    check("a window on $TOP refuses (EQUARANTINED)", region_set(TOP) == EQUARANTINED)
    check("the objects still read", length(HOLD) == 2 && all(h isa ManyNode for h in HOLD))
end

every_region_opens()
the_chain_reaches_the_top()
a_tree_with_many_leaves()
the_mask_has_every_bit()

finish("regions_many")
