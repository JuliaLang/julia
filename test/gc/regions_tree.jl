# This file is a part of Julia. License is MIT: https://julialang.org/license

# The region tree: a store is legal only toward the storing object's own
# region or an ancestor on its branch. The default is the chain 0 <- 1 <-
# 2 <- ...; a declared tree replaces it. A declaration itself is refused
# while any region is live anywhere, so the live-child bookkeeping a
# reset depends on stays exact. This script declares the tree once and
# then exercises it: first the clean trunk-and-leaves cases, then four
# escapes that each quarantine one region permanently.
include(joinpath(@__DIR__, "regions_api.jl"))

# ---- declarations and their refusals, before any region is used ----

@noinline function tree_bad_declarations()
    for n in (-1, MAX_REGIONS, 99)
        check("parent_of($n) is 0", parent_of(n) == 0)
        check("declare_parent($n, 0) refuses", declare_parent(n, 0) == EINVAL)
    end
    check("a parent equal to the child is rejected", declare_parent(3, 3) == EINVAL)
    check("a parent higher than the child is rejected", declare_parent(3, 5) == EINVAL)
end

@noinline function tree_open_close(n)
    region_set(n)
    region_set(0)
    nothing
end

# A declaration is refused while the child is live, and the live-child
# count is exact enough that the parent becomes declarable again once
# the child resets.
@noinline function tree_declare_while_live()
    check("declare 5 <- 4 before use", declare_parent(5, 4) == 0)
    tree_open_close(5)                      # 5 becomes live; 4 gains a live child
    check("declaring while a region is live refuses", declare_parent(5, 0) == ECHILD)
    check("the parent stays 4", parent_of(5) == 4)
    check("region 5 not quarantined", quarantined(5) == 0)
    check("reset 5", !refused(region_reset(5)))
    tree_open_close(4)
    check("region 4 not quarantined", quarantined(4) == 0)
    check("reset 4 after its child", !refused(region_reset(4)))
    check("declare after both resets succeeds", declare_parent(5, 0) == 0)
    check("the parent is 0 now", parent_of(5) == 0)
end

@noinline function tree_declare_while_window_open_probe()
    region_set(1)
    result = declare_parent(2, 1)
    region_set(0)
    return result
end

# A declaration is refused while a window is open anywhere, before the
# live-child check even runs.
@noinline function tree_declare_busy_while_open()
    check("a declaration while a window is open refuses",
          tree_declare_while_window_open_probe() == EBUSY)
    check("region 1 not quarantined", quarantined(1) == 0)
    check("reset 1", !refused(region_reset(1)))
end

# The final tree: trunk 1 with leaves 2, 3, 4; a separate branch 6 <- 5,
# 7 <- 6, then 6 is redeclared to hang off the root instead of 5 -- the
# redeclaration a later case below depends on.
@noinline function tree_declare_final()
    check("declare 2 <- 1", declare_parent(2, 1) == 0)
    check("declare 3 <- 1", declare_parent(3, 1) == 0)
    check("declare 4 <- 1", declare_parent(4, 1) == 0)
    check("declare 6 <- 5", declare_parent(6, 5) == 0)
    check("declare 7 <- 6", declare_parent(7, 6) == 0)
    check("redeclare 6 <- 0", declare_parent(6, 0) == 0)

    check("parent of 2 is 1", parent_of(2) == 1)
    check("parent of 3 is 1", parent_of(3) == 1)
    check("parent of 4 is 1", parent_of(4) == 1)
    check("parent of 5 is 0", parent_of(5) == 0)
    check("parent of 6 is 0", parent_of(6) == 0)
    check("parent of 7 is 6", parent_of(7) == 6)
    check("parent of 1 is 0", parent_of(1) == 0)
end

# ---- the trunk-and-leaves subtree: clean use ----

mutable struct TreeNode
    f::Any
end

const TREE_TRUNK = 1
tree_leaf_of(w) = 1 + w                 # w is the worker index (1..3), not a thread id

const tree_counter_lock = ReentrantLock()
const tree_counter = Ref(0)

@noinline function tree_build_trunk()
    region_set(TREE_TRUNK)
    t = TreeNode(777)
    region_set(0)
    return t
end

# The chain stays in this frame, which has returned before the reset runs:
# the reset checks the execution roots, and a local that still names one of
# the region's objects refuses it. The trunk arrives as a raw pointer (see
# tree_run_workers) and becomes an object reference only here, in the frame
# that stores the legal edge.
@noinline function tree_round!(leaf, trunk_ptr::Ptr{Cvoid})
    region_set(leaf)
    c = TreeNode(nothing)
    for _ in 1:500
        c = TreeNode(c)
    end
    region_set(0)
    c.f = unsafe_pointer_to_objref(trunk_ptr)   # leaf -> trunk: legal, an edge into an ancestor
    return nothing
end

@noinline function tree_worker(w, trunk_ptr::Ptr{Cvoid})
    leaf = tree_leaf_of(w)
    for round in 1:50
        tree_round!(leaf, trunk_ptr)
        lock(tree_counter_lock) do
            tree_counter[] += 1
        end
        local r = reset_via_call(leaf)
        check("worker $w round $round leaf reset (code $(code(r)))", !refused(r))
    end
end

# Three workers each churn their own leaf under the shared trunk, storing
# a leaf -> trunk edge every round, and increment a counter under a lock.
# A region is reset on the heap that allocated it, so the schedule is
# :static: a worker that waits for the lock stays on its thread. Runs at
# any thread count, including one.
#
# The workers get the trunk as a raw pointer, not as the object. The closure
# that Threads.@threads builds is a region-0 object, and it stores every
# captured variable into itself: a closure that captured the trunk would
# hold a region-1 reference in a region-0 object, an escape by the rule,
# and the barrier would quarantine the trunk. The raw pointer carries no
# reference; GC.@preserve keeps the trunk alive while the workers run.
#
# The trunk object is a local of this frame, and the frame has returned
# before the global reset runs: the global reset checks the execution roots
# like the per-heap reset, and a local that names a trunk object refuses it.
@noinline function tree_run_workers()
    trunk_obj = tree_build_trunk()
    GC.@preserve trunk_obj begin
        trunk_ptr = pointer_from_objref(trunk_obj)
        Threads.@threads :static for w in 1:3
            tree_worker(w, trunk_ptr)
        end
    end
    return trunk_obj.f
end

function tree_multithread_leaves()
    check("the trunk survived the workers", tree_run_workers() == 777)
    check("the shared counter reached every round", tree_counter[] == 3 * 50)
    for w in 1:3
        check("leaf $(tree_leaf_of(w)) not quarantined", quarantined(tree_leaf_of(w)) == 0)
    end
    check("the trunk not quarantined", quarantined(TREE_TRUNK) == 0)
    r = region_reset_global(TREE_TRUNK)
    check("the global trunk reset succeeds (code $(code(r)))", !refused(r))
end

@noinline function tree_fill_region(n, k)
    region_set(n)
    last = TreeNode(nothing)
    for _ in 1:k
        last = TreeNode(last)
    end
    region_set(0)
    return last
end

# A region with a live child must not reset -- the child may hold a
# legal reference into it, which the reset would dangle -- so the leaf
# resets first, and only then does the trunk's reset succeed. The second
# half churns a leaf many times while the trunk stays live underneath it.
function tree_reset_order()
    trunk_obj = tree_fill_region(TREE_TRUNK, 1000)
    leaf_obj = tree_fill_region(2, 1000)
    leaf_obj.f = trunk_obj                          # leaf -> trunk: legal
    check("resetting the trunk with a live leaf refuses",
          code(region_reset(TREE_TRUNK)) == ECHILD)
    check("the leaf resets", !refused(region_reset(2)))
    check("the trunk resets once its only child is gone", !refused(region_reset(TREE_TRUNK)))

    trunk_obj2 = tree_fill_region(TREE_TRUNK, 5000)
    trunk_obj2.f = 424242
    for i in 1:200
        leaf = tree_fill_region(2, 2000)
        leaf.f = trunk_obj2                         # leaf references the live trunk each round
        check("leaf churn round $i resets", !refused(region_reset(2)))
    end
    check("the trunk survives 200 leaf resets", trunk_obj2.f == 424242)
    check("the trunk resets last", !refused(region_reset(TREE_TRUNK)))
end

# ---- the global reset checks the execution roots ----
# A parked task keeps a trunk object on its frame. The per-heap checked
# reset refuses that with EROOT; the global reset must refuse it as well,
# whichever heap the task's objects landed on. The window is closed before
# the task parks, so the reset is not busy: only the root check can refuse.
@noinline function tree_park_with_trunk(parked, release)
    region_set(TREE_TRUNK)
    t = TreeNode(4242)
    region_set(0)
    check("the parked task's object is in the trunk", escape(t) == TREE_TRUNK)
    notify(parked)                       # park: the window is closed, t lives on this frame
    wait(release)
    return t.f
end

function tree_global_reset_root_check()
    parked, release = Base.Event(), Base.Event()
    tA = Threads.@spawn tree_park_with_trunk(parked, release)
    wait(parked)
    r = region_reset_global(TREE_TRUNK)
    check("the global reset refuses while a parked task roots a trunk object (code $(code(r)))",
          code(r) == EROOT)
    notify(release)
    check("the parked task reads its trunk object", fetch(tA) == 4242)
    r2 = region_reset_global(TREE_TRUNK)
    check("the global reset succeeds once the task is gone (code $(code(r2)))", !refused(r2))
    check("the trunk not quarantined by the root check", quarantined(TREE_TRUNK) == 0)
end

@noinline function tree_check_clean_before_escapes()
    for r in 1:4
        check("region $r not quarantined before the escape cases", quarantined(r) == 0)
    end
end

# ---- the escape barrier across the declared tree: four permanent
#      quarantines, one region each ----

@noinline tree_make(n) = (region_set(n); b = TreeNode(nothing); region_set(0); b)
@noinline tree_store!(p, c) = (p.f = c; nothing)

# A declared tree isolates siblings: a store from one leaf into another
# is an escape in both directions, even though both may reference their
# shared trunk. A store toward an ancestor is legal either way; a store
# toward a leaf that is not the object's own branch is not.
function tree_sibling_isolation()
    check("a forward parent is rejected", declare_parent(1, 2) == EINVAL)

    a = tree_make(2)
    b = tree_make(3)
    tree_store!(a, b)                   # a region-3 child into a region-2 parent: siblings under 1
    check("a sibling store quarantines the child's region", quarantined(3) == 1)
    check("the parent's own region is untouched", quarantined(2) == 0)

    trunk_obj = tree_make(TREE_TRUNK)
    leaf = tree_make(2)
    tree_store!(trunk_obj, leaf)        # a region-2 child into a region-1 parent: leaf into trunk
    check("a leaf-into-trunk store quarantines the leaf", quarantined(2) == 1)

    fresh_trunk = tree_make(TREE_TRUNK)
    fresh_leaf = tree_make(4)
    tree_store!(fresh_leaf, fresh_trunk) # a region-1 child into a region-4 parent: trunk into leaf
    check("a trunk-into-leaf store is legal", quarantined(4) == 0)
end

@noinline function tree_disciplined_growth()
    region_set(TREE_TRUNK)
    v = Vector{Any}()
    for i in 1:10_000
        push!(v, i)                     # every resize lands in the trunk
    end
    region_set(0)
    return v
end

@noinline function tree_hazard_growth(v)
    region_set(4)
    for i in 1:100_000
        push!(v, i)                     # the trunk vector's resize now lands in the leaf
    end
    region_set(0)
end

# A growable trunk structure must be grown with the trunk current: grown
# under a leaf's window instead, its new backing memory is a leaf object
# stored into the trunk vector -- a trunk (older) holding a leaf
# (younger) child, which the barrier quarantines. The trunk with a live
# quarantined child can never reset again; that refusal is the
# documented consequence, not a bug to route around.
function tree_lock_discipline()
    v = tree_disciplined_growth()
    check("growing the trunk vector in the trunk does not escape",
          quarantined(4) == 0 && quarantined(TREE_TRUNK) == 0)
    tree_hazard_growth(v)
    check("growing a trunk vector under a leaf window quarantines the leaf",
          quarantined(4) == 1)
    check("the trunk itself is not quarantined", quarantined(TREE_TRUNK) == 0)
    check("the trunk cannot reset with a live quarantined child",
          code(region_reset(TREE_TRUNK)) == ECHILD)
end

@noinline tree_make_ref(n) = (region_set(n); o = Ref{Any}(nothing); region_set(0); o)

# After 6 was redeclared to hang off the root instead of 5, region 7's
# ancestors are 6 and 0, not 5: a store of a region-5 object into a
# region-7 object is now an escape, even though both descend from the
# original 5 <- 4 <- ... branch by number alone.
function tree_redeclared_ancestor()
    a = tree_make_ref(5)
    holder = tree_make_ref(7)
    check("the object is in region 5", region_of(a) == 5)
    check("the holder is in region 7", region_of(holder) == 7)
    holder[] = a                        # region 5 into region 7: 5 is not an ancestor of 7 here
    check("the store quarantined region 5", quarantined(5) == 1)
    check("region 7 is not quarantined", quarantined(7) == 0)
end

@noinline function tree_final_quarantine_mask()
    for r in (2, 3, 4, 5)
        check("region $r is quarantined", quarantined(r) == 1)
    end
    for r in (1, 6, 7)
        check("region $r is not quarantined", quarantined(r) == 0)
    end
end

# ---- driver: declarations first, then the clean cases, then the escapes ----

tree_bad_declarations()
tree_declare_while_live()
tree_declare_busy_while_open()
tree_declare_final()

tree_multithread_leaves()
tree_reset_order()
tree_global_reset_root_check()
tree_check_clean_before_escapes()

tree_sibling_isolation()
tree_lock_discipline()
tree_redeclared_ancestor()

tree_final_quarantine_mask()

finish("regions_tree")
