# Demonstrator C: optimistic concurrent persistent BST, regions vs the stock
# collector, on the SAME code. N threads insert keys into ONE shared persistent
# tree. Each insert reads the shared root, PATH-COPIES a new version (O(depth)
# new nodes, sharing the untouched subtrees) in its own sibling leaf, then
# commits by a compare-and-swap on the root. Under contention most attempts
# lose and DISCARD their path -- pure allocation garbage. The region resets a
# lost attempt in O(1); the stock collector must trace and free it. This is the
# allocation-bound shape, so the win should show on WALL time, not just pauses.
#
# The design that keeps loser garbage in the leaf: build the path in the leaf,
# then VALIDATE (re-read the root) before copying the winning spine to region
# 0. A thread that lost the race fails the validation and resets its leaf
# WITHOUT ever touching region 0 -- so a lost attempt makes zero region-0
# garbage. Only a would-be winner copies its O(depth) spine into region 0 and
# CAS-commits.
#   run with: julia -t4 optimistic_bst.jl
include(joinpath(@__DIR__, "demo_common.jl"))
using .DemoCommon
using Printf

@noinline region_set(n)        = ccall(:jl_gc_region_set, Cint, (Cint,), n)
@noinline unsafe_region_reset(n)      = UInt64(ccall(:jl_gc_region_unsafe_reset, UInt64, (Cint,), n))
@noinline region_parent!(c, p) = ccall(:jl_gc_region_declare_parent, Cint, (Cint, Cint), c, p)
@noinline region_of(x)         = Int(ccall(:jl_gc_region_of, Cint, (Any,), x))
@noinline quarantined(n)       = Int(ccall(:jl_gc_region_quarantined, Cint, (Cint,), n)) != 0

mutable struct Node
    key::Int
    left::Union{Node,Nothing}
    right::Union{Node,Nothing}
end
mutable struct Shared
    @atomic root::Union{Node,Nothing}
end

# Persistent insert: returns a new tree with `key` added, path-copying the
# spine from the root to the insertion point and SHARING the untouched
# subtrees. The new spine nodes are allocated in whatever region is current.
function pinsert(node::Union{Node,Nothing}, key::Int)
    node === nothing && return Node(key, nothing, nothing)
    if key < node.key
        return Node(node.key, pinsert(node.left, key), node.right)
    elseif key > node.key
        return Node(node.key, node.left, pinsert(node.right, key))
    else
        return node                       # already present: no new spine
    end
end

# Copy only the NEW spine (nodes NOT in region 0) into region 0, keeping the
# shared region-0 subtrees. Called with region 0 current, only when about to
# commit -- so a lost attempt never allocates here.
function copy_spine_r0(node::Union{Node,Nothing})
    node === nothing && return nothing
    region_of(node) == 0 && return node   # a shared subtree: keep it
    Node(node.key, copy_spine_r0(node.left), copy_spine_r0(node.right))
end

# Insert `key` into the shared tree with optimistic retry. `leaf == 0` is stock
# mode (speculative garbage falls to the collector); otherwise each attempt is
# built in `leaf` and a lost attempt resets it. `work` is the speculative
# transaction body -- throwaway objects a real transaction allocates while it
# reads and computes, all discarded on abort. Returns the number of retries.
function commit_insert!(sh::Shared, key::Int, leaf::Int, work::Int)
    retries = 0
    while true
        old = @atomic sh.root
        leaf != 0 && region_set(leaf)
        # The speculative transaction body: `work` throwaway nodes, kept from
        # elision by a running sum. This is the allocation a lost attempt
        # discards -- leaf garbage in region mode, collector garbage in stock.
        if work > 0
            junk = Vector{Node}(undef, work)
            @inbounds for w in 1:work; junk[w] = Node(w, nothing, nothing); end
            s = 0; @inbounds for w in 1:work; s += junk[w].key; end
            key += (s & 0)                       # data-depend on junk, no effect
        end
        cand = pinsert(old, key)                 # the path-copy, in the leaf
        leaf != 0 && region_set(0)
        if (@atomic sh.root) !== old             # validate: someone committed first
            leaf != 0 && unsafe_region_reset(leaf)      # discard the attempt, leaf-only garbage
            retries += 1
            continue
        end
        newroot = leaf != 0 ? copy_spine_r0(cand) : cand   # winner copies its spine to r0
        repl = @atomicreplace sh.root old => newroot
        leaf != 0 && unsafe_region_reset(leaf)
        repl.success && return retries
        retries += 1                              # rare CAS race: retry
    end
end

leaf_of(tid) = tid
inorder(node, out) = (node === nothing && return; inorder(node.left, out); push!(out, node.key); inorder(node.right, out))

function drive(keys, tree::Bool, work::Int)
    sh = Shared(nothing)
    nextk = Threads.Atomic{Int}(1)
    retries = Threads.Atomic{Int}(0)
    n = length(keys)
    Threads.@threads for _ in 1:Threads.nthreads()
        leaf = tree ? leaf_of(Threads.threadid()) : 0
        local r = 0
        while true
            i = Threads.atomic_add!(nextk, 1)
            i > n && break
            r += commit_insert!(sh, keys[i], leaf, work)
        end
        Threads.atomic_add!(retries, r)
    end
    tree && region_reset_global_safe()
    out = Int[]; inorder((@atomic sh.root), out)
    (out, retries[], n)
end

# The tree lives in region 0 here, so there is nothing to globally reset; the
# helper is a no-op kept for symmetry with the trunk demonstrators.
region_reset_global_safe() = nothing

# Declare one sibling leaf per thread (children of region 0, mutually isolated).
for t in 1:Threads.nthreads()
    region_parent!(leaf_of(t), 0)
end

function run_scale(label, nkeys, work)
    keys = shuffle_keys(nkeys)
    r_out = Ref{Vector{Int}}(); s_out = Ref{Vector{Int}}(); r_ab = Ref(0); s_ab = Ref(0)
    reg, sto, _ = DemoCommon.ab(
        () -> (o = drive(keys, true,  work); r_out[] = o[1]; r_ab[] = o[2]; sum(o[1])),
        () -> (o = drive(keys, false, work); s_out[] = o[1]; s_ab[] = o[2]; sum(o[1])); reps = 3)
    report_table("C", "$label  ($nkeys keys, work=$work, $(Threads.nthreads()) threads)", reg, sto)
    anyq = any(quarantined(leaf_of(t)) for t in 1:Threads.nthreads())
    ok = sort(r_out[]) == sort(s_out[]) && length(unique(r_out[])) == length(r_out[])
    @printf("  region aborts %d   stock aborts %d   abort rate ~%.0f%%\n",
            r_ab[], s_ab[], 100 * r_ab[] / max(1, nkeys))
    println("  same key set: ", ok, "   quarantine: ", anyq)
    ok && !anyq
end

# Deterministic key shuffle so both runs insert the same set.
function shuffle_keys(n)
    ks = collect(1:n); st = UInt64(0x243F6A8885A308D3)
    for i in n:-1:2
        st = st * 6364136223846793005 + 1442695040888963407
        j = Int(st >> 33) % i + 1
        ks[i], ks[j] = ks[j], ks[i]
    end
    ks
end

# Warm the STOCK path FIRST so pinsert/commit_insert! compile at region 0;
# compiling inside a leaf window would escape (a TypeMapEntry into region 0).
drive(shuffle_keys(1000), false, 64); drive(shuffle_keys(1000), true, 64)
# Sweep the speculative transaction weight at a fixed key count. The single-
# insert case (work=0) is the region's WORST case -- minimal discarded
# allocation, maximal per-attempt overhead. As the transaction body grows, the
# discarded allocation per abort grows, and the region's O(1) reset should
# overtake the collector's trace-and-free. The crossover is the point.
println("Demonstrator C -- optimistic concurrent persistent BST, regions vs stock")
println("(4 threads on one shared root: ~3x abort rate; sweeping the transaction weight)\n")
const KEYS = 80_000
allok = true
allok &= run_scale("work=0   ", KEYS, 0)
allok &= run_scale("work=64  ", KEYS, 64)
allok &= run_scale("work=256 ", KEYS, 256)
allok &= run_scale("work=1024", KEYS, 1024)
println()
println(allok ? "DEMO C: consistent (same key set, no escape)" : "DEMO C: CHECK (keys or escape)")
