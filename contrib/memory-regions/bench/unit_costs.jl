# The unit costs of the region runtime, one function per cost, min of N.
#
#   julia unit_costs.jl [N]        prints one TSV row per cost: name, value,
#                                   unit, and the per-copy samples of the row
#   julia unit_costs.jl stock [N]  runs only the rows that use no region entry
#                                   point (store_disarmed, construct_two,
#                                   construct_shared, box_twin, alloc_stock,
#                                   stock_mark), so the script
#                                   runs on a vanilla julia too
#
# Run pinned on an isolated core. The rows, in the order they run (the
# disarmed store runs in a child process, because the first window of a
# process arms the barrier for the rest of its life):
#
#   store_disarmed    dst[k] = src[k] over Vector{Any}, barrier disarmed     ns/store
#   store_armed       the same copy loop, barrier armed, region-0 child      ns/store
#   store_region      the copy loop over two rings made in region 1          ns/store
#   window_pair       region_set(1); region_set(0)                           ns/pair
#   switch_pair       region_set(2); region_set(1) inside a window           ns/pair
#   construct_two     Two(Ref(i), Ref(i+1)): two boxed fields, constructed   ns/object
#                     (three allocations and one construction per object)
#   construct_shared  Two(src[k], src[k']): two shared boxed children, one   ns/object
#                     allocation; the construction barrier without the
#                     allocation of the children
#   box_twin          dst[k] = Twin(src[k], src[k']): an immutable with two   ns/object
#                     pointer fields, boxed at the store; the barrier of a
#                     fresh box, one check per pointer field of the value
#   alloc_stock       Ref(i) into a ring in region 0, slices of 100 000       ns/object
#   alloc_region      the same slices in region 1, one reset per slice        ns/object
#   reset_slice       the unsafe reset of a region that holds 1000 Refs      ns/reset
#   reset_slice_checked  the same reset through the checked entry              ns/reset
#   stock_mark        GC.gc() over a live set of 2^21 tree nodes + 10^6 Refs ms/collection
#
# The children of the stores vary, so the compiler cannot hoist the barrier
# out of the loop; with one invariant child it does, and the loop measures
# nothing. The rings the region rounds fill are made inside the window: a
# store of a region object into a region-0 ring is an escape, and an escape
# quarantines the region. The script exits with 1 when a region is
# quarantined at the end, because every region row is then invalid.
#
# The cost of a tight loop depends on where the compiler places its code:
# the same copy loop runs at 1.37, 1.44 or 1.64 ns per store in different
# positions, and one process gives every loop one position. The script
# compiles COPIES copies of each tight loop, so the copies land in different
# positions, and reports the min over the copies: the cost of the
# instructions, not of one placement. The rows of the runtime entries
# (window, switch, reset, mark) spend their time in C code with one fixed
# placement per binary, so they have one copy.
#
# The costs are independent of every other measurement: the script uses the
# runtime entry points directly and needs no other file.

region_set(n)         = ccall(:jl_gc_region_set, Cint, (Cint,), n)
region_quarantined(n) = ccall(:jl_gc_region_quarantined, Cint, (Cint,), n)

# A refused reset (a code above typemax - 16) leaves the region full, and
# the next slice then grows the region instead of reusing its pages: the
# row would measure page faults. Every reset of the script is checked.
function unsafe_region_reset(n)
    r = ccall(:jl_gc_region_unsafe_reset, UInt64, (Cint,), n)
    r > typemax(UInt64) - 16 && error("the reset of region $n was refused with code $(reinterpret(Int64, r))")
    return r
end

const STOCK = length(ARGS) >= 1 && ARGS[1] == "stock"
const N = STOCK ? (length(ARGS) >= 2 ? parse(Int, ARGS[2]) : 5) :
                  (length(ARGS) >= 1 ? parse(Int, ARGS[1]) : 5)
const ITERS = 20_000_000
const COPIES = 8
const SRC = Any[Ref(i) for i in 1:1024]
const DST = Vector{Any}(undef, 1024)
fill!(DST, nothing)

mutable struct Two
    a::Any
    b::Any
end
struct Twin
    a::Any
    b::Any
end
mutable struct Node
    left::Union{Node, Nothing}
    right::Union{Node, Nothing}
    v::Int
end

# min of N repetitions of f(), each timed once; f returns the count of
# operations, so the result is ns per operation.
function best(f, n = N)
    t = Inf
    for _ in 1:n
        t0 = time_ns()
        ops = f()
        t = min(t, (time_ns() - t0) / ops)
    end
    return t
end

# Every copy's own minimum. The row is their minimum - the cost of the
# instructions and not of one placement - and the samples go into the fourth
# column, so a reader can see how far the placements spread.
copy_samples(run, loops) = [best(() -> run(loop)) for loop in loops]

sig4(v) = round(v; sigdigits = 4)

row(name, value, unit, samples = Float64[]) =
    println(name, '\t', sig4(value), '\t', unit, '\t', join(map(sig4, samples), ','))

# A row measured over the copies of a tight loop.
rowc(name, unit, run, loops) = (s = copy_samples(run, loops); row(name, minimum(s), unit, s))

# The three tight loops, COPIES times each. A copy differs from the others
# only in its name, so it has the same instructions in a different place.
for c in 1:COPIES
    @eval begin
        @noinline function $(Symbol(:copy_loop_, c))(dst, src, iters)
            for i in 1:iters
                k = (i & 1023) + 1
                @inbounds dst[k] = src[k]
            end
            return iters
        end
        @noinline function $(Symbol(:construct_loop_, c))(dst, iters)
            for i in 1:iters
                @inbounds dst[(i & 1023) + 1] = Two(Ref(i), Ref(i + 1))
            end
            return iters
        end
        @noinline function $(Symbol(:construct_shared_loop_, c))(dst, src, iters)
            for i in 1:iters
                k = (i & 1023) + 1
                @inbounds dst[k] = Two(src[k], src[((i + 1) & 1023) + 1])
            end
            return iters
        end
        @noinline function $(Symbol(:box_twin_loop_, c))(dst, src, iters)
            for i in 1:iters
                k = (i & 1023) + 1
                @inbounds dst[k] = Twin(src[k], src[((i + 1) & 1023) + 1])
            end
            return iters
        end
        @noinline function $(Symbol(:alloc_loop_, c))(dst, iters)
            for i in 1:iters
                @inbounds dst[(i & 1023) + 1] = Ref(i)
            end
            return iters
        end
    end
end
const COPY_LOOPS      = [getfield(@__MODULE__, Symbol(:copy_loop_, c)) for c in 1:COPIES]
const CONSTRUCT_LOOPS = [getfield(@__MODULE__, Symbol(:construct_loop_, c)) for c in 1:COPIES]
const CONSTRUCT_SHARED_LOOPS = [getfield(@__MODULE__, Symbol(:construct_shared_loop_, c)) for c in 1:COPIES]
const BOX_TWIN_LOOPS  = [getfield(@__MODULE__, Symbol(:box_twin_loop_, c)) for c in 1:COPIES]
const ALLOC_LOOPS     = [getfield(@__MODULE__, Symbol(:alloc_loop_, c)) for c in 1:COPIES]

@noinline function make_ring()
    ring = Vector{Any}(undef, 1024)
    for i in 1:1024
        @inbounds ring[i] = Ref(i)
    end
    return ring
end

# The copy loop over two rings made inside the window: parent and child are
# region objects, so the barrier walks the page map twice, not once.
@noinline function copy_region_round(copy_loop, iters)
    region_set(1)
    src = make_ring()
    dst = make_ring()
    copy_loop(dst, src, iters)
    region_set(0)
    unsafe_region_reset(1)
    return iters
end

@noinline function window_loop(iters)
    for _ in 1:iters
        region_set(1)
        region_set(0)
    end
    return iters
end

# Region 2 is a child of region 1 in the default chain: a live child refuses
# the parent's reset, so the loop resets 2 before 1.
@noinline function switch_loop(iters)
    region_set(1)
    for _ in 1:iters
        region_set(2)
        region_set(1)
    end
    region_set(0)
    unsafe_region_reset(2)
    unsafe_region_reset(1)
    return iters
end

# A round allocates a slice of 100 000 Refs (1.6 MB), so the measurement is
# the allocator and not the page faults of a region that grows without
# bound; the rounds repeat until iters objects are allocated.
const SLICE = 100_000

@noinline function alloc_stock_round(alloc_loop, iters)
    for _ in 1:(iters ÷ SLICE)
        ring = Vector{Any}(undef, 1024)
        fill!(ring, nothing)
        alloc_loop(ring, SLICE)
    end
    return iters
end

@noinline function alloc_region_round(alloc_loop, iters)
    for _ in 1:(iters ÷ SLICE)
        region_set(1)
        ring = Vector{Any}(undef, 1024)
        fill!(ring, nothing)
        alloc_loop(ring, SLICE)
        region_set(0)
        unsafe_region_reset(1)
    end
    return iters
end

@noinline function fill_slice()
    region_set(1)
    ring = Vector{Any}(undef, 1024)
    fill!(ring, nothing)
    alloc_loop_1(ring, 1000)
    region_set(0)
    nothing
end

# The reset alone: fill, then time the one call. 2000 calls per repetition;
# the min over every call. The reading includes the pair of clock reads
# around the call (about 10 ns on the measurement host), so the row is an
# upper bound on the reset.
function reset_slice()
    t = Inf
    for _ in 1:N
        for _ in 1:2000
            fill_slice()
            t0 = time_ns()
            unsafe_region_reset(1)
            t = min(t, Float64(time_ns() - t0))
        end
    end
    return t
end

# The same reset through the checked entry, which stops the world, marks
# from the execution roots of every thread with the region filter, and frees
# in the same pause. That check is what stands between a live local and a
# freed object, and this row is its price. The two rows are the two entries
# a program chooses between, so the table carries both.
function reset_slice_checked()
    t = Inf
    for _ in 1:N
        for _ in 1:200
            fill_slice()
            t0 = time_ns()
            r = ccall(:jl_gc_region_reset, UInt64, (Cint,), 1)
            t = min(t, Float64(time_ns() - t0))
            r > typemax(UInt64) - 16 &&
                error("the checked reset was refused with code $(reinterpret(Int64, r))")
        end
    end
    return t
end

function build_tree(depth)
    depth == 0 && return nothing
    return Node(build_tree(depth - 1), build_tree(depth - 1), depth)
end

function stock_mark()
    tree = build_tree(21)
    refs = [Ref(i) for i in 1:1_000_000]
    GC.gc()
    t = Inf
    for _ in 1:N
        t0 = time_ns()
        GC.gc()
        t = min(t, (time_ns() - t0) / 1e6)
    end
    GC.@preserve tree refs nothing
    return t
end

# The child process never opens a window: its copy loop runs with the
# barrier disarmed.
if get(ENV, "UNIT_COSTS_CHILD", "") == "disarmed"
    foreach(loop -> loop(DST, SRC, 1000), COPY_LOOPS)
    s = copy_samples(loop -> loop(DST, SRC, ITERS), COPY_LOOPS)
    println(sig4(minimum(s)), '\t', join(map(sig4, s), ','))
    exit(0)
end
child = read(setenv(`$(Base.julia_cmd()) --startup-file=no $(@__FILE__) $N`, "UNIT_COSTS_CHILD" => "disarmed"), String)

# Warm every loop once on a small count, so the timed run measures the loop
# and not the compiler. Every compilation happens outside a window: a type
# the compiler makes inside a window and caches in a region-0 table is an
# escape.
foreach(loop -> loop(DST, SRC, 1000), COPY_LOOPS)
foreach(loop -> loop(DST, 1000), CONSTRUCT_LOOPS)
foreach(loop -> loop(DST, SRC, 1000), CONSTRUCT_SHARED_LOOPS)
foreach(loop -> loop(DST, SRC, 1000), BOX_TWIN_LOOPS)
foreach(loop -> alloc_stock_round(loop, 1000), ALLOC_LOOPS)
if !STOCK
    make_ring()
    window_loop(10); unsafe_region_reset(1); switch_loop(10)
    foreach(loop -> copy_region_round(loop, 1000), COPY_LOOPS)
    foreach(loop -> alloc_region_round(loop, 1000), ALLOC_LOOPS)
    fill_slice(); unsafe_region_reset(1)
end

let parts = split(strip(child), '\t')
    samples = length(parts) >= 2 ? map(x -> parse(Float64, x), split(parts[2], ',')) : Float64[]
    row("store_disarmed", parse(Float64, parts[1]), "ns/store", samples)
end
if !STOCK
    rowc("store_armed", "ns/store", loop -> loop(DST, SRC, ITERS), COPY_LOOPS)
    rowc("store_region", "ns/store", loop -> copy_region_round(loop, ITERS), COPY_LOOPS)
    row("window_pair", best(() -> window_loop(ITERS)), "ns/pair")
    row("switch_pair", best(() -> switch_loop(ITERS)), "ns/pair")
end
rowc("construct_two", "ns/object", loop -> loop(DST, ITERS), CONSTRUCT_LOOPS)
rowc("construct_shared", "ns/object", loop -> loop(DST, SRC, ITERS), CONSTRUCT_SHARED_LOOPS)
rowc("box_twin", "ns/object", loop -> loop(DST, SRC, ITERS), BOX_TWIN_LOOPS)
rowc("alloc_stock", "ns/object", loop -> alloc_stock_round(loop, ITERS), ALLOC_LOOPS)
if !STOCK
    rowc("alloc_region", "ns/object", loop -> alloc_region_round(loop, ITERS), ALLOC_LOOPS)
    row("reset_slice", reset_slice(), "ns/reset")
    row("reset_slice_checked", reset_slice_checked(), "ns/reset")
end
row("stock_mark", stock_mark(), "ms/collection")

if !STOCK && (region_quarantined(1) != 0 || region_quarantined(2) != 0)
    println(stderr, "unit_costs: a region was quarantined; the region rows are invalid")
    exit(1)
end
