# The Julia face of the runtime regions (src/gc-regions.h; the design and
# the rules are in doc/src/devdocs/gc-regions.md). It needs a julia built
# from this branch. Every entry takes region numbers; the meaning of a
# number belongs to the caller.

module Regions

export region_set, region_reset, unsafe_region_reset, region_reserve, @with_region, @in_region_of,
       region_current, region_of,
       region_collect, region_collect_coop, region_check, region_debug,
       region_quarantined, region_parent!, region_tree!, region_parent_of, region_reset_global,
       region_census_threshold!, region_pages

# Declare the region tree. A region's number is a topological order of the
# tree: a parent's number is smaller than its child's. The default, until
# a declaration, is the chain 0 <- 1 <- 2 <- ..., so an object of region r
# may reference its own region and every older one. A declaration lets two
# leaves over a shared trunk be mutually isolated: neither may reference the
# other, only their common ancestors.
region_parent!(child::Int, parent::Int) =
    (ccall(:jl_gc_region_declare_parent, Cint, (Cint, Cint), child, parent) == 0 ||
     error("bad region edge: parent ($parent) must be >= 0 and < child ($child)"))
region_parent_of(child::Int) = Int(ccall(:jl_gc_region_parent_of, Cint, (Cint,), child))
# Arm the open-region census: when a region window grows past `pages`, its dead
# cells are reclaimed in place (the live search state on the stack is kept), so
# a computation whose garbage dies inside the window does not grow the region
# without bound. 0 disables it; the reset stays the fast common path.
region_census_threshold!(pages::Int) = ccall(:jl_gc_region_census_threshold, Cvoid, (Cint,), pages)
region_pages(n::Int) = Int(ccall(:jl_gc_region_pages, Cint, (Cint,), n))
# Declare the whole tree at once: parents[i] is the parent of region i, for
# i = 1.. (region 0 is the root and takes no entry). parents[i] < i.
function region_tree!(parents::AbstractVector{<:Integer})
    for child in 1:length(parents)
        region_parent!(child, Int(parents[child]))
    end
    return nothing
end

region_set(n::Int)     = Int(ccall(:jl_gc_region_set, Cint, (Cint,), n))
region_current()       = Int(ccall(:jl_gc_region_current, Cint, ()))
region_of(x)           = Int(ccall(:jl_gc_region_of, Cint, (Any,), x))
region_debug(on::Int)  = ccall(:jl_gc_region_set_debug, Cvoid, (Cint,), on)
# Has an escape quarantined this region? A store that breaks the reference
# rule quarantines the region of the child, process-wide and for the life of
# the process: from then on its reset and its censuses refuse with -5, a
# window on it is refused, and its memory is retained. Every entry that
# refuses returns that code, so a program can read the state from a return
# value; this asks the question directly, which is what a program wants
# before it opens a window or after it caught a refusal.
region_quarantined(n::Int) = ccall(:jl_gc_region_quarantined, Cint, (Cint,), n) != 0
# Claim and prefault `bytes` of heap before the loop starts: the runtime
# maps whole blocks, populated by the kernel at the claim, into its clean
# pool, and prefaults every later block too. A loop whose heap fits the
# reserve takes no page fault while it runs. Returns the bytes mapped.
region_reserve(bytes::Integer) = UInt64(ccall(:jl_gc_heap_reserve, UInt64, (UInt64,), bytes))

# A window and a borrow are two policies over one act of the runtime: make
# region `n` the allocation target of this thread. The task owns a window: it
# follows the task across a yield, it counts itself so that a global reset or
# a census knows a window is open, it pins the task to its thread, and it can
# refuse. The thread owns a borrow: it installs and nothing else, so it never
# refuses and it costs two field writes. A window is a scope for a unit of
# work; a borrow is for one allocation that must land where another object
# lives (`@in_region_of` below).
#
# The other two combinations of "who owns it" and "what is written down" are
# not offered. A borrow the task owns would close the rule "do not yield
# inside a borrow" and is deferred; a counted borrow would make an ordinary
# `push!` refuse a global reset for the length of one allocation and buy
# nothing. The devdoc has the table.
#
# The readable form of a window: run the body with region `n` current and
# come back to the region that was current before, however the body leaves
# (return, break, or a throw). Measured, the try/finally adds nothing over
# the bare region_set pair (10.35 against 10.39 ns for the enter-and-leave).
#
#     const EVENT = 3          # the caller names its own slots
#     @with_region EVENT begin
#         process_event!(...)
#     end
macro with_region(n, body)
    quote
        local prev = region_set($(esc(n)))
        try
            $(esc(body))
        finally
            region_set(prev)
        end
    end
end

# A buffer that replaces the buffer of an object that exists takes the
# lifetime of that object, so it must take its region as well. Allocated in
# the region of the open window instead, it is a younger object held by an
# older container: an escape, and the region is quarantined for an operation
# the program has every right to make.
#
# Base does this for its own containers - a vector that grows, a dictionary
# that rehashes, an `IOBuffer` that reallocates - through the same pair of
# entry points. `@in_region_of` gives it to a program with a container of its
# own:
#
#     mutable struct RingBuffer
#         data::Memory{Float64}
#     end
#
#     function grow!(rb::RingBuffer)
#         new = @in_region_of rb Memory{Float64}(undef, 2 * length(rb.data))
#         copyto!(new, rb.data)
#         rb.data = new              # legal: `new` lives where `rb` lives
#     end
#
# The region comes from the container and not from the buffer it replaces: an
# empty container can share the one permanent empty `Memory`, which belongs
# to region 0.
#
# Two warnings. A borrow is not a window: it changes no window state, and it
# is meant for one allocation, not for a phase of the program. **Do not
# yield inside one** - a task switch would save the borrowed region as the
# task's window, and the task would keep allocating there after the borrow
# ended. Keep the body short and free of `wait`, `sleep`, `lock`, `@spawn`
# and IO. The macro gives the region back however the body leaves.
macro in_region_of(like, body)
    quote
        local lent = ccall(:jl_gc_region_borrow, Cint, (Cint,),
                           ccall(:jl_gc_region_of, Cint, (Any,), $(esc(like))))
        try
            $(esc(body))
        finally
            ccall(:jl_gc_region_unborrow, Cvoid, (Cint,), lent)
        end
    end
end

# The reset, the check, and the scoped collect are @noinline JULIA calls on
# purpose: a plain ccall is not a safepoint, so the caller would keep live
# references in registers where the precise stack scan cannot see them. A
# Julia call is a safepoint boundary -- the caller spills every live value
# into its GC frame, which is exactly what the rule-5 scan reads.
# region_reset checks the execution roots before it frees: the escape
# barrier sees the heap and not the stack, so this check is what stands
# between a live local and a freed object. It stops the world for the scan
# and the free together.
#
# The reset must not run in a frame that still names one of the region's
# objects. A Julia frame roots a local until the frame ends, whether or not
# the program still reads it, so build and use the region's objects in one
# function and reset after it returned. A reset under such a root returns
# -8; run region_debug(1) to have the runtime name the objects it found.
#
# It returns the pages reclaimed, or a refusal code cast to UInt64
# (reinterpret(Int64, r) reads it): -1 a bad region number, -2 the region is
# current or finalizers run on this thread, -3 the safepoint race was lost
# too often, -5 quarantined by an escape, -7 a live child region exists
# (reset it first), -8 an execution root references into the region.
@noinline region_reset(n::Int)   = UInt64(ccall(:jl_gc_region_reset, UInt64, (Cint,), n))
# The reset without the root check and without its pause. A reference from a
# stack slot, a register or a parked task's stack is left pointing into freed
# memory, and the next collection reports CORPSE and aborts.
#
# Use it where the pause of the checked entry is the thing being measured, or
# in a loop that has shown with the checked entry that no root survives its
# window. The benchmarks and the demonstrators of this folder use it for that
# reason; a program that has not made that case uses region_reset.
@noinline unsafe_region_reset(n::Int) = UInt64(ccall(:jl_gc_region_unsafe_reset, UInt64, (Cint,), n))
# The cross-heap reset of a shared region (a trunk): stops the world and
# resets every thread heap's instance as one act, because trunk objects on
# different heaps legally reference each other. The same codes as the
# single-heap reset, plus -3 for a lost safepoint race and -6 for pending
# finalizers (a census runs them first).
@noinline region_reset_global(n::Int) = UInt64(ccall(:jl_gc_region_reset_global, UInt64, (Cint,), n))
@noinline region_check(n::Int)   = Int64(ccall(:jl_gc_region_check, Int64, (Cint,), n))
# The stop-the-world census: the cells freed, or a refusal code (-1 a bad or
# unused region, -2 a window is open, -5 quarantined, -6 pending finalizers).
@noinline region_collect(n::Int) = Int64(ccall(:jl_gc_region_collect, Int64, (Cint,), n))
# The cooperative census: no stop-the-world. The driving loop calls it at
# a quiet boundary; -4 means another thread runs managed code and the
# stop-the-world entry must be used instead.
@noinline region_collect_coop(n::Int) = Int64(ccall(:jl_gc_region_collect_coop, Int64, (Cint,), n))

end # module Regions
