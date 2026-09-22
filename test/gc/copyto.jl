# Young references copied into old memories must survive after the source is gone,
# for small and large spans, inline pointer fields, and wide pointer descriptors.
struct CopyInline
    first::Any
    payload::Int
    last::Any
end

struct CopyWideInline
    padding::NTuple{256,UInt}
    value::Any
end

struct CopyUndef
    value::Any
    CopyUndef() = new()
end

struct CopyPartial
    first::CopyUndef
    last::Any
end

@noinline function copy_young_refs!(dest, make_element, n, young)
    # `dest[1]` only references old objects by now
    src = [i in young ? make_element(i) : dest[1] for i in 1:n]
    copyto!(dest, 3, src, 1, n)
    return nothing
end

@noinline function copy_partial!(dest)
    child = Ref(42)
    src = [CopyPartial(CopyUndef(), child)]
    weak = WeakRef(child)
    copyto!(dest, src)
    return weak
end

const copy_elements = (
    (Ref{Int}, x -> x[]),
    (i -> CopyInline(nothing, i, Ref(i)), x -> (x.first, x.payload, x.last[])),
    (i -> CopyWideInline(ntuple(_ -> UInt(0), 256), Ref(i)), x -> x.value[]),
)

# Allocate every destination up front so that they all age together, rather
# than collecting once per case.
let cases = [(make_element, read_element, n, young, fill(make_element(0), len))
             for (make_element, read_element) in copy_elements
             for n in (1, 2, 3, 4, 5, 65) for len in (n + 4, 4n + 4)
             # all of the copied elements are young, or only the last one is
             for young in (1:n, n:n)],
    dest_any = Any[nothing for i in 1:16],
    dest_partial = fill(CopyPartial(CopyUndef(), nothing), 8)
    # We want the destination GenericMemory's to be OLD_MARKED
    for _ in 1:3
        GC.gc(false)
    end
    src_any = vcat(Any[nothing], Any[Ref(100 * i) for i in 1:15])
    copyto!(dest_any, src_any)
    for (make_element, _, n, young, dest) in cases
        copy_young_refs!(dest, make_element, n, young)
    end
    weak = copy_partial!(dest_partial)
    GC.@preserve dest_partial begin
        GC.gc(false)
        GC.gc(false)
        @assert weak.value !== nothing
        @assert weak.value[] == 42
    end
    for (x, y) in zip(dest_any, src_any)
        @assert x === y
    end
    for (make_element, read_element, n, young, dest) in cases
        for i in 1:n
            @assert read_element(dest[i + 2]) == read_element(make_element(i in young ? i : 0))
        end
        @assert read_element(dest[1]) == read_element(make_element(0))
        @assert read_element(dest[end]) == read_element(make_element(0))
    end
end

# A concurrent store into an old source must not let a copied young reference escape the barrier.
@noinline copy_race_ready(flag) = flag[]
@noinline copy_race_started(dest) = dest[1] === :src
@noinline copy_race_holds(dest, weak) = dest[end] === weak.value

@noinline function copy_race_writer!(src, dest, ready)
    child = Ref(42)
    weak = WeakRef(child)
    Threads.atomic_xchg!(ready, true)
    while !copy_race_started(dest) end
    src[end] = child
    return weak
end

function copy_race_trial(n)
    src = fill!(Memory{Any}(undef, n), :src)
    dest = fill!(Memory{Any}(undef, n), :dest)
    GC.gc(true)
    GC.gc(true)
    ready = Threads.Atomic{Bool}(false)
    result = Ref{WeakRef}()
    Threads.@threads :static for worker in 1:2
        if worker == 1
            while !copy_race_ready(ready) end
            copyto!(dest, src)
        else
            result[] = copy_race_writer!(src, dest, ready)
        end
    end
    weak = result[]
    copy_race_holds(dest, weak) || return false
    src[end] = nothing
    GC.@preserve dest begin
        GC.gc(false)
        @assert weak.value !== nothing "concurrently copied child was collected"
    end
    return true
end

if Threads.nthreads(:default) >= 2
    # Large enough that the copy outlasts an OS timeslice.
    reproduced = any(_ -> copy_race_trial(16_000_000), 1:10)
    # We could `@assert reproduced` to enforce that we tested the intended race
    # here, but that depends on OS scheduling so it would likely be flaky in CI.
    # (this test still catches bugs whenever the race is 'lucky enough' to occur)
end
