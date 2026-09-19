dest = Any[nothing for i=1:16]
# We want the destination GenericMemory to be OLD_MARKED
for i in 1:5
    GC.gc(true)
    GC.gc(false)
end

src = vcat(Any[nothing], Any[Ref(100 * i) for i=1:15])
copyto!(dest, src)
for (x, y) in zip(dest, src)
    @assert x === y
end

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

@noinline function copy_young_refs!(dest, make_element, n)
    src = [make_element(i) for i in 1:n]
    copyto!(dest, 3, src, 1, n)
    return nothing
end

for (make_element, read_element) in (
    (Ref{Int}, x -> x[]),
    (i -> CopyInline(nothing, i, Ref(i)), x -> (x.first, x.payload, x.last[])),
    (i -> CopyWideInline(ntuple(_ -> UInt(0), 256), Ref(i)), x -> x.value[]),
)
    for n in (1, 2, 3, 4, 5, 65), len in (n + 4, 4n + 4)
        local dest = fill(make_element(0), len)
        for _ in 1:3
            GC.gc(true)
            GC.gc(false)
        end
        copy_young_refs!(dest, make_element, n)
        GC.gc(false)
        GC.gc(false)
        for i in 1:n
            @assert read_element(dest[i + 2]) == read_element(make_element(i))
        end
        @assert read_element(dest[1]) == read_element(make_element(0))
        @assert read_element(dest[end]) == read_element(make_element(0))
    end
end

struct CopyUndef
    value::Any
    CopyUndef() = new()
end

struct CopyPartial
    first::CopyUndef
    last::Any
end

@noinline function copy_partial!(dest)
    child = Ref(42)
    src = [CopyPartial(CopyUndef(), child)]
    weak = WeakRef(child)
    copyto!(dest, src)
    return weak
end

let dest = fill(CopyPartial(CopyUndef(), nothing), 8)
    for _ in 1:3
        GC.gc(true)
        GC.gc(false)
    end
    weak = copy_partial!(dest)
    GC.@preserve dest begin
        GC.gc(false)
        GC.gc(false)
        @assert weak.value !== nothing
        @assert weak.value[] == 42
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
    @assert any(_ -> copy_race_trial(1_000_000), 1:10) "copy race interleaving was not exercised"
end
