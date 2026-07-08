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
