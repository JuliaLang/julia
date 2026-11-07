# This file is a part of Julia. License is MIT: https://julialang.org/license

# Regression test for #63893: GC must not hang when an adopted foreign thread reuses an exited
# thread's pthread id and reaches a safepoint. On macOS, the Mach exception handler must skip
# the exited thread's retained TLS entry.

mutable struct Flag
    @atomic val::Int  # 0: starting, 1: running, 2: asked to stop
end

# keep the flags rooted while foreign threads hold raw pointers to them
const flags = Flag[]

# spin without yielding, but remain responsive to GC
function spin_while(f)
    while f()
        GC.safepoint()
        ccall(:jl_cpu_pause, Cvoid, ())
    end
end

function spin_on(arg::Ptr{Cvoid})
    f = unsafe_pointer_to_objref(arg)::Flag
    @atomic f.val = 1
    spin_while(() -> (@atomic f.val) == 1)
    return
end
# compile up front, so that the foreign threads don't have to
precompile(spin_on, (Ptr{Cvoid},))

function start_foreign_thread()
    f = Flag(0)
    push!(flags, f)
    cb = @cfunction(spin_on, Cvoid, (Ptr{Cvoid},))
    tid = Ref{UInt}(0)
    err = @ccall uv_thread_create(tid::Ptr{UInt}, cb::Ptr{Cvoid}, pointer_from_objref(f)::Ptr{Cvoid})::Cint
    err == 0 || Base.uv_error("uv_thread_create", err)
    spin_while(() -> (@atomic f.val) != 1)
    return f, tid
end

function stop_foreign_thread((f, tid))
    @atomic f.val = 2
    err = @ccall gc_safe=true uv_thread_join(tid::Ptr{UInt})::Cint
    err == 0 || Base.uv_error("uv_thread_join", err)
end

for i in 1:5
    # adopt a thread and let it exit, leaving a dead entry in jl_all_tls_states
    stop_foreign_thread(start_foreign_thread())

    # adopt another thread, likely reusing the pthread id, and collect while it spins
    b = start_foreign_thread()
    GC.gc()
    stop_foreign_thread(b)
end
