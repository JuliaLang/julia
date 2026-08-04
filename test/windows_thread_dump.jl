# This file is a part of Julia. License is MIT: https://julialang.org/license

# Dump the threads of another (possibly wedged) Windows process.
#
# The in-process unwinder cannot do this on 32-bit Windows: jl_unw_step passes
# the global hMainThread to StackWalk64, so it can only ever walk the main
# thread, and it needs DbgHelp under jl_in_stackwalk — which a watchdog inside
# a wedged process must not touch, since suspending a thread that holds that
# lock and then calling DbgHelp deadlocks. Reading the state from outside has
# neither problem: nothing here takes a lock in the target.
#
# Usage: julia windows_thread_dump.jl <pid>
#
# Prints, per thread, the instruction/stack/frame pointers, a frame-pointer
# walk where one is available, and the top of the stack as return-address
# candidates, plus the module map so any address can be attributed to a module
# (and symbolized offline against the matching binary).

module WindowsThreadDump

const TH32CS_SNAPTHREAD = 0x00000004
const THREAD_GET_CONTEXT = 0x0008
const THREAD_SUSPEND_RESUME = 0x0002
const THREAD_QUERY_INFORMATION = 0x0040
const PROCESS_QUERY_INFORMATION = 0x0400
const PROCESS_VM_READ = 0x0010
const CONTEXT_i386 = 0x00010000
const CONTEXT_CONTROL = CONTEXT_i386 | 0x0001
const CONTEXT_INTEGER = CONTEXT_i386 | 0x0002
const CONTEXT_FULL = CONTEXT_CONTROL | CONTEXT_INTEGER | (CONTEXT_i386 | 0x0004)

# THREADENTRY32: dwSize, cntUsage, th32ThreadID, th32OwnerProcessID, ...
const THREADENTRY32_SIZE = 28
# CONTEXT (x86) field offsets
const CTX_SIZE = 716
const OFF_EBP = 180
const OFF_EIP = 184
const OFF_ESP = 196

u32(buf, off) = reinterpret(UInt32, buf[off+1:off+4])[1]

function thread_ids(pid::Integer)
    snap = ccall((:CreateToolhelp32Snapshot, "kernel32"), stdcall, Ptr{Cvoid},
                 (UInt32, UInt32), TH32CS_SNAPTHREAD, 0)
    snap == Ptr{Cvoid}(-1 % UInt) && error("CreateToolhelp32Snapshot failed")
    ids = UInt32[]
    try
        entry = zeros(UInt8, THREADENTRY32_SIZE)
        entry[1:4] = reinterpret(UInt8, [UInt32(THREADENTRY32_SIZE)])
        ok = ccall((:Thread32First, "kernel32"), stdcall, Cint,
                   (Ptr{Cvoid}, Ptr{UInt8}), snap, entry) != 0
        while ok
            if u32(entry, 12) == UInt32(pid)   # th32OwnerProcessID
                push!(ids, u32(entry, 8))      # th32ThreadID
            end
            entry[1:4] = reinterpret(UInt8, [UInt32(THREADENTRY32_SIZE)])
            ok = ccall((:Thread32Next, "kernel32"), stdcall, Cint,
                       (Ptr{Cvoid}, Ptr{UInt8}), snap, entry) != 0
        end
    finally
        ccall((:CloseHandle, "kernel32"), stdcall, Cint, (Ptr{Cvoid},), snap)
    end
    return ids
end

function module_map(hproc)
    mods = Vector{Ptr{Cvoid}}(undef, 512)
    needed = Ref{UInt32}(0)
    ok = ccall((:EnumProcessModules, "psapi"), stdcall, Cint,
               (Ptr{Cvoid}, Ptr{Ptr{Cvoid}}, UInt32, Ptr{UInt32}),
               hproc, mods, sizeof(mods), needed) != 0
    ok || return
    n = min(length(mods), Int(needed[]) ÷ sizeof(Ptr{Cvoid}))
    println("---- module map")
    name = Vector{UInt8}(undef, 512)
    info = zeros(UInt8, 12)   # MODULEINFO: lpBaseOfDll, SizeOfImage, EntryPoint
    for i in 1:n
        len = ccall((:GetModuleFileNameExA, "psapi"), stdcall, UInt32,
                    (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{UInt8}, UInt32),
                    hproc, mods[i], name, length(name))
        ccall((:GetModuleInformation, "psapi"), stdcall, Cint,
              (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{UInt8}, UInt32), hproc, mods[i], info, 12)
        base = u32(info, 0); size = u32(info, 4)
        nm = len > 0 ? String(name[1:len]) : "?"
        println("  0x", string(base, base=16), "-0x", string(base + size, base=16),
                "  ", basename(nm))
    end
end

read_word(hproc, addr) = begin
    buf = zeros(UInt8, 4); got = Ref{Csize_t}(0)
    ok = ccall((:ReadProcessMemory, "kernel32"), stdcall, Cint,
               (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{UInt8}, Csize_t, Ptr{Csize_t}),
               hproc, Ptr{Cvoid}(UInt(addr)), buf, 4, got) != 0
    (ok && got[] == 4) ? u32(buf, 0) : nothing
end

function dump(pid::Integer)
    hproc = ccall((:OpenProcess, "kernel32"), stdcall, Ptr{Cvoid},
                  (UInt32, Cint, UInt32),
                  PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, 0, UInt32(pid))
    hproc == C_NULL && error("OpenProcess($pid) failed")
    println("==== external thread dump of pid ", pid)
    try
        module_map(hproc)
        for tid in thread_ids(pid)
            h = ccall((:OpenThread, "kernel32"), stdcall, Ptr{Cvoid},
                      (UInt32, Cint, UInt32),
                      THREAD_GET_CONTEXT | THREAD_SUSPEND_RESUME | THREAD_QUERY_INFORMATION,
                      0, tid)
            h == C_NULL && continue
            try
                ccall((:SuspendThread, "kernel32"), stdcall, UInt32, (Ptr{Cvoid},), h)
                ctx = zeros(UInt8, CTX_SIZE)
                ctx[1:4] = reinterpret(UInt8, [UInt32(CONTEXT_FULL)])
                if ccall((:GetThreadContext, "kernel32"), stdcall, Cint,
                         (Ptr{Cvoid}, Ptr{UInt8}), h, ctx) != 0
                    eip = u32(ctx, OFF_EIP); esp = u32(ctx, OFF_ESP); ebp = u32(ctx, OFF_EBP)
                    println("---- thread ", tid, " eip=0x", string(eip, base=16),
                            " esp=0x", string(esp, base=16), " ebp=0x", string(ebp, base=16))
                    # frame-pointer walk (valid only for frames that keep ebp)
                    fp = ebp; frames = String[]
                    for _ in 1:32
                        (fp == 0 || fp < esp) && break
                        ret = read_word(hproc, fp + 4)
                        next = read_word(hproc, fp)
                        ret === nothing && break
                        push!(frames, "0x" * string(ret, base=16))
                        (next === nothing || next <= fp) && break
                        fp = next
                    end
                    isempty(frames) || println("     fp-walk: ", join(frames, " "))
                    # raw stack words: return-address candidates for FPO frames
                    cand = String[]
                    for k in 0:63
                        w = read_word(hproc, esp + 4k)
                        w === nothing && break
                        w > 0x10000 && push!(cand, "0x" * string(w, base=16))
                    end
                    isempty(cand) || println("     stack: ", join(cand, " "))
                end
            finally
                ccall((:ResumeThread, "kernel32"), stdcall, UInt32, (Ptr{Cvoid},), h)
                ccall((:CloseHandle, "kernel32"), stdcall, Cint, (Ptr{Cvoid},), h)
            end
        end
    finally
        ccall((:CloseHandle, "kernel32"), stdcall, Cint, (Ptr{Cvoid},), hproc)
    end
    println("==== end external thread dump")
end

end # module

if abspath(PROGRAM_FILE) == @__FILE__
    WindowsThreadDump.dump(parse(Int, ARGS[1]))
end

# Value of `include(...)`, so a caller does not have to know which module the
# include landed in (test files run inside a module created by the harness).
WindowsThreadDump.dump
