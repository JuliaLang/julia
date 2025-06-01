# This file is a part of Julia. License is MIT: https://julialang.org/license

module Sys
@doc """
Provide methods for retrieving information about hardware and the operating system.
""" Sys

export BINDIR,
       STDLIB,
       CPU_THREADS,
       CPU_NAME,
       WORD_SIZE,
       ARCH,
       MACHINE,
       KERNEL,
       JIT,
       cpu_info,
       cpu_summary,
       uptime,
       loadavg,
       free_memory,
       total_memory,
       free_physical_memory,
       total_physical_memory,
       isapple,
       isbsd,
       isdragonfly,
       isfreebsd,
       islinux,
       isnetbsd,
       isopenbsd,
       isunix,
       iswindows,
       isjsvm,
       isexecutable,
       isreadable,
       iswritable,
       username,
       which,
       detectwsl

import ..Base: show

"""
    Sys.BINDIR::String

A string containing the full path to the directory containing the `julia` executable.
"""
global BINDIR::String = ccall(:jl_get_julia_bindir, Any, ())::String

"""
    Sys.STDLIB::String

A string containing the full path to the directory containing the `stdlib` packages.
"""
global STDLIB::String = "$BINDIR/../share/julia/stdlib/v$(VERSION.major).$(VERSION.minor)" # for bootstrap
# In case STDLIB change after julia is built, the variable below can be used
# to update cached method locations to updated ones.
const BUILD_STDLIB_PATH = STDLIB
# Similarly, this is the root of the julia repo directory that julia was built from
const BUILD_ROOT_PATH = "$BINDIR/../.."

# helper to avoid triggering precompile warnings

"""
    Sys.CPU_THREADS::Int

The number of logical CPU cores available in the system, i.e. the number of threads
that the CPU can run concurrently. Note that this is not necessarily the number of
CPU cores, for example, in the presence of
[hyper-threading](https://en.wikipedia.org/wiki/Hyper-threading).

See Hwloc.jl or CpuId.jl for extended information, including number of physical cores.
"""
global CPU_THREADS::Int = 1 # for bootstrap, changed on startup

"""
    Sys.ARCH::Symbol

A symbol representing the architecture of the build configuration.
"""
const ARCH = ccall(:jl_get_ARCH, Any, ())::Symbol


"""
    Sys.KERNEL::Symbol

A symbol representing the name of the operating system, as returned by `uname` of the build configuration.
"""
const KERNEL = ccall(:jl_get_UNAME, Any, ())::Symbol

"""
    Sys.MACHINE::String

A string containing the build triple.
"""
const MACHINE = Base.MACHINE::String

"""
    Sys.WORD_SIZE::Int

Standard word size on the current machine, in bits.
"""
const WORD_SIZE = Core.sizeof(Int) * 8

"""
    Sys.SC_CLK_TCK:

The number of system "clock ticks" per second, corresponding to `sysconf(_SC_CLK_TCK)` on
POSIX systems, or `0` if it is unknown.

CPU times, e.g. as returned by `Sys.cpu_info()`, are in units of ticks, i.e. units of `1 / Sys.SC_CLK_TCK` seconds if `Sys.SC_CLK_TCK > 0`.
"""
global SC_CLK_TCK::Clong

"""
    Sys.CPU_NAME::String

A string representing the name of CPU.

# Examples
For example, `Sys.CPU_NAME` might equal `"tigerlake"` on an
[Intel Core "Tiger Lake" CPU](https://en.wikipedia.org/wiki/Tiger_Lake),
or `"apple-m1"` on an [Apple M1 CPU](https://en.wikipedia.org/wiki/Apple_M1).

Note: Included in the detailed system information via `versioninfo(verbose=true)`.
"""
global CPU_NAME::String

"""
    Sys.JIT::String

A string representing the specific Just-In-Time (JIT) compiler being utilized in the current runtime.

# Examples
Currently, this equals `"ORCJIT"` for the LLVM "ORC" ("On-Request Compilation") JIT library:
```jldoctest
julia> Sys.JIT
"ORCJIT"
```

Note: Included in the detailed system information via `versioninfo(verbose=true)`.
"""
global JIT::String

function __init__()
    env_threads = nothing
    if haskey(ENV, "JULIA_CPU_THREADS")
        env_threads = ENV["JULIA_CPU_THREADS"]
    end
    global CPU_THREADS = if env_threads !== nothing
        env_threads = tryparse(Int, env_threads)
        if env_threads === nothing || env_threads <= 0
            env_threads = Int(ccall(:jl_cpu_threads, Int32, ()))
            Core.print(Core.stderr, "WARNING: couldn't parse `JULIA_CPU_THREADS` environment variable. Defaulting Sys.CPU_THREADS to $env_threads.\n")
        end
        env_threads
    else
        Int(ccall(:jl_cpu_threads, Int32, ()))
    end
    global SC_CLK_TCK = ccall(:jl_SC_CLK_TCK, Clong, ())
    global CPU_NAME = ccall(:jl_get_cpu_name, Ref{String}, ())
    global JIT = ccall(:jl_get_JIT, Ref{String}, ())
    __init_build()
    nothing
end
# Populate the paths needed by sysimg compilation, e.g. `generate_precompile.jl`,
# without pulling in anything unnecessary like `CPU_NAME`
function __init_build()
    global BINDIR = ccall(:jl_get_julia_bindir, Any, ())::String
    vers = "v$(string(VERSION.major)).$(string(VERSION.minor))"
    global STDLIB = abspath(BINDIR, "..", "share", "julia", "stdlib", vers)
    nothing
end

mutable struct UV_cpu_info_t
    model::Ptr{UInt8}
    speed::Int32
    cpu_times!user::UInt64
    cpu_times!nice::UInt64
    cpu_times!sys::UInt64
    cpu_times!idle::UInt64
    cpu_times!irq::UInt64
end

"""
    Sys.CPUinfo

The `CPUinfo` type is a mutable struct with the following fields:
- `model::String`: CPU model information.
- `speed::Int32`: CPU speed (in MHz).
- `cpu_times!user::UInt64`: Time spent in user mode. CPU state shows CPU time used by user space processes.
- `cpu_times!nice::UInt64`: Time spent in nice mode. CPU state is a subset of the "user" state and shows the CPU time used by processes that have a positive niceness, meaning a lower priority than other tasks.
- `cpu_times!sys::UInt64`: Time spent in system mode. CPU state shows the amount of CPU time used by the kernel.
- `cpu_times!idle::UInt64`: Time spent in idle mode. CPU state shows the CPU time that's not actively being used.
- `cpu_times!irq::UInt64`: Time spent handling interrupts. CPU state shows the amount of time the CPU has been servicing hardware interrupts.

The times are in units of `1/Sys.SC_CLK_TCK` seconds if `Sys.SC_CLK_TCK > 0`; otherwise they are in
unknown units.

Note: Included in the detailed system information via `versioninfo(verbose=true)`.
"""
mutable struct CPUinfo
    model::String
    speed::Int32
    cpu_times!user::UInt64
    cpu_times!nice::UInt64
    cpu_times!sys::UInt64
    cpu_times!idle::UInt64
    cpu_times!irq::UInt64
    CPUinfo(model,speed,u,n,s,id,ir)=new(model,speed,u,n,s,id,ir)
end
CPUinfo(info::UV_cpu_info_t) = CPUinfo(unsafe_string(info.model), info.speed,
    info.cpu_times!user, info.cpu_times!nice, info.cpu_times!sys,
    info.cpu_times!idle, info.cpu_times!irq)

public CPUinfo

function _show_cpuinfo(io::IO, info::Sys.CPUinfo, header::Bool=true, prefix::AbstractString="    ")
    tck = SC_CLK_TCK
    if header
        println(io, info.model, ": ")
        print(io, " "^length(prefix))
        println(io, "    ", lpad("speed", 5), "    ", lpad("user", 9), "    ", lpad("nice", 9), "    ",
                lpad("sys", 9), "    ", lpad("idle", 9), "    ", lpad("irq", 9))
    end
    print(io, prefix)
    unit = tck > 0 ? " s  " : "    "
    tc = max(tck, 1)
    d(i, unit=unit) = lpad(string(round(Int64,i)), 9) * unit
    print(io,
          lpad(string(info.speed), 5), " MHz  ",
          d(info.cpu_times!user / tc), d(info.cpu_times!nice / tc), d(info.cpu_times!sys / tc),
          d(info.cpu_times!idle / tc), d(info.cpu_times!irq / tc, tck > 0 ? " s" : "  "))
    if tck <= 0
        print(io, "ticks")
    end
end

show(io::IO, ::MIME"text/plain", info::CPUinfo) = _show_cpuinfo(io, info, true, "    ")

function _cpu_summary(io::IO, cpu::AbstractVector{CPUinfo}, i, j)
    if j-i < 9
        header = true
        for x = i:j
            header || println(io)
            _show_cpuinfo(io, cpu[x], header, "#$(x-i+1) ")
            header = false
        end
    else
        summary = CPUinfo(cpu[i].model,0,0,0,0,0,0)
        count = j - i + 1
        for x = i:j
            summary.speed += cpu[i].speed
            summary.cpu_times!user += cpu[x].cpu_times!user
            summary.cpu_times!nice += cpu[x].cpu_times!nice
            summary.cpu_times!sys += cpu[x].cpu_times!sys
            summary.cpu_times!idle += cpu[x].cpu_times!idle
            summary.cpu_times!irq += cpu[x].cpu_times!irq
        end
        summary.speed = div(summary.speed,count)
        _show_cpuinfo(io, summary, true, "#1-$(count) ")
    end
    println(io)
end

"""
    Sys.cpu_summary(io::IO=stdout, cpu::AbstractVector{CPUinfo}=cpu_info())

Print a summary of CPU information to the `io` stream (defaulting to [`stdout`](@ref)), organizing and displaying aggregated data for CPUs with the same model, for a given array of `CPUinfo` data structures
describing a set of CPUs (which defaults to the return value of the [`Sys.cpu_info`](@ref) function).

The summary includes aggregated information for each distinct CPU model,
providing details such as average CPU speed and total time spent in different modes (user, nice, sys, idle, irq) across all cores with the same model.

Note: Included in the detailed system information via `versioninfo(verbose=true)`.
"""
function cpu_summary(io::IO=stdout, cpu::AbstractVector{CPUinfo} = cpu_info())
    model = cpu[1].model
    first = 1
    for i = 2:length(cpu)
        if model != cpu[i].model
            _cpu_summary(io, cpu, first, i-1)
            first = i
        end
    end
    _cpu_summary(io, cpu, first, length(cpu))
end

"""
    Sys.cpu_info()

Return a vector of `CPUinfo` objects, where each object represents information about a CPU core.

This is pretty-printed in a tabular format by `Sys.cpu_summary`, which is included in the output
of `versioninfo(verbose=true)`, so most users will not need to access the `CPUinfo`
data structures directly.

The function provides information about each CPU, including model, speed, and usage statistics such as user time, nice time, system time, idle time, and interrupt time.

"""
function cpu_info()
    UVcpus = Ref{Ptr{UV_cpu_info_t}}()
    count = Ref{Int32}()
    err = ccall(:uv_cpu_info, Int32, (Ptr{Ptr{UV_cpu_info_t}}, Ptr{Int32}), UVcpus, count)
    Base.uv_error("uv_cpu_info", err)
    cpus = Vector{CPUinfo}(undef, count[])
    for i = 1:length(cpus)
        cpus[i] = CPUinfo(unsafe_load(UVcpus[], i))
    end
    ccall(:uv_free_cpu_info, Cvoid, (Ptr{UV_cpu_info_t}, Int32), UVcpus[], count[])
    return cpus
end

"""
    Sys.uptime()

Gets the current system uptime in seconds.
"""
function uptime()
    uptime_ = Ref{Float64}()
    err = ccall(:uv_uptime, Int32, (Ptr{Float64},), uptime_)
    Base.uv_error("uv_uptime", err)
    return uptime_[]
end

"""
    Sys.loadavg()

Get the load average. See: https://en.wikipedia.org/wiki/Load_(computing).
"""
function loadavg()
    loadavg_ = Vector{Float64}(undef, 3)
    ccall(:uv_loadavg, Cvoid, (Ptr{Float64},), loadavg_)
    return loadavg_
end

"""
    Sys.free_physical_memory()

Get the free memory of the system in bytes. The entire amount may not be available to the
current process; use `Sys.free_memory()` for the actually available amount.
"""
free_physical_memory() = ccall(:uv_get_free_memory, UInt64, ())

"""
    Sys.total_physical_memory()

Get the total memory in RAM (including that which is currently used) in bytes. The entire
amount may not be available to the current process; see `Sys.total_memory()`.
"""
total_physical_memory() = ccall(:uv_get_total_memory, UInt64, ())

"""
    Sys.free_memory()

Get the total free memory in RAM in bytes.
"""
free_memory() = ccall(:uv_get_available_memory, UInt64, ())

"""
    Sys.total_memory()

Get the total memory in RAM (including that which is currently used) in bytes.
This amount may be constrained, e.g., by Linux control groups. For the unconstrained
amount, see `Sys.total_physical_memory()`.
"""
function total_memory()
    constrained = ccall(:uv_get_constrained_memory, UInt64, ())
    physical = total_physical_memory()
    if 0 < constrained <= physical
        return constrained
    else
        return physical
    end
end

"""
    Sys.get_process_title()

Get the process title. On some systems, will always return an empty string.
"""
function get_process_title()
    buf = Vector{UInt8}(undef, 512)
    err = ccall(:uv_get_process_title, Cint, (Ptr{UInt8}, Cint), buf, 512)
    Base.uv_error("get_process_title", err)
    return unsafe_string(pointer(buf))
end

"""
    Sys.set_process_title(title::AbstractString)

Set the process title. No-op on some operating systems.
"""
function set_process_title(title::AbstractString)
    err = ccall(:uv_set_process_title, Cint, (Cstring,), title)
    Base.uv_error("set_process_title", err)
end

"""
    Sys.maxrss()

Get the maximum resident set size utilized in bytes.
See also:
    - man page of `getrusage`(2) on Linux and BSD.
    - Windows API `GetProcessMemoryInfo`.
"""
maxrss() = ccall(:jl_maxrss, Csize_t, ())

"""
    Sys.isunix([os])

Predicate for testing if the OS provides a Unix-like interface.
See documentation in [Handling Operating System Variation](@ref).
"""
function isunix(os::Symbol)
    if iswindows(os)
        return false
    elseif islinux(os) || isbsd(os)
        return true
    elseif os === :Emscripten
        # Emscripten implements the POSIX ABI and provides traditional
        # Unix-style operating system functions such as file system support.
        # Therefore, we consider it a unix, even though this need not be
        # generally true for a jsvm embedding.
        return true
    else
        throw(ArgumentError("unknown operating system \"$os\""))
    end
end

"""
    Sys.islinux([os])

Predicate for testing if the OS is a derivative of Linux.
See documentation in [Handling Operating System Variation](@ref).
"""
islinux(os::Symbol) = (os === :Linux)

"""
    Sys.isbsd([os])

Predicate for testing if the OS is a derivative of BSD.
See documentation in [Handling Operating System Variation](@ref).

!!! note
    The Darwin kernel descends from BSD, which means that `Sys.isbsd()` is
    `true` on macOS systems. To exclude macOS from a predicate, use
    `Sys.isbsd() && !Sys.isapple()`.
"""
isbsd(os::Symbol) = (isfreebsd(os) || isopenbsd(os) || isnetbsd(os) || isdragonfly(os) || isapple(os))

"""
    Sys.isfreebsd([os])

Predicate for testing if the OS is a derivative of FreeBSD.
See documentation in [Handling Operating System Variation](@ref).

!!! note
    Not to be confused with `Sys.isbsd()`, which is `true` on FreeBSD but also on
    other BSD-based systems. `Sys.isfreebsd()` refers only to FreeBSD.
!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.
"""
isfreebsd(os::Symbol) = (os === :FreeBSD)

"""
    Sys.isopenbsd([os])

Predicate for testing if the OS is a derivative of OpenBSD.
See documentation in [Handling Operating System Variation](@ref).

!!! note
    Not to be confused with `Sys.isbsd()`, which is `true` on OpenBSD but also on
    other BSD-based systems. `Sys.isopenbsd()` refers only to OpenBSD.
!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.
"""
isopenbsd(os::Symbol) = (os === :OpenBSD)

"""
    Sys.isnetbsd([os])

Predicate for testing if the OS is a derivative of NetBSD.
See documentation in [Handling Operating System Variation](@ref).

!!! note
    Not to be confused with `Sys.isbsd()`, which is `true` on NetBSD but also on
    other BSD-based systems. `Sys.isnetbsd()` refers only to NetBSD.
!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.
"""
isnetbsd(os::Symbol) = (os === :NetBSD)

"""
    Sys.isdragonfly([os])

Predicate for testing if the OS is a derivative of DragonFly BSD.
See documentation in [Handling Operating System Variation](@ref).

!!! note
    Not to be confused with `Sys.isbsd()`, which is `true` on DragonFly but also on
    other BSD-based systems. `Sys.isdragonfly()` refers only to DragonFly.
!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.
"""
isdragonfly(os::Symbol) = (os === :DragonFly)

"""
    Sys.iswindows([os])

Predicate for testing if the OS is a derivative of Microsoft Windows NT.
See documentation in [Handling Operating System Variation](@ref).
"""
iswindows(os::Symbol) = (os === :Windows || os === :NT)

"""
    Sys.isapple([os])

Predicate for testing if the OS is a derivative of Apple Macintosh OS X or Darwin.
See documentation in [Handling Operating System Variation](@ref).
"""
isapple(os::Symbol) = (os === :Apple || os === :Darwin)

"""
    Sys.isjsvm([os])

Predicate for testing if Julia is running in a JavaScript VM (JSVM),
including e.g. a WebAssembly JavaScript embedding in a web browser.

!!! compat "Julia 1.2"
    This function requires at least Julia 1.2.
"""
isjsvm(os::Symbol) = (os === :Emscripten)

"""
    Sys.detectwsl()

Runtime predicate for testing if Julia is running inside
Windows Subsystem for Linux (WSL).

!!! note
    Unlike `Sys.iswindows`, `Sys.islinux` etc., this is a runtime test, and thus
    cannot meaningfully be used in `@static if` constructs.

!!! compat "Julia 1.12"
    This function requires at least Julia 1.12.
"""
function detectwsl()
    # We use the same approach as canonical/snapd do to detect WSL
    islinux() && (
        isfile("/proc/sys/fs/binfmt_misc/WSLInterop")
        || isdir("/run/WSL")
    )
end

for f in (:isunix, :islinux, :isbsd, :isapple, :iswindows, :isfreebsd, :isopenbsd, :isnetbsd, :isdragonfly, :isjsvm)
    @eval $f() = $(getfield(@__MODULE__, f)(KERNEL))
end

if iswindows()
    function windows_version()
        verinfo = ccall(:GetVersion, UInt32, ())
        VersionNumber(verinfo & 0xFF, (verinfo >> 8) & 0xFF, verinfo >> 16)
    end
else
    windows_version() = v"0.0"
end

"""
    Sys.windows_version()

Return the version number for the Windows NT Kernel as a `VersionNumber`,
i.e. `v"major.minor.build"`, or `v"0.0.0"` if this is not running on Windows.
"""
windows_version

const WINDOWS_VISTA_VER = v"6.0"

const isexecutable = Base.isexecutable
const isreadable   = Base.isreadable
const iswritable   = Base.iswritable

"""
    Sys.which(program_name::String)

Given a program name, search the current `PATH` to find the first binary with
the proper executable permissions that can be run and return an absolute path
to it, or return `nothing` if no such program is available. If a path with
a directory in it is passed in for `program_name`, tests that exact path
for executable permissions only (with `.exe` and `.com` extensions added on
Windows platforms); no searching of `PATH` is performed.
"""
function which(program_name::String)
    if isempty(program_name)
       return nothing
    end
    # Build a list of program names that we're going to try
    program_names = String[]
    base_pname = basename(program_name)
    if iswindows()
        # If the file already has an extension, try that name first
        if !isempty(splitext(base_pname)[2])
            push!(program_names, base_pname)
        end

        # But also try appending .exe and .com`
        for pe in (".exe", ".com")
            push!(program_names, string(base_pname, pe))
        end
    else
        # On non-windows, we just always search for what we've been given
        push!(program_names, base_pname)
    end

    path_dirs = String[]
    program_dirname = dirname(program_name)
    # If we've been given a path that has a directory name in it, then we
    # check to see if that path exists.  Otherwise, we search the PATH.
    if isempty(program_dirname)
        # If we have been given just a program name (not a relative or absolute
        # path) then we should search `PATH` for it here:
        pathsep = iswindows() ? ';' : ':'
        path_dirs = map(abspath, eachsplit(get(ENV, "PATH", ""), pathsep))

        # On windows we always check the current directory as well
        if iswindows()
            pushfirst!(path_dirs, pwd())
        end
    else
        push!(path_dirs, abspath(program_dirname))
    end

    # Here we combine our directories with our program names, searching for the
    # first match among all combinations.
    for path_dir in path_dirs
        for pname in program_names
            program_path = joinpath(path_dir, pname)
            try
                # If we find something that matches our name and we can execute
                if isfile(program_path) && isexecutable(program_path)
                    return program_path
                end
            catch e
                # If we encounter a permission error, we skip this directory
                # and continue to the next directory in the PATH variable.
                if isa(e, Base.IOError) && e.code == Base.UV_EACCES
                    # Permission denied, continue searching
                    continue
                else
                    # Rethrow the exception if it's not a permission error
                    rethrow(e)
                end
            end
        end
    end

    # If we couldn't find anything, don't return anything
    nothing
end
which(program_name::AbstractString) = which(String(program_name))

"""
    Sys.username()::String

Return the username for the current user. If the username cannot be determined
or is empty, this function throws an error.

To retrieve a username that is overridable via an environment variable,
e.g., `USER`, consider using
```julia
user = get(Sys.username, ENV, "USER")
```

!!! compat "Julia 1.11"
    This function requires at least Julia 1.11.

See also [`homedir`](@ref).
"""
function username()
    pw = Libc.getpw()
    isempty(pw.username) && Base.uv_error("username", Base.UV_ENOENT)
    return pw.username
end

end # module Sys
