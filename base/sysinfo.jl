# This file is a part of Julia. License is MIT: https://julialang.org/license

module Sys
@doc """
Provide methods for retrieving information about hardware and the operating system.
""" Sys

export BINDIR,
       CPU_CORES,
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
       isapple,
       isbsd,
       islinux,
       isunix,
       iswindows

import ..Base: show

"""
    Sys.BINDIR

A string containing the full path to the directory containing the `julia` executable.
"""
BINDIR = ccall(:jl_get_julia_bindir, Any, ())

_early_init() = global BINDIR = ccall(:jl_get_julia_bindir, Any, ())

# helper to avoid triggering precompile warnings

global CPU_CORES
"""
    Sys.CPU_CORES

The number of logical CPU cores available in the system.

See the Hwloc.jl package for extended information, including number of physical cores.
"""
:CPU_CORES

"""
    Sys.ARCH

A symbol representing the architecture of the build configuration.
"""
const ARCH = ccall(:jl_get_ARCH, Any, ())


"""
    Sys.KERNEL

A symbol representing the name of the operating system, as returned by `uname` of the build configuration.
"""
const KERNEL = ccall(:jl_get_UNAME, Any, ())

"""
    Sys.MACHINE

A string containing the build triple.
"""
const MACHINE = Base.MACHINE

"""
    Sys.WORD_SIZE

Standard word size on the current machine, in bits.
"""
const WORD_SIZE = Core.sizeof(Int) * 8

function __init__()
    global CPU_CORES =
        haskey(ENV,"JULIA_CPU_CORES") ? parse(Int,ENV["JULIA_CPU_CORES"]) :
                                        Int(ccall(:jl_cpu_cores, Int32, ()))
    global SC_CLK_TCK = ccall(:jl_SC_CLK_TCK, Clong, ())
    global CPU_NAME = ccall(:jl_get_cpu_name, Ref{String}, ())
    global JIT = ccall(:jl_get_JIT, Ref{String}, ())
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

show(io::IO, info::CPUinfo) = _show_cpuinfo(io, info, true, "    ")

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

function cpu_info()
    UVcpus = Ref{Ptr{UV_cpu_info_t}}()
    count = Ref{Int32}()
    Base.uv_error("uv_cpu_info",ccall(:uv_cpu_info, Int32, (Ptr{Ptr{UV_cpu_info_t}}, Ptr{Int32}), UVcpus, count))
    cpus = Vector{CPUinfo}(uninitialized, count[])
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
    Base.uv_error("uv_uptime",ccall(:uv_uptime, Int32, (Ptr{Float64},), uptime_))
    return uptime_[]
end

"""
    Sys.loadavg()

Get the load average. See: https://en.wikipedia.org/wiki/Load_(computing).
"""
function loadavg()
    loadavg_ = Vector{Float64}(uninitialized, 3)
    ccall(:uv_loadavg, Cvoid, (Ptr{Float64},), loadavg_)
    return loadavg_
end

free_memory() = ccall(:uv_get_free_memory, UInt64, ())
total_memory() = ccall(:uv_get_total_memory, UInt64, ())

"""
    Sys.get_process_title()

Get the process title. On some systems, will always return an empty string.
"""
function get_process_title()
    buf = Vector{UInt8}(uninitialized, 512)
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
    - man page of getrusage(2) on Linux and FreeBSD.
    - windows api `GetProcessMemoryInfo`
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
    else
        throw(ArgumentError("unknown operating system \"$os\""))
    end
end

"""
    Sys.islinux([os])

Predicate for testing if the OS is a derivative of Linux.
See documentation in [Handling Operating System Variation](@ref).
"""
islinux(os::Symbol) = (os == :Linux)

"""
    Sys.isbsd([os])

Predicate for testing if the OS is a derivative of BSD.
See documentation in [Handling Operating System Variation](@ref).

!!! note
    The Darwin kernel descends from BSD, which means that `Sys.isbsd()` is
    `true` on macOS systems. To exclude macOS from a predicate, use
    `Sys.isbsd() && !Sys.isapple()`.
"""
isbsd(os::Symbol) = (os == :FreeBSD || os == :OpenBSD || os == :NetBSD || os == :DragonFly || os == :Darwin || os == :Apple)

"""
    Sys.iswindows([os])

Predicate for testing if the OS is a derivative of Microsoft Windows NT.
See documentation in [Handling Operating System Variation](@ref).
"""
iswindows(os::Symbol) = (os == :Windows || os == :NT)

"""
    Sys.isapple([os])

Predicate for testing if the OS is a derivative of Apple Macintosh OS X or Darwin.
See documentation in [Handling Operating System Variation](@ref).
"""
isapple(os::Symbol) = (os == :Apple || os == :Darwin)

for f in (:isunix, :islinux, :isbsd, :isapple, :iswindows)
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

Returns the version number for the Windows NT Kernel as a `VersionNumber`,
i.e. `v"major.minor.build"`, or `v"0.0.0"` if this is not running on Windows.
"""
windows_version

const WINDOWS_VISTA_VER = v"6.0"

end # module Sys
