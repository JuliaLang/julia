# This file is a part of Julia. License is MIT: http://julialang.org/license

module Sys

export  CPU_CORES,
        WORD_SIZE,
        ARCH,
        MACHINE,
        KERNEL,
        JIT,
        cpu_info,
        cpu_name,
        cpu_summary,
        uptime,
        loadavg,
        free_memory,
        total_memory

import ..Base: show

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
    # set CPU core count
    global CPU_CORES =
        haskey(ENV,"JULIA_CPU_CORES") ? parse(Int,ENV["JULIA_CPU_CORES"]) :
                                        Int(ccall(:jl_cpu_cores, Int32, ()))
    global SC_CLK_TCK = ccall(:jl_SC_CLK_TCK, Clong, ())
    global cpu_name = ccall(:jl_get_cpu_name, Ref{String}, ())
    global JIT = ccall(:jl_get_JIT, Ref{String}, ())
end

type UV_cpu_info_t
    model::Ptr{UInt8}
    speed::Int32
    cpu_times!user::UInt64
    cpu_times!nice::UInt64
    cpu_times!sys::UInt64
    cpu_times!idle::UInt64
    cpu_times!irq::UInt64
end
type CPUinfo
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

show(io::IO, info::CPUinfo) = Base._show_cpuinfo(io, info, true, "    ")

function _cpu_summary(io::IO, cpu::Array{CPUinfo}, i, j)
    if j-i < 9
        header = true
        for x = i:j
            header || println(io)
            Base._show_cpuinfo(io, cpu[x], header, "#$(x-i+1) ")
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
        Base._show_cpuinfo(io, summary, true, "#1-$(count) ")
    end
    println(io)
end

function cpu_summary(io::IO=STDOUT, cpu::Array{CPUinfo}=cpu_info())
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
    UVcpus = Array{Ptr{UV_cpu_info_t}}(1)
    count = Array{Int32}(1)
    Base.uv_error("uv_cpu_info",ccall(:uv_cpu_info, Int32, (Ptr{Ptr{UV_cpu_info_t}}, Ptr{Int32}), UVcpus, count))
    cpus = Array{CPUinfo}(count[1])
    for i = 1:length(cpus)
        cpus[i] = CPUinfo(unsafe_load(UVcpus[1], i))
    end
    ccall(:uv_free_cpu_info, Void, (Ptr{UV_cpu_info_t}, Int32), UVcpus[1], count[1])
    return cpus
end

function uptime()
    uptime_ = Array{Float64}(1)
    Base.uv_error("uv_uptime",ccall(:uv_uptime, Int32, (Ptr{Float64},), uptime_))
    return uptime_[1]
end

function loadavg()
    loadavg_ = Array{Float64}(3)
    ccall(:uv_loadavg, Void, (Ptr{Float64},), loadavg_)
    return loadavg_
end

free_memory() = ccall(:uv_get_free_memory, UInt64, ())
total_memory() = ccall(:uv_get_total_memory, UInt64, ())

"""
    Sys.get_process_title()

Get the process title. On some systems, will always return empty string. (not exported)
"""
function get_process_title()
    buf = zeros(UInt8, 512)
    err = ccall(:uv_get_process_title, Cint, (Ptr{UInt8}, Cint), buf, 512)
    Base.uv_error("get_process_title", err)
    return unsafe_string(pointer(buf))
end

"""
    Sys.set_process_title(title::AbstractString)

Set the process title. No-op on some operating systems. (not exported)
"""
function set_process_title(title::AbstractString)
    err = ccall(:uv_set_process_title, Cint, (Cstring,), title)
    Base.uv_error("set_process_title", err)
end

maxrss() = ccall(:jl_maxrss, Csize_t, ())

if is_windows()
    function windows_version()
        verinfo = ccall(:GetVersion, UInt32, ())
        (Int(verinfo & 0xFF), Int((verinfo >> 8) & 0xFF))
    end
else
    windows_version() = (0, 0)
end
"""
    windows_version()

Returns the version number for the Windows NT Kernel as a (major, minor) pair,
or `(0, 0)` if this is not running on Windows.
"""
windows_version

const WINDOWS_VISTA_VER = (6, 0)

end # module Sys
