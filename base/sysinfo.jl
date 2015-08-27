# This file is a part of Julia. License is MIT: http://julialang.org/license

module Sys

export  CPU_CORES,
        OS_NAME,
        WORD_SIZE,
        ARCH,
        MACHINE,
        cpu_info,
        cpu_name,
        cpu_summary,
        uptime,
        loadavg,
        free_memory,
        total_memory

import ..Base: WORD_SIZE, OS_NAME, ARCH, MACHINE
import ..Base: show, uv_error

global CPU_CORES

function init_sysinfo()
    # set CPU core count
    global const CPU_CORES =
        haskey(ENV,"JULIA_CPU_CORES") ? parse(Int,ENV["JULIA_CPU_CORES"]) :
                                        Int(ccall(:jl_cpu_cores, Int32, ()))
    global const SC_CLK_TCK = ccall(:jl_SC_CLK_TCK, Clong, ())
    global const cpu_name = ccall(:jl_get_cpu_name, Any, ())::UTF8String
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
    model::UTF8String
    speed::Int32
    cpu_times!user::UInt64
    cpu_times!nice::UInt64
    cpu_times!sys::UInt64
    cpu_times!idle::UInt64
    cpu_times!irq::UInt64
    CPUinfo(model,speed,u,n,s,id,ir)=new(model,speed,u,n,s,id,ir)
end
CPUinfo(info::UV_cpu_info_t) = CPUinfo(bytestring(info.model), info.speed,
    info.cpu_times!user, info.cpu_times!nice, info.cpu_times!sys,
    info.cpu_times!idle, info.cpu_times!irq)

function show(io::IO, info::CPUinfo, header::Bool=true, prefix::AbstractString="    ")
    tck = SC_CLK_TCK
    if header
        println(io, info.model, ": ")
        print(io, " "^length(prefix))
        if tck > 0
            @printf io "    %5s    %9s    %9s    %9s    %9s    %9s\n" "speed" "user" "nice" "sys" "idle" "irq"
        else
            @printf io "    %5s    %9s  %9s  %9s  %9s  %9s ticks\n" "speed" "user" "nice" "sys" "idle" "irq"
        end
    end
    print(io, prefix)
    if tck > 0
        @printf io "%5d MHz  %9d s  %9d s  %9d s  %9d s  %9d s" info.speed info.cpu_times!user/tck info.cpu_times!nice/tck info.cpu_times!sys/tck info.cpu_times!idle/tck info.cpu_times!irq/tck
    else
        @printf io "%5d MHz  %9d  %9d  %9d  %9d  %9d ticks" info.speed info.cpu_times!user info.cpu_times!nice info.cpu_times!sys info.cpu_times!idle info.cpu_times!irq
    end
end

function _cpu_summary(io::IO, cpu::Array{CPUinfo}, i, j)
    if j-i < 9
        header = true
        for x = i:j
            if header == false println(io) end
            show(io,cpu[x],header,"#$(x-i+1) ")
            header = false
        end
    else
        summary = CPUinfo(cpu[i].model,0,0,0,0,0,0)
        count = j-i+1
        for x = i:j
            summary.speed += cpu[i].speed
            summary.cpu_times!user += cpu[x].cpu_times!user
            summary.cpu_times!nice += cpu[x].cpu_times!nice
            summary.cpu_times!sys += cpu[x].cpu_times!sys
            summary.cpu_times!idle += cpu[x].cpu_times!idle
            summary.cpu_times!irq += cpu[x].cpu_times!irq
        end
        summary.speed = div(summary.speed,count)
        show(io,summary,true,"#1-$(count) ")
    end
    println(io)
end

function cpu_summary(io::IO=STDOUT, cpu::Array{CPUinfo}=cpu_info())
    model = cpu[1].model
    first = 1
    for i = 2:length(cpu)
        if model != cpu[i].model
            _cpu_summary(io,cpu,first,i-1)
            first = i
        end
    end
    _cpu_summary(io,cpu,first,length(cpu))
end

function cpu_info()
    UVcpus = Array(Ptr{UV_cpu_info_t},1)
    count = Array(Int32,1)
    uv_error("uv_cpu_info",ccall(:uv_cpu_info, Int32, (Ptr{Ptr{UV_cpu_info_t}}, Ptr{Int32}), UVcpus, count))
    cpus = Array(CPUinfo,count[1])
    for i = 1:length(cpus)
        cpus[i] = CPUinfo(unsafe_load(UVcpus[1],i))
    end
    ccall(:uv_free_cpu_info, Void, (Ptr{UV_cpu_info_t}, Int32), UVcpus[1], count[1])
    return cpus
end

function uptime()
    uptime_ = Array(Float64,1)
    uv_error("uv_uptime",ccall(:uv_uptime, Int32, (Ptr{Float64},), uptime_))
    return uptime_[1]
end

function loadavg()
    loadavg_ = Array(Float64,3)
    ccall(:uv_loadavg, Void, (Ptr{Float64},), loadavg_)
    return loadavg_
end

free_memory() = ccall(:uv_get_free_memory, UInt64, ())
total_memory() = ccall(:uv_get_total_memory, UInt64, ())

function get_process_title()
    buf = zeros(UInt8, 512)
    err = ccall(:uv_get_process_title, Cint, (Ptr{UInt8}, Cint), buf, 512)
    uv_error("get_process_title", err)
    return bytestring(pointer(buf))
end
function set_process_title(title::AbstractString)
    err = ccall(:uv_set_process_title, Cint, (Cstring,), title)
    uv_error("set_process_title", err)
end

end # module Sys
