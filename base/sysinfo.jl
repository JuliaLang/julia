module Sys

export  CPU_CORES,
        OS_NAME,
        WORD_SIZE,
        ARCH,
        MACHINE,
        cpu_info,
        cpu_summary,
        uptime,
        loadavg,
        free_memory,
        total_memory,
        shlib_ext

import ..Base: WORD_SIZE, OS_NAME, ARCH, MACHINE
import ..Base: show

function init()
    # set CPU core count
    global const CPU_CORES = int(
        haskey(ENV,"JULIA_CPU_CORES") ?
        ENV["JULIA_CPU_CORES"] :
        ccall(:jl_cpu_cores, Int32, ())
    )
    global const SC_CLK_TCK = ccall(:jl_SC_CLK_TCK, Clong, ())
end

type UV_cpu_info_t
    model::Ptr{Uint8}
    speed::Int32
    cpu_times!user::Uint64
    cpu_times!nice::Uint64
    cpu_times!sys::Uint64
    cpu_times!idle::Uint64
    cpu_times!irq::Uint64
end
type CPUinfo
    model::ASCIIString
    speed::Int32
    cpu_times!user::Uint64
    cpu_times!nice::Uint64
    cpu_times!sys::Uint64
    cpu_times!idle::Uint64
    cpu_times!irq::Uint64
    CPUinfo(model,speed,u,n,s,id,ir)=new(model,speed,u,n,s,id,ir)
end
CPUinfo(info::UV_cpu_info_t) = CPUinfo(bytestring(info.model), info.speed,
    info.cpu_times!user, info.cpu_times!nice, info.cpu_times!sys,
    info.cpu_times!idle, info.cpu_times!irq)

function show(io::IO, info::CPUinfo, header::Bool=true, prefix::String="    ")
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
    cpus
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

free_memory() = ccall(:uv_get_free_memory, Uint64, ())
total_memory() = ccall(:uv_get_total_memory, Uint64, ())

if OS_NAME === :Darwin
    const shlib_ext = "dylib"
elseif OS_NAME === :Windows
    const shlib_ext = "dll"
else
    #assume OS_NAME === :Linux, or similar
    const shlib_ext = "so"
end

end
