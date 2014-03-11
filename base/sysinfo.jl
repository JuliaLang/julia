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
        dlext,
	shlib_ext,
        dllist,
        dlpath

import ..Base: WORD_SIZE, OS_NAME, ARCH, MACHINE
import ..Base: show, uv_error

global CPU_CORES

function init_sysinfo()
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
    const dlext = "dylib"
elseif OS_NAME === :Windows
    const dlext = "dll"
else
    #assume OS_NAME === :Linux, or similar
    const dlext = "so"
end

# This is deprecated!  use dlext instead!
const shlib_ext = dlext

@linux_only begin
    immutable dl_phdr_info
        # Base address of object
        addr::Cuint

        # Null-terminated name of object
        name::Ptr{Uint8}

        # Pointer to array of ELF program headers for this object
        phdr::Ptr{Void}

        # Number of program headers for this object
        phnum::Cshort
    end

    # This callback function called by dl_iterate_phdr() on Linux
    function dl_phdr_info_callback( di_ptr::Ptr{dl_phdr_info}, size::Csize_t, dynamic_libraries_ptr::Ptr{Array{String,1}} )
        di = unsafe_load(di_ptr)

        # Skip over objects without a path (as they represent this own object)
        name = bytestring(di.name)
        if !isempty(name)
            dynamic_libraries = unsafe_pointer_to_objref( dynamic_libraries_ptr )
            push!(dynamic_libraries, name )
        end
        convert(Cint, 0)::Cint
    end
end #@linux_only

function dllist()
    dynamic_libraries = Array(String,0)

    @linux_only begin
        const callback = cfunction(dl_phdr_info_callback, Cint, (Ptr{dl_phdr_info}, Csize_t, Ptr{Array{String,1}} ))
        ccall( cglobal("dl_iterate_phdr"), Cint, (Ptr{Void}, Ptr{Void}), callback, pointer_from_objref(dynamic_libraries) )
    end

    @osx_only begin
        numImages = ccall( cglobal("_dyld_image_count"), Cint, (), )

        # start at 1 instead of 0 to skip self
        for i in 1:numImages-1
            name = bytestring(ccall( cglobal("_dyld_get_image_name"), Ptr{Uint8}, (Uint32,), uint32(i)))
            push!(dynamic_libraries, name)
        end
    end

    dynamic_libraries
end

function dlpath( handle::Ptr{Void} )
    return bytestring(ccall( :jl_pathname_for_handle, Ptr{Uint8}, (Ptr{Void},), handle ))
end

function dlpath( libname::String )
    handle = dlopen(libname)
    path = dlpath( handle )
    dlclose(handle)
    return path
end

end
