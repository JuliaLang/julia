# timing

# system date in seconds
time() = ccall(:clock_now, Float64, ())

# high-resolution relative time, in nanoseconds
time_ns() = ccall(:jl_hrtime, Uint64, ())

function tic()
    t0 = time_ns()
    task_local_storage(:TIMERS, (t0, get(task_local_storage(), :TIMERS, ())))
    return t0
end

function toq()
    t1 = time_ns()
    timers = get(task_local_storage(), :TIMERS, ())
    if is(timers,())
        error("toc() without tic()")
    end
    t0 = timers[1]
    task_local_storage(:TIMERS, timers[2])
    (t1-t0)/1e9
end

function toc()
    t = toq()
    println("elapsed time: ", t, " seconds")
    return t
end

# print elapsed time, return expression value
macro time(ex)
    quote
        local t0 = time_ns()
        local val = $(esc(ex))
        local t1 = time_ns()
        println("elapsed time: ", (t1-t0)/1e9, " seconds")
        val
    end
end

# print nothing, return elapsed time
macro elapsed(ex)
    quote
        local t0 = time_ns()
        local val = $(esc(ex))
        (time_ns()-t0)/1e9
    end
end

# print nothing, return value & elapsed time
macro timed(ex)
    quote
        local t0 = time_ns()
        local val = $(esc(ex))
        val, (time_ns()-t0)/1e9
    end
end

function peakflops(n)
    a = rand(n,n)
    t = @elapsed a*a
    t = @elapsed a*a
    floprate = (2.0*float64(n)^3/t)
    println("The peak flop rate is ", floprate*1e-9, " gigaflops")
    floprate
end

peakflops() = peakflops(2000)

# searching definitions

function whicht(f, types)
    for m = methods(f, types)
        if isa(m[3],LambdaStaticData)
            lsd = m[3]::LambdaStaticData
            d = f.env.defs
            while !is(d,())
                if is(d.func.code, lsd)
                    print(OUTPUT_STREAM, f.env.name)
                    show(OUTPUT_STREAM, d); println(OUTPUT_STREAM)
                    return
                end
                d = d.next
            end
        end
    end
end

which(f, args...) = whicht(f, map(a->(isa(a,Type) ? Type{a} : typeof(a)), args))

macro which(ex)
    ex = expand(ex)
    exret = Expr(:call, :error, "expression is not a function call")
    if !isa(ex, Expr)
        # do nothing -> error
    elseif ex.head == :call
        exret = Expr(:call, :which, map(esc, ex.args)...)
    elseif ex.head == :body
        a1 = ex.args[1]
        if isa(a1, Expr) && a1.head == :call
            a11 = a1.args[1]
            if isa(a11, TopNode) && a11.name == :setindex!
                exret = Expr(:call, :which, a11, map(esc, a1.args[2:end])...)
            end
        end
    elseif ex.head == :thunk
        exret = Expr(:call, :error, "expression is not a function call, or is too complex for @which to analyze; "
                                  * "break it down to simpler parts if possible")
    end
    exret
end

# source files, editing

function find_source_file(file)
    if file[1]!='/' && !is_file_readable(file)
        file2 = find_in_path(file)
        if is_file_readable(file2)
            return file2
        else
            file2 = "$JULIA_HOME/../share/julia/base/$file"
            if is_file_readable(file2)
                return file2
            end
        end
    end
    return file
end

edit(file::String) = edit(file, 1)
function edit(file::String, line::Integer)
    editor = get(ENV, "JULIA_EDITOR", "emacs")
    issrc = length(file)>2 && file[end-2:end] == ".jl"
    if issrc
        file = find_source_file(file)
    end
    if editor == "emacs"
        if issrc
            jmode = "$JULIA_HOME/../../contrib/julia-mode.el"
            run(`emacs $file --eval "(progn
                                     (require 'julia-mode \"$jmode\")
                                     (julia-mode)
                                     (goto-line $line))"`)
        else
            run(`emacs $file --eval "(goto-line $line)"`)
        end
    elseif editor == "vim"
        run(`vim $file +$line`)
    elseif editor == "textmate"
        run(`mate $file -l $line`)
    elseif editor == "subl"
        run(`subl $file:$line`)
    elseif editor == "notepad"
        run(`notepad $file`)
    elseif editor == "start" || editor == "open"
        if OS_NAME == :Windows
            run(`start /b $file`)
        elseif OS_NAME == :Darwin
            run(`open -t $file`)
        else
            error("Don't know how to launch the default editor on your platform")
        end
    else
        error("Invalid JULIA_EDITOR value: $(repr(editor))")
    end
    nothing
end
edit(file::String) = edit(file, 1)

function less(file::String, line::Integer)
    pager = get(ENV, "PAGER", "less")
    run(`$pager +$(line)g $file`)
end
less(file::String) = less(file, 1)

edit(f::Function)    = edit(functionloc(f)...)
edit(f::Function, t) = edit(functionloc(f,t)...)
less(f::Function)    = less(functionloc(f)...)
less(f::Function, t) = less(functionloc(f,t)...)

# print a warning only once

const have_warned = (ByteString=>Bool)[]
function warn_once(msg::String...)
    msg = bytestring(msg...)
    has(have_warned,msg) && return
    have_warned[msg] = true
    warn(msg)
end

# system information

versioninfo() = versioninfo(false)
versioninfo(verbose::Bool) = versioninfo(OUTPUT_STREAM, verbose)
function versioninfo(io::IO, verbose::Bool)
    println(io,             "Julia $version_string")
    println(io,             commit_string)
    println(io,             "Platform Info:")
    println(io,             "  OS_NAME: ", OS_NAME)
    println(io,             "  WORD_SIZE: ", WORD_SIZE)
    if verbose
        lsb = readchomp(ignorestatus(`lsb_release -ds`) .> SpawnNullStream())
        if lsb != ""
            println(io,     "           ", lsb)
        end
        println(io,         "  uname: ",readchomp(`uname -mprsv`))
        println(io,         "Memory: $(total_memory()/2^30) GB ($(free_memory()/2^20) MB free)")
        try println(io,     "Uptime: $(uptime()) sec") catch end
        print(io,           "Load Avg: ")
        print_matrix(io,    Base.loadavg()')
        println(io          )
        println(io,         cpu_info())
    end
    if Base.libblas_name == "libopenblas"
        openblas_config = chop(bytestring( ccall((:openblas_get_config, Base.libblas_name), Ptr{Uint8}, () )))
        println(io,         "  BLAS: ",libblas_name, " (", openblas_config, ")")
    else
        println(io,         "  BLAS: ",libblas_name)
    end
    println(io,             "  LAPACK: ",liblapack_name)
    println(io,             "  LIBM: ",libm_name)
    if verbose
        println(io,         "Environment:")
        for (k,v) in ENV
            if !is(match(r"JULIA|PATH|FLAG|^TERM$|HOME",k), nothing)
                println(io, "  $(k) = $(v)")
            end
        end
        println(io          )
        println(io,         "Packages Installed:")
        Pkg.status(io       )
    end
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
    SC_CLK_TCK::Int
    CPUinfo(model,speed,u,n,s,id,ir,ticks)=new(model,speed,u,n,s,id,ir,ticks)
end
CPUinfo(info::UV_cpu_info_t, ticks) = CPUinfo(bytestring(info.model), info.speed,
    info.cpu_times!user, info.cpu_times!nice, info.cpu_times!sys,
    info.cpu_times!idle, info.cpu_times!irq, ticks)

show(io::IO, cpu::CPUinfo) = show(io, cpu, true, "    ")
function show(io::IO, info::CPUinfo, header::Bool, prefix::String)
    tck = info.SC_CLK_TCK 
    if header
        println(io, info.model, ": ")
        print(" "^length(prefix))
        if tck > 0
            @printf io "    %5s    %9s    %9s    %9s    %9s    %9s\n" "speed" "user" "nice" "sys" "idle" "irq"
        else
            @printf io "    %5s    %9s  %9s  %9s  %9s  %9s ticks\n" "speed" "user" "nice" "sys" "idle" "irq"
        end
    end
    print(prefix)
    if tck > 0
        @printf io "%5d MHz  %9d s  %9d s  %9d s  %9d s  %9d s" info.speed info.cpu_times!user/tck info.cpu_times!nice/tck info.cpu_times!sys/tck info.cpu_times!idle/tck info.cpu_times!irq/tck
    else
        @printf io "%5d MHz  %9d  %9d  %9d  %9d  %9d ticks" info.speed info.cpu_times!user info.cpu_times!nice info.cpu_times!sys info.cpu_times!idle info.cpu_times!irq
    end
end
function cpu_summary(io::IO, cpu::Array{CPUinfo}, i, j)
    if j-i < 9
        header = true
        for x = i:j
            if header == false println() end
            show(io,cpu[x],header,"#$(x-i+1) ")
            header = false
        end
    else
        summary = CPUinfo(cpu[i].model,0,0,0,0,0,0,cpu[i].SC_CLK_TCK)
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
function show(io::IO, cpu::Array{CPUinfo})
    model = cpu[1].model
    first = 1
    for i = 2:length(cpu)
        if model != cpu[i].model
            cpu_summary(io,cpu,first,i-1)
            first = i
        end
    end
    cpu_summary(io,cpu,first,length(cpu))
end
repl_show(io::IO, cpu::Array{CPUinfo}) = show(io, cpu)
function cpu_info()
    SC_CLK_TCK = ccall(:SC_CLK_TCK, Int, ())
    UVcpus = Array(Ptr{UV_cpu_info_t},1)
    count = Array(Int32,1)
    uv_error("uv_cpu_info",ccall(:uv_cpu_info, UV_error_t, (Ptr{Ptr{UV_cpu_info_t}}, Ptr{Int32}), UVcpus, count))
    cpus = Array(CPUinfo,count[1])
    for i = 1:length(cpus)
        cpus[i] = CPUinfo(unsafe_ref(UVcpus[1],i),SC_CLK_TCK)
    end
    ccall(:uv_free_cpu_info, Void, (Ptr{UV_cpu_info_t}, Int32), UVcpus[1], count[1])
    cpus
end

function uptime()
    uptime_ = Array(Float64,1)
    uv_error("uv_uptime",ccall(:uv_uptime, UV_error_t, (Ptr{Float64},), uptime_))
    return uptime_[1]
end

function loadavg()
    loadavg_ = Array(Float64,3)
    ccall(:uv_loadavg, Void, (Ptr{Float64},), loadavg_)
    return loadavg_
end

free_memory() = ccall(:uv_get_free_memory, Uint64, ())
total_memory() = ccall(:uv_get_total_memory, Uint64, ())


# `methodswith` -- shows a list of methods using the type given

function methodswith(io::IO, t::Type, m::Module, showparents::Bool)
    for nm in names(m)
        try
           mt = eval(nm)
           d = mt.env.defs
           while !is(d,())
               if any(map(x -> x == t || (showparents && t <: x && x != Any && x != ANY && !isa(x, TypeVar)), d.sig))
                   print(io, nm)
                   show(io, d)
                   println(io)
               end
               d = d.next
           end
        end
    end
end

methodswith(t::Type, m::Module, showparents::Bool) = methodswith(OUTPUT_STREAM, t, m, showparents)
methodswith(t::Type, showparents::Bool) = methodswith(OUTPUT_STREAM, t, showparents)
methodswith(t::Type, m::Module) = methodswith(OUTPUT_STREAM, t, m, false)
methodswith(t::Type) = methodswith(OUTPUT_STREAM, t, false)
function methodswith(io::IO, t::Type, showparents::Bool)
    mainmod = ccall(:jl_get_current_module, Any, ())::Module
    # find modules in Main
    for nm in names(mainmod)
        if isdefined(mainmod,nm)
            mod = eval(mainmod, nm)
            if isa(mod, Module)
                methodswith(io, t, mod, showparents)
            end
        end
    end
end
