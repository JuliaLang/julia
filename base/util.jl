# flush C stdio output from external libraries:

flush_cstdio() = ccall(:jl_flush_cstdio, Void, ())

# timing

# system date in seconds
time() = ccall(:clock_now, Float64, ())

# high-resolution relative time, in nanoseconds
time_ns() = ccall(:jl_hrtime, Uint64, ())

# total number of bytes allocated so far
gc_bytes() = ccall(:jl_gc_total_bytes, Csize_t, ())

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
        local b0 = gc_bytes()
        local t0 = time_ns()
        local val = $(esc(ex))
        local t1 = time_ns()
        local b1 = gc_bytes()
        println("elapsed time: ", (t1-t0)/1e9, " seconds (", int(b1-b0), " bytes allocated)")
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

# measure bytes allocated without any contamination from compilation
macro allocated(ex)
    quote
        let
            local f
            function f()
                b0 = gc_bytes()
                $(esc(ex))
                b1 = gc_bytes()
                int(b1-b0)
            end
            f()
        end
    end
end

# print nothing, return value, elapsed time & bytes allocated
macro timed(ex)
    quote
        local b0 = gc_bytes()
        local t0 = time_ns()
        local val = $(esc(ex))
        local t1 = time_ns()
        local b1 = gc_bytes()
        val, (t1-t0)/1e9, int(b1-b0)
    end
end

# searching definitions

function whicht(f, types)
    for m in methods(f, types)
        lsd = m.func.code::LambdaStaticData
        d = f.env.defs
        while !is(d,())
            if is(d.func.code, lsd)
                print(STDOUT, f.env.name)
                show(STDOUT, d); println(STDOUT)
                return
            end
            d = d.next
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
            if a11 == :setindex!
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
        if file2 != nothing
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

function edit(file::String, line::Integer)
    if OS_NAME == :Windows || OS_NAME == :Darwin
        default_editor = "open"
    elseif isreadable("/etc/alternatives/editor")
        default_editor = "/etc/alternatives/editor"
    else
        default_editor = "emacs"
    end
    envvar = haskey(ENV,"JULIA_EDITOR") ? "JULIA_EDITOR" : "EDITOR"
    editor = get(ENV, envvar, default_editor)
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
    elseif editor == "textmate" || editor == "mate"
        spawn(`mate $file -l $line`)
    elseif editor == "subl"
        spawn(`subl $file:$line`)
    elseif OS_NAME == :Windows && (editor == "start" || editor == "open")
        spawn(`start /b $file`)
    elseif OS_NAME == :Darwin && (editor == "start" || editor == "open")
        spawn(`open -t $file`)
    elseif editor == "kate"
        spawn(`kate $file -l $line`)
    else
        run(`$(shell_split(editor)) $file`)
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
function warn_once(msg::String...; depth=0)
    msg = bytestring(msg...)
    haskey(have_warned,msg) && return
    have_warned[msg] = true
    warn(msg; depth=depth+1)
end

# BLAS utility routines
function blas_vendor()
    try
        cglobal((:openblas_set_num_threads, Base.libblas_name), Void)
        return :openblas
    end
    try
        cglobal((:MKL_Set_Num_Threads, Base.libblas_name), Void)
        return :mkl
    end
    return :unknown
end

openblas_get_config() = chop(bytestring( ccall((:openblas_get_config, Base.libblas_name), Ptr{Uint8}, () )))

function blas_set_num_threads(n::Integer)
    blas = blas_vendor()
    if blas == :openblas
        return ccall((:openblas_set_num_threads, Base.libblas_name), Void, (Int32,), n)
    elseif blas == :mkl
        # MKL may let us set the number of threads in several ways
        return ccall((:MKL_Set_Num_Threads, Base.libblas_name), Void, (Cint,), n)
    end

    # OSX BLAS looks at an environment variable
    @osx_only ENV["VECLIB_MAXIMUM_THREADS"] = n

    return nothing
end

function check_blas()
    if blas_vendor() == :openblas
        openblas_config = openblas_get_config()
        openblas64 = ismatch(r".*USE64BITINT.*", openblas_config)
        if Base.USE_BLAS64 != openblas64
            if !openblas64
                println("ERROR: OpenBLAS was not built with 64bit integer support.")
                println("You're seeing this error because Julia was built with USE_BLAS64=1")
                println("Please rebuild Julia with USE_BLAS64=0")
            else
                println("ERROR: Julia was not built with support for OpenBLAS with 64bit integer support")
                println("You're seeing this error because Julia was built with USE_BLAS64=0")
                println("Please rebuild Julia with USE_BLAS64=1")
            end
            println("Quitting.")
            quit()
        end
    end
end

# system information

function versioninfo(io::IO=STDOUT, verbose::Bool=false)
    println(io,             "Julia Version $VERSION")
    println(io,             commit_string)
    println(io,             "Platform Info:")
    println(io,             "  System: ", Sys.OS_NAME, " (", Sys.MACHINE, ")")
    println(io,             "  WORD_SIZE: ", Sys.WORD_SIZE)
    if verbose
        lsb = ""
        @linux_only try lsb = readchomp(`lsb_release -ds` .> SpawnNullStream()) end
        @windows_only try lsb = strip(readall(`$(ENV["COMSPEC"]) /c ver`)) end
        if lsb != ""
            println(io,     "           ", lsb)
        end
        println(io,         "  uname: ",readchomp(`uname -mprsv`))
        println(io,         "Memory: $(Sys.total_memory()/2^30) GB ($(Sys.free_memory()/2^20) MB free)")
        try println(io,     "Uptime: $(Sys.uptime()) sec") catch end
        print(io,           "Load Avg: ")
        print_matrix(io,    Sys.loadavg()')
        println(io          )
        Sys.cpu_summary(io)
    end
    if Base.libblas_name == "libopenblas" || blas_vendor() == :openblas
        openblas_config = openblas_get_config()
        println(io,         "  BLAS: libopenblas (", openblas_config, ")")
    else
        println(io,         "  BLAS: ",libblas_name)
    end
    println(io,             "  LAPACK: ",liblapack_name)
    println(io,             "  LIBM: ",libm_name)
    if verbose
        println(io,         "Environment:")
        for (k,v) in ENV
            if !is(match(r"JULIA|PATH|FLAG|^TERM$|HOME",bytestring(k)), nothing)
                println(io, "  $(k) = $(v)")
            end
        end
        println(io          )
        println(io,         "Package Directory: ", Pkg.dir())
        Pkg.status(io)
    end
end
versioninfo(verbose::Bool) = versioninfo(STDOUT,verbose)

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

methodswith(t::Type, m::Module, showparents::Bool) = methodswith(STDOUT, t, m, showparents)
methodswith(t::Type, showparents::Bool) = methodswith(STDOUT, t, showparents)
methodswith(t::Type, m::Module) = methodswith(STDOUT, t, m, false)
methodswith(t::Type) = methodswith(STDOUT, t, false)
function methodswith(io::IO, t::Type, showparents::Bool)
    mainmod = current_module()
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
