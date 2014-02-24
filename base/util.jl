# flush C stdio output from external libraries:

flush_cstdio() = ccall(:jl_flush_cstdio, Void, ())

# timing

# system date in seconds
time() = ccall(:clock_now, Float64, ())

# high-resolution relative time, in nanoseconds
time_ns() = ccall(:jl_hrtime, Uint64, ())

# total number of bytes allocated so far
gc_bytes() = ccall(:jl_gc_total_bytes, Int64, ())

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
    t0 = timers[1]::Uint64
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
        println("elapsed time: ", (t1-t0)/1e9, " seconds (", b1-b0, " bytes allocated)")
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
                b1-b0
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
        val, (t1-t0)/1e9, b1-b0
    end
end

# searching definitions

function which(f::Callable, args...)
    if !isgeneric(f)
        throw(ErrorException("not a generic function, no methods available"))
    end
    ms = methods(f, map(a->(isa(a,Type) ? Type{a} : typeof(a)), args))
    isempty(ms) && throw(MethodError(f, args))
    ms[1]
end

macro which(ex0)
    if isa(ex0,Expr) &&
        any(a->(Meta.isexpr(a,:kw) || Meta.isexpr(a,:parameters)), ex0.args)
        # keyword args not used in dispatch, so just remove them
        args = filter(a->!(Meta.isexpr(a,:kw) || Meta.isexpr(a,:parameters)), ex0.args)
        return Expr(:call, :which, map(esc, args)...)
    end
    if isa(ex0, Expr) && ex0.head == :call
        return Expr(:call, :which, map(esc, ex0.args)...)
    end
    ex = expand(ex0)
    exret = Expr(:call, :error, "expression is not a function call")
    if !isa(ex, Expr)
        # do nothing -> error
    elseif ex.head == :call
        if any(e->(isa(e,Expr) && e.head==:(...)), ex0.args) &&
            isa(ex.args[1],TopNode) && ex.args[1].name == :apply
            exret = Expr(:call, ex.args[1], :which,
                         Expr(:tuple, esc(ex.args[2])),
                         map(esc, ex.args[3:end])...)
        else
            exret = Expr(:call, :which, map(esc, ex.args)...)
        end
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
    (isabspath(file) || isfile(file)) && return file
    file2 = find_in_path(file)
    file2 != nothing && return file2
    file2 = "$JULIA_HOME/../share/julia/base/$file"
    isfile(file2) ? file2 : nothing
end

function edit(file::String, line::Integer)
    if OS_NAME == :Windows || OS_NAME == :Darwin
        default_editor = "open"
    elseif isreadable("/etc/alternatives/editor")
        default_editor = "/etc/alternatives/editor"
    else
        default_editor = "emacs"
    end
    editor = get(ENV,"JULIA_EDITOR", get(ENV,"VISUAL", get(ENV,"EDITOR", default_editor)))
    if ispath(editor)
        if isreadable(editor)
            edpath = realpath(editor)
            edname = basename(edpath)
        else
            error("can't find \"$editor\"")
        end
    else
        edpath = edname = editor
    end
    issrc = length(file)>2 && file[end-2:end] == ".jl"
    if issrc
        file = find_source_file(file)
    end
    if beginswith(edname, "emacs")
        jmode = joinpath(JULIA_HOME, "..", "..", "contrib", "julia-mode.el")
        if issrc && isreadable(jmode)
            run(`$edpath $file --eval "(progn
                                     (require 'julia-mode \"$jmode\")
                                     (julia-mode)
                                     (goto-line $line))"`)
        else
            run(`$edpath $file --eval "(goto-line $line)"`)
        end
    elseif edname == "vim"
        run(`$edpath $file +$line`)
    elseif edname == "textmate" || edname == "mate"
        spawn(`$edpath $file -l $line`)
    elseif edname == "subl"
        spawn(`$edpath $file:$line`)
    elseif OS_NAME == :Windows && (edname == "start" || edname == "open")
        spawn(`start /b $file`)
    elseif OS_NAME == :Darwin && (edname == "start" || edname == "open")
        spawn(`open -t $file`)
    elseif edname == "kate"
        spawn(`$edpath $file -l $line`)
    elseif edname == "nano"
        run(`$edpath +$line $file`)
    else
        run(`$(shell_split(edpath)) $file`)
    end
    nothing
end
edit(file::String) = edit(file, 1)

function less(file::String, line::Integer)
    pager = get(ENV, "PAGER", "less")
    run(`$pager +$(line)g $file`)
end
less(file::String) = less(file, 1)

edit(f::Union(Function,DataType))    = edit(functionloc(f)...)
edit(f::Union(Function,DataType), t) = edit(functionloc(f,t)...)
less(f::Union(Function,DataType))    = less(functionloc(f)...)
less(f::Union(Function,DataType), t) = less(functionloc(f,t)...)

# clipboard copy and paste

@osx_only begin
    function clipboard(x)
        w,p = writesto(`pbcopy`)
        print(w,x)
        close(w)
        wait(p)
    end
    clipboard() = readall(`pbpaste`)
end

@linux_only begin
    _clipboardcmd = nothing
    function clipboardcmd()
        global _clipboardcmd
        _clipboardcmd !== nothing && return _clipboardcmd
        for cmd in (:xclip, :xsel)
            success(`which $cmd` |> DevNull) && return _clipboardcmd = cmd
        end
        error("no clipboard command found, please install xsel or xclip")
    end
    function clipboard(x)
        c = clipboardcmd()
        cmd = c == :xsel  ? `xsel --nodetach --input --clipboard` :
              c == :xclip ? `xclip -quiet -in -selection clipboard` :
            error("unexpected clipboard command: $c")
        w,p = writesto(cmd)
        print(w,x)
        close(w)
        wait(p)
    end
    function clipboard()
        c = clipboardcmd()
        cmd = c == :xsel  ? `xsel --nodetach --output --clipboard` :
              c == :xclip ? `xclip -quiet -out -selection clipboard` :
            error("unexpected clipboard command: $c")
        readall(cmd)
    end
end

@windows_only begin
    function clipboard(x::String)
        ccall((:OpenClipboard, "user32"), stdcall, Bool, (Ptr{Void},), C_NULL)
        ccall((:EmptyClipboard, "user32"), stdcall, Bool, ())
        p = ccall((:GlobalAlloc, "kernel32"), stdcall, Ptr{Void}, (Uint16,Int32), 2, length(x)+1)
        p = ccall((:GlobalLock, "kernel32"), stdcall, Ptr{Void}, (Ptr{Void},), p)
        # write data to locked, allocated space
        ccall(:memcpy, Ptr{Void}, (Ptr{Void},Ptr{Uint8},Int32), p, x, length(x)+1)
        ccall((:GlobalUnlock, "kernel32"), stdcall, Void, (Ptr{Void},), p)
        # set clipboard data type to 13 for Unicode text/string
        p = ccall((:SetClipboardData, "user32"), stdcall, Ptr{Void}, (Uint32, Ptr{Void}), 1, p)
        ccall((:CloseClipboard, "user32"), stdcall, Void, ())
    end
    clipboard(x) = clipboard(sprint(io->print(io,x))::ByteString)

    function clipboard()
        ccall((:OpenClipboard, "user32"), stdcall, Bool, (Ptr{Void},), C_NULL)
        s = bytestring(ccall((:GetClipboardData, "user32"), stdcall, Ptr{Uint8}, (Uint32,), 1))
        ccall((:CloseClipboard, "user32"), stdcall, Void, ())
        return s
    end
end

if !isdefined(:clipboard)
    clipboard(x="") = error("clipboard functionality not implemented for $OS_NAME")
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
    blas = blas_vendor()
    if blas == :openblas
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
    elseif blas == :mkl
        if Base.USE_BLAS64
            ENV["MKL_INTERFACE_LAYER"] = "ILP64"
        end
    end

    #
    # Check if BlasInt is the expected bitsize, by triggering an error
    #
    (_, info) = LinAlg.LAPACK.potrf!('U', [1.0 0.0; 0.0 -1.0])
    if info != 2 # mangled info code
        if info == 2^33
            error("""BLAS and LAPACK are compiled with 32-bit integer support, but Julia expects 64-bit integers. Please build Julia with USE_BLAS64=0.""")
        elseif info == 0
            error("""BLAS and LAPACK are compiled with 64-bit integer support but Julia expects 32-bit integers. Please build Julia with USE_BLAS64=1.""")
        else
            error("""The LAPACK library produced an undefined error code. Please verify the installation of BLAS and LAPACK.""")
        end
    end

end

# system information

function versioninfo(io::IO=STDOUT, verbose::Bool=false)
    println(io,             "Julia Version $VERSION")
    if !isempty(GIT_VERSION_INFO.commit_short)
      println(io,             "Commit $(GIT_VERSION_INFO.commit_short) ($(GIT_VERSION_INFO.date_string))")
    end
    if ccall(:jl_is_debugbuild, Cint, ())!=0
        println(io, "DEBUG build")
    end
    println(io,             "Platform Info:")
    println(io,             "  System: ", Sys.OS_NAME, " (", Sys.MACHINE, ")")

    cpu = Sys.cpu_info()
    println(io,         "  CPU: ", cpu[1].model)
    println(io,             "  WORD_SIZE: ", Sys.WORD_SIZE)
    if verbose
        lsb = ""
        @linux_only try lsb = readchomp(`lsb_release -ds` .> DevNull) end
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
        println(io          )
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

function methodswith(t::Type, m::Module, showparents::Bool=false)
    meths = Method[]
    for nm in names(m)
        try
           mt = eval(m, nm)
           d = mt.env.defs
           while !is(d,())
               if any(map(x -> x == t || (showparents && t <: x && x != Any && x != ANY && !isa(x, TypeVar)), d.sig))
                   push!(meths, d)
               end
               d = d.next
           end
        end
    end
    return meths
end

function methodswith(t::Type, showparents::Bool=false)
    meths = Method[]
    mainmod = current_module()
    # find modules in Main
    for nm in names(mainmod)
        if isdefined(mainmod,nm)
            mod = eval(mainmod, nm)
            if isa(mod, Module)
                meths = [meths, methodswith(t, mod, showparents)]
            end
        end
    end
    return meths
end

## printing with color ##

function with_output_color(f::Function, color::Symbol, io::IO, args...)
    have_color || return f(io, args...)
    print(io, get(text_colors, color, color_normal))
    try f(io, args...)
    finally
        print(io, color_normal)
    end
end

print_with_color(color::Symbol, io::IO, msg::String...) =
    with_output_color(print, color, io, msg...)
print_with_color(color::Symbol, msg::String...) =
    print_with_color(color, STDOUT, msg...)

## file downloading ##

downloadcmd = nothing
function download(url::String, filename::String)
    global downloadcmd
    if downloadcmd === nothing
        for checkcmd in (:curl, :wget, :fetch)
            if success(`which $checkcmd` |> DevNull)
                downloadcmd = checkcmd
                break
            end
        end
    end
    if downloadcmd == :wget
        run(`wget -O $filename $url`)
    elseif downloadcmd == :curl
        run(`curl -o $filename -L $url`)
    elseif downloadcmd == :fetch
        run(`fetch -f $filename $url`)
    else
        error("no download agent available; install curl, wget, or fetch")
    end
    filename
end
function download(url::String)
    filename = tempname()
    download(url, filename)
end

## warnings and messages ##

function info(msg::String...; prefix="INFO: ")
    with_output_color(print, :blue, STDERR, prefix, chomp(string(msg...)))
    println(STDERR)
end

# print a warning only once

const have_warned = Set()
warn_once(msg::String...) = warn(msg..., once=true)

function warn(msg::String...; prefix="WARNING: ", once=false, key=nothing, bt=nothing)
    str = chomp(bytestring(msg...))
    if once
        if key === nothing
            key = str
        end
        (key in have_warned) && return
        push!(have_warned, key)
    end
    with_output_color(print, :red,  STDERR, prefix, str)
    if bt !== nothing
        show_backtrace(STDERR, bt)
    end
    println(STDERR)
end

warn(err::Exception; prefix="ERROR: ", kw...) =
    warn(sprint(io->showerror(io,err)), prefix=prefix; kw...)
