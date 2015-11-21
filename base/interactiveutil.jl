# This file is a part of Julia. License is MIT: http://julialang.org/license

# editing files

doc"""
    editor()

Determines the editor to use when running functions like `edit`. Returns an Array compatible
for use within backticks. You can change the editor by setting JULIA_EDITOR, VISUAL, or
EDITOR as an environmental variable.
"""
function editor()
    if OS_NAME == :Windows || OS_NAME == :Darwin
        default_editor = "open"
    elseif isfile("/etc/alternatives/editor")
        default_editor = "/etc/alternatives/editor"
    else
        default_editor = "emacs"
    end
    # Note: the editor path can include spaces (if escaped) and flags.
    args = shell_split(get(ENV,"JULIA_EDITOR", get(ENV,"VISUAL", get(ENV,"EDITOR", default_editor))))
    isempty(args) && error("editor is empty")
    return args
end

function edit(path::AbstractString, line::Integer=0)
    command = editor()
    name = basename(first(command))
    issrc = length(path)>2 && path[end-2:end] == ".jl"
    if issrc
        f = find_source_file(path)
        f !== nothing && (path = f)
    end
    background = true
    line_unsupported = false
    if startswith(name, "emacs") || name == "gedit"
        cmd = line != 0 ? `$command +$line $path` : `$command $path`
    elseif name == "vi" || name == "vim" || name == "nvim" || name == "mvim" || name == "nano"
        cmd = line != 0 ? `$command +$line $path` : `$command $path`
        background = false
    elseif name == "textmate" || name == "mate" || name == "kate"
        cmd = line != 0 ? `$command $path -l $line` : `$command $path`
    elseif startswith(name, "subl") || name == "atom"
        cmd = line != 0 ? `$command $path:$line` : `$command $path`
    elseif OS_NAME == :Windows && (name == "start" || name == "open")
        cmd = `cmd /c start /b $path`
        line_unsupported = true
    elseif OS_NAME == :Darwin && (name == "start" || name == "open")
        cmd = `open -t $path`
        line_unsupported = true
    else
        cmd = `$command $path`
        background = false
        line_unsupported = true
    end

    if background
        spawn(pipeline(cmd, stderr=STDERR))
    else
        run(cmd)
    end
    line != 0 && line_unsupported && println("Unknown editor: no line number information passed.\nThe method is defined at line $line.")

    nothing
end

function edit(m::Method)
    tv, decls, file, line = arg_decl_parts(m)
    edit(string(file), line)
end

edit(f)          = edit(functionloc(f)...)
edit(f, t::ANY)  = edit(functionloc(f,t)...)
edit(file, line::Integer) = error("could not find source file for function")

# terminal pager

function less(file::AbstractString, line::Integer)
    pager = get(ENV, "PAGER", "less")
    run(`$pager +$(line)g $file`)
end

less(file::AbstractString) = less(file, 1)
less(f)          = less(functionloc(f)...)
less(f, t::ANY)  = less(functionloc(f,t)...)
less(file, line::Integer) = error("could not find source file for function")

# clipboard copy and paste

@osx_only begin
    function clipboard(x)
        open(pipeline(`pbcopy`, stderr=STDERR), "w") do io
            print(io, x)
        end
    end
    clipboard() = readall(`pbpaste`)
end

@linux_only begin
    _clipboardcmd = nothing
    function clipboardcmd()
        global _clipboardcmd
        _clipboardcmd !== nothing && return _clipboardcmd
        for cmd in (:xclip, :xsel)
            success(pipeline(`which $cmd`, DevNull)) && return _clipboardcmd = cmd
        end
        error("no clipboard command found, please install xsel or xclip")
    end
    function clipboard(x)
        c = clipboardcmd()
        cmd = c == :xsel  ? `xsel --nodetach --input --clipboard` :
              c == :xclip ? `xclip -quiet -in -selection clipboard` :
            error("unexpected clipboard command: $c")
        open(pipeline(cmd, stderr=STDERR), "w") do io
            print(io, x)
        end
    end
    function clipboard()
        c = clipboardcmd()
        cmd = c == :xsel  ? `xsel --nodetach --output --clipboard` :
              c == :xclip ? `xclip -quiet -out -selection clipboard` :
            error("unexpected clipboard command: $c")
        readall(pipeline(cmd, stderr=STDERR))
    end
end

@windows_only begin # TODO: these functions leak memory and memory locks if they throw an error
    function clipboard(x::AbstractString)
        if containsnul(x)
            throw(ArgumentError("Windows clipboard strings cannot contain NUL character"))
        end
        systemerror(:OpenClipboard, 0==ccall((:OpenClipboard, "user32"), stdcall, Cint, (Ptr{Void},), C_NULL))
        systemerror(:EmptyClipboard, 0==ccall((:EmptyClipboard, "user32"), stdcall, Cint, ()))
        x_u16 = utf16(x)
        # copy data to locked, allocated space
        p = ccall((:GlobalAlloc, "kernel32"), stdcall, Ptr{UInt16}, (UInt16, Int32), 2, sizeof(x_u16)+2)
        systemerror(:GlobalAlloc, p==C_NULL)
        plock = ccall((:GlobalLock, "kernel32"), stdcall, Ptr{UInt16}, (Ptr{UInt16},), p)
        systemerror(:GlobalLock, plock==C_NULL)
        ccall(:memcpy, Ptr{UInt16}, (Ptr{UInt16},Ptr{UInt16},Int), plock, x_u16, sizeof(x_u16)+2)
        systemerror(:GlobalUnlock, 0==ccall((:GlobalUnlock, "kernel32"), stdcall, Cint, (Ptr{Void},), plock))
        pdata = ccall((:SetClipboardData, "user32"), stdcall, Ptr{UInt16}, (UInt32, Ptr{UInt16}), 13, p)
        systemerror(:SetClipboardData, pdata!=p)
        ccall((:CloseClipboard, "user32"), stdcall, Void, ())
    end
    clipboard(x) = clipboard(sprint(io->print(io,x))::ByteString)

    function clipboard()
        systemerror(:OpenClipboard, 0==ccall((:OpenClipboard, "user32"), stdcall, Cint, (Ptr{Void},), C_NULL))
        pdata = ccall((:GetClipboardData, "user32"), stdcall, Ptr{UInt16}, (UInt32,), 13)
        systemerror(:SetClipboardData, pdata==C_NULL)
        systemerror(:CloseClipboard, 0==ccall((:CloseClipboard, "user32"), stdcall, Cint, ()))
        plock = ccall((:GlobalLock, "kernel32"), stdcall, Ptr{UInt16}, (Ptr{UInt16},), pdata)
        systemerror(:GlobalLock, plock==C_NULL)
        s = utf8(utf16(plock))
        systemerror(:GlobalUnlock, 0==ccall((:GlobalUnlock, "kernel32"), stdcall, Cint, (Ptr{UInt16},), plock))
        return s
    end
end

if !isdefined(:clipboard)
    clipboard(x="") = error("clipboard functionality not implemented for $OS_NAME")
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
        @linux_only try lsb = readchomp(pipeline(`lsb_release -ds`, stderr=DevNull)) end
        @windows_only try lsb = strip(readall(`$(ENV["COMSPEC"]) /c ver`)) end
        if lsb != ""
            println(io,     "           ", lsb)
        end
        println(io,         "  uname: ",readchomp(`uname -mprsv`))
        println(io,         "Memory: $(Sys.total_memory()/2^30) GB ($(Sys.free_memory()/2^20) MB free)")
        try println(io,     "Uptime: $(Sys.uptime()) sec") end
        print(io,           "Load Avg: ")
        print_matrix(io,    Sys.loadavg()')
        println(io          )
        Sys.cpu_summary(io)
        println(io          )
    end
    if Base.libblas_name == "libopenblas" || blas_vendor() == :openblas || blas_vendor() == :openblas64
        openblas_config = openblas_get_config()
        println(io,         "  BLAS: libopenblas (", openblas_config, ")")
    else
        println(io,         "  BLAS: ",libblas_name)
    end
    println(io,             "  LAPACK: ",liblapack_name)
    println(io,             "  LIBM: ",libm_name)
    println(io,             "  LLVM: libLLVM-",libllvm_version)
    if verbose
        println(io,         "Environment:")
        for (k,v) in ENV
            if !is(match(r"JULIA|PATH|FLAG|^TERM$|HOME", bytestring(k)), nothing)
                println(io, "  $(k) = $(v)")
            end
        end
        println(io          )
        println(io,         "Package Directory: ", Pkg.dir())
        Pkg.status(io)
    end
end
versioninfo(verbose::Bool) = versioninfo(STDOUT,verbose)

# displaying type-ambiguity warnings

function code_warntype(io::IO, f, t::ANY)
    task_local_storage(:TYPEEMPHASIZE, true)
    try
        ct = code_typed(f, t)
        for ast in ct
            println(io, "Variables:")
            vars = ast.args[2][1]
            for v in vars
                print(io, "  ", v[1])
                show_expr_type(io, v[2])
                print(io, '\n')
            end
            print(io, "\nBody:\n  ")
            show_unquoted(io, ast.args[3], 2)
            print(io, '\n')
        end
    finally
        task_local_storage(:TYPEEMPHASIZE, false)
    end
    nothing
end
code_warntype(f, t::ANY) = code_warntype(STDOUT, f, t)

typesof(args...) = Tuple{map(a->(isa(a,Type) ? Type{a} : typeof(a)), args)...}

gen_call_with_extracted_types(fcn, ex0::Symbol) = Expr(:call, fcn, Meta.quot(ex0))
function gen_call_with_extracted_types(fcn, ex0)
    if isa(ex0, Expr) &&
        any(a->(Meta.isexpr(a, :kw) || Meta.isexpr(a, :parameters)), ex0.args)
        # keyword args not used in dispatch, so just remove them
        args = filter(a->!(Meta.isexpr(a, :kw) || Meta.isexpr(a, :parameters)), ex0.args)
        return Expr(:call, fcn, esc(args[1]),
                    Expr(:call, typesof, map(esc, args[2:end])...))
    end
    exret = Expr(:none)
    ex = expand(ex0)
    if !isa(ex, Expr)
        exret = Expr(:call, :error, "expression is not a function call or symbol")
    elseif ex.head == :call
        if any(e->(isa(e, Expr) && e.head==:(...)), ex0.args) &&
            isa(ex.args[1], TopNode) && ex.args[1].name == :_apply
            # check for splatting
            exret = Expr(:call, ex.args[1], ex.args[2], fcn,
                        Expr(:tuple, esc(ex.args[3]),
                            Expr(:call, typesof, map(esc, ex.args[4:end])...)))
        else
            exret = Expr(:call, fcn, esc(ex.args[1]),
                         Expr(:call, typesof, map(esc, ex.args[2:end])...))
        end
    elseif ex.head == :body
        a1 = ex.args[1]
        if isa(a1, Expr) && a1.head == :call
            a11 = a1.args[1]
            if a11 == :setindex!
                exret = Expr(:call, fcn, a11,
                             Expr(:call, typesof, map(esc, a1.args[2:end])...))
            end
        end
    end
    if ex.head == :thunk || exret.head == :none
        exret = Expr(:call, :error, "expression is not a function call, "
                                  * "or is too complex for @$fcn to analyze; "
                                  * "break it down to simpler parts if possible")
    end
    exret
end

for fname in [:which, :less, :edit, :code_typed, :code_warntype,
              :code_lowered, :code_llvm, :code_llvm_raw, :code_native]
    @eval begin
        macro ($fname)(ex0)
            gen_call_with_extracted_types($(Expr(:quote,fname)), ex0)
        end
    end
end

# `methodswith` -- shows a list of methods using the type given

function type_close_enough(x::ANY, t::ANY)
    x == t && return true
    return (isa(x,DataType) && isa(t,DataType) && x.name === t.name &&
            !isleaftype(t) && x <: t)
end

function methodswith(t::Type, f::Function, showparents::Bool=false, meths = Method[])
    if !isa(f.env, MethodTable)
        return meths
    end
    d = f.env.defs
    while d !== nothing
        if any(x -> (type_close_enough(x, t) ||
                     (showparents ? (t <: x && (!isa(x,TypeVar) || x.ub != Any)) :
                      (isa(x,TypeVar) && x.ub != Any && t == x.ub)) &&
                     x != Any && x != ANY),
               d.sig.parameters)
            push!(meths, d)
        end
        d = d.next
    end
    return meths
end

function methodswith(t::Type, m::Module, showparents::Bool=false)
    meths = Method[]
    for nm in names(m)
        if isdefined(m, nm)
            f = getfield(m, nm)
            if isa(f, Function)
                methodswith(t, f, showparents, meths)
            end
        end
    end
    return unique(meths)
end

function methodswith(t::Type, showparents::Bool=false)
    meths = Method[]
    mainmod = current_module()
    # find modules in Main
    for nm in names(mainmod)
        if isdefined(mainmod,nm)
            mod = getfield(mainmod, nm)
            if isa(mod, Module)
                append!(meths, methodswith(t, mod, showparents))
            end
        end
    end
    return unique(meths)
end

# file downloading

downloadcmd = nothing
@unix_only function download(url::AbstractString, filename::AbstractString)
    global downloadcmd
    if downloadcmd === nothing
        for checkcmd in (:curl, :wget, :fetch)
            if success(pipeline(`which $checkcmd`, DevNull))
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

@windows_only function download(url::AbstractString, filename::AbstractString)
    res = ccall((:URLDownloadToFileW,:urlmon),stdcall,Cuint,
                (Ptr{Void},Cwstring,Cwstring,Cuint,Ptr{Void}),C_NULL,url,filename,0,C_NULL)
    if res != 0
        error("automatic download failed (error: $res): $url")
    end
    filename
end

function download(url::AbstractString)
    filename = tempname()
    download(url, filename)
end

# workspace management

function workspace()
    last = Core.Main
    b = last.Base
    ccall(:jl_new_main_module, Any, ())
    m = Core.Main
    ccall(:jl_add_standard_imports, Void, (Any,), m)
    eval(m,
         Expr(:toplevel,
              :(const Base = $(Expr(:quote, b))),
              :(const LastMain = $(Expr(:quote, last)))))
    empty!(package_locks)
    nothing
end

# testing

function runtests(tests = ["all"], numcores = ceil(Int,CPU_CORES/2))
    if isa(tests,AbstractString)
        tests = split(tests)
    end
    ENV2 = copy(ENV)
    ENV2["JULIA_CPU_CORES"] = "$numcores"
    try
        run(setenv(`$(julia_cmd()) $(joinpath(JULIA_HOME,
            Base.DATAROOTDIR, "julia", "test", "runtests.jl")) $tests`, ENV2))
    catch
        buf = PipeBuffer()
        versioninfo(buf)
        error("A test has failed. Please submit a bug report (https://github.com/JuliaLang/julia/issues)\n" *
              "including error messages above and the output of versioninfo():\n$(readall(buf))")
    end
end

# testing


doc"""
    whos([io,] [Module,] [pattern::Regex])

Print information about exported global variables in a module, optionally restricted to those matching `pattern`.

The memory consumption estimate is an approximate lower bound on the size of the internal structure of the object.
"""
function whos(io::IO=STDOUT, m::Module=current_module(), pattern::Regex=r"")
    maxline = tty_size()[2]
    line = zeros(UInt8, maxline)
    head = PipeBuffer(maxline + 1)
    for v in sort!(names(m))
        s = string(v)
        if isdefined(m, v) && ismatch(pattern, s)
            value = getfield(m, v)
            @printf head "%30s " s
            try
                bytes = summarysize(value)
                if bytes < 10_000
                    @printf(head, "%6d bytes  ", bytes)
                else
                    @printf(head, "%6d KB     ", bytes ÷ (1024))
                end
                print(head, summary(value))
                print(head, " : ")
                show(head, value)
            catch e
                print(head, "#=ERROR: unable to show value=#")
            end

            newline = search(head, UInt8('\n')) - 1
            if newline < 0
                newline = nb_available(head)
            end
            if newline > maxline
                newline = maxline - 1 # make space for ...
            end
            line = resize!(line, newline)
            line = read!(head, line)

            write(io, line)
            if nb_available(head) > 0 # more to read? replace with ...
                print(io, '\u2026') # hdots
            end
            println(io)
            seekend(head) # skip the rest of the text
        end
    end
end
whos(m::Module, pat::Regex=r"") = whos(STDOUT, m, pat)
whos(pat::Regex) = whos(STDOUT, current_module(), pat)

#################################################################################

"""
    Base.summarysize(obj; exclude=Union{Module,Function,DataType,TypeName}) -> Int

Compute the amount of memory used by all unique objects reachable from the argument.
Keyword argument `exclude` specifies a type of objects to exclude from the traversal.
"""
summarysize(obj; exclude = Union{Module,Function,DataType,TypeName}) =
    summarysize(obj, ObjectIdDict(), exclude)

summarysize(obj::Symbol, seen, excl) = 0

function summarysize(obj::DataType, seen, excl)
    key = pointer_from_objref(obj)
    haskey(seen, key) ? (return 0) : (seen[key] = true)
    size = 7*sizeof(Int) + 6*sizeof(Int32) + 4*nfields(obj) + ifelse(WORD_SIZE==64,4,0)
    size += summarysize(obj.parameters, seen, excl)::Int
    size += summarysize(obj.types, seen, excl)::Int
    return size
end

summarysize(obj::TypeName, seen, excl) = Core.sizeof(obj)

summarysize(obj::ANY, seen, excl) = _summarysize(obj, seen, excl)
# define the general case separately to make sure it is not specialized for every type
function _summarysize(obj::ANY, seen, excl)
    key = pointer_from_objref(obj)
    haskey(seen, key) ? (return 0) : (seen[key] = true)
    size = Core.sizeof(obj)
    ft = typeof(obj).types
    for i in 1:nfields(obj)
        if !isbits(ft[i]) && isdefined(obj,i)
            val = obj.(i)
            if !isa(val,excl)
                size += summarysize(val, seen, excl)::Int
            end
        end
    end
    return size
end

function summarysize(obj::Array, seen, excl)
    haskey(seen, obj) ? (return 0) : (seen[obj] = true)
    size = Core.sizeof(obj)
    # TODO: add size of jl_array_t
    if !isbits(eltype(obj))
        for i in 1:length(obj)
            if ccall(:jl_array_isassigned, Cint, (Any, UInt), obj, i-1) == 1
                val = obj[i]
                if !isa(val, excl)
                    size += summarysize(val, seen, excl)::Int
                end
            end
        end
    end
    return size
end

function summarysize(obj::SimpleVector, seen, excl)
    key = pointer_from_objref(obj)
    haskey(seen, key) ? (return 0) : (seen[key] = true)
    size = Core.sizeof(obj)
    for i in 1:length(obj)
        if isassigned(obj, i)
            val = obj[i]
            if !isa(val, excl)
                size += summarysize(val, seen, excl)::Int
            end
        end
    end
    return size
end

function summarysize(obj::Module, seen, excl)
    haskey(seen, obj) ? (return 0) : (seen[obj] = true)
    size::Int = Core.sizeof(obj)
    for binding in names(obj, true)
        if isdefined(obj, binding)
            value = getfield(obj, binding)
            if !isa(value, Module) || module_parent(value) === obj
                size += summarysize(value, seen, excl)::Int
            end
        end
    end
    return size
end

function summarysize(obj::Task, seen, excl)
    haskey(seen, obj) ? (return 0) : (seen[obj] = true)
    size::Int = Core.sizeof(obj)
    if isdefined(obj, :code)
        size += summarysize(obj.code, seen, excl)::Int
    end
    size += summarysize(obj.storage, seen, excl)::Int
    size += summarysize(obj.backtrace, seen, excl)::Int
    size += summarysize(obj.donenotify, seen, excl)::Int
    size += summarysize(obj.exception, seen, excl)::Int
    size += summarysize(obj.result, seen, excl)::Int
    # TODO: add stack size, and possibly traverse stack roots
    return size
end

function summarysize(obj::MethodTable, seen, excl)
    haskey(seen, obj) ? (return 0) : (seen[obj] = true)
    size::Int = Core.sizeof(obj)
    size += summarysize(obj.defs, seen, excl)::Int
    size += summarysize(obj.cache, seen, excl)::Int
    size += summarysize(obj.cache_arg1, seen, excl)::Int
    size += summarysize(obj.cache_targ, seen, excl)::Int
    if isdefined(obj, :kwsorter)
        size += summarysize(obj.kwsorter, seen, excl)::Int
    end
    return size
end

function summarysize(m::Method, seen, excl)
    size::Int = 0
    while true
        haskey(seen, m) ? (return size) : (seen[m] = true)
        size += Core.sizeof(m)
        size += summarysize(m.func, seen, excl)::Int
        size += summarysize(m.sig, seen, excl)::Int
        size += summarysize(m.tvars, seen, excl)::Int
        size += summarysize(m.invokes, seen, excl)::Int
        m.next === nothing && break
        m = m.next::Method
    end
    return size
end
