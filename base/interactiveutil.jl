# editing files

function edit(file::AbstractString, line::Integer)
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
        f = find_source_file(file)
        f != nothing && (file = f)
    end
    no_line_msg = "Unknown editor: no line number information passed.\nThe method is defined at line $line."
    if beginswith(edname, "emacs") || edname == "gedit"
        spawn(`$edpath +$line $file`)
    elseif edname == "vim" || edname == "nvim" || edname == "nano"
        run(`$edpath +$line $file`)
    elseif edname == "textmate" || edname == "mate" || edname == "kate"
        spawn(`$edpath $file -l $line`)
    elseif beginswith(edname, "subl") || edname == "atom"
        spawn(`$(shell_split(edpath)) $file:$line`)
    elseif OS_NAME == :Windows && (edname == "start" || edname == "open")
        spawn(`cmd /c start /b $file`)
        println(no_line_msg)
    elseif OS_NAME == :Darwin && (edname == "start" || edname == "open")
        spawn(`open -t $file`)
        println(no_line_msg)
    else
        run(`$(shell_split(edpath)) $file`)
        println(no_line_msg)
    end
    nothing
end

function edit( m::Method )
    tv, decls, file, line = arg_decl_parts(m)
    edit( string(file), line )
end

edit(file::AbstractString) = edit(file, 1)
edit(f::Callable)               = edit(functionloc(f)...)
edit(f::Callable, t::(Type...)) = edit(functionloc(f,t)...)

# terminal pager

function less(file::AbstractString, line::Integer)
    pager = get(ENV, "PAGER", "less")
    run(`$pager +$(line)g $file`)
end

less(file::AbstractString) = less(file, 1)
less(f::Callable)               = less(functionloc(f)...)
less(f::Callable, t::(Type...)) = less(functionloc(f,t)...)

# clipboard copy and paste

@osx_only begin
    function clipboard(x)
        open(`pbcopy`, "w") do io
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
            success(`which $cmd` |> DevNull) && return _clipboardcmd = cmd
        end
        error("no clipboard command found, please install xsel or xclip")
    end
    function clipboard(x)
        c = clipboardcmd()
        cmd = c == :xsel  ? `xsel --nodetach --input --clipboard` :
              c == :xclip ? `xclip -quiet -in -selection clipboard` :
            error("unexpected clipboard command: $c")
        open(cmd, "w") do io
            print(io, x)
        end
    end
    function clipboard()
        c = clipboardcmd()
        cmd = c == :xsel  ? `xsel --nodetach --output --clipboard` :
              c == :xclip ? `xclip -quiet -out -selection clipboard` :
            error("unexpected clipboard command: $c")
        readall(cmd)
    end
end

@windows_only begin # TODO: these functions leak memory and memory locks if they throw an error
    function clipboard(x::AbstractString)
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
        @linux_only try lsb = readchomp(`lsb_release -ds` .> DevNull) end
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

# searching definitions

function which(f, t::(Type...))
    ms = methods(f, t)
    isempty(ms) && error("no method found for the specified argument types")
    ms[1]
end

# displaying type-ambiguity warnings

function code_warntype(io::IO, f, t::(Type...))
    global show_expr_type_emphasize
    state = show_expr_type_emphasize::Bool
    ct = code_typed(f, t)
    show_expr_type_emphasize::Bool = true
    for ast in ct
        println(io, "Variables:")
        vars = ast.args[2][2]
        for v in vars
            print(io, "  ", v[1])
            show_expr_type(io, v[2])
            print(io, '\n')
        end
        print(io, "\nBody:\n  ")
        show_unquoted(io, ast.args[3], 2)
        print(io, '\n')
    end
    show_expr_type_emphasize::Bool = false
    nothing
end
code_warntype(f, t::(Type...)) = code_warntype(STDOUT, f, t)

typesof(args...) = map(a->(isa(a,Type) ? Type{a} : typeof(a)), args)

function gen_call_with_extracted_types(fcn, ex0)
    if isa(ex0, Expr) &&
        any(a->(Meta.isexpr(a, :kw) || Meta.isexpr(a, :parameters)), ex0.args)
        # keyword args not used in dispatch, so just remove them
        args = filter(a->!(Meta.isexpr(a, :kw) || Meta.isexpr(a, :parameters)), ex0.args)
        return Expr(:call, fcn, esc(args[1]),
                    Expr(:call, :typesof, map(esc, args[2:end])...))
    end
    if isa(ex0, Expr) && ex0.head == :call
        return Expr(:call, fcn, esc(ex0.args[1]),
                    Expr(:call, :typesof, map(esc, ex0.args[2:end])...))
    end
    ex = expand(ex0)
    exret = Expr(:call, :error, "expression is not a function call")
    if !isa(ex, Expr)
        # do nothing -> error
    elseif ex.head == :call
        if any(e->(isa(e, Expr) && e.head==:(...)), ex0.args) &&
            isa(ex.args[1], TopNode) && ex.args[1].name == :apply
            exret = Expr(:call, ex.args[1], fcn,
                         Expr(:tuple, esc(ex.args[2])),
                         Expr(:call, :typesof, map(esc, ex.args[3:end])...))
        else
            exret = Expr(:call, fcn, esc(ex.args[1]),
                         Expr(:call, :typesof, map(esc, ex.args[2:end])...))
        end
    elseif ex.head == :body
        a1 = ex.args[1]
        if isa(a1, Expr) && a1.head == :call
            a11 = a1.args[1]
            if a11 == :setindex!
                exret = Expr(:call, fcn, a11,
                             Expr(:call, :typesof, map(esc, a1.args[2:end])...))
            end
        end
    elseif ex.head == :thunk
        exret = Expr(:call, :error, "expression is not a function call, "
                                  * "or is too complex for @which to analyze; "
                                  * "break it down to simpler parts if possible")
    end
    exret
end

for fname in [:which, :less, :edit, :code_typed, :code_warntype, :code_lowered, :code_llvm, :code_native]
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
    while !is(d,())
        if any(x -> (type_close_enough(x, t) ||
                     (showparents ? (t <: x && (!isa(x,TypeVar) || x.ub != Any)) :
                      (isa(x,TypeVar) && x.ub != Any && t == x.ub)) &&
                     x != Any && x != ANY),
               d.sig)
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
            f = eval(m, nm)
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
            mod = eval(mainmod, nm)
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

@windows_only function download(url::AbstractString, filename::AbstractString)
    res = ccall((:URLDownloadToFileW,:urlmon),stdcall,Cuint,
                (Ptr{Void},Ptr{UInt16},Ptr{UInt16},Cint,Ptr{Void}),0,utf16(url),utf16(filename),0,0)
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
    empty!(package_list)
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
