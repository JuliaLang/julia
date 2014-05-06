# editing

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
        spawn(`$edpath +$line $file`)
    elseif edname == "vim"
        run(`$edpath $file +$line`)
    elseif edname == "textmate" || edname == "mate"
        spawn(`$edpath $file -l $line`)
    elseif beginswith(edname, "subl")
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
edit(f::Callable)               = edit(functionloc(f)...)
edit(f::Callable, t::(Type...)) = edit(functionloc(f,t)...)
less(f::Callable)               = less(functionloc(f)...)
less(f::Callable, t::(Type...)) = less(functionloc(f,t)...)

function edit( m::Method )
    tv, decls, file, line = arg_decl_parts(m)
    edit( string(file), line )
end


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

# searching definitions

function which(f::Callable, t::(Type...))
    if !isgeneric(f)
        throw(ErrorException("not a generic function, no methods available"))
    end
    ms = methods(f, t)
    isempty(ms) && error("no method found for the specified argument types")
    ms[1]
end

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

for fname in [:which, :less, :edit, :code_typed, :code_lowered, :code_llvm, :code_native]
    @eval begin
        macro ($fname)(ex0)
            gen_call_with_extracted_types($(Expr(:quote,fname)), ex0)
        end
    end
end

# `methodswith` -- shows a list of methods using the type given

function methodswith(t::Type, m::Module, showparents::Bool=false)
    meths = Method[]
    for nm in names(m)
        try
           mt = eval(m, nm)
           d = mt.env.defs
           while !is(d,())
               if any(map(x -> begin
                                   x == t || (showparents ? (t <: x && (!isa(x,TypeVar) || x.ub != Any)) :
                                                            (isa(x,TypeVar) && x.ub != Any && t == x.ub)) &&
                                   x != Any && x != ANY
                               end, d.sig))
                   push!(meths, d)
               end
               d = d.next
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
                meths = [meths, methodswith(t, mod, showparents)]
            end
        end
    end
    return meths
end

## file downloading ##

downloadcmd = nothing
@unix_only function download(url::String, filename::String)
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

@windows_only function download(url::String, filename::String)
    b_dest = bytestring(filename)
    b_url = bytestring(url)
    res = ccall((:URLDownloadToFileA,:urlmon),stdcall,Cuint,
                (Ptr{Void},Ptr{Uint8},Ptr{Uint8},Cint,Ptr{Void}),0,b_url,b_dest,0,0)
    if res != 0
        error("automatic download failed (error: $res): $url")
    end
    filename
end

function download(url::String)
    filename = tempname()
    download(url, filename)
end
