# This file is a part of Julia. License is MIT: http://julialang.org/license

# editing files

"""
    editor()

Determines the editor to use when running functions like `edit`. Returns an Array compatible
for use within backticks. You can change the editor by setting `JULIA_EDITOR`, `VISUAL` or
`EDITOR` as an environment variable.
"""
function editor()
    if is_windows() || is_apple()
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

"""
    edit(path::AbstractString, line::Integer=0)

Edit a file or directory optionally providing a line number to edit the file at.
Returns to the `julia` prompt when you quit the editor. The editor can be changed
by setting `JULIA_EDITOR`, `VISUAL` or `EDITOR` as an environment variable.
"""
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
    elseif startswith(name, "vim.") || name == "vi" || name == "vim" || name == "nvim" || name == "mvim" || name == "nano"
        cmd = line != 0 ? `$command +$line $path` : `$command $path`
        background = false
    elseif name == "textmate" || name == "mate" || name == "kate"
        cmd = line != 0 ? `$command $path -l $line` : `$command $path`
    elseif startswith(name, "subl") || startswith(name, "atom")
        cmd = line != 0 ? `$command $path:$line` : `$command $path`
    elseif startswith(name, "notepad++")
        cmd = line != 0 ? `$command $path -n$line` : `$command $path`
    elseif is_windows() && (name == "start" || name == "open")
        cmd = `cmd /c start /b $path`
        line_unsupported = true
    elseif is_apple() && (name == "start" || name == "open")
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

"""
    edit(function, [types])

Edit the definition of a function, optionally specifying a tuple of types to
indicate which method to edit. The editor can be changed by setting `JULIA_EDITOR`,
`VISUAL` or `EDITOR` as an environment variable.
"""
edit(f)          = edit(functionloc(f)...)
edit(f, t::ANY)  = edit(functionloc(f,t)...)
edit(file, line::Integer) = error("could not find source file for function")

# terminal pager

if is_windows()
    function less(file::AbstractString, line::Integer)
        pager = get(ENV, "PAGER", "more")
        g = pager == "more" ? "" : "g"
        run(Cmd(`$pager +$(line)$(g) \"$file\"`, windows_verbatim = true))
    end
else
    function less(file::AbstractString, line::Integer)
        pager = get(ENV, "PAGER", "less")
        run(`$pager +$(line)g $file`)
    end
end

"""
    less(file::AbstractString, [line::Integer])

Show a file using the default pager, optionally providing a starting line number. Returns to
the `julia` prompt when you quit the pager.
"""
less(file::AbstractString) = less(file, 1)

"""
    less(function, [types])

Show the definition of a function using the default pager, optionally specifying a tuple of
types to indicate which method to see.
"""
less(f)          = less(functionloc(f)...)
less(f, t::ANY)  = less(functionloc(f,t)...)
less(file, line::Integer) = error("could not find source file for function")

# clipboard copy and paste

if is_apple()
    function clipboard(x)
        open(pipeline(`pbcopy`, stderr=STDERR), "w") do io
            print(io, x)
        end
    end
    clipboard() = readstring(`pbpaste`)

elseif is_linux()
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
              c == :xclip ? `xclip -silent -in -selection clipboard` :
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
        readstring(pipeline(cmd, stderr=STDERR))
    end

elseif is_windows()
    # TODO: these functions leak memory and memory locks if they throw an error
    function clipboard(x::AbstractString)
        if containsnul(x)
            throw(ArgumentError("Windows clipboard strings cannot contain NUL character"))
        end
        systemerror(:OpenClipboard, 0==ccall((:OpenClipboard, "user32"), stdcall, Cint, (Ptr{Void},), C_NULL))
        systemerror(:EmptyClipboard, 0==ccall((:EmptyClipboard, "user32"), stdcall, Cint, ()))
        x_u16 = cwstring(x)
        # copy data to locked, allocated space
        p = ccall((:GlobalAlloc, "kernel32"), stdcall, Ptr{UInt16}, (UInt16, Int32), 2, sizeof(x_u16))
        systemerror(:GlobalAlloc, p==C_NULL)
        plock = ccall((:GlobalLock, "kernel32"), stdcall, Ptr{UInt16}, (Ptr{UInt16},), p)
        systemerror(:GlobalLock, plock==C_NULL)
        ccall(:memcpy, Ptr{UInt16}, (Ptr{UInt16},Ptr{UInt16},Int), plock, x_u16, sizeof(x_u16))
        systemerror(:GlobalUnlock, 0==ccall((:GlobalUnlock, "kernel32"), stdcall, Cint, (Ptr{Void},), plock))
        pdata = ccall((:SetClipboardData, "user32"), stdcall, Ptr{UInt16}, (UInt32, Ptr{UInt16}), 13, p)
        systemerror(:SetClipboardData, pdata!=p)
        ccall((:CloseClipboard, "user32"), stdcall, Void, ())
    end
    clipboard(x) = clipboard(sprint(io->print(io,x))::String)
    function clipboard()
        systemerror(:OpenClipboard, 0==ccall((:OpenClipboard, "user32"), stdcall, Cint, (Ptr{Void},), C_NULL))
        pdata = ccall((:GetClipboardData, "user32"), stdcall, Ptr{UInt16}, (UInt32,), 13)
        systemerror(:SetClipboardData, pdata==C_NULL)
        systemerror(:CloseClipboard, 0==ccall((:CloseClipboard, "user32"), stdcall, Cint, ()))
        plock = ccall((:GlobalLock, "kernel32"), stdcall, Ptr{UInt16}, (Ptr{UInt16},), pdata)
        systemerror(:GlobalLock, plock==C_NULL)
        # find NUL terminator (0x0000 16-bit code unit)
        len = 0
        while unsafe_load(plock, len+1) != 0; len += 1; end
        # get Vector{UInt16}, transcode data to UTF-8, make a String of it
        s = transcode(String, unsafe_wrap(Array, plock, len))
        systemerror(:GlobalUnlock, 0==ccall((:GlobalUnlock, "kernel32"), stdcall, Cint, (Ptr{UInt16},), plock))
        return s
    end

else
    clipboard(x="") = error("`clipboard` function not implemented for $(Sys.KERNEL)")
end


"""
    clipboard(x)

Send a printed form of `x` to the operating system clipboard ("copy").
"""
clipboard(x)

"""
    clipboard() -> AbstractString

Return a string with the contents of the operating system clipboard ("paste").
"""
clipboard()


# system information

# used by sysinfo.jl
function _show_cpuinfo(io::IO, info::Sys.CPUinfo, header::Bool=true, prefix::AbstractString="    ")
    tck = Sys.SC_CLK_TCK
    if header
        println(io, info.model, ": ")
        print(io, " "^length(prefix))
        if tck > 0
            @printf(io, "    %5s    %9s    %9s    %9s    %9s    %9s\n",
                    "speed", "user", "nice", "sys", "idle", "irq")
        else
            @printf(io, "    %5s    %9s  %9s  %9s  %9s  %9s ticks\n",
                    "speed", "user", "nice", "sys", "idle", "irq")
        end
    end
    print(io, prefix)
    if tck > 0
        @printf(io, "%5d MHz  %9d s  %9d s  %9d s  %9d s  %9d s",
                info.speed, info.cpu_times!user / tck, info.cpu_times!nice / tck,
                info.cpu_times!sys / tck, info.cpu_times!idle / tck, info.cpu_times!irq / tck)
    else
        @printf(io, "%5d MHz  %9d  %9d  %9d  %9d  %9d ticks",
                info.speed, info.cpu_times!user, info.cpu_times!nice,
                info.cpu_times!sys, info.cpu_times!idle, info.cpu_times!irq)
    end
end


"""
    versioninfo(io::IO=STDOUT, verbose::Bool=false)

Print information about the version of Julia in use. If the `verbose` argument is `true`,
detailed system information is shown as well.
"""
function versioninfo(io::IO=STDOUT, verbose::Bool=false)
    println(io,             "Julia Version $VERSION")
    if !isempty(GIT_VERSION_INFO.commit_short)
        println(io,         "Commit $(GIT_VERSION_INFO.commit_short) ($(GIT_VERSION_INFO.date_string))")
    end
    if ccall(:jl_is_debugbuild, Cint, ())!=0
        println(io, "DEBUG build")
    end
    println(io,             "Platform Info:")
    println(io,             "  OS: ", is_windows() ? "Windows" : is_apple() ?
        "macOS" : Sys.KERNEL, " (", Sys.MACHINE, ")")

    cpu = Sys.cpu_info()
    println(io,             "  CPU: ", cpu[1].model)
    println(io,             "  WORD_SIZE: ", Sys.WORD_SIZE)
    if verbose
        lsb = ""
        if is_linux()
            try lsb = readchomp(pipeline(`lsb_release -ds`, stderr=DevNull)) end
        end
        if is_windows()
            try lsb = strip(readstring(`$(ENV["COMSPEC"]) /c ver`)) end
        end
        if lsb != ""
            println(io,     "           ", lsb)
        end
        if is_unix()
            println(io,         "  uname: ", readchomp(`uname -mprsv`))
        end
        println(io,         "Memory: $(Sys.total_memory()/2^30) GB ($(Sys.free_memory()/2^20) MB free)")
        try println(io,     "Uptime: $(Sys.uptime()) sec") end
        print(io,           "Load Avg: ")
        print_matrix(io,    Sys.loadavg()')
        println(io          )
        Sys.cpu_summary(io)
        println(io          )
    end
    if Base.libblas_name == "libopenblas" || BLAS.vendor() == :openblas || BLAS.vendor() == :openblas64
        openblas_config = BLAS.openblas_get_config()
        println(io,         "  BLAS: libopenblas (", openblas_config, ")")
    else
        println(io,         "  BLAS: ",libblas_name)
    end
    println(io,             "  LAPACK: ",liblapack_name)
    println(io,             "  LIBM: ",libm_name)
    println(io,             "  LLVM: libLLVM-",libllvm_version," (", Sys.JIT, ", ", Sys.cpu_name, ")")
    if verbose
        println(io,         "Environment:")
        for (k,v) in ENV
            if match(r"JULIA|PATH|FLAG|^TERM$|HOME", String(k)) !== nothing
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


"""
    code_warntype([io::IO], f, types)

Prints lowered and type-inferred ASTs for the methods matching the given generic function
and type signature to `io` which defaults to `STDOUT`. The ASTs are annotated in such a way
as to cause "non-leaf" types to be emphasized (if color is available, displayed in red).
This serves as a warning of potential type instability. Not all non-leaf types are particularly
problematic for performance, so the results need to be used judiciously.
See [`@code_warntype`](@ref man-code-warntype) for more information.
"""
function code_warntype(io::IO, f, t::ANY)
    emph_io = IOContext(io, :TYPEEMPHASIZE => true)
    for (src, rettype) in code_typed(f, t)
        println(emph_io, "Variables:")
        slotnames = sourceinfo_slotnames(src)
        for i = 1:length(slotnames)
            print(emph_io, "  ", slotnames[i])
            if isa(src.slottypes, Array)
                show_expr_type(emph_io, src.slottypes[i], true)
            end
            print(emph_io, '\n')
        end
        print(emph_io, "\nBody:\n  ")
        body = Expr(:body)
        body.args = src.code
        body.typ = rettype
        # Fix slot names and types in function body
        show_unquoted(IOContext(IOContext(emph_io, :SOURCEINFO => src),
                                          :SOURCE_SLOTNAMES => slotnames),
                      body, 2)
        print(emph_io, '\n')
    end
    nothing
end
code_warntype(f, t::ANY) = code_warntype(STDOUT, f, t)

typesof(args...) = Tuple{map(a->(isa(a,Type) ? Type{a} : typeof(a)), args)...}

gen_call_with_extracted_types(fcn, ex0::Symbol) = Expr(:call, fcn, Meta.quot(ex0))
function gen_call_with_extracted_types(fcn, ex0)
    if isa(ex0, Expr)
        if any(a->(Meta.isexpr(a, :kw) || Meta.isexpr(a, :parameters)), ex0.args)
            # remove keyword args, but call the kwfunc
            args = filter(a->!(Meta.isexpr(a, :kw) || Meta.isexpr(a, :parameters)), ex0.args)
            return quote
                local arg1 = $(esc(args[1]))
                $(fcn)(Core.kwfunc(arg1),
                       Tuple{Vector{Any}, Core.Typeof(arg1),
                             $(typesof)($(map(esc, args[2:end])...)).parameters...})
            end
        elseif ex0.head == :call
            return Expr(:call, fcn, esc(ex0.args[1]),
                        Expr(:call, typesof, map(esc, ex0.args[2:end])...))
        end
    end
    exret = Expr(:none)
    is_macro = false
    ex = expand(ex0)
    if isa(ex0, Expr) && ex0.head == :macrocall # Make @edit @time 1+2 edit the macro
        is_macro = true
        exret = Expr(:call, fcn,  esc(ex0.args[1]), typesof(ex0.args[2:end]...))
    elseif !isa(ex, Expr)
        exret = Expr(:call, :error, "expression is not a function call or symbol")
    elseif ex.head == :call
        if any(e->(isa(e, Expr) && e.head==:(...)), ex0.args) &&
            (ex.args[1] === GlobalRef(Core,:_apply) ||
             ex.args[1] === GlobalRef(Base,:_apply))
            # check for splatting
            exret = Expr(:call, ex.args[1], fcn,
                        Expr(:tuple, esc(ex.args[2]),
                            Expr(:call, typesof, map(esc, ex.args[3:end])...)))
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
    if (!is_macro && ex.head == :thunk) || exret.head == :none
        exret = Expr(:call, :error, "expression is not a function call, "
                                  * "or is too complex for @$fcn to analyze; "
                                  * "break it down to simpler parts if possible")
    end
    exret
end

for fname in [:which, :less, :edit, :functionloc, :code_warntype,
              :code_llvm, :code_llvm_raw, :code_native]
    @eval begin
        macro ($fname)(ex0)
            gen_call_with_extracted_types($(Expr(:quote,fname)), ex0)
        end
    end
end

for fname in [:code_typed, :code_lowered]
    @eval begin
        macro ($fname)(ex0)
            thecall = gen_call_with_extracted_types($(Expr(:quote,fname)), ex0)
            quote
                results = $thecall
                length(results) == 1 ? results[1] : results
            end
        end
    end
end

"""
    @which

Applied to a function or macro call, it evaluates the arguments to the specified call, and
returns the `Method` object for the method that would be called for those arguments. Applied
to a variable, it returns the module in which the variable was bound. It calls out to the
`which` function.
"""
:@which

"""
    @less

Evaluates the arguments to the function or macro call, determines their types, and calls the `less`
function on the resulting expression.
"""
:@less

"""
    @edit

Evaluates the arguments to the function or macro call, determines their types, and calls the `edit`
function on the resulting expression.
"""
:@edit

"""
    @functionloc

Applied to a function or macro call, it evaluates the arguments to the specified call, and
returns a tuple `(filename,line)` giving the location for the method that would be called for those arguments.
It calls out to the `functionloc` function.
"""
:@functionloc

"""
    @code_typed

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_typed`](@ref) on the resulting expression.
"""
:@code_typed

"""
    @code_warntype

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_warntype`](@ref) on the resulting expression.
"""
:@code_warntype

"""
    @code_lowered

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_lowered`](@ref) on the resulting expression.
"""
:@code_lowered

"""
    @code_llvm

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_llvm`](@ref) on the resulting expression.
"""
:@code_llvm

"""
    @code_native

Evaluates the arguments to the function or macro call, determines their types, and calls
[`code_native`](@ref) on the resulting expression.
"""
:@code_native

function type_close_enough(x::ANY, t::ANY)
    x == t && return true
    return (isa(x,DataType) && isa(t,DataType) && x.name === t.name &&
            !isleaftype(t) && x <: t) ||
           (isa(x,Union) && isa(t,DataType) && (type_close_enough(x.a, t) || type_close_enough(x.b, t)))
end

# `methodswith` -- shows a list of methods using the type given
"""
    methodswith(typ[, module or function][, showparents::Bool=false])

Return an array of methods with an argument of type `typ`.

The optional second argument restricts the search to a particular module or function
(the default is all modules, starting from Main).

If optional `showparents` is `true`, also return arguments with a parent type of `typ`,
excluding type `Any`.
"""
function methodswith(t::Type, f::Function, showparents::Bool=false, meths = Method[])
    for d in methods(f)
        if any(function (x)
                   let x = rewrap_unionall(x, d.sig)
                       (type_close_enough(x, t) ||
                        (showparents ? (t <: x && (!isa(x,TypeVar) || x.ub != Any)) :
                         (isa(x,TypeVar) && x.ub != Any && t == x.ub)) &&
                        x != Any && x != ANY)
                   end
               end,
               unwrap_unionall(d.sig).parameters)
            push!(meths, d)
        end
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
    mainmod = Main
    # find modules in Main
    for nm in names(mainmod)
        if isdefined(mainmod, nm)
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
if is_windows()
    function download(url::AbstractString, filename::AbstractString)
        res = ccall((:URLDownloadToFileW,:urlmon),stdcall,Cuint,
                    (Ptr{Void},Cwstring,Cwstring,Cuint,Ptr{Void}),C_NULL,url,filename,0,C_NULL)
        if res != 0
            error("automatic download failed (error: $res): $url")
        end
        filename
    end
else
    function download(url::AbstractString, filename::AbstractString)
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
end
function download(url::AbstractString)
    filename = tempname()
    download(url, filename)
end

"""
    download(url::AbstractString, [localfile::AbstractString])

Download a file from the given url, optionally renaming it to the given local file name.
Note that this function relies on the availability of external tools such as `curl`, `wget`
or `fetch` to download the file and is provided for convenience. For production use or
situations in which more options are needed, please use a package that provides the desired
functionality instead.
"""
download(url, filename)

# workspace management

"""
    workspace()

Replace the top-level module (`Main`) with a new one, providing a clean workspace. The
previous `Main` module is made available as `LastMain`. A previously-loaded package can be
accessed using a statement such as `using LastMain.Package`.

This function should only be used interactively.
"""
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

"""
    runtests([tests=["all"] [, numcores=ceil(Int, Sys.CPU_CORES / 2) ]])

Run the Julia unit tests listed in `tests`, which can be either a string or an array of
strings, using `numcores` processors. (not exported)
"""
function runtests(tests = ["all"], numcores = ceil(Int, Sys.CPU_CORES / 2))
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
              "including error messages above and the output of versioninfo():\n$(readstring(buf))")
    end
end

# testing


"""
    whos(io::IO=STDOUT, m::Module=current_module(), pattern::Regex=r"")

Print information about exported global variables in a module, optionally restricted to those matching `pattern`.

The memory consumption estimate is an approximate lower bound on the size of the internal structure of the object.
"""
function whos(io::IO=STDOUT, m::Module=current_module(), pattern::Regex=r"")
    maxline = displaysize(io)[2]
    line = zeros(UInt8, maxline)
    head = PipeBuffer(maxline + 1)
    for v in sort!(names(m))
        s = string(v)
        if isdefined(m, v) && ismatch(pattern, s)
            value = getfield(m, v)
            @printf head "%30s " s
            try
                if value ∈ (Base, Main, Core)
                    print(head, "              ")
                else
                    bytes = summarysize(value)
                    if bytes < 10_000
                        @printf(head, "%6d bytes  ", bytes)
                    else
                        @printf(head, "%6d KB     ", bytes ÷ (1024))
                    end
                end
                print(head, summary(value))
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
