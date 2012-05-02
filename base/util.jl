# shell

ls() = system("ls")

catfile(file::String) = system(strcat("cat ", file))

# timing

time() = ccall(:clock_now, Float64, ())

function tic()
    t0 = time()
    tls(:TIMERS, (t0, get(tls(), :TIMERS, ())))
    return t0
end

function toq()
    t1 = time()
    timers = get(tls(), :TIMERS, ())
    if is(timers,())
        error("toc() without tic()")
    end
    t0 = timers[1]
    tls(:TIMERS, timers[2])
    t1-t0
end

function toc()
    t = toq()
    println("elapsed time: ", t, " seconds")
    return t
end

# print elapsed time, return expression value
macro time(ex)
    @gensym t0 val t1
    quote
        local $t0 = time()
        local $val = $ex
        local $t1 = time()
        println("elapsed time: ", $t1-$t0, " seconds")
        $val
    end
end

# print nothing, return elapsed time
macro elapsed(ex)
    @gensym t0 val t1
    quote
        local $t0 = time()
        local $val = $ex
        local $t1 = time()
        $t1-$t0
    end
end

function peakflops()
    a = rand(2000,2000)
    t = @elapsed a*a
    t = @elapsed a*a
    floprate = (2*2000.0^3/t)
    println("The peak flop rate is ", floprate*1e-9, " gigaflops")
    floprate
end

# source files, editing

function function_loc(f::Function, types)
    for m = getmethods(f, types)
        if isa(m[3],LambdaStaticData)
            lsd = m[3]::LambdaStaticData
            ln = lsd.line
            if ln > 0
                return (string(lsd.file), ln)
            end
        end
    end
    error("could not find function definition")
end
function_loc(f::Function) = function_loc(f, (Any...))

edit(file::String) = edit(file, 1)
function edit(file::String, line::Int)
    editor = get(ENV, "JULIA_EDITOR", "emacs")
    issrc = file[end-2:end] == ".jl"
    if issrc
        if file[1]!='/' && !is_file_readable(file)
            file2 = "$JULIA_HOME/base/$file"
            if is_file_readable(file2)
                file = file2
            end
        end
        if editor == "emacs"
            jmode = "$JULIA_HOME/contrib/julia-mode.el"
            run(`emacs $file --eval "(progn
                                     (require 'julia-mode \"$jmode\")
                                     (julia-mode)
                                     (goto-line $line))"`)
        elseif editor == "vim"
            run(`vim $file +$line`)
        elseif editor == "textmate"
            run(`mate $file -l $line`)
        elseif editor == "subl"
            run(`subl $file:$line`)
        else
            error("Invalid JULIA_EDITOR value: $(sprint(show, editor))")
        end
    else
        if editor == "emacs"
            run(`emacs $file --eval "(goto-line $line)"`)
        elseif editor == "vim"
            run(`vim $file +$line`)
        elseif editor == "textmate"
            run(`mate $file -l $line`)
        elseif editor == "subl"
            run(`subl $file:$line`)
        else
            error("Invalid JULIA_EDITOR value: $(sprint(show, editor))")
        end
    end
    nothing
end
edit(file::String) = edit(file, 1)

function less(file::String, line::Int)
    pager = get(ENV, "PAGER", "less")
    run(`$pager +$(line)g $file`)
end
less(file::String) = less(file, 1)

edit(f::Function)    = edit(function_loc(f)...)
edit(f::Function, t) = edit(function_loc(f,t)...)
less(f::Function)    = less(function_loc(f)...)
less(f::Function, t) = less(function_loc(f,t)...)

# remote/parallel load

include_string(txt::ByteString) = ccall(:jl_load_file_string, Void, (Ptr{Uint8},), txt)

function is_file_readable(path)
    local f
    try
        f = open(cstring(path))
    catch
        return false
    end
    close(f)
    return true
end

function find_in_path(fname)
    if fname[1] == '/'
        return fname
    end
    for pfx in LOAD_PATH
        if pfx != "" && pfx[end] != '/'
            pfxd = strcat(pfx,"/",fname)
        else
            pfxd = strcat(pfx,fname)
        end
        if is_file_readable(pfxd)
            return pfxd
        end
    end
    return fname
end

begin
local in_load = false
local in_remote_load = false
local load_dict = {}
global load, remote_load

load(fname::String) = load(cstring(fname))
function load(fname::ByteString)
    if in_load
        path = find_in_path(fname)
        push(load_dict, fname)
        f = open(path)
        push(load_dict, readall(f))
        close(f)
        include(path)
        return
    elseif in_remote_load
        for i=1:2:length(load_dict)
            if load_dict[i] == fname
                return include_string(load_dict[i+1])
            end
        end
    else
        in_load = true
        iserr, err = false, ()
        try
            load(fname)
            for p = 1:nprocs()
                if p != myid()
                    remote_do(p, remote_load, load_dict)
                end
            end
        catch e
            iserr, err = true, e
        end
        load_dict = {}
        in_load = false
        if iserr throw(err); end
    end
end

function remote_load(dict)
    load_dict = dict
    in_remote_load = true
    try
        load(dict[1])
    catch e
        in_remote_load = false
        throw(e)
    end
    in_remote_load = false
    nothing
end
end

# help

function parse_help(stream)
    helpdb = Dict()
    for l = each_line(stream)
        if isempty(l)
            continue
        end
        if length(l) >= 3 && l[1:3]=="## "
            heading = l[4:end-1]
            category = Dict()
            helpdb[heading] = category
            continue
        end
        if l[1]=='`'
            parts = split(l, '—')
            sig = parts[1][2:end-2]
            if length(parts) > 1
                desc = parts[2]
            else
                desc = ""
            end
            m = match(r"(\w+!?)\(", sig)
            if m != nothing
                # found something of the form "f("
                funcname = m.captures[1]
            else
                # otherwise use whatever's between the ``
                funcname = sig
            end
            entry = (sig, desc)
            if has(category,funcname)
                push(category[funcname], entry)
            else
                category[funcname] = {entry}
            end
        end
    end
    helpdb
end

_jl_helpdb = nothing

const _jl_help_url = "https://raw.github.com/JuliaLang/julialang.github.com/master/manual/standard-library-reference/index.md"

function _jl_init_help()
    global _jl_helpdb
    if _jl_helpdb == nothing
        println("Downloading help data...")
        cmd = `curl -s $_jl_help_url`
        stream = fdio(read_from(cmd).fd, true)
        spawn(cmd)
        _jl_helpdb = parse_help(stream)
    end
end

function help()
    _jl_init_help()
    print(
" Welcome to Julia. The full manual is available at

    http://julialang.org/manual/

 To get help on a function, try help(function). To search all help text,
 try apropos(\"string\"). To see available functions, try help(category),
 for one of the following categories:

")
    for (cat, tabl) = _jl_helpdb
        if !isempty(tabl)
            print("  ")
            show(cat); println()
        end
    end
end

function help(cat::String)
    _jl_init_help()
    if !has(_jl_helpdb, cat)
        # if it's not a category, try another named thing
        return help_for(cat)
    end
    println("Help is available for the following items:")
    for (func, _) = _jl_helpdb[cat]
        print(func, " ")
    end
    println()
end

function _jl_print_help_entries(entries)
    first = true
    for desc in entries
        if !first
            println()
        end
        print(desc[1], "\n ", desc[2])
        first = false
    end
end

help_for(s::String) = help_for(s, 0)
function help_for(fname::String, obj)
    _jl_init_help()
    n = 0
    for (cat, tabl) = _jl_helpdb
        for (func, entries) = tabl
            if func == fname
                _jl_print_help_entries(entries)
                n+=1
                break
            end
        end
    end
    if n == 0
        if isgeneric(obj)
            repl_show(obj); println()
        else
            println("No help information found.")
        end
    end
end

function apropos(txt::String)
    _jl_init_help()
    n = 0
    r = Regex("\\Q$txt", PCRE_CASELESS)
    first = true
    for (cat, tabl) = _jl_helpdb
        if matches(r, cat)
            println("Category: \"$cat\"")
            first = false
        end
    end
    for (cat, tabl) = _jl_helpdb
        for (func, entries) = tabl
            if matches(r, func) || anyp(e->(matches(r,e[1]) || matches(r,e[2])),
                                        entries)
                if !first
                    println()
                end
                _jl_print_help_entries(entries)
                first = false
                n+=1
            end
        end
    end
    if n == 0
        println("No help information found.")
    end
end

function help(f::Function)
    if is(f,help)
        return help()
    end
    help_for(string(f), f)
end

help(t::CompositeKind) = help_for(string(t.name),t)

function help(x)
    show(x)
    t = typeof(x)
    println(" is of type $t")
    if isa(t,CompositeKind)
        println("  which has fields $(t.names)")
    end
end
