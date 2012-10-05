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

# high-resolution time
# returns in nanoseconds
function time_ns()
    return ccall(:jl_hrtime, Uint64, ())
end

# print elapsed time, return expression value
macro time(ex)
    quote
        local t0 = time()
        local val = $(esc(ex))
        local t1 = time()
        println("elapsed time: ", t1-t0, " seconds")
        val
    end
end

# print nothing, return elapsed time
macro elapsed(ex)
    quote
        local t0 = time()
        local val = $(esc(ex))
        time()-t0
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

# source files, editing, function reflection

function function_loc(f::Function, types)
    for m = methods(f, types)
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

function whicht(f, types)
    for m = methods(f, types)
        if isa(m[3],LambdaStaticData)
            lsd = m[3]::LambdaStaticData
            d = f.env.defs
            while !is(d,())
                if is(d.func.code, lsd)
                    print(stdout_stream, f.env.name)
                    show(stdout_stream, d); println(stdout_stream)
                    return
                end
                d = d.next
            end
        end
    end
end

which(f, args...) = whicht(f, map(a->(isa(a,Type) ? Type{a} : typeof(a)), args))

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
            error("Invalid JULIA_EDITOR value: $(repr(editor))")
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
            error("Invalid JULIA_EDITOR value: $(repr(editor))")
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

disassemble(f::Function, types::Tuple) =
    print(ccall(:jl_dump_function, Any, (Any,Any), f, types)::ByteString)

function methods(f::Function)
    if !isgeneric(f)
        error("methods: error: not a generic function")
    end
    f.env
end

methods(t::CompositeKind) = (methods(t,Tuple);  # force constructor creation
                             t.env)


# require
# Store list of files and their load time
global _jl_package_list = Dict{ByteString,Float64}()
require(fname::String) = require(bytestring(fname))
require(f::String, fs::String...) = (require(f); for x in fs require(x); end)
function require(name::ByteString)
    path = find_in_path(name)
    if !has(_jl_package_list,path)
        load_now(name)
    else
        # Determine whether the file has been modified since it was last loaded
        if mtime(path) > _jl_package_list[path]
            load_now(name)
        end
    end
end

const load = require

# remote/parallel load

include_string(txt::ByteString) = ccall(:jl_load_file_string, Void, (Ptr{Uint8},), txt)

function is_file_readable(path)
    local f
    try
        f = open(bytestring(path))
    catch
        return false
    end
    close(f)
    return true
end

function find_in_path(fname)
    if fname[1] == '/'
        return real_path(fname)
    end
    for pfx in LOAD_PATH
        if pfx != "" && pfx[end] != '/'
            pfxd = strcat(pfx,"/",fname)
        else
            pfxd = strcat(pfx,fname)
        end
        if is_file_readable(pfxd)
            return real_path(pfxd)
        end
    end
    return real_path(fname)
end

begin
local in_load = false
local in_remote_load = false
local load_dict = {}
global load_now, remote_load

load_now(fname::String) = load_now(bytestring(fname))
function load_now(fname::ByteString)
    if in_load
        path = find_in_path(fname)
        push(load_dict, fname)
        f = open(path)
        push(load_dict, readall(f))
        close(f)
        include(path)
        _jl_package_list[path] = time()
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
            ccall(:jl_register_toplevel_eh, Void, ())
            load_now(fname)
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
        load_now(dict[1])
    catch e
        in_remote_load = false
        throw(e)
    end
    in_remote_load = false
    nothing
end
end

evalfile(fname::String) = eval(parse(readall(fname))[1])

# help

_jl_help_category_list = nothing
_jl_help_category_dict = nothing
_jl_help_function_dict = nothing

function _jl_init_help()
    global _jl_help_category_list, _jl_help_category_dict, _jl_help_function_dict
    if _jl_help_category_dict == nothing
        println("Loading help data...")
        helpdb = evalfile("$JULIA_HOME/../lib/julia/helpdb.jl")
        _jl_help_category_list = {}
        _jl_help_category_dict = Dict()
        _jl_help_function_dict = Dict()
        for (cat,func,desc) in helpdb
            if !has(_jl_help_category_dict, cat)
                push(_jl_help_category_list, cat)
                _jl_help_category_dict[cat] = {}
            end
            push(_jl_help_category_dict[cat], func)
            if !has(_jl_help_function_dict, func)
                _jl_help_function_dict[func] = {}
            end
            push(_jl_help_function_dict[func], desc)
        end
    end
end

function help()
    _jl_init_help()
    print(
" Welcome to Julia. The full manual is available at

    http://docs.julialang.org

 To get help on a function, try help(function). To search all help text,
 try apropos(\"string\"). To see available functions, try help(category),
 for one of the following categories:

")
    for cat = _jl_help_category_list
        if !isempty(_jl_help_category_dict[cat])
            print("  ")
            show(cat); println()
        end
    end
end

function help(cat::String)
    _jl_init_help()
    if !has(_jl_help_category_dict, cat)
        # if it's not a category, try another named thing
        return help_for(cat)
    end
    println("Help is available for the following items:")
    for func = _jl_help_category_dict[cat]
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
        println(strip(desc))
        first = false
    end
end

help_for(s::String) = help_for(s, 0)
function help_for(fname::String, obj)
    _jl_init_help()
    if has(_jl_help_function_dict, fname)
        _jl_print_help_entries(_jl_help_function_dict[fname])
    else
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
    r = Regex("\\Q$txt", PCRE.CASELESS)
    first = true
    for (cat, _) in _jl_help_category_dict
        if ismatch(r, cat)
            println("Category: \"$cat\"")
            first = false
        end
    end
    for (func, entries) in _jl_help_function_dict
        if ismatch(r, func) || anyp(e->ismatch(r,e), entries)
            if !first
                println()
            end
            _jl_print_help_entries(entries)
            first = false
            n+=1
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

# misc

times(f::Function, n::Int) = for i=1:n f() end
