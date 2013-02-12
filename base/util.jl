# timing

time() = ccall(:clock_now, Float64, ())

function tic()
    t0 = time()
    task_local_storage(:TIMERS, (t0, get(task_local_storage(), :TIMERS, ())))
    return t0
end

function toq()
    t1 = time()
    timers = get(task_local_storage(), :TIMERS, ())
    if is(timers,())
        error("toc() without tic()")
    end
    t0 = timers[1]
    task_local_storage(:TIMERS, timers[2])
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

# print nothing, return value & elapsed time
macro timed(ex)
    quote
        local t0 = time()
        local val = $(esc(ex))
        val, time()-t0
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

# source files, editing, function reflection

function function_loc(f::Function, types)
    for m = methods(f, types)
        if isa(m[3],LambdaStaticData)
            lsd = m[3]::LambdaStaticData
            ln = lsd.line
            if ln > 0
                return (find_in_path(string(lsd.file)), ln)
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
    exret = expr(:call, :error, "expression is not a function call")
    if !isa(ex, Expr)
        # do nothing -> error
    elseif ex.head == :call
        exret = expr(:call, :which, map(esc, ex.args)...)
    elseif ex.head == :body
        a1 = ex.args[1]
        if isa(a1, Expr) && a1.head == :call
            a11 = a1.args[1]
            if isa(a11, TopNode) && a11.name == :assign
                exret = expr(:call, :which, eval(expr(:toplevel, :assign)), map(esc, a1.args[2:end])...)
            end
        end
    elseif ex.head == :thunk
        exret = expr(:call, :error, "expression is not a function call, or is too complex for @which to analyze; "
                                  * "break it down to simpler parts if possible")
    end
    exret
end

edit(file::String) = edit(file, 1)
function edit(file::String, line::Integer)
    editor = get(ENV, "JULIA_EDITOR", "emacs")
    issrc = length(file)>2 && file[end-2:end] == ".jl"
    if issrc
        if file[1]!='/' && !is_file_readable(file)
            file2 = "$JULIA_HOME/../lib/julia/base/$file"
            if is_file_readable(file2)
                file = file2
            end
        end
        if editor == "emacs"
            jmode = "$JULIA_HOME/../../contrib/julia-mode.el"
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

function less(file::String, line::Integer)
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
        error("methods: not a generic function")
    end
    f.env
end

methods(t::CompositeKind) = (methods(t,Tuple);  # force constructor creation
                             t.env)

# help

help_category_list = nothing
help_category_dict = nothing
help_module_dict = nothing
help_function_dict = nothing

function decor_help_desc(func::String, mfunc::String, desc::String)
    sd = split(desc, '\n')
    for i = 1:length(sd)
        if begins_with(sd[i], func)
            sd[i] = mfunc * sd[i][length(func)+1:end]
        else
            break
        end
    end
    return join(sd, '\n')
end

function init_help()
    global help_category_list, help_category_dict,
           help_module_dict, help_function_dict
    if help_category_dict == nothing
        println("Loading help data...")
        helpdb = evalfile("$JULIA_HOME/../share/julia/helpdb.jl")
        help_category_list = {}
        help_category_dict = Dict()
        help_module_dict = Dict()
        help_function_dict = Dict()
        for (cat,mod,func,desc) in helpdb
            if !has(help_category_dict, cat)
                push!(help_category_list, cat)
                help_category_dict[cat] = {}
            end
            if !isempty(mod)
                if begins_with(func, '@')
                    mfunc = "@" * mod * "." * func[2:]
                else
                    mfunc = mod * "." * func
                end
                desc = decor_help_desc(func, mfunc, desc)
            else
                mfunc = func
            end
            push!(help_category_dict[cat], mfunc)
            if !has(help_function_dict, mfunc)
                help_function_dict[mfunc] = {}
            end
            push!(help_function_dict[mfunc], desc)
            if !has(help_module_dict, func)
                help_module_dict[func] = {}
            end
            if !contains(help_module_dict[func], mod)
                push!(help_module_dict[func], mod)
            end
        end
    end
end


function help()
    init_help()
    print(
" Welcome to Julia. The full manual is available at

    http://docs.julialang.org

 To get help on a function, try help(function). To search all help text,
 try apropos(\"string\"). To see available functions, try help(category),
 for one of the following categories:

")
    for cat = help_category_list
        if !isempty(help_category_dict[cat])
            print("  ")
            show(cat); println()
        end
    end
end

function help(cat::String)
    init_help()
    if !has(help_category_dict, cat)
        # if it's not a category, try another named thing
        return help_for(cat)
    end
    println("Help is available for the following items:")
    for func = help_category_dict[cat]
        print(func, " ")
    end
    println()
end

function print_help_entries(entries)
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
    init_help()
    found = false
    if contains(fname, '.')
        if has(help_function_dict, fname)
            print_help_entries(help_function_dict[fname])
            found = true
        end
    else
        macrocall = ""
        if begins_with(fname, '@')
            sfname = fname[2:]
            macrocall = "@"
        else
            sfname = fname
        end
        if has(help_module_dict, fname)
            allmods = help_module_dict[fname]
            alldesc = {}
            for mod in allmods
                mod_prefix = isempty(mod) ? "" : mod * "."
                append!(alldesc, help_function_dict[macrocall * mod_prefix * sfname])
            end
            print_help_entries(alldesc)
            found = true
        end
    end
    if !found
        if isgeneric(obj)
            repl_show(obj); println()
        else
            println("No help information found.")
        end
    end
end

function apropos(txt::String)
    init_help()
    n = 0
    r = Regex("\\Q$txt", PCRE.CASELESS)
    for (cat, _) in help_category_dict
        if ismatch(r, cat)
            println("Category: \"$cat\"")
        end
    end
    for (func, entries) in help_function_dict
        if ismatch(r, func) || any(e->ismatch(r,e), entries)
            for desc in entries
                nl = search(desc,'\n')
                if nl != 0
                    println(desc[1:(nl-1)])
                else
                    println(desc)
                end
            end
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

# print a warning only once

const have_warned = (ByteString=>Bool)[]
function warn_once(msg::String...)
    msg = bytestring(msg...)
    has(have_warned,msg) && return
    have_warned[msg] = true
    warn(msg)
end
