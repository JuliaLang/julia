module Help

export help, apropos

CATEGORY_LIST = nothing
CATEGORY_DICT = nothing
MODULE_DICT   = nothing
FUNCTION_DICT = nothing

function clear_cache()
    global CATEGORY_LIST = nothing
    global CATEGORY_DICT = nothing
    global MODULE_DICT   = nothing
    global FUNCTION_DICT = nothing
end

function decor_help_desc(func::String, mfunc::String, desc::String)
    sd = split(desc, '\n')
    for i = 1:length(sd)
        if beginswith(sd[i], func)
            sd[i] = mfunc * sd[i][length(func)+1:end]
        else
            break
        end
    end
    return join(sd, '\n')
end

function helpdb_filename()
    root = "$JULIA_HOME/../share/julia"
    file = "helpdb.jl"
    for loc in [Base.locale()]
        fn = joinpath(root, loc, file)
        if isfile(fn)
            return fn
        end
    end
    joinpath(root, file)
end

function init_help()
    global CATEGORY_LIST, CATEGORY_DICT,
           MODULE_DICT, FUNCTION_DICT
    if CATEGORY_DICT == nothing
        println("Loading help data...")
        helpdb = evalfile(helpdb_filename())
        CATEGORY_LIST = {}
        CATEGORY_DICT = Dict()
        MODULE_DICT = Dict()
        FUNCTION_DICT = Dict()
        for (cat,mod,func,desc) in helpdb
            if !has(CATEGORY_DICT, cat)
                push!(CATEGORY_LIST, cat)
                CATEGORY_DICT[cat] = {}
            end
            if !isempty(mod)
                if beginswith(func, '@')
                    mfunc = "@" * mod * "." * func[2:]
                else
                    mfunc = mod * "." * func
                end
                desc = decor_help_desc(func, mfunc, desc)
            else
                mfunc = func
            end
            push!(CATEGORY_DICT[cat], mfunc)
            if !has(FUNCTION_DICT, mfunc)
                FUNCTION_DICT[mfunc] = {}
            end
            push!(FUNCTION_DICT[mfunc], desc)
            if !has(MODULE_DICT, func)
                MODULE_DICT[func] = {}
            end
            if !contains(MODULE_DICT[func], mod)
                push!(MODULE_DICT[func], mod)
            end
        end
    end
end

function help()
    init_help()
    print(
"""

 Welcome to Julia. The full manual is available at

    http://docs.julialang.org

 To get help, try help(function), help("@macro"), or help("variable").
 To search all help text, try apropos("string"). To see available functions,
 try help(category), for one of the following categories:

""")
    for cat = CATEGORY_LIST
        if !isempty(CATEGORY_DICT[cat])
            print("  ")
            show(cat); println()
        end
    end
end

function help(cat::String)
    init_help()
    if !has(CATEGORY_DICT, cat)
        # if it's not a category, try another named thing
        return help_for(cat)
    end
    println("Help is available for the following items:")
    for func = CATEGORY_DICT[cat]
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
    if has(FUNCTION_DICT, fname)
        print_help_entries(FUNCTION_DICT[fname])
        found = true
    else
        macrocall = ""
        if beginswith(fname, '@')
            sfname = fname[2:]
            macrocall = "@"
        else
            sfname = fname
        end
        if has(MODULE_DICT, fname)
            allmods = MODULE_DICT[fname]
            alldesc = {}
            for mod in allmods
                mod_prefix = isempty(mod) ? "" : mod * "."
                append!(alldesc, FUNCTION_DICT[macrocall * mod_prefix * sfname])
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
    for (cat, _) in CATEGORY_DICT
        if ismatch(r, cat)
            println("Category: \"$cat\"")
        end
    end
    for (func, entries) in FUNCTION_DICT
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

help(t::DataType) = help_for(string(t.name),t)

function help(x)
    show(x)
    t = typeof(x)
    println(" is of type $t")
    if isa(t,DataType) && length(t.names)>0
        println("  which has fields $(t.names)")
    end
end

end # module
