module Help

export help, apropos, @help

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
            if !haskey(CATEGORY_DICT, cat)
                push!(CATEGORY_LIST, cat)
                CATEGORY_DICT[cat] = {}
            end
            if !isempty(mod)
                mfunc = mod * "." * func
                desc = decor_help_desc(func, mfunc, desc)
            else
                mfunc = func
            end
            push!(CATEGORY_DICT[cat], mfunc)
            if !haskey(FUNCTION_DICT, mfunc)
                FUNCTION_DICT[mfunc] = {}
            end
            push!(FUNCTION_DICT[mfunc], desc)
            if !haskey(MODULE_DICT, func)
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
    if !haskey(CATEGORY_DICT, cat)
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
    if haskey(FUNCTION_DICT, fname)
        print_help_entries(FUNCTION_DICT[fname])
        found = true
    else
        if haskey(MODULE_DICT, fname)
            allmods = MODULE_DICT[fname]
            alldesc = {}
            for mod in allmods
                mfname = isempty(mod) ? fname : mod * "." * fname
                append!(alldesc, FUNCTION_DICT[mfname])
            end
            print_help_entries(alldesc)
            found = true
        end
    end
    if !found
        if isa(obj, DataType)
            print("DataType   : ")
            repl_show(obj)
            println()
            println("  supertype: ", super(obj))
            if obj.abstract
                st = subtypes(obj)
                if length(st) > 0
                    print("  subtypes : ")
                    showcompact(st)
                    println()
                end
            end
            if length(obj.names) > 0
                println("  fields   : ", obj.names)
            end
        elseif isgeneric(obj)
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
help(t::Module) = help(string(t))

function help(x)
    show(x)
    t = typeof(x)
    if isa(t,DataType)
        println(" is of type")
        help(t)
    else
        println(" is of type $t")
    end
end

macro help(ex)
    if !isa(ex, Expr)
        return Expr(:call, :help, esc(ex))
    elseif ex.head == :macrocall && length(ex.args) == 1
        # e.g., "julia> @help @printf"
        return Expr(:call, :help, string(ex.args[1]))
    else
        return Expr(:macrocall, symbol("@which"), esc(ex))
    end
end

end # module
