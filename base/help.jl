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
    sd = convert(Array{ByteString,1}, split(desc, '\n'))
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
        info("Loading help data...")
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
            if !in(mod, MODULE_DICT[func])
                push!(MODULE_DICT[func], mod)
            end
        end
    end
end

function help(io::IO)
    init_help()
    print(io,
"""

 Welcome to Julia. The full manual is available at

    http://docs.julialang.org

 To get help, try help(function), help("@macro"), or help("variable").
 To search all help text, try apropos("string"). To see available functions,
 try help(category), for one of the following categories:

""")
    for cat = CATEGORY_LIST
        if !isempty(CATEGORY_DICT[cat])
            print(io, "  ")
            show(io, cat); println(io)
        end
    end
end

function help(io::IO, cat::String)
    init_help()
    if !haskey(CATEGORY_DICT, cat)
        # if it's not a category, try another named thing
        return help_for(io, cat)
    end
    println(io, "Help is available for the following items:")
    for func = CATEGORY_DICT[cat]
        print(io, func, " ")
    end
    println(io)
end

function print_help_entries(io::IO, entries)
    first = true
    for desc in entries
        if !first
            println(io)
        end
        println(io, strip(desc))
        first = false
    end
end

func_expr_from_symbols(s::Vector{Symbol}) = length(s) == 1 ? s[1] : Expr(:., func_expr_from_symbols(s[1:end-1]), Expr(:quote, s[end]))

help_for(io::IO, s::String) = help_for(io, s, 0)

function help_for(io::IO, fname::String, obj)
    init_help()
    found = false
    if haskey(FUNCTION_DICT, fname)
        print_help_entries(FUNCTION_DICT[fname])
        found = true
    elseif haskey(MODULE_DICT, fname)
        allmods = MODULE_DICT[fname]
        alldesc = {}
        for mod in allmods
            mfname = isempty(mod) ? fname : mod * "." * fname
            if isgeneric(obj)
                mf = eval(func_expr_from_symbols(map(symbol, split(mfname, "."))))
                if mf === obj
                    append!(alldesc, FUNCTION_DICT[mfname])
                    found = true
                end
            else
                append!(alldesc, FUNCTION_DICT[mfname])
                found = true
            end
        end
        found && print_help_entries(io, alldesc)
    elseif haskey(FUNCTION_DICT, "Base." * fname)
        print_help_entries(io, FUNCTION_DICT["Base." * fname])
        found = true
    end
    if !found
        if isa(obj, DataType)
            print(io, "DataType   : ")
            writemime(io, "text/plain", obj)
            println(io)
            println(io, "  supertype: ", super(obj))
            if obj.abstract
                st = subtypes(obj)
                if length(st) > 0
                    print(io, "  subtypes : ")
                    showcompact(io, st)
                    println(io)
                end
            end
            if length(obj.names) > 0
                println(io, "  fields   : ", obj.names)
            end
        elseif isgeneric(obj)
            writemime(io, "text/plain", obj); println()
        else
            println(io, "No help information found.")
        end
    end
end

function apropos(io::IO, txt::String)
    init_help()
    n = 0
    r = Regex("\\Q$txt", Base.PCRE.CASELESS)
    for (cat, _) in CATEGORY_DICT
        if ismatch(r, cat)
            println(io, "Category: \"$cat\"")
        end
    end
    for (func, entries) in FUNCTION_DICT
        if ismatch(r, func) || any(e->ismatch(r,e), entries)
            for desc in entries
                nl = search(desc,'\n')
                if nl != 0
                    println(io, desc[1:(nl-1)])
                else
                    println(io, desc)
                end
            end
            n+=1
        end
    end
    if n == 0
        println(io, "No help information found.")
    end
end

function help(io::IO, f::Function)
    if is(f,help)
        return help(io)
    end
    help_for(io, string(f), f)
end

help(io::IO, t::DataType) = help_for(io, string(t.name),t)
help(io::IO, t::Module) = help(io, string(t))

function help(io::IO, x)
    show(io, x)
    t = typeof(x)
    if isa(t,DataType)
        println(io, " is of type")
        help(io, t)
    else
        println(io, " is of type $t")
    end
end

help(args...) = help(STDOUT, args...)

# check whether an expression is a qualified name, e.g. Base.FFTW.FORWARD
isname(n::Symbol) = true
isname(ex::Expr) = ((ex.head == :. && isname(ex.args[1]) && isname(ex.args[2]))
                    || (ex.head == :quote && isname(ex.args[1])))

macro help(ex)
    if !isa(ex, Expr) || isname(ex)
        return Expr(:call, :help, esc(ex))
    elseif ex.head == :macrocall && length(ex.args) == 1
        # e.g., "julia> @help @printf"
        return Expr(:call, :help, string(ex.args[1]))
    else
        return Expr(:macrocall, symbol("@which"), esc(ex))
    end
end

end # module
