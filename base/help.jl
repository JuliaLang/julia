module Help

export help, apropos, @help

MODULE_DICT   = nothing
FUNCTION_DICT = nothing

function clear_cache()
    global MODULE_DICT   = nothing
    global FUNCTION_DICT = nothing
end

function decor_help_desc(func::AbstractString, mfunc::AbstractString, desc::AbstractString)
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
    file = "helpdb.jl"
    for loc in [Base.locale()]
        fn = joinpath(JULIA_HOME, Base.DOCDIR, loc, file)
        if isfile(fn)
            return fn
        end
    end
    joinpath(JULIA_HOME, Base.DOCDIR, file)
end

function init_help()
    global MODULE_DICT, FUNCTION_DICT
    if FUNCTION_DICT == nothing
        helpdb = evalfile(helpdb_filename())
        MODULE_DICT = Dict()
        FUNCTION_DICT = Dict()
        for (mod,func,desc) in helpdb
            if !isempty(mod)
                mfunc = mod * "." * func
                desc = decor_help_desc(func, mfunc, desc)
            else
                mfunc = func
            end
            if !haskey(FUNCTION_DICT, mfunc)
                FUNCTION_DICT[mfunc] = []
            end
            push!(FUNCTION_DICT[mfunc], desc)
            if !haskey(MODULE_DICT, func)
                MODULE_DICT[func] = []
            end
            if !in(mod, MODULE_DICT[func])
                push!(MODULE_DICT[func], mod)
            end
        end
    end
end

function help(io::IO)
    print(io, """

     Welcome to Julia. The full manual is available at

        http://docs.julialang.org

     To get help, try help(function), help("@macro"), or help("variable").
     To search all help text, try apropos("string").
    """)
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

function help(io::IO, fname::AbstractString, obj=0)
    init_help()
    found = false
    if haskey(FUNCTION_DICT, fname)
        print_help_entries(io, FUNCTION_DICT[fname])
        found = true
    elseif haskey(MODULE_DICT, fname)
        allmods = MODULE_DICT[fname]
        alldesc = []
        for mod in allmods
            mfname = isempty(mod) ? fname : mod * "." * fname
            if isgeneric(obj)
                mf = eval(func_expr_from_symbols(map(symbol, split(mfname, r"(?<!\.)\."))))
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
            println(io, "Symbol not found. Falling back on apropos search ...")
            apropos(io, fname)
        end
    end
end

apropos() = help()

apropos(s::AbstractString) = apropos(STDOUT, s)
function apropos(io::IO, txt::AbstractString)
    init_help()
    n = 0
    r = Regex("\\Q$txt", Base.PCRE.CASELESS)
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

help(io::IO, f::Function) = help(io, string(f), f)
help(io::IO, t::DataType) = help(io, string(t.name), t)
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
help(::IO, args...) = error("too many arguments to help()")

# check whether an expression is a qualified name, e.g. Base.FFTW.FORWARD
isname(n::Symbol) = true
isname(ex::Expr) = ((ex.head == :. && isname(ex.args[1]) && isname(ex.args[2]))
                    || (ex.head == :quote && isname(ex.args[1])))

macro help_(ex)
    if ex === :? || ex === :help
        return Expr(:call, :help)
    elseif !isa(ex, Expr) || isname(ex)
        return Expr(:call, :help, esc(ex))
    elseif ex.head == :macrocall && length(ex.args) == 1
        # e.g., "julia> @help @printf"
        return Expr(:call, :help, string(ex.args[1]))
    else
        return Expr(:macrocall, symbol("@which"), esc(ex))
    end
end

macro help (ex)
  Base.Docs.replhelp(ex)
end

end # module
