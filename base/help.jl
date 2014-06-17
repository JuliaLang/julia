module Help

export help, apropos, doc, @help, @doc
import Base.WeakObjectIdDict
# TODO:
# - switch off when not running interactively (but what about tests?)
# - make @doc work for methods
# - make @doc work for modules

typealias HelpDict Dict{Symbol,Any}
# keys
# :desc : help text
# :mod  : module which contains this object/concept/keyword (if applicable)

# internal data
NOOBJ_DICT = Dict{String, HelpDict}()   # To keep documentation for non-object entities, like keywords, ccall, &&, ||, ", ', etc.  
                                         # And also for macros as they cannot be used as dict-keys directly.
APROPOS_DICT = ObjectIdDict()        # this is used in apropos search. Maps objects to their help text [:desc]
APROPOS_DICT[NOOBJ_DICT] = Dict{String, String}() # to hold the apropos for stuff in NOOBJ_DICT
INIT_OLD_HELP = true # flag

# low level functions
##
function hasdoc(obj)
    if hasmeta(obj, :doc)
        true
    elseif haskey(NOOBJ_DICT, obj)
        true
    else
        false
    end
end

function getdoc(obj, default)
    doc = getmeta(obj, :doc, nothing)
    if doc!=nothing
        doc[:doc]
    elseif haskey(NOOBJ_DICT, obj)
        NOOBJ_DICT[obj]
    else
        default
    end
end
function getdoc(obj)
    doc = getdoc(obj, nothing)
    doc==nothing ? error("No documentation associated with $obj") : doc
end

function setdoc!(obj, doc::HelpDict; string_into_meta=false)
    # If string_into_meta==true then add the doc to the metadata of the
    # string-object, otherwise is goes into the NOOBJ_DICT (default).
    if !isa(obj, String) || string_into_meta
        setmeta!(obj, :doc, doc)
        APROPOS_DICT[obj] = doc[:desc]
    else
        NOOBJ_DICT[obj] = doc
        APROPOS_DICT[NOOBJ_DICT][obj] = doc[:desc]
    end
end

# making docs from helpdb.jl
##

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
    # note that this is slower than the old init_help function, I
    # think because of all the parse-ing
    global INIT_OLD_HELP
    if INIT_OLD_HELP
        INIT_OLD_HELP = false
        println("Loading help data...")
        helpdb = evalfile(helpdb_filename())
        for hd in helpdb
            mod_,obj,desc = hd
            # split objects up between what goes into META and what
            # goes into NOOBJ_DICT
            if obj[1]=='@' # a macro
                obj = obj # keep as string
            else # These try-catch blocks are slow but I don't know
                 # how else to do it:
                try
                    try
                        obj = eval(parse(obj))
                    catch
                        obj = eval(parse(mod_ * "." * obj))
                    end
                catch
                    obj = obj # keep as string
                end
            end
            try
                mod_ = eval(parse(mod_))
            catch
                mod_ = mod_ # keep as string
            end
            if !hasdoc(obj) # do not overwrite existing doc
                hd = HelpDict() 
                hd[:desc]= desc; hd[:mod]=mod_; 
                setdoc!(obj, hd)
            else # append to it
                hd = getdoc(obj)
                hd[:desc] *= "\n$(string(mod_)).$desc"
            end
        end
    end
end

# user-created help
##
function doc(obj, docstr::String; mod=nothing, string_into_meta=false)
    # the module cannot be set automatically with this function, use the macro instead
    hd = HelpDict()
    hd[:desc] = docstr
    hd[:mod] = mod
    setdoc!(obj, hd; string_into_meta=string_into_meta)
end

macro doc(args...)
    errmsg = """
             @doc input needs to be:
              - doc-string, Julia-object
             """
    if length(args)==2
        docstr, obj = args
    elseif length(args)==3
        docstr, obj = args
    else
        error(errmsg)
    end
    # a few cases depending on the object type:
    if isa(obj, Symbol)
        return quote
            key = $(esc(obj))
            doc(key, $(esc(docstr)); mod=current_module(), string_into_meta=true)
        end
    elseif isa(obj, Expr)
        if obj.head==:macro
            macrostr = "@" * string(obj.args[1].args[1])
            return quote
                $(esc(obj))
                doc($macrostr, $(esc(docstr)); mod=current_module())
            end
        elseif obj.head==:type
            symbol = obj.args[2]
            return quote
                $(esc(obj))
                doc($(esc(symbol)), $(esc(docstr)); mod=current_module())
            end
        elseif obj.head==:abstract # abstract types
            symbol = obj.args[1]
            return quote
                $(esc(obj))
                doc($(esc(symbol)), $(esc(docstr)); mod=current_module())
            end
        elseif obj.head==:module 
            # https://groups.google.com/d/msg/julia-users/bnAYUdHzmQA/zzVOr4IrUO0J
            error("@doc not implemented for modules use doc function instead.")
        else # functions
            return quote
                key = $(esc(obj))
                doc(key, $(esc(docstr)); mod=current_module())
            end
            ## for specific methods this would look like:
            # return quote
            #     genfn = $(esc(obj))
            #     meth = collect(methods(genfn))[end]
            #     doc(meth, $(esc(docstr)); mod=current_module())
            # end
        end
    else # literals, e.g. 5, "a string"
        return quote
            key = $(esc(obj))
            doc(key, $(esc(docstr)); mod=current_module())
        end
    end
end

# get help
##
function help(io::IO)
    print(io, """

     Welcome to Julia. The full manual is available at

        http://docs.julialang.org

     To get help, try help(function), help("@macro"), or help("variable").
     To search all help text, try apropos("string").
    """)
end

function _decor_help_desc(obj, mod, desc::String)
    # adds information about module, signature and such.  Probably needs updating.
    func = string(obj)
    if !isequal(mod,nothing)
        mfunc = string(mod) * "." * func
    else
        mfunc = string(obj)
    end

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

function help(io::IO, obj)
    init_help()
    docdict = getdoc(obj, nothing)
    if docdict!=nothing
        out = _decor_help_desc(obj, docdict[:mod], docdict[:desc])
        print(io, out)
    else 
        if isa(obj, DataType) # try to print something generic:
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
            writemime(io, "text/plain", obj); println(io)
        else
            println(io, "Symbol not found. Falling back on apropos search ...")
            apropos(io, fname)
        end
    end
end

apropos() = help()

help(args...) = help(STDOUT, args...)

apropos(s::String) = apropos(STDOUT, s)
function apropos(io::IO, txt::String)
    # This could be done better.
    init_help()
    n = 0
    r = Regex("\\Q$txt", Base.PCRE.CASELESS)
    for (obj, desc) in APROPOS_DICT
        if isequal(obj,NOOBJ_DICT)
            for (objj, desc) in obj
                if ismatch(r, string(obj)) || ismatch(r, desc)
                    println(io, "Object: \"$objj\"")
                    n = 1
                end
            end
        else
            if ismatch(r, string(obj)) || ismatch(r, desc)
                println(io, "Object: \"$obj\"")
                n = 1
            end
        end
    end
    if n == 0
        println(io, "No help information found.")
    end
end

# check whether an expression is a qualified name, e.g. Base.FFTW.FORWARD
isname(n::Symbol) = true
isname(ex::Expr) = ((ex.head == :. && isname(ex.args[1]) && isname(ex.args[2]))
                    || (ex.head == :quote && isname(ex.args[1])))

macro help(ex) # this macro is needed for calling help like e.g.: `?sin` 
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
