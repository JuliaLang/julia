module Help

export help, apropos, doc, @help, @doc
import Base.WeakObjectIdDict
# TODO:
# - switch off when not running interactively (but what about tests?)
# - make @doc work for methods
# - make @doc work for modules
# - make an APROPOS_DICT mapping keywords to objects


# :mod  : module which contains this object/concept/keyword (if applicable)
type HelpEntry
    desc::String                       # description
    mod::Union(Module,String)  # module of entry. This is useful for type instances
                                       # for which figuring out the defining module is hard.
    HelpEntry() = new("", "")
    HelpEntry(desc) = new(desc, "")
    HelpEntry(desc, mod) = new(desc, mod)
end

# internal data
NOOBJ_DICT = Dict{String, HelpEntry}()   # To keep documentation for non-object entities, like keywords, ccall, &&, ||, ", ', etc.  
                                         # And also for macros as they cannot be used as dict-keys directly.
INIT_OLD_HELP = true # flag

# low level functions
##
function hasdoc(obj)
    if hasmeta(obj, :doc) || haskey(NOOBJ_DICT, obj)
        true
    else
        false
    end
end

function getdoc(obj)
    doc = getdoc(obj, nothing)
    doc==nothing ? error("No documentation associated with $obj") : doc
end
function getdoc(obj, default)
    if hasmeta(obj, :doc)
        getmeta(obj, :doc)
    elseif haskey(NOOBJ_DICT, obj)
        NOOBJ_DICT[obj]
    else
        default
    end
end
function getdoc!(obj; string_into_meta=false)
    # If string_into_meta==true then add the doc to the metadata of the
    # string-object, otherwise is goes into the NOOBJ_DICT (default).
    if hasdoc(obj)
        return getdoc(obj)
    else
        he = HelpEntry()
        setdoc!(obj, he; string_into_meta=string_into_meta)
        return he
    end
end


function setdoc!(obj, doc::HelpEntry; string_into_meta=false)
    # If string_into_meta==true then add the doc to the metadata of the
    # string-object, otherwise is goes into the NOOBJ_DICT (default).
    if !isa(obj, String) || string_into_meta
        setmeta!(obj, :doc, doc)
    else
        NOOBJ_DICT[obj] = doc
    end
    nothing
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
        for he in helpdb
            mod_,obj,desc = he
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
            henew = getdoc!(obj)
            henew.desc *= desc
            henew.mod = mod_
        end
    end
end

# user-created help
##
function doc(obj, docstr::String; mod="", string_into_meta=false)
    # the module cannot be set automatically with this function, use the macro instead
    setdoc!(obj, HelpEntry(docstr, mod); string_into_meta=string_into_meta)
end

macro doc(args...)
    errmsg = """
             @doc input needs to be:
              - doc-string, Julia-object
             """
    if length(args)==2
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
    if !isequal(mod,"")
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
    if hasdoc(obj)
        he = getdoc(obj)
        out = _decor_help_desc(obj, he.mod, he.desc)
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
    init_help()
    n = 0
    r = Regex("\\Q$txt", Base.PCRE.CASELESS)
    println(io, "The string '$txt' appears in the documentation of following objects:")
    for (obj, cont) in Base._META
        if hasdoc(obj)
            desc = getdoc(obj).desc
            if ismatch(r, string(obj)) || ismatch(r, desc)
                println(io, string(obj))
            end
        end
        n+=1
    end
    for (obj, cont) in NOOBJ_DICT
        desc = cont.desc
        if ismatch(r, obj) || ismatch(r, desc)
            println(io, obj)
        end
        n+=1
    end
    if n == 0
        println(io, "Non occurrence found.")
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
