module Docstrings

import Base.Meta: isexpr

export @doc, @docstrings, docs, parsedocs, setmeta!

# Name of global used to store a module's Metadata object.
const METADATA = :__METADATA__


## types


# Lazy-loading documentation object. Initially the raw documentation string is stored in
# `data` while `obj` field remains undefined. The parsed documentation AST/object/etc. is
# cached in `obj` on first request for it.
type Docs{format} # format::Symbol
    data::String
    obj

    Docs(data::String) = new(data)

    # Pass `Docs` objects straight through. Simplifies code in `Entry` constructor.
    Docs(docs::Docs) = docs
end

# Getter for the parsed object stored in a `Docs` object. When the field is undefined
# parse the `data` field and store the result in `obj`.
docs(docs::Docs) = isdefined(docs, :obj) ? docs.obj : parsedocs(docs)

# Provide format-specific (eg. ::Docs{:md} for Markdown) methods that parse the `data`
# field into an AST, assign it to the `obj` field, and also return the AST.
parsedocs(docs::Docs) = error("No parser available docstring format.")

# Get doc format from file extension. Empty docstring created when file does not exist.
function externaldocs(mod, meta)
    file = abspath(joinpath(metadata(mod).meta[:root]), get(meta, :file, ""))
    isfile(file) ? readdocs(file) : Docs{metadata(mod).meta[:format]}("")
end

# Read contents of a file into a `Docs{format}` object where `format` is the file extension.
readdocs(file) = Docs{format(file)}(readall(file))

# Extract the format of a file based on the file's extension.
format(file) = symbol(splitext(file)[end][2:end])

# Stores the documentation and additional metadata associated with an object.
type Entry{category} # category::Symbol
    modname::Module
    docs::Docs
    meta::Dict{Symbol, Any}

    function Entry(modname::Module, source, doc, meta::Dict = Dict())
        meta[:source] = source
        new(modname, Docs{metadata(modname).meta[:format]}(doc), meta)
    end

    # No docstring was provided, try to read from :file. Blank docs field when no :file.
    function Entry(modname::Module, source, meta::Dict = Dict())
        meta[:source] = source
        new(modname, externaldocs(modname, meta), meta)
    end

    Entry(args...) = error("@doc: incorrect arguments given to macro:\n$(args)")
end

const DEFAULT_METADATA = [
    :format => :md
    ]

# Module-level object to store metadata associated with objects from the module.
type Metadata
    modname::Module
    entries::Dict{Any, Entry}
    meta::Dict{Symbol, Any}

    function Metadata(m::Module, file, meta::Dict = Dict())
        meta = merge(DEFAULT_METADATA, meta)
        meta[:root] = file == nothing ? pwd() : dirname(file) # Usage from REPL needs nothing check.
        new(m, Dict{Any, Entry}(), meta)
    end
end

# Warn the author about overwritten metadata.
function setmeta!(md::Metadata, object, entry::Entry)
    haskey(md.entries, object) && warn("Overwriting metadata for `$(md.modname).$(object)`.")
    md.entries[object] = entry
    nothing
end

# Metatdata interface for *single* objects. `args` is the docstring and metadata. `source`
# is tuple of line number and filename.
function setmeta!(modname, object, category, source, args...)
    setmeta!(metadata(modname), object, Entry{category}(modname, source, args...))
end

# For varargs method definitions since they generate multiple method objects. Use the
# Metadata object for each object's documentation.
function setmeta!(modname, objects::Set, category, source, args...)
    entry = Entry{category}(modname, source, args...)
    meta = metadata(modname)
    for object in objects
        setmeta!(meta, object, entry)
    end
end

# Return the Metadata object called __METADATA__ stored in a module.
function metadata(modname)
    if !isdefined(modname, METADATA)
        error("No metadata defined in module $(modname). Call @docstrings first.")
    end
    getfield(modname, METADATA)
end


## macros


# What does the expression `ex` represent? Can it be documented? :symbol is used to
# resolve functions and modules is the calling module's context -- after `@doc` has
# returned.
object_category(ex) =
    ismethod(ex) ? :method :
    ismacro(ex)  ? :macro  :
    istype(ex)   ? :type   :
    isglobal(ex) ? :global :
    issymbol(ex) ? :symbol :
    error("@doc: cannot document object:\n$(ex)")

ismethod(ex) = isexpr(ex, [:function, :(=)]) && isexpr(ex.args[1], :call)
isglobal(ex) = isexpr(ex, [:global, :const, :(=)]) && !isexpr(ex.args[1], :call)
istype(ex)   = isexpr(ex, [:type, :abstract, :typealias])
ismacro(ex)  = isexpr(ex, :macro)

# Handle module/function as symbols in calling module.
issymbol(s::Symbol) = true
issymbol(ex) = false

# What does `symb` represent in the current module?
function lateguess(curmod, symb)
    isdefined(curmod, symb) || error("@doc: undefined object: $(symb)")
    guess(getfield(curmod, symb))
end
guess(f::Function) = :function
guess(m::Module)   = :module
guess(unknown)     = error("@doc: cannot document a $(unknown)")

# Extract the symbol identifying an expression.
name(ex::Expr) = name(isa(ex.args[1], Bool) ? ex.args[2] : ex.args[1])
name(s::Symbol) = s

# Split the expressions passed to `@doc` into data and object. The docstring and metadata
# dict in the first tuple are the data, while the second returned value is the piece of
# code being documented.
function separate(expr)
    data, obj = expr.args
    (data,), obj
end
function separate(docs, expr)
    meta, obj = expr.args
    (docs, meta), obj
end

# Attaching metadata to a generic function rather than the specific method which the
# `@doc` is applied to.
function docstar(symb::Symbol, args...)
    (generic = symb == :(*);), generic ? args : (symb, args...)
end
docstar(args...) = (false, args)

# Returns the line number and filename of the documented object. This is based on the
# `LineNumberNode` provided by `->` and is sometimes a few lines out.
function findsource(obj)
    loc = obj.args[1].args
    (loc[1], string(loc[2]))
end

# Metadata initialisation macro.
macro docstrings(args...)
    :(const $(esc(METADATA)) = Metadata(current_module(), @__FILE__, $(args...)))
end

macro doc(args...); doc(args...); end

function doc(args...)
    isexpr(last(args), :(->)) || error("@doc: use `->` to separate docs/object:\n$(args)")

    # Check for `@doc*` syntax and separate the args out.
    generic, args = docstar(args...)

    # Separate out the documentation and metadata (data) from the object (obj).
    data, obj = separate(args...)

    # Find the category and name of an object. Build corresponding quoted expressions for
    # use in the returned quote. Macros names are prefixed by `@` here.
    c, n   = object_category(obj.args[2]), name(obj.args[2])
    qc, qn = Expr(:quote, c), Expr(:quote, c == :macro ? symbol("@$(n)") : n)

    (generic && c != :method) && error("@doc: generic docstrings only allowed for methods.")

    # Capture the line and file.
    source = findsource(obj)

    if generic

        # Generic function docs attached to a method definition.
        esc(:($obj; Docstrings.setmeta!(current_module(), $n, :function, $source, $(data...))))

    elseif c == :method

        # Find all newly defined methods resulting from the current definition.
        before = gensym()
        oset = :($before = isdefined($qn) ? Set(methods($n)) : Set{Method}())
        nset = :(setdiff(Set(methods($n)), $before))

        esc(:($oset; $obj; Docstrings.setmeta!(current_module(), $nset, :method, $source, $(data...))))

    else

        # Category of metadata.
        cat = c == :symbol ? :(Docstrings.lateguess(current_module(), $qn)) : :($qc)

        # Macros, types, globals, modules, functions (not attached to a method)
        var = c in (:type, :symbol) ? :($n) : :($qn)
        esc(:($obj; Docstrings.setmeta!(current_module(), $var, $cat, $source, $(data...))))

    end
end

end
