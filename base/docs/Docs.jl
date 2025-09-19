# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Docs

The `Docs` module provides the [`@doc`](@ref) macro which can be used to set and retrieve
documentation metadata for Julia objects.

Please see the manual section on [documentation](@ref man-documentation) for more
information.
"""
module Docs

@nospecialize # don't specialize on any arguments of the methods declared herein

"""
# Documentation

Functions, methods and types can be documented by placing a string before the definition:

    \"\"\"
        foo(x)

    Return a fooified version of `x`.
    \"\"\"
    foo(x) = ...

The `@doc` macro can be used directly to both set and retrieve documentation / metadata.
The macro has special parsing so that the documented object may occur on the next line:

    @doc "blah"
    function foo() ...

By default, documentation is written as Markdown, but any object can be used as
the first argument.

## Documenting objects separately from their definitions
You can document an object before or after its definition with

    @doc "foo" function_to_doc
    @doc "bar" TypeToDoc

For macros, the syntax is `@doc "macro doc" :(Module.@macro)` or `@doc "macro doc"
:(string_macro"")` for string macros. Without the quote `:()` the expansion of the macro
will be documented.

## Retrieving Documentation
You can retrieve docs for functions, macros and other objects as follows:

    @doc foo
    @doc @time
    @doc md""

!!! compat "Julia 1.11"
    In Julia 1.11 and newer, retrieving documentation with the `@doc` macro requires that
    the `REPL` stdlib is loaded.

## Functions & Methods
Placing documentation before a method definition (e.g. `function foo() ...` or `foo() = ...`)
will cause that specific method to be documented, as opposed to the whole function. Method
docs are concatenated together in the order they were defined to provide docs for the
function.
"""
:(Core.@doc)

include("bindings.jl")

import .Base.Meta: quot, isexpr, unblock, unescape, uncurly
import .Base: Callable, with_output_color
using .Base: RefValue, mapany

export doc, hasdoc, undocumented_names

# Basic API / Storage

const modules = Module[]
const META    = gensym(:meta)
const METAType = IdDict{Any,Any}

function meta(m::Module; autoinit::Bool=true)
    if !invokelatest(isdefinedglobal, m, META)
        return autoinit ? initmeta(m) : nothing
    end
    # TODO: This `invokelatest` is not technically required, but because
    # of the automatic constant backdating is currently required to avoid
    # a warning.
    return invokelatest(getglobal, m, META)::METAType
end

function initmeta(m::Module)
    if !invokelatest(isdefinedglobal, m, META)
        val = METAType()
        Core.eval(m, :(const $META = $val))
        push!(modules, m)
        return val
    end
    return invokelatest(getglobal, m, META)
end

# Docsystem types.
# ================

"""
    Docs.DocStr

Stores the contents of a single docstring as well as related metadata.

Both the raw text, `.text`, and the parsed markdown, `.object`, are tracked by this type.
Parsing of the raw text is done lazily when a request is made to render the docstring,
which helps to reduce total precompiled image size.

The `.data` fields stores several values related to the docstring, such as: path,
linenumber, source code, and fielddocs.
"""
mutable struct DocStr
    text   :: Core.SimpleVector
    object :: Any
    data   :: Dict{Symbol, Any}
end

macro ref(x)
    binding = bindingexpr(namify(x))
    typesig = signature(x)
    return esc(docexpr(__source__, __module__, binding, typesig))
end

"""
    MultiDoc

Stores a collection of docstrings for related objects, ie. a `Function`/`DataType` and
associated `Method` objects.

Each documented object in a `MultiDoc` is referred to by its signature which is represented
by a `Union` of `Tuple` types. For example, the following `Method` definition

    f(x, y) = ...

is stored as `Tuple{Any, Any}` in the `MultiDoc` while

    f(x::T, y = ?) where {T} = ...

is stored as `Union{Tuple{T, Any}, Tuple{T}} where T`.

Note: The `Function`/`DataType` object's signature is always `Union{}`.
"""
mutable struct MultiDoc
    "Ordered (via definition order) vector of object signatures."
    order::Vector{Type}
    "Documentation for each object. Keys are signatures."
    docs::METAType

    MultiDoc() = new(Type[], METAType())
end

# Docstring registration.
# =======================

"""
    Docs.doc!(__module__, binding, str, sig)

Adds a new docstring `str` to the docsystem of `__module__` for `binding` and signature `sig`.
"""
function doc!(__module__::Module, b::Binding, str::DocStr, @nospecialize sig = Union{})
    # Module docstrings are in the module itself
    if defined(b)
        obj = resolve(b)
        if isa(obj, Module)
            __module__ = obj
        end
    end
    initmeta(__module__)
    m = get!(meta(__module__), b, MultiDoc())
    if haskey(m.docs, sig)
        # We allow for docstrings to be updated, but print a warning since it is possible
        # that over-writing a docstring *may* have been accidental.  The warning
        # is suppressed for symbols in Main (or current active module),
        # for interactive use (#23011).
        __module__ === Base.active_module() ||
            @warn "Replacing docs for `$b :: $sig` in module `$(__module__)`"
    else
        # The ordering of docstrings for each Binding is defined by the order in which they
        # are initially added. Replacing a specific docstring does not change its ordering.
        push!(m.order, sig)
    end
    m.docs[sig] = str
    str.data[:binding] = b
    str.data[:typesig] = sig
    return b
end

# Docstring lookup.
# =================

"""
    getdoc(obj)
    getdoc(obj, sig)

Return a custom docstring object associated with the object `obj` and, optionally, the tuple
type signature `sig`. See `MultiDoc` docs for an explanation of the possible values of `sig`.

The returned object can either be a markdown object generated by `Markdown.parse` or some
other custom type used to display non-markdown formatted documentation.

A return value of `nothing` can be used to signify to the docsystem that no documentation
was found for `obj`, in which case the docsystem will fall back to searching for the
`Binding` associated with `obj` instead.
"""
function getdoc end

getdoc(@nospecialize(x), @nospecialize(sig)) = getdoc(x)
getdoc(@nospecialize(x)) = nothing

"""
`catdoc(xs...)`: Combine the documentation metadata `xs` into a single meta object.
"""
catdoc() = nothing
catdoc(xs...) = vcat(xs...)

const keywords = Dict{Symbol, DocStr}()

"""
    @__doc__(ex)

Low-level macro used to mark expressions returned by a macro that should be documented. If
more than one expression is marked then the same docstring is applied to each expression.

    macro example(f)
        quote
            \$(f)() = 0
            @__doc__ \$(f)(x) = 1
            \$(f)(x, y) = 2
        end |> esc
    end

`@__doc__` has no effect when a macro that uses it is not documented.

!!! compat "Julia 1.12"

    This section documents a very subtle corner case that is only relevant to
    macros which themselves both define other macros and then attempt to use them
    within the same expansion. Such macros were impossible to write prior to
    Julia 1.12 and are still quite rare. If you are not writing such a macro,
    you may ignore this note.

    In versions prior to Julia 1.12, macroexpansion would recursively expand through
    `Expr(:toplevel)` blocks. This behavior was changed in Julia 1.12 to allow
    macros to recursively define other macros and use them in the same returned
    expression. However, to preserve backwards compatibility with existing uses of
    `@__doc__`, the doc system will still expand through `Expr(:toplevel)` blocks
    when looking for `@__doc__` markers. As a result, macro-defining-macros will
    have an observable behavior difference when annotated with a docstring:

    ```julia
    julia> macro macroception()
        Expr(:toplevel, :(macro foo() 1 end), :(@foo))
    end

    julia> @macroception
    1

    julia> "Docstring" @macroception
    ERROR: LoadError: UndefVarError: `@foo` not defined in `Main`
    ```

    The supported workaround is to manually expand the `@__doc__` macro in the
    defining macro, which the docsystem will recognize and suppress the recursive
    expansion:

    ```julia
    julia> macro macroception()
        Expr(:toplevel,
            macroexpand(__module__, :(@__doc__ macro foo() 1 end); recursive=false),
            :(@foo))
    end

    julia> @macroception
    1

    julia> "Docstring" @macroception
    1
    ```
"""
:(Core.@__doc__)

# Helpers for single-arg @doc
function _lookup_doc(binding::Binding, sig::Type = Union{})
    if defined(binding)
        result = getdoc(resolve(binding), sig)
        result === nothing || return result
    end
    # Lookup first match for `binding` and `sig` in all modules of the docsystem.
    for mod in modules
        dict = meta(mod; autoinit=false)
        isnothing(dict) && continue
        if haskey(dict, binding)
            multidoc = dict[binding]
            for msig in multidoc.order
                sig <: msig && return multidoc.docs[msig]
            end
            # if no matching signatures, return first
            if !isempty(multidoc.docs)
                return first(values(multidoc.docs))
            end
        end
    end
    return nothing
end
_lookup_doc(obj::UnionAll) = _lookup_doc(Base.unwrap_unionall(obj))
_lookup_doc(object, sig::Type = Union{}) = _lookup_doc(aliasof(object, typeof(object)), sig)
_lookup_doc(object, sig...)              = _lookup_doc(object, Tuple{sig...})

function _docm_get(source::LineNumberNode, mod::Module, ex)
    @nospecialize ex
    if (REPL = Base.REPL_MODULE_REF[]) !== Base
        # TODO: this is a shim to continue to allow `@doc` for looking up docstrings
        return invokelatest(REPL.lookup_doc, ex)
    end
    # simple lookup
    if isa(ex, Expr) && ex.head !== :(.) && Base.isoperator(ex.head)
        # handle syntactic operators, e.g. +=, ::, .=
        ex = ex.head
    end
    if haskey(keywords, ex)
        return keywords[ex]
    elseif !isa(ex, Expr) && !isa(ex, Symbol)
        return :($(_lookup_doc)($(typeof)($(esc(ex)))))
    end
    binding = esc(bindingexpr(namify(ex)))
    if isexpr(ex, :call) || isexpr(ex, :macrocall) || isexpr(ex, :where)
        sig = esc(signature(ex))
        return :($(_lookup_doc)($binding, $sig))
    else
        return :($(_lookup_doc)($binding))
    end
    return nothing
end

function docerror(@nospecialize ex)
    txt = """
    cannot document the following expression:

    $(isa(ex, AbstractString) ? repr(ex) : ex)"""
    if isexpr(ex, :macrocall)
        txt *= "\n\n'$(ex.args[1])' not documentable. See 'Base.@__doc__' docs for details."
    end
    return :($(error)($txt, "\n"))
end

include("utils.jl")

# Generally x is a Julia object, but we can document undefined names with Symbol
function setdoc(mod::Module, str::Core.SimpleVector, lnn::LineNumberNode,
                @nospecialize(x), foo, @nospecialize(sig=Union{}))
    mdict = Dict{Symbol,Any}()
    mdict[:path] = String(lnn.file)
    mdict[:linenumber] = lnn.line
    mdict[:module] = mod
    # TODO struct field metadata
    dstr = DocStr(str, x, mdict)

    if x isa Base.BaseDocs.Keyword
        setindex!(keywords, dstr, x.name)
        return nothing
    end
    name = x isa Symbol ? x :
        x isa Method ? x.name :
        x isa Module || x isa Function ? nameof(x) :
        Symbol(string(x))

    if x isa Method
        sig = x.sig
    end
    doc!(mod, Binding(mod, name), dstr, sig)
    # x
end

# Swap out the bootstrap macro with the real one.
Core._set_docm_get!(_docm_get)
Core._set_setdoc!(setdoc)

function loaddocs(docs::Base.CoreDocs.DocLinkedList)
    while isdefined(docs, :args)
        setdoc(docs.args...)
        docs = docs.next
    end
    nothing
end

# FIXME: formatdoc, parsedoc, apropos, and doc are defined here (but only doc is exported)
# for historical reasons (#25738), but are *implemented* in REPL/src/docview.jl, while
# apropos is *exported* by InteractiveUtils and doc is exported by Docs.  Seems
# like a more sensible refactoring should be possible.

function formatdoc end
function parsedoc end

"""
    apropos([io::IO=stdout], pattern::Union{AbstractString,Regex})

Search available docstrings for entries containing `pattern`.

When `pattern` is a string, case is ignored. Results are printed to `io`.

`apropos` can be called from the help mode in the REPL by wrapping the query in double quotes:
```
help?> "pattern"
```

!!! compat "Julia 1.11"
    In Julia 1.11 and newer, `apropos` requires that the `REPL` stdlib is loaded.
"""
function apropos end

"""
    Docs.doc(binding, sig)

Return all documentation that matches both `binding` and `sig`.

If `getdoc` returns a non-`nothing` result on the value of the binding, then a
dynamic docstring is returned instead of one based on the binding itself.

!!! compat "Julia 1.11"
    In Julia 1.11 and newer, `Docs.doc` requires that the `REPL` stdlib is loaded.
"""
function doc end

"""
    Docs.hasdoc(mod::Module, sym::Symbol)::Bool

Return `true` if `sym` in `mod` has a docstring and `false` otherwise.
"""
hasdoc(mod::Module, sym::Symbol) = hasdoc(Docs.Binding(mod, sym))
function hasdoc(binding::Docs.Binding, sig::Type = Union{})
    # this function is based on the Base.Docs.doc method implemented
    # in REPL/src/docview.jl.  TODO: refactor and unify these methods.
    defined(binding) && !isnothing(getdoc(resolve(binding), sig)) && return true
    for mod in modules
        dict = meta(mod; autoinit=false)
        !isnothing(dict) && haskey(dict, binding) && return true
    end
    alias = aliasof(binding)
    return alias == binding ? false : hasdoc(alias, sig)
end


"""
    undocumented_names(mod::Module; private=false)

Return a sorted vector of undocumented symbols in `module` (that is, lacking docstrings).
`private=false` (the default) returns only identifiers declared with `public` and/or
`export`, whereas `private=true` returns all symbols in the module (excluding
compiler-generated hidden symbols starting with `#`).

See also: [`names`](@ref), [`Docs.hasdoc`](@ref), [`Base.ispublic`](@ref).
"""
function undocumented_names(mod::Module; private::Bool=false)
    filter!(names(mod; all=true)) do sym
        !hasdoc(mod, sym) && !startswith(string(sym), '#') &&
            (private || Base.ispublic(mod, sym))
    end
end

end
