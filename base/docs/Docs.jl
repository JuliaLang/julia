# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Docs

The `Docs` module provides the `@doc` macro which can be used to set and retrieve
documentation metadata for Julia objects.

Please see the manual section on documentation for more
information.
"""
module Docs

"""
# Documentation

Functions, methods and types can be documented by placing a string before the definition:

    \"""
    # The Foo Function
    `foo(x)`: Foo the living hell out of `x`.
    \"""
    foo(x) = ...

The `@doc` macro can be used directly to both set and retrieve documentation / metadata. By
default, documentation is written as Markdown, but any object can be placed before the
arrow. For example:

    @doc "blah" ->
    function foo() ...

The `->` is not required if the object is on the same line, e.g.

    @doc "foo" foo

## Documenting objects after they are defined
You can document an object after its definition by

    @doc "foo" function_to_doc
    @doc "bar" TypeToDoc

For macros, the syntax is `@doc "macro doc" :(@Module.macro)` or `@doc "macro doc"
:(string_macro"")` for string macros. Without the quote `:()` the expansion of the macro
will be documented.

## Retrieving Documentation
You can retrieve docs for functions, macros and other objects as follows:

    @doc foo
    @doc @time
    @doc md""

## Functions & Methods
Placing documentation before a method definition (e.g. `function foo() ...` or `foo() = ...`)
will cause that specific method to be documented, as opposed to the whole function. Method
docs are concatenated together in the order they were defined to provide docs for the
function.
"""
:(Core.@doc)

include("bindings.jl")

import Base.Markdown: @doc_str, MD
import Base.Meta: quot, isexpr
import Base: Callable
import ..CoreDocs: lazy_iterpolate

export doc

# Basic API / Storage

const modules = Module[]
const META    = gensym(:meta)

meta(m::Module) = isdefined(m, META) ? getfield(m, META) : ObjectIdDict()

function initmeta(m::Module)
    if !isdefined(m, META)
        eval(m, :(const $META = $(ObjectIdDict())))
        push!(modules, m)
    end
    nothing
end

function signature!(tv, expr::Expr)
    is_macrocall = isexpr(expr, :macrocall)
    if is_macrocall || isexpr(expr, :call)
        sig = :(Union{Tuple{}})
        first_arg = is_macrocall ? 3 : 2 # skip function arguments
        for arg in expr.args[first_arg:end]
            isexpr(arg, :parameters) && continue
            if isexpr(arg, :kw) # optional arg
                push!(sig.args, :(Tuple{$(sig.args[end].args[2:end]...)}))
            end
            push!(sig.args[end].args, argtype(arg))
        end
        if isexpr(expr.args[1], :curly) && isempty(tv)
            append!(tv, tvar.(expr.args[1].args[2:end]))
        end
        for i = length(tv):-1:1
            push!(sig.args, :(Tuple{$(tv[i].args[1])}))
        end
        for i = length(tv):-1:1
            sig = Expr(:where, sig, tv[i])
        end
        sig
    elseif isexpr(expr, :where)
        append!(tv, tvar.(expr.args[2:end]))
        signature!(tv, expr.args[1])
    else
        signature!(tv, expr.args[1])
    end
end
signature!(tv, other) = :(Union{})
signature(expr::Expr) = signature!([], expr)
signature(other) = signature!([], other)

function argtype(expr::Expr)
    isexpr(expr, :(::))  && return expr.args[end]
    isexpr(expr, :(...)) && return :(Vararg{$(argtype(expr.args[1]))})
    argtype(expr.args[1])
end
argtype(other) = :Any

tvar(x::Expr)   = x
tvar(s::Symbol) = :($s <: Any)

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
    object :: Nullable
    data   :: Dict{Symbol, Any}
end

function docstr(binding::Binding, @nospecialize typesig = Union{})
    for m in modules
        dict = meta(m)
        if haskey(dict, binding)
            docs = dict[binding].docs
            if haskey(docs, typesig)
                return docs[typesig]
            end
        end
    end
    error("could not find matching docstring for '$binding :: $typesig'.")
end
docstr(object, data = Dict()) = _docstr(object, data)

_docstr(vec::Core.SimpleVector, data) = DocStr(vec,            Nullable(),       data)
_docstr(str::AbstractString,    data) = DocStr(Core.svec(str), Nullable(),       data)
_docstr(object,                 data) = DocStr(Core.svec(),    Nullable(object), data)

_docstr(doc::DocStr, data) = (doc.data = merge(data, doc.data); doc)

macro ref(x)
    binding = bindingexpr(namify(x))
    typesig = signature(x)
    esc(docexpr(__source__, __module__, binding, typesig))
end

docexpr(__source__, __module__, args...) = Expr(:call, docstr, args...)

function formatdoc(d::DocStr)
    buffer = IOBuffer()
    for part in d.text
        formatdoc(buffer, d, part)
    end
    Markdown.parse(seekstart(buffer))
end
@noinline formatdoc(buffer, d, part) = print(buffer, part)

function parsedoc(d::DocStr)
    if isnull(d.object)
        md = formatdoc(d)
        md.meta[:module] = d.data[:module]
        md.meta[:path]   = d.data[:path]
        d.object = Nullable(md)
    end
    get(d.object)
end

"""
    MultiDoc

Stores a collection of docstrings for related objects, ie. a `Function`/`DataType` and
associated `Method` objects.

Each documented object in a `MultiDoc` is referred to by it's signature which is represented
by a `Union` of `Tuple` types. For example the following `Method` definition

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
    docs::ObjectIdDict

    MultiDoc() = new(Type[], ObjectIdDict())
end

# Docstring registration.
# =======================

"""
    Docs.doc!(__module__, binding, str, sig)

Adds a new docstring `str` to the docsystem of `__module__` for `binding` and signature `sig`.
"""
function doc!(__module__::Module, b::Binding, str::DocStr, @nospecialize sig = Union{})
    initmeta(__module__)
    m = get!(meta(__module__), b, MultiDoc())
    if haskey(m.docs, sig)
        # We allow for docstrings to be updated, but print a warning since it is possible
        # that over-writing a docstring *may* have been accidental.  The warning
        # is suppressed for symbols in Main, for interactive use (#23011).
        __module__ == Main || warn("replacing docs for '$b :: $sig' in module '$(__module__)'.")
    else
        # The ordering of docstrings for each Binding is defined by the order in which they
        # are initially added. Replacing a specific docstring does not change it's ordering.
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

getdoc(x, sig) = getdoc(x)
getdoc(x) = nothing

"""
    Docs.doc(binding, sig)

Returns all documentation that matches both `binding` and `sig`.

If `getdoc` returns a non-`nothing` result on the value of the binding, then a
dynamic docstring is returned instead of one based on the binding itself.
"""
function doc(binding::Binding, sig::Type = Union{})
    if defined(binding)
        result = getdoc(resolve(binding), sig)
        result === nothing || return result
    end
    results, groups = DocStr[], MultiDoc[]
    # Lookup `binding` and `sig` for matches in all modules of the docsystem.
    for mod in modules
        dict = meta(mod)
        if haskey(dict, binding)
            multidoc = dict[binding]
            push!(groups, multidoc)
            for msig in multidoc.order
                sig <: msig && push!(results, multidoc.docs[msig])
            end
        end
    end
    if isempty(groups)
        # When no `MultiDoc`s are found that match `binding` then we check whether `binding`
        # is an alias of some other `Binding`. When it is we then re-run `doc` with that
        # `Binding`, otherwise if it's not an alias then we generate a summary for the
        # `binding` and display that to the user instead.
        alias = aliasof(binding)
        alias == binding ? summarize(alias, sig) : doc(alias, sig)
    else
        # There was at least one match for `binding` while searching. If there weren't any
        # matches for `sig` then we concatenate *all* the docs from the matching `Binding`s.
        if isempty(results)
            for group in groups, each in group.order
                push!(results, group.docs[each])
            end
        end
        # Get parsed docs and concatenate them.
        md = catdoc(map(parsedoc, results)...)
        # Save metadata in the generated markdown.
        if isa(md, Markdown.MD)
            md.meta[:results] = results
            md.meta[:binding] = binding
            md.meta[:typesig] = sig
        end
        return md
    end
end

# Some additional convenience `doc` methods that take objects rather than `Binding`s.
doc(obj::UnionAll) = doc(Base.unwrap_unionall(obj))
doc(object, sig::Type = Union{}) = doc(aliasof(object, typeof(object)), sig)
doc(object, sig...)              = doc(object, Tuple{sig...})

"""
    Docs.fielddoc(binding, field)

Returns documentation for a particular `field` of a type if it exists.
"""
function fielddoc(binding::Binding, field::Symbol)
    for mod in modules
        dict = meta(mod)
        if haskey(dict, binding)
            multidoc = dict[binding]
            if haskey(multidoc.docs, Union{})
                fields = multidoc.docs[Union{}].data[:fields]
                if haskey(fields, field)
                    doc = fields[field]
                    return isa(doc, Markdown.MD) ? doc : Markdown.parse(doc)
                end
            end
        end
    end
    fields = join(["`$f`" for f in fieldnames(resolve(binding))], ", ", ", and ")
    fields = isempty(fields) ? "no fields" : "fields $fields"
    Markdown.parse("`$(resolve(binding))` has $fields.")
end

# As with the additional `doc` methods, this converts an object to a `Binding` first.
fielddoc(object, field::Symbol) = fielddoc(aliasof(object, typeof(object)), field)

# Object Summaries.
# =================

function summarize(binding::Binding, sig)
    io = IOBuffer()
    println(io, "No documentation found.\n")
    if defined(binding)
        summarize(io, resolve(binding), binding)
    else
        println(io, "Binding `", binding, "` does not exist.")
    end
    md = Markdown.parse(seekstart(io))
    # Save metadata in the generated markdown.
    md.meta[:results] = DocStr[]
    md.meta[:binding] = binding
    md.meta[:typesig] = sig
    return md
end

function summarize(io::IO, λ::Function, binding)
    kind = startswith(string(binding.var), '@') ? "macro" : "`Function`"
    println(io, "`", binding, "` is a ", kind, ".")
    println(io, "```\n", methods(λ), "\n```")
end

function summarize(io::IO, T::DataType, binding)
    println(io, "# Summary")
    println(io, "```")
    println(io,
            T.abstract ? "abstract type" :
            T.mutable  ? "mutable struct" :
            Base.isstructtype(T) ? "struct" : "primitive type",
            " ", T, " <: ", supertype(T)
            )
    println(io, "```")
    if !T.abstract && T.name !== Tuple.name && !isempty(fieldnames(T))
        println(io, "# Fields")
        println(io, "```")
        pad = maximum(length(string(f)) for f in fieldnames(T))
        for (f, t) in zip(fieldnames(T), T.types)
            println(io, rpad(f, pad), " :: ", t)
        end
        println(io, "```")
    end
    if !isempty(subtypes(T))
        println(io, "# Subtypes")
        println(io, "```")
        for t in subtypes(T)
            println(io, t)
        end
        println(io, "```")
    end
    if supertype(T) != Any
        println(io, "# Supertype Hierarchy")
        println(io, "```")
        Base.show_supertypes(io, T)
        println(io)
        println(io, "```")
    end
end

function summarize(io::IO, m::Module, binding)
    readme = Pkg.dir(string(m), "README.md")
    if isfile(readme)
        println(io, "Displaying the `README.md` for the module instead.\n")
        println(io, "---\n")
        println(io, read(readme, String))
    else
        println(io, "No docstring or `README.md` found for module `", m, "`.\n")
    end
end

function summarize(io::IO, ::T, binding) where T
    println(io, "`", binding, "` is of type `", T, "`.\n")
    summarize(io, T, binding)
end

# Utilities.
# ==========

"""
`catdoc(xs...)`: Combine the documentation metadata `xs` into a single meta object.
"""
catdoc() = nothing
catdoc(xs...) = vcat(xs...)

const keywords = Dict{Symbol, DocStr}()

isdoc(s::AbstractString) = true

isdoc(x) = isexpr(x, :string) ||
    (isexpr(x, :macrocall) && x.args[1] === Symbol("@doc_str")) ||
    (isexpr(x, :call) && x.args[1] === Base.Markdown.doc_str)

function unblock(ex)
    isexpr(ex, :block) || return ex
    exs = filter(ex -> !(isa(ex, LineNumberNode) || isexpr(ex, :line)), ex.args)
    length(exs) == 1 || return ex
    return unblock(exs[1])
end

uncurly(ex) = isexpr(ex, :curly) ? ex.args[1] : ex

namify(x) = nameof(x, isexpr(x, :macro))

function nameof(x::Expr, ismacro)
    if isexpr(x, :.)
        ismacro ? macroname(x) : x
    else
        n = isexpr(x, (:module, :struct)) ? 2 : 1
        nameof(x.args[n], ismacro)
    end
end
nameof(q::QuoteNode, ismacro) = nameof(q.value, ismacro)
nameof(s::Symbol, ismacro)    = ismacro ? macroname(s) : s
nameof(other, ismacro)        = other

macroname(s::Symbol) = Symbol('@', s)
macroname(x::Expr)   = Expr(x.head, x.args[1], macroname(x.args[end].value))

isfield(x) = isexpr(x, :.) &&
    (isa(x.args[1], Symbol) || isfield(x.args[1])) &&
    (isa(x.args[2], QuoteNode) || isexpr(x.args[2], :quote))

# @doc expression builders.
# =========================

"""
    Docs.metadata(source, module, expr, ismodule)

Build a `Dict` expression containing metadata captured from the expression `expr`.

Fields that may be included in the returned `Dict`:

- `:path`:       Symbol representing the file where `expr` is defined.
- `:linenumber`: Linenumber where `expr` is defined.
- `:module`:     Module where the docstring is defined.
- `:fields`:     `Dict` of all field docs found in `expr`. Only for concrete types.
"""
function metadata(__source__, __module__, expr, ismodule)
    args = []
    # Filename and linenumber of the docstring.
    __file__ = isa(__source__.file, Symbol) ? String(__source__.file) : ""
    push!(args, Pair(:path, __file__))
    push!(args, Pair(:linenumber, __source__.line))
    # Module in which the docstring is defined.
    if ismodule # Module docs go inside the module with name `expr`
        push!(args, :($Pair(:module, $expr)))
    else
        push!(args, Pair(:module, __module__))
    end
    if isexpr(expr, :struct)
        # Field docs for concrete types.
        fields = []
        tmp = nothing
        for each in expr.args[3].args
            if isdoc(each)
                tmp = each
            elseif tmp !== nothing && (isa(each, Symbol) || isexpr(each, :(::)))
                push!(fields, (namify(each), tmp))
                tmp = nothing
            end
        end
        dict = :($(Dict)($([:($(Pair)($(quot(f)), $d)) for (f, d) in fields]...)))
        push!(args, :($(Pair)(:fields, $dict)))
    end
    :($(Dict)($(args...)))
end

function keyworddoc(__source__, __module__, str, def)
    docstr = esc(docexpr(__source__, __module__, lazy_iterpolate(str), metadata(__source__, __module__, def, false)))
    return :($(keywords)[$(esc(quot(def.name)))] = $docstr)
end

function objectdoc(__source__, __module__, str, def, expr, sig = :(Union{}))
    binding = esc(bindingexpr(namify(expr)))
    docstr  = esc(docexpr(__source__, __module__, lazy_iterpolate(str), metadata(__source__, __module__, expr, false)))
    quote
        $(esc(def))
        $(doc!)($__module__, $binding, $docstr, $(esc(sig)))
    end
end

function calldoc(__source__, __module__, str, def)
    args = def.args[2:end]
    if isempty(args) || all(validcall, args)
        objectdoc(__source__, __module__, str, nothing, def, signature(def))
    else
        docerror(def)
    end
end
validcall(x) = isa(x, Symbol) || isexpr(x, (:(::), :..., :kw, :parameters))

function moduledoc(__source__, __module__, meta, def, def′)
    name  = namify(def′)
    docex = Expr(:call, doc!, name, bindingexpr(name),
        docexpr(__source__, name, lazy_iterpolate(meta), metadata(__source__, __module__, name, true)))
    if def === nothing
        esc(:($eval($name, $(quot(docex)))))
    else
        def = unblock(def)
        block = def.args[3].args
        if !def.args[1]
            isempty(block) && error("empty baremodules are not documentable.")
            insert!(block, 2, :(import Base: @doc))
        end
        push!(block, docex)
        esc(Expr(:toplevel, def))
    end
end

# Shares a single doc, `meta`, between several expressions from the tuple expression `ex`.
function multidoc(__source__, __module__, meta, ex, define)
    out = Expr(:toplevel)
    str = docexpr(__source__, __module__, lazy_iterpolate(meta), metadata(__source__, __module__, ex, false))
    ref = Ref{DocStr}()
    for (n, arg) in enumerate(ex.args)
        # The first `arg` to be documented needs to also create the docstring for the group.
        # Subsequent `arg`s just need `ref` to be able to find the docstring without having
        # to create an entirely new one each.
        docstr = n === 1 ? :($(ref)[] = $str) : :($(ref)[])
        push!(out.args, docm(__source__, __module__, docstr, arg, define))
    end
    return out
end

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
"""
:(Core.@__doc__)

function __doc__!(meta, def, define)
    # Two cases must be handled here to avoid redefining all definitions contained in `def`:
    if define
        # `def` has not been defined yet (this is the common case, i.e. when not generating
        # the Base image). We just need to convert each `@__doc__` marker to an `@doc`.
        finddoc(def) do each
            each.head = :macrocall
            each.args = [Symbol("@doc"), nothing, meta, each.args[end], define] # TODO: forward line number info
        end
    else
        # `def` has already been defined during Base image gen so we just need to find and
        # document any subexpressions marked with `@__doc__`.
        docs  = []
        found = finddoc(def) do each
            push!(docs, :(@doc($meta, $(each.args[end]), $define)))
        end
        # If any subexpressions have been documented then replace the entire expression with
        # just those documented subexpressions to avoid redefining any definitions.
        if found
            def.head = :toplevel
            def.args = docs
        end
        found
    end
end
# Walk expression tree `def` and call `λ` when any `@__doc__` markers are found. Returns
# `true` to signify that at least one `@__doc__` has been found, and `false` otherwise.
function finddoc(λ, def::Expr)
    if isexpr(def, :block, 2) && isexpr(def.args[1], :meta, 1) && def.args[1].args[1] === :doc
        # Found the macroexpansion of an `@__doc__` expression.
        λ(def)
        true
    else
        found = false
        for each in def.args
            found |= finddoc(λ, each)
        end
        found
    end
end
finddoc(λ, def) = false

# Predicates and helpers for `docm` expression selection:

const FUNC_HEADS    = [:function, :macro, :(=)]
const BINDING_HEADS = [:typealias, :const, :global, :(=)]  # deprecation: remove `typealias` post-0.6
# For the special `:@mac` / `:(Base.@mac)` syntax for documenting a macro after definition.
isquotedmacrocall(x) =
    isexpr(x, :copyast, 1) &&
    isa(x.args[1], QuoteNode) &&
    isexpr(x.args[1].value, :macrocall, 2)
# Simple expressions / atoms the may be documented.
isbasicdoc(x) = isexpr(x, :.) || isa(x, Union{QuoteNode, Symbol})
is_signature(x) = isexpr(x, :call) || (isexpr(x, :(::), 2) && isexpr(x.args[1], :call)) || isexpr(x, :where)

function docm(source::LineNumberNode, mod::Module, meta, ex, define = true)
    # Some documented expressions may be decorated with macro calls which obscure the actual
    # expression. Expand the macro calls and remove extra blocks.
    x = unblock(macroexpand(mod, ex))
    # Don't try to redefine expressions. This is only needed for `Base` img gen since
    # otherwise calling `loaddocs` would redefine all documented functions and types.
    def = define ? x : nothing
    if isa(x, GlobalRef) && (x::GlobalRef).mod == mod
        x = (x::GlobalRef).name
    end

    # Keywords using the `@kw_str` macro in `base/docs/basedocs.jl`.
    #
    #   "..."
    #   kw"if", kw"else"
    #
    isa(x, Base.BaseDocs.Keyword) ? keyworddoc(source, mod, meta, x) :

    # Method / macro definitions and "call" syntax.
    #
    #   function f(...) ... end
    #   f(...) = ...
    #   macro m(...) end
    #   function f end
    #   f(...)
    #
    isexpr(x, FUNC_HEADS) && is_signature(x.args[1])   ? objectdoc(source, mod, meta, def, x, signature(x)) :
    isexpr(x, [:function, :macro])  && !isexpr(x.args[1], :call) ? objectdoc(source, mod, meta, def, x) :
    isexpr(x, :call)                                   ? calldoc(source, mod, meta, x) :

    # Type definitions.
    #
    #   struct T ... end
    #   abstract type T end
    #   primitive type T N end
    #
    isexpr(x, [:struct, :abstract, :primitive]) ? objectdoc(source, mod, meta, def, x) :

    # "Bindings". Names that resolve to objects with different names, ie.
    #
    #   const T = S
    #   T = S
    #   global T = S
    #
    isexpr(x, BINDING_HEADS) && !isexpr(x.args[1], :call) ? objectdoc(source, mod, meta, def, x) :

    # Quoted macrocall syntax. `:@time` / `:(Base.@time)`.
    isquotedmacrocall(x) ? objectdoc(source, mod, meta, def, x) :
    # Modules and baremodules.
    isexpr(x, :module) ? moduledoc(source, mod, meta, def, x) :
    # Document several expressions with the same docstring. `a, b, c`.
    isexpr(x, :tuple) ? multidoc(source, mod, meta, x, define) :
    # Errors generated by calling `macroexpand` are passed back to the call site.
    isexpr(x, :error) ? esc(x) :
    # When documenting macro-generated code we look for embedded `@__doc__` calls.
    __doc__!(meta, x, define) ? esc(x) :
    # Any "basic" expression such as a bare function or module name or numeric literal.
    isbasicdoc(x) ? objectdoc(source, mod, meta, nothing, x) :

    # All other expressions are undocumentable and should be handled on a case-by-case basis
    # with `@__doc__`. Unbound string literals are also undocumentable since they cannot be
    # retrieved from the module's metadata `ObjectIdDict` without a reference to the string.
    docerror(ex)
end

function docerror(ex)
    txt = """
    cannot document the following expression:

    $(isa(ex, AbstractString) ? repr(ex) : ex)"""
    if isexpr(ex, :macrocall)
        txt *= "\n\n'$(ex.args[1])' not documentable. See 'Base.@__doc__' docs for details."
    end
    :($(error)($txt, "\n"))
end

function docm(source::LineNumberNode, mod::Module, ex)
    if isexpr(ex, :->)
        docm(source, mod, ex.args...)
    elseif haskey(keywords, ex)
        parsedoc(keywords[ex])
    elseif isa(ex, Union{Expr, Symbol})
        binding = esc(bindingexpr(namify(ex)))
        if isexpr(ex, :call) || isexpr(ex, :macrocall)
            sig = esc(signature(ex))
            :($(doc)($binding, $sig))
        else
            :($(doc)($binding))
        end
    else
        :($(doc)($(typeof)($(esc(ex)))))
    end
end

# MD support
catdoc(md::MD...) = MD(md...)

include("utils.jl")

# Swap out the bootstrap macro with the real one.
Core.atdoc!(docm)

macro local_hygiene(expr)
    # removes `esc` Exprs relative to the module argument to expand
    # and resolves everything else relative to this (Doc) module
    # this allows us to get good errors and backtraces
    # from calling docm (by not using macros),
    # while also getting macro-expansion correct (by using the macro-expander)
    return expr
end
function loaddocs(docs)
    unescape = GlobalRef(Docs, Symbol("@local_hygiene"))
    for (mod, ex, str, file, line) in docs
        data = Dict(:path => string(file), :linenumber => line)
        doc = docstr(str, data)
        docstring = docm(LineNumberNode(line, file), mod, doc, ex, false) # expand the real @doc macro now
        eval(mod, Expr(:macrocall, unescape, nothing, docstring))
    end
    empty!(docs)
end

end
