# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Docs

The `Docs` module provides the `@doc` macro which can be used to set and retrieve
documentation metadata for Julia objects.

Please see the manual section on documentation for more
information.
"""
module Docs

@nospecialize # don't specialize on any arguments of the methods declared herein

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

import .Base.Meta: quot, isexpr
import .Base: Callable, with_output_color
using .Base: RefValue
import ..CoreDocs: lazy_iterpolate

export doc

# Basic API / Storage

const modules = Module[]
const META    = gensym(:meta)

meta(m::Module) = isdefined(m, META) ? getfield(m, META) : IdDict()

function initmeta(m::Module)
    if !isdefined(m, META)
        Core.eval(m, :(const $META = $(IdDict())))
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
        return sig
    elseif isexpr(expr, :where)
        append!(tv, tvar.(expr.args[2:end]))
        return signature!(tv, expr.args[1])
    else
        return signature!(tv, expr.args[1])
    end
end
signature!(tv, @nospecialize(other)) = :(Union{})
signature(expr::Expr) = signature!([], expr)
signature(@nospecialize other) = signature!([], other)

function argtype(expr::Expr)
    isexpr(expr, :(::))  && return expr.args[end]
    isexpr(expr, :(...)) && return :(Vararg{$(argtype(expr.args[1]))})
    return argtype(expr.args[1])
end
argtype(@nospecialize other) = :Any

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
    object :: Any
    data   :: Dict{Symbol, Any}
end

function docstr(binding::Binding, typesig = Union{})
    @nospecialize typesig
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

_docstr(vec::Core.SimpleVector, data) = DocStr(vec,            nothing, data)
_docstr(str::AbstractString,    data) = DocStr(Core.svec(str), nothing, data)
_docstr(object,                 data) = DocStr(Core.svec(),     object, data)

_docstr(doc::DocStr, data) = (doc.data = merge(data, doc.data); doc)

macro ref(x)
    binding = bindingexpr(namify(x))
    typesig = signature(x)
    return esc(docexpr(__source__, __module__, binding, typesig))
end

docexpr(__source__, __module__, args...) = Expr(:call, docstr, args...)

"""
    MultiDoc

Stores a collection of docstrings for related objects, ie. a `Function`/`DataType` and
associated `Method` objects.

Each documented object in a `MultiDoc` is referred to by it's signature which is represented
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
    docs::IdDict{Any,Any}

    MultiDoc() = new(Type[], IdDict())
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
        __module__ == Main || @warn "Replacing docs for `$b :: $sig` in module `$(__module__)`"
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

getdoc(@nospecialize(x), @nospecialize(sig)) = getdoc(x)
getdoc(@nospecialize(x)) = nothing

# Utilities.
# ==========

"""
`catdoc(xs...)`: Combine the documentation metadata `xs` into a single meta object.
"""
catdoc() = nothing
catdoc(xs...) = vcat(xs...)

const keywords = Dict{Symbol, DocStr}()

function unblock(@nospecialize ex)
    isexpr(ex, :block) || return ex
    exs = filter(ex -> !(isa(ex, LineNumberNode) || isexpr(ex, :line)), ex.args)
    length(exs) == 1 || return ex
    return unblock(exs[1])
end

uncurly(@nospecialize ex) = isexpr(ex, :curly) ? ex.args[1] : ex

namify(@nospecialize x) = astname(x, isexpr(x, :macro))

function astname(x::Expr, ismacro::Bool)
    if isexpr(x, :.)
        ismacro ? macroname(x) : x
    # Call overloading, e.g. `(a::A)(b) = b` or `function (a::A)(b) b end` should document `A(b)`
    elseif (isexpr(x, :function) || isexpr(x, :(=))) && isexpr(x.args[1], :call) && isexpr(x.args[1].args[1], :(::))
        return astname(x.args[1].args[1].args[end], ismacro)
    else
        n = isexpr(x, (:module, :struct)) ? 2 : 1
        astname(x.args[n], ismacro)
    end
end
astname(q::QuoteNode, ismacro::Bool) = astname(q.value, ismacro)
astname(s::Symbol, ismacro::Bool)    = ismacro ? macroname(s) : s
astname(@nospecialize(other), ismacro::Bool) = other

macroname(s::Symbol) = Symbol('@', s)
macroname(x::Expr)   = Expr(x.head, x.args[1], macroname(x.args[end].value))

isfield(@nospecialize x) = isexpr(x, :.) &&
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
        last_docstr = nothing
        for each in expr.args[3].args
            if isa(each, Symbol) || isexpr(each, :(::))
                # a field declaration
                if last_docstr !== nothing
                    push!(fields, (namify(each), last_docstr))
                    last_docstr = nothing
                end
            elseif isexpr(each, :function) || isexpr(each, :(=))
                break
            elseif isa(each, String) || isexpr(each, :string) || isexpr(each, :call) ||
                (isexpr(each, :macrocall) && each.args[1] === Symbol("@doc_str"))
                # forms that might be doc strings
                last_docstr = each
            end
        end
        dict = :($(Dict)($([:($(Pair)($(quot(f)), $d)) for (f, d) in fields]...)))
        push!(args, :($(Pair)(:fields, $dict)))
    end
    return :($(Dict)($(args...)))
end

function keyworddoc(__source__, __module__, str, def::Base.BaseDocs.Keyword)
    @nospecialize str
    docstr = esc(docexpr(__source__, __module__, lazy_iterpolate(str), metadata(__source__, __module__, def, false)))
    return :($setindex!($(keywords), $docstr, $(esc(quot(def.name)))); nothing)
end

function objectdoc(__source__, __module__, str, def, expr, sig = :(Union{}))
    @nospecialize str def expr sig
    binding = esc(bindingexpr(namify(expr)))
    docstr  = esc(docexpr(__source__, __module__, lazy_iterpolate(str), metadata(__source__, __module__, expr, false)))
    # Note: we want to avoid introducing line number nodes here (issue #24468)
    return Expr(:block, esc(def), :($(doc!)($__module__, $binding, $docstr, $(esc(sig)))))
end

function calldoc(__source__, __module__, str, def::Expr)
    @nospecialize str
    args = def.args[2:end]
    if isempty(args) || all(validcall, args)
        objectdoc(__source__, __module__, str, nothing, def, signature(def))
    else
        docerror(def)
    end
end
validcall(x) = isa(x, Symbol) || isexpr(x, (:(::), :..., :kw, :parameters))

function moduledoc(__source__, __module__, meta, def, def′::Expr)
    @nospecialize meta def
    name  = namify(def′)
    docex = Expr(:call, doc!, name, bindingexpr(name),
        docexpr(__source__, name, lazy_iterpolate(meta), metadata(__source__, __module__, name, true)))
    if def === nothing
        esc(:(Core.eval($name, $(quot(docex)))))
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
function multidoc(__source__, __module__, meta, ex::Expr, define::Bool)
    @nospecialize meta
    out = Expr(:block)
    str = docexpr(__source__, __module__, lazy_iterpolate(meta), metadata(__source__, __module__, ex, false))
    ref = RefValue{DocStr}()
    first = true
    for arg in ex.args
        # The first `arg` to be documented needs to also create the docstring for the group
        # (after doing the action defined by the argument).
        # Subsequent `arg`s just need `ref` to be able to find the docstring without having
        # to create an entirely new one each.
        if first
            first = false
            docstr = :($getindex($setindex!($(ref), $str)))
        else
            docstr = :($getindex($(ref)))
        end
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

function __doc__!(meta, def, define::Bool)
    @nospecialize meta def
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
finddoc(λ, @nospecialize def) = false

# Predicates and helpers for `docm` expression selection:

const FUNC_HEADS    = [:function, :macro, :(=)]
const BINDING_HEADS = [:const, :global, :(=)]
# For the special `:@mac` / `:(Base.@mac)` syntax for documenting a macro after definition.
isquotedmacrocall(@nospecialize x) =
    isexpr(x, :copyast, 1) &&
    isa(x.args[1], QuoteNode) &&
    isexpr(x.args[1].value, :macrocall, 2)
# Simple expressions / atoms the may be documented.
isbasicdoc(@nospecialize x) = isexpr(x, :.) || isa(x, Union{QuoteNode, Symbol})
is_signature(@nospecialize x) = isexpr(x, :call) || (isexpr(x, :(::), 2) && isexpr(x.args[1], :call)) || isexpr(x, :where)

function docm(source::LineNumberNode, mod::Module, ex)
    @nospecialize ex
    if isexpr(ex, :->) && length(ex.args) > 1
        return docm(source, mod, ex.args...)
    else
        # TODO: this is a shim to continue to allow `@doc` for looking up docstrings
        REPL = Base.REPL_MODULE_REF[]
        return REPL.lookup_doc(ex)
    end
end

function docm(source::LineNumberNode, mod::Module, meta, ex, define::Bool = true)
    @nospecialize meta ex
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
    doc =
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
    # retrieved from the module's metadata `IdDict` without a reference to the string.
    docerror(ex)

    return doc
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

# Swap out the bootstrap macro with the real one.
Core.atdoc!(docm)

function loaddocs(docs)
    for (mod, ex, str, file, line) in docs
        data = Dict(:path => string(file), :linenumber => line)
        doc = docstr(str, data)
        docstring = docm(LineNumberNode(line, file), mod, doc, ex, false) # expand the real @doc macro now
        Core.eval(mod, Expr(Core.unescape, docstring, Docs))
    end
    empty!(docs)
    nothing
end

function formatdoc end
function parsedoc end
function apropos end
function doc end

end
