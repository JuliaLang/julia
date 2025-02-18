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
import ..CoreDocs: lazy_iterpolate

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

function signature!(tv::Vector{Any}, expr::Expr)
    is_macrocall = isexpr(expr, :macrocall)
    if is_macrocall || isexpr(expr, :call)
        sig = :(Union{Tuple{}})
        first_arg = is_macrocall ? 3 : 2 # skip function arguments
        for arg in expr.args[first_arg:end]
            isexpr(arg, :parameters) && continue
            if isexpr(arg, :kw) # optional arg
                push!(sig.args, :(Tuple{$((sig.args[end]::Expr).args[2:end]...)}))
            end
            push!((sig.args[end]::Expr).args, argtype(arg))
        end
        if isexpr(expr.args[1], :curly) && isempty(tv)
            append!(tv, mapany(tvar, (expr.args[1]::Expr).args[2:end]))
        end
        for i = length(tv):-1:1
            push!(sig.args, :(Tuple{$((tv[i]::Expr).args[1])}))
        end
        for i = length(tv):-1:1
            sig = Expr(:where, sig, tv[i])
        end
        return sig
    elseif isexpr(expr, :where)
        append!(tv, mapany(tvar, expr.args[2:end]))
        return signature!(tv, expr.args[1])
    else
        return signature!(tv, expr.args[1])
    end
end
signature!(tv::Vector{Any}, @nospecialize(other)) = :(Union{})
signature(expr::Expr) = signature!([], expr)
signature(@nospecialize other) = signature!([], other)

function argtype(expr::Expr)
    isexpr(expr, :(::))  && return expr.args[end]
    isexpr(expr, :(...)) && return :(Vararg{$(argtype(expr.args[1]))})
    if isexpr(expr, :meta) && length(expr.args) == 2
        a1 = expr.args[1]
        if a1 === :nospecialize || a1 === :specialize
            return argtype(expr.args[2])
        end
    end
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
        dict = meta(m; autoinit=false)
        isnothing(dict) && continue
        if haskey(dict, binding)
            docs = dict[binding].docs
            if haskey(docs, typesig)
                return docs[typesig]
            end
        end
    end
    error("could not find matching docstring for '$binding :: $typesig'.")
end
docstr(object, data = Dict{Symbol,Any}()) = _docstr(object, data)

_docstr(vec::Core.SimpleVector, data::Dict{Symbol,Any}) = DocStr(vec,            nothing, data)
_docstr(str::AbstractString,    data::Dict{Symbol,Any}) = DocStr(Core.svec(str), nothing, data)
_docstr(object,                 data::Dict{Symbol,Any}) = DocStr(Core.svec(),     object, data)

_docstr(doc::DocStr, data::Dict{Symbol,Any}) = (doc.data = merge(data, doc.data); doc)

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

# Utilities.
# ==========

"""
`catdoc(xs...)`: Combine the documentation metadata `xs` into a single meta object.
"""
catdoc() = nothing
catdoc(xs...) = vcat(xs...)

const keywords = Dict{Symbol, DocStr}()

namify(@nospecialize x) = astname(x, isexpr(x, :macro))::Union{Symbol,Expr,GlobalRef}

function astname(x::Expr, ismacro::Bool)
    head = x.head
    if head === :.
        ismacro ? macroname(x) : x
    elseif head === :call && isexpr(x.args[1], :(::))
        return astname((x.args[1]::Expr).args[end], ismacro)
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
        P = Pair{Symbol,Any}
        fields = P[]
        last_docstr = nothing
        for each in (expr.args[3]::Expr).args
            eachex = unescape(each)
            if isa(eachex, Symbol) || isexpr(eachex, :(::))
                # a field declaration
                if last_docstr !== nothing
                    push!(fields, P(namify(eachex::Union{Symbol,Expr}), last_docstr))
                    last_docstr = nothing
                end
            elseif isexpr(eachex, :function) || isexpr(eachex, :(=))
                break
            elseif isa(eachex, String) || isexpr(eachex, :string) || isexpr(eachex, :call) ||
                (isexpr(eachex, :macrocall) && eachex.args[1] === Symbol("@doc_str"))
                # forms that might be doc strings
                last_docstr = each
            end
        end
        dict = :($(Dict{Symbol,Any})($([(:($(P)($(quot(f)), $d)))::Expr for (f, d) in fields]...)))
        push!(args, :($(Pair)(:fields, $dict)))
    end
    return :($(Dict{Symbol,Any})($(args...)))
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
    args = callargs(def)
    if isempty(args) || all(validcall, args)
        objectdoc(__source__, __module__, str, nothing, def, signature(def))
    else
        docerror(def)
    end
end
callargs(ex::Expr) = isexpr(ex, :where) ? callargs(ex.args[1]) :
    isexpr(ex, :call) ? ex.args[2:end] : error("Invalid expression to callargs: $ex")
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
            pushfirst!(block, :(import Base: @doc))
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

function __doc__!(source, mod, meta, def, define::Bool)
    @nospecialize source mod meta def
    # Two cases must be handled here to avoid redefining all definitions contained in `def`:
    if define
        function replace_meta_doc(each)
            each.head = :macrocall
            each.args = Any[Symbol("@doc"), source, mod, nothing, meta, each.args[end], define]
        end

        # `def` has not been defined yet (this is the common case, i.e. when not generating
        # the Base image). We just need to convert each `@__doc__` marker to an `@doc`.
        found = finddoc(replace_meta_doc, mod, def; expand_toplevel = false)

        if !found
            found = finddoc(replace_meta_doc, mod, def; expand_toplevel = true)
        end
    else
        # `def` has already been defined during Base image gen so we just need to find and
        # document any subexpressions marked with `@__doc__`.
        docs  = []
        found = finddoc(mod, def; expand_toplevel = true) do each
            push!(docs, :(@doc($source, $mod, $meta, $(each.args[end]), $define)))
        end
        # If any subexpressions have been documented then replace the entire expression with
        # just those documented subexpressions to avoid redefining any definitions.
        if found
            def.head = :toplevel
            def.args = docs
        end
    end
    return found
end
# Walk expression tree `def` and call `λ` when any `@__doc__` markers are found. Returns
# `true` to signify that at least one `@__doc__` has been found, and `false` otherwise.
function finddoc(λ, mod::Module, def::Expr; expand_toplevel::Bool=false)
    if isexpr(def, :block, 2) && isexpr(def.args[1], :meta, 1) && (def.args[1]::Expr).args[1] === :doc
        # Found the macroexpansion of an `@__doc__` expression.
        λ(def)
        true
    else
        if expand_toplevel && isexpr(def, :toplevel)
            for i = 1:length(def.args)
                def.args[i] = macroexpand(mod, def.args[i])
            end
        end
        found = false
        for each in def.args
            found |= finddoc(λ, mod, each; expand_toplevel)
        end
        found
    end
end
finddoc(λ, mod::Module, @nospecialize def; expand_toplevel::Bool=false) = false

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

function _doc(binding::Binding, sig::Type = Union{})
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

# Some additional convenience `doc` methods that take objects rather than `Binding`s.
_doc(obj::UnionAll) = _doc(Base.unwrap_unionall(obj))
_doc(object, sig::Type = Union{}) = _doc(aliasof(object, typeof(object)), sig)
_doc(object, sig...)              = _doc(object, Tuple{sig...})

function simple_lookup_doc(ex)
    if isa(ex, Expr) && ex.head !== :(.) && Base.isoperator(ex.head)
        # handle syntactic operators, e.g. +=, ::, .=
        ex = ex.head
    end
    if haskey(keywords, ex)
        return keywords[ex]
    elseif !isa(ex, Expr) && !isa(ex, Symbol)
        return :($(_doc)($(typeof)($(esc(ex)))))
    end
    binding = esc(bindingexpr(namify(ex)))
    if isexpr(ex, :call) || isexpr(ex, :macrocall) || isexpr(ex, :where)
        sig = esc(signature(ex))
        :($(_doc)($binding, $sig))
    else
        :($(_doc)($binding))
    end
end

function docm(source::LineNumberNode, mod::Module, ex)
    @nospecialize ex
    if isexpr(ex, :->) && length(ex.args) > 1
        return docm(source, mod, ex.args...)
    elseif (REPL = Base.REPL_MODULE_REF[]) !== Base
        # TODO: this is a shim to continue to allow `@doc` for looking up docstrings
        return invokelatest(REPL.lookup_doc, ex)
    else
        return simple_lookup_doc(ex)
    end
    return nothing
end
# Drop incorrect line numbers produced by nested macro calls.
docm(source::LineNumberNode, mod::Module, _, _, x...) = docm(source, mod, x...)

# iscallexpr checks if an expression is a :call expression. The call expression may be
# also part of a :where expression, so it unwraps the :where layers until it reaches the
# "actual" expression
iscallexpr(ex::Expr) = isexpr(ex, :where) ? iscallexpr(ex.args[1]) : isexpr(ex, :call)
iscallexpr(ex) = false

function docm(source::LineNumberNode, mod::Module, meta, ex, define::Bool = true)
    @nospecialize meta ex
    # Some documented expressions may be decorated with macro calls which obscure the actual
    # expression. Expand the macro calls.
    x = macroexpand(mod, ex)
    return _docm(source, mod, meta, x, define)
end

function _docm(source::LineNumberNode, mod::Module, meta, x, define::Bool = true)
    if isexpr(x, :var"hygienic-scope")
        x.args[1] = _docm(source, mod, meta, x.args[1])
        return x
    elseif isexpr(x, :escape)
        x.args[1] = _docm(source, mod, meta, x.args[1])
        return x
    elseif isexpr(x, :block)
        docarg = 0
        for i = 1:length(x.args)
            isa(x.args[i], LineNumberNode) && continue
            if docarg == 0
                docarg = i
                continue
            end
            # More than one documentable expression in the block, treat it as a whole
            # expression, which will fall through and look for (Expr(:meta, doc))
            docarg = 0
            break
        end
        if docarg != 0
            x.args[docarg] = _docm(source, mod, meta, x.args[docarg], define)
            return x
        end
    end

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
    # Including if the "call" expression is wrapped in "where" expression(s) (#32960), i.e.
    #
    #   f(::T) where T
    #   f(::T, ::U) where T where U
    #
    isexpr(x, FUNC_HEADS) && is_signature((x::Expr).args[1]) ? objectdoc(source, mod, meta, def, x::Expr, signature(x::Expr)) :
    isexpr(x, [:function, :macro])  && !isexpr((x::Expr).args[1], :call) ? objectdoc(source, mod, meta, def, x::Expr) :
    iscallexpr(x) ? calldoc(source, mod, meta, x::Expr) :

    # Type definitions.
    #
    #   struct T ... end
    #   abstract type T end
    #   primitive type T N end
    #
    isexpr(x, [:struct, :abstract, :primitive]) ? objectdoc(source, mod, meta, def, x::Expr) :

    # "Bindings". Names that resolve to objects with different names, ie.
    #
    #   const T = S
    #   T = S
    #   global T = S
    #
    isexpr(x, BINDING_HEADS) && !isexpr((x::Expr).args[1], :call) ? objectdoc(source, mod, meta, def, x::Expr) :

    # Quoted macrocall syntax. `:@time` / `:(Base.@time)`.
    isquotedmacrocall(x) ? objectdoc(source, mod, meta, def, x) :
    # Modules and baremodules.
    isexpr(x, :module) ? moduledoc(source, mod, meta, def, x::Expr) :
    # Document several expressions with the same docstring. `a, b, c`.
    isexpr(x, :tuple) ? multidoc(source, mod, meta, x::Expr, define) :
    # Errors generated by calling `macroexpand` are passed back to the call site.
    isexpr(x, :error) ? esc(x) :
    # When documenting macro-generated code we look for embedded `@__doc__` calls.
    __doc__!(source, mod, meta, x, define) ? esc(x) :
    # Any "basic" expression such as a bare function or module name or numeric literal.
    isbasicdoc(x) ? objectdoc(source, mod, meta, nothing, x) :

    # All other expressions are undocumentable and should be handled on a case-by-case basis
    # with `@__doc__`. Unbound string literals are also undocumentable since they cannot be
    # retrieved from the module's metadata `IdDict` without a reference to the string.
    docerror(x)

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

function loaddocs(docs::Base.CoreDocs.DocLinkedList)
    while isdefined(docs, :doc)
        (mod, ex, str, file, line) = docs.doc
        data = Dict{Symbol,Any}(:path => string(file), :linenumber => line)
        doc = docstr(str, data)
        lno = LineNumberNode(line, file)
        docstring = docm(lno, mod, doc, ex, false) # expand the real @doc macro now
        Core.eval(mod, Expr(:var"hygienic-scope", docstring, Docs, lno))
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
"""
function apropos end

"""
    Docs.doc(binding, sig)

Return all documentation that matches both `binding` and `sig`.

If `getdoc` returns a non-`nothing` result on the value of the binding, then a
dynamic docstring is returned instead of one based on the binding itself.
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
