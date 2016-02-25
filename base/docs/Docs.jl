# This file is a part of Julia. License is MIT: http://julialang.org/license

"""
The Docs module provides the `@doc` macro which can be used to set and retrieve
documentation metadata for Julia objects. Please see docs for the `@doc` macro for more
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
:(Base.DocBootstrap.@doc)

include("bindings.jl")

import Base.Markdown: @doc_str, MD
import Base.Meta: quot, isexpr
import Base: Callable

export doc

# Basic API / Storage

const modules = Module[]

const META′ = :__META__

meta(mod) = mod.(META′)

meta() = meta(current_module())

macro init()
    META = esc(META′)
    quote
        if !isdefined($(Expr(:quote, META′)))
            const $META = ObjectIdDict()
            doc!($META, @doc_str $("Documentation metadata for `$(current_module())`."))
            push!(modules, current_module())
            nothing
        end
    end
end

"`doc!(obj, data)`: Associate the metadata `data` with `obj`."
function doc!(obj, data)
    meta()[obj] = data
end

function get_obj_meta(obj)
    for mod in modules
        if haskey(meta(mod), obj)
            return meta(mod)[obj]
        end
    end
    nothing
end

"`doc(obj)`: Get the metadata associated with `obj`."
function doc(obj)
    get_obj_meta(obj)
end

function write_lambda_signature(io::IO, lam::LambdaInfo)
    ex = Base.uncompressed_ast(lam)
    write(io, '(')
    nargs = length(ex.args[1])
    for (i,arg) in enumerate(ex.args[1])
        i==1 && continue
        if isa(arg,Expr)
            argname, argtype = arg.args
        else
            argname, argtype = arg, :Any
        end
        if argtype === :Any || argtype === :ANY
            write(io, argname)
        elseif isa(argtype,Expr) && argtype.head === :... &&
               (argtype.args[end] === :Any || argtype.args[end] === :ANY)
            write(io, argname, "...")
        else
            write(io, argname, "::", argtype)
        end
        i < nargs && write(io, ',')
    end
    write(io, ')')
    return io
end

function functionsummary(func::Function)
    io  = IOBuffer()
    write(io, "```julia\n")
    print(io, methods(func))
    # TODO jb/functions better summary for closures?
    write(io, "\n```")
    return Markdown.parse(takebuf_string(io))
end

function qualified_name(b::Binding)
    if b.mod === Main
        string(b.var)
    else
        join((b.mod, b.var), '.')
    end
end

function doc(b::Binding)
    d = get_obj_meta(b)
    if d !== nothing
        return d
    end
    if !(b.defined)
        return Markdown.parse("""

        No documentation found.

        Binding `$(qualified_name(b))` does not exist.
        """)
    end
    v = getfield(b.mod,b.var)
    d = doc(v)
    if d !== nothing
        return d
    end
    if startswith(string(b.var),'@')
        # check to see if the binding var is a macro
        d = catdoc(Markdown.parse("""

        No documentation found.

        `$(qualified_name(b))` is a macro.
        """), functionsummary(v))
    elseif isa(v,Function)
        d = catdoc(Markdown.parse("""

        No documentation found.

        `$(qualified_name(b))` is a `Function`.
        """), functionsummary(v))
    elseif isa(v,DataType)
        d = catdoc(Markdown.parse("""

        No documentation found.

        """), typesummary(v))
    else
        T = typeof(v)
        d = catdoc(Markdown.parse("""

        No documentation found.

        `$(qualified_name(b))` is of type `$T`:
        """), typesummary(typeof(v)))
    end
    return d
end

# Function / Method support

function signature(expr::Expr)
    if isexpr(expr, [:call, :macrocall])
        sig = :(Union{Tuple{}})
        for arg in expr.args[2:end]
            isexpr(arg, :parameters) && continue
            if isexpr(arg, :kw) # optional arg
                push!(sig.args, :(Tuple{$(sig.args[end].args[2:end]...)}))
            end
            push!(sig.args[end].args, argtype(arg))
        end
        Expr(:let, Expr(:block, sig), typevars(expr)...)
    else
        signature(expr.args[1])
    end
end

function argtype(expr::Expr)
    isexpr(expr, :(::))  && return expr.args[end]
    isexpr(expr, :(...)) && return :(Vararg{$(argtype(expr.args[1]))})
    argtype(expr.args[1])
end
argtype(::Symbol) = :Any

function typevars(expr::Expr)
    isexpr(expr, :curly) && return [tvar(x) for x in expr.args[2:end]]
    typevars(expr.args[1])
end
typevars(::Symbol) = []

tvar(x::Expr)   = :($(x.args[1]) = TypeVar($(quot(x.args[1])), $(x.args[2]), true))
tvar(s::Symbol) = :($(s) = TypeVar($(quot(s)), Any, true))


"""
    MultiDoc

Stores a collection of docstrings for related objects, ie. a `Function`/`DataType` and
associated `Method` objects.

Each documented object in a `MultiDoc` is referred to by it's signature which is represented
by a `Union` of `Tuple` types. For example the following `Method` definition

    f(x, y) = ...

is stored as `Tuple{Any, Any}` in the `MultiDoc` while

    f{T}(x::T, y = ?) = ...

is stored as `Union{Tuple{T}, Tuple{T, Any}}`.

Note: The `Function`/`DataType` object's signature is always `Union{}`.
"""
type MultiDoc
    "Sorted (via `type_morespecific`) vector of object signatures."
    order::Vector{Type}
    "Documentation for each object. Keys are signatures."
    docs::ObjectIdDict
    "Source `Expr` for each object. As with `.docs` the keys are signatures."
    source::ObjectIdDict
    "Stores the documentation for individual fields of a type."
    fields::Dict{Symbol, Any}

    MultiDoc() = new([], ObjectIdDict(), ObjectIdDict(), Dict())
end

function doc!(λ::Callable, sig::ANY, docstr, expr::Expr, fields::Dict)
    m = get!(meta(), λ, MultiDoc())
    if !haskey(m.docs, sig)
        push!(m.order, sig)
        sort!(m.order, lt = type_morespecific)
    end
    m.docs[sig] = docstr
    m.fields    = fields
    return m
end
doc!(λ::Callable, docstr)                 = doc!(λ, Union{}, docstr, :(),  Dict())
doc!(λ::Callable, docstr, fields)         = doc!(λ, Union{}, docstr, :(),  fields)
doc!(λ::Callable, sig::ANY, docstr, expr) = doc!(λ, sig,     docstr, expr, Dict())


type_morespecific(a::Type, b::Type) = ccall(:jl_type_morespecific, Int32, (Any,Any), a, b) > 0

"""
`catdoc(xs...)`: Combine the documentation metadata `xs` into a single meta object.
"""
catdoc() = nothing
catdoc(xs...) = vcat(xs...)

# Type Documentation

isdoc(s::AbstractString) = true

isdoc(x) = isexpr(x, :string) ||
    (isexpr(x, :macrocall) && x.args[1] == symbol("@doc_str")) ||
    (isexpr(x, :call) && x.args[1] == Expr(:., Base.Markdown, QuoteNode(:doc_str)))

dict_expr(d) = :(Dict($([:(Pair($(Expr(:quote, f)), $d)) for (f, d) in d]...)))

function field_meta(def)
    meta = Dict()
    doc = nothing
    for l in def.args[3].args
        if isdoc(l)
            doc = mdify(l)
        elseif doc !== nothing && (isa(l, Symbol) || isexpr(l, :(::)))
            meta[namify(l)] = doc
            doc = nothing
        end
    end
    return dict_expr(meta)
end

function doc(obj::Base.Callable, sig::Type = Union)
    sig !== Union && isempty(methods(obj, sig)) && return nothing
    results, groups = [], []
    for m in modules
        if haskey(meta(m), obj)
            docs = meta(m)[obj]
            if isa(docs, MultiDoc)
                push!(groups, docs)
                for msig in docs.order
                    if sig <: msig
                        push!(results, (msig, docs.docs[msig]))
                    end
                end
            else
                push!(results, (Union{}, docs))
            end
        end
    end
    # If all method signatures are Union{} ( ⊥ ), concat all docstrings.
    if isempty(results)
        for group in groups
            append!(results, [group.docs[s] for s in reverse(group.order)])
        end
     else
        sort!(results, lt = (a, b) -> type_morespecific(first(a), first(b)))
        results = map(last, results)
    end
    catdoc(results...)
end
doc(f::Base.Callable, args::Any...) = doc(f, Tuple{args...})

function typesummary(T::DataType)
    parts = UTF8String[
    """
    **Summary:**
    ```julia
    $(T.abstract ? "abstract" : T.mutable ? "type" : "immutable") $T <: $(supertype(T))
    ```
    """
    ]
    if !isempty(fieldnames(T))
        pad    = maximum([length(string(f)) for f in fieldnames(T)])
        fields = ["$(rpad(f, pad)) :: $(t)" for (f,t) in zip(fieldnames(T), T.types)]
        push!(parts,
        """
        **Fields:**
        ```julia
        $(join(fields, "\n"))
        ```
        """)
    end
    if !isempty(subtypes(T))
        push!(parts,
        """
        **Subtypes:**
        ```julia
        $(join(subtypes(T),'\n'))
        ```
        """)
    end
    Markdown.parse(join(parts,'\n'))
end

isfield(x) = isexpr(x, :.) &&
  (isa(x.args[1], Symbol) || isfield(x.args[1])) &&
  (isa(x.args[2], QuoteNode) || isexpr(x.args[2], :quote))

function fielddoc(T, k)
    for mod in modules
        docs = meta(mod)
        if haskey(docs, T) && isa(docs[T], MultiDoc)
            fields = docs[T].fields
            if haskey(fields, k)
                return fields[k]
            end
        end
    end
    fields = join(["`$f`" for f in fieldnames(T)], ", ", ", and ")
    fields = isempty(fields) ? "no fields" : "fields $fields"
    Markdown.parse("`$T` has $fields.")
end

# Generic Callables

# Modules

function doc(m::Module)
    md = get_obj_meta(m)
    md === nothing || return md
    readme = Pkg.dir(string(m), "README.md")
    if isfile(readme)
        return Markdown.parse_file(readme)
    end
end

# Keywords

const keywords = Dict{Symbol,Any}()

# Usage macros

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
        n = isexpr(x, [:module, :type, :bitstype]) ? 2 : 1
        nameof(x.args[n], ismacro)
    end
end
nameof(q::QuoteNode, ismacro) = nameof(q.value, ismacro)
nameof(s::Symbol, ismacro)    = ismacro ? macroname(s) : s
nameof(other, ismacro)        = other

macroname(s::Symbol) = symbol('@', s)
macroname(x::Expr)   = Expr(x.head, x.args[1], macroname(x.args[end].value))

function mdify(ex)
    if isa(ex, AbstractString) || isexpr(ex, :string)
        :(Markdown.doc_str($(esc(ex)), @__FILE__, current_module()))
    else
        esc(ex)
    end
end

function namedoc(meta, def, def′)
    quote
        @init
        $(esc(def))
        doc!($(esc(namify(def′))), $(mdify(meta)))
        nothing
    end
end

function funcdoc(meta, def, def′)
    f = esc(namify(def′))
    quote
        @init
        $(esc(def))
        doc!($f, $(esc(signature(def′))), $(mdify(meta)), $(esc(quot(def′))))
        $f
    end
end

function typedoc(meta, def, def′)
    quote
        @init
        $(esc(def))
        doc!($(esc(namify(def′))), $(mdify(meta)), $(field_meta(unblock(def′))))
        nothing
    end
end

function moddoc(meta, def, def′)
    name  = namify(def′)
    docex = :(@doc $meta $name)
    if def == nothing
        esc(:(eval($name, $(quot(docex)))))
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

function vardoc(meta, def, name)
    quote
        @init
        $(esc(def))
        doc!(@var($(esc(namify(name)))), $(mdify(meta)))
    end
end

function multidoc(meta, ex, define)
    out = Expr(:toplevel)
    out.args = [:(@doc($meta, $obj, $define)) for obj in ex.args]
    esc(out)
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
:(Base.@__doc__)

function __doc__!(meta, def, define)
    # Two cases must be handled here to avoid redefining all definitions contained in `def`:
    if define
        # `def` has not been defined yet (this is the common case, i.e. when not generating
        # the Base image). We just need to convert each `@__doc__` marker to an `@doc`.
        finddoc(def) do each
            each.head = :macrocall
            each.args = [symbol("@doc"), meta, each.args[end], define]
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

const FUNC_HEADS    = [:function, :stagedfunction, :macro, :(=)]
const BINDING_HEADS = [:typealias, :const, :global, :(=)]
# For the special `:@mac` / `:(Base.@mac)` syntax for documenting a macro after defintion.
isquotedmacrocall(x) =
    isexpr(x, :copyast, 1) &&
    isa(x.args[1], QuoteNode) &&
    isexpr(x.args[1].value, :macrocall, 1)
# Simple expressions / atoms the may be documented.
isbasicdoc(x) = isexpr(x, :.) || isa(x, Union{QuoteNode, Symbol, Real})

function docm(meta, ex, define = true)
    # Some documented expressions may be decorated with macro calls which obscure the actual
    # expression. Expand the macro calls and remove extra blocks.
    x = unblock(macroexpand(ex))
    # Don't try to redefine expressions. This is only needed for `Base` img gen since
    # otherwise calling `loaddocs` would redefine all documented functions and types.
    def = define ? x : nothing

    # Method / macro definitions and "call" syntax.
    #
    #   function f(...) ... end
    #   f(...) = ...
    #   macro m(...) end
    #   function f end
    #   f(...)
    #
    isexpr(x, FUNC_HEADS) &&  isexpr(x.args[1], :call) ? funcdoc(meta, def, x) :
    isexpr(x, :function)  && !isexpr(x.args[1], :call) ? namedoc(meta, def, x) :
    isexpr(x, :call)                                   ? funcdoc(meta, nothing, x) :

    # Type definitions.
    #
    #   type T ... end
    #   abstract T
    #   bitstype N T
    #
    isexpr(x, :type)                  ? typedoc(meta, def, x) :
    isexpr(x, [:abstract, :bitstype]) ? namedoc(meta, def, x) :

    # "Bindings". Names that resolve to objects with different names, ie.
    #
    #   typealias T S
    #   const T = S
    #   T = S
    #   global T = S
    #
    isexpr(x, BINDING_HEADS) && !isexpr(x.args[1], :call) ? vardoc(meta, def, x) :

    # Quoted macrocall syntax. `:@time` / `:(Base.@time)`.
    isquotedmacrocall(x) ? namedoc(meta, def, x) :
    # Modules and baremodules.
    isexpr(x, :module) ? moddoc(meta, def, x) :
    # Document several expressions with the same docstring. `a, b, c`.
    isexpr(x, :tuple) ? multidoc(meta, x, define) :
    # Errors generated by calling `macroexpand` are passed back to the call site.
    isexpr(x, :error) ? esc(x) :
    # When documenting macro-generated code we look for embedded `@__doc__` calls.
    __doc__!(meta, x, define) ? esc(x) :
    # Any "basic" expression such as a bare function or module name or numeric literal.
    isbasicdoc(x) ? namedoc(meta, nothing, x) :

    # All other expressions are undocumentable and should be handled on a case-by-case basis
    # with `@__doc__`. Unbound string literals are also undocumentable since they cannot be
    # retrieved from the `__META__` `ObjectIdDict` without a reference to the string.
    docerror(ex)
end

function docerror(ex)
    txt = """
    invalid doc expression:

    @doc "..." $(isa(ex, AbstractString) ? repr(ex) : ex)"""
    if isexpr(ex, :macrocall)
        txt *= "\n\n'$(ex.args[1])' not documentable. See 'Base.@__doc__' docs for details."
    end
    :(error($txt, "\n"))
end

function docm(ex)
    if isexpr(ex, :->)
        docm(ex.args...)
    elseif haskey(keywords, ex)
        keywords[ex]
    elseif isexpr(ex, [:call, :macrocall])
        :(doc($(esc(namify(ex))), $(esc(signature(ex)))))
    elseif isexpr(ex, :quote, 1) && isexpr(ex.args[1], :macrocall, 1)
        :(doc(@var($(esc(namify(ex))))))
    elseif isexpr(ex, :.) || isa(ex, Symbol)
        :(doc(@var($(esc(ex)))))
    else
        :(doc($(esc(ex))))
    end
end

# Swap out the bootstrap macro with the real one

Base.DocBootstrap.setexpand!(docm)

# Names are resolved relative to the Base module, so inject the ones we need there.

eval(Base, :(import .Docs: @init, @var, doc!, doc, @doc_str))

Base.DocBootstrap.loaddocs()

# MD support

catdoc(md::MD...) = MD(md...)

include("utils.jl")

end
