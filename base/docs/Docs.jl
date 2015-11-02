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
:(Base.@doc)

include("bindings.jl")

import Base.Meta: quot, isexpr
import Base: DocObj, writemime

export doc

# Basic API / Storage

const modules = Module[]

const META′ = :__META__

meta(mod) = isdefined(mod, META′) ? mod.(META′) : init_meta(mod)

meta() = meta(current_module())

function init_meta(mod)
    META = ObjectIdDict()
    META[META] = DocObj("Documentation metadata for `$mod`.", :Markdown, Symbol(""))
    push!(modules, mod)
    eval(mod, :(const $(META′) = $META))
    META
end

"`doc!(obj, mod, data)`: Associate the metadata `data` with `obj` in module `mod`."
function doc!(obj, mod::Module, data::DocObj)
    meta(mod)[obj] = data
end
doc!(obj, data::DocObj) = doc!(obj, current_module(), data)

function get_obj_meta(obj)
    for mod in modules
        haskey(meta(mod), obj) && return meta(mod)[obj]
    end
end

"`doc(obj)`: Get the metadata associated with `obj`."
function doc(obj)
    get_obj_meta(obj)
end

function write_lambda_signature(io::IO, lam::LambdaStaticData)
    ex = Base.uncompressed_ast(lam)
    write(io, '(')
    nargs = length(ex.args[1])
    for (i,arg) in enumerate(ex.args[1])
        argname, argtype = arg.args
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

function macrosummary(name::Symbol, func::Function)
    if !isdefined(func,:code) || func.code == nothing
        return DocObj("\n", :string, Symbol(""))
    end
    io  = IOBuffer()
    write(io, "```julia\n")
    write(io, name)
    write_lambda_signature(io, func.code)
    write(io, "\n```")
    return DocObj(takebuf_string(io), :Markdown, Symbol(""))
end

function functionsummary(func::Function)
    io  = IOBuffer()
    write(io, "```julia\n")
    if isgeneric(func)
        print(io, methods(func))
    else
        if isdefined(func,:code) && func.code !== nothing
            write_lambda_signature(io, func.code)
            write(io, " -> ...")
        end
    end
    write(io, "\n```")
    return DocObj(takebuf_string(io), :Markdown, Symbol(""))
end

function doc(b::Binding)
    d = get_obj_meta(b)
    if d === nothing
        v = getfield(b.mod,b.var)
        d = doc(v)
        if d === nothing
            if startswith(string(b.var),'@')
                # check to see if the binding var is a macro
                d = catdoc(Any[DocObj("""
                No documentation found.
                """, :Markdown, Symbol("")), macrosummary(b.var, v)])
            elseif isa(v,Function)
                d = catdoc(Any[DocObj("""
                No documentation found.

                `$(b.mod === Main ? b.var : join((b.mod, b.var),'.'))` is $(isgeneric(v) ? "a generic" : "an anonymous") `Function`.
                """, :Markdown, Symbol("")), functionsummary(v)])
            elseif isa(v,DataType)
                d = catdoc(Any[DocObj("""
                No documentation found.
                """, :Markdown, Symbol("")), typesummary(v)])
            else
                T = typeof(v)
                d = catdoc(Any[DocObj("""
                No documentation found.

                `$(b.mod === Main ? b.var : join((b.mod, b.var),'.'))` is of type `$T`:
                """, :Markdown, Symbol("")), typesummary(typeof(v))])
            end
        end
    end
    return d
end

# Function / Method support

function signature(expr::Expr)
    if isexpr(expr, :call)
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

type FuncDoc
    main
    order::Vector{Type}
    meta::ObjectIdDict
end

FuncDoc() = FuncDoc(nothing, [], ObjectIdDict())

# handles the :(function foo end) form
function doc!(f::Function, mod::Module, data::DocObj)
    doc_function!(f, mod, Union{}, data)
end

type_morespecific(a::Type, b::Type) =
    (ccall(:jl_type_morespecific, Int32, (Any,Any), a, b) > 0)

# handles the :(function foo(x...); ...; end) form
function doc_function!(f, mod::Module, sig::ANY, data::DocObj)
    fd = get!(meta(mod), f, isa(f, DataType) ? TypeDoc() : FuncDoc())
    isa(fd, FuncDoc) || isa(fd, TypeDoc) || error("can not document a method when the object already has metadata")
    haskey(fd.meta, sig) || push!(fd.order, sig)
    sort!(fd.order, lt=type_morespecific)
    fd.meta[sig] = data
end
doc_function!(f, sig::ANY, data::DocObj) = doc_function!(f, current_module(), sig, data)

"""
`catdoc(xs)`: Combine the documentation metadata `xs` into a single meta object.
"""
catdoc(xs::Vector{Any}) = isempty(xs) ? nothing : DocObj(xs, :catdoc, @__FILE__)

# Type Documentation

isdoc(s::AbstractString) = true

isdoc(x) = isexpr(x, :string) ||
    (isexpr(x, :macrocall) && x.args[1] == symbol("@doc_str")) ||
    (isexpr(x, :call) && x.args[1] == Expr(:., Base, QuoteNode(:DocObj)))

function field_meta(def)
    fielddesc = Expr(:call)
    meta = fielddesc.args
    push!(meta, :vcat)
    doc = nothing
    for l in def.args[3].args
        if isdoc(l)
            doc = doc_expr(l)
        elseif doc !== nothing && (isa(l, Symbol) || isexpr(l, :(::)))
            push!(meta, :(Pair($(Expr(:quote, namify(l))), $doc)))
            doc = nothing
        end
    end
    return :(Dict{Symbol, Any}($fielddesc))
end

type TypeDoc
    main
    fields::Dict{Symbol, Any}
    order::Vector{Type}
    meta::ObjectIdDict
end

TypeDoc() = TypeDoc(nothing, Dict(), [], ObjectIdDict())

function doc_type!(t::DataType, mod::Module, data::DocObj, fields)
    td = get!(meta(mod), t, TypeDoc())
    td.main = data
    td.fields = fields
end
doc_type!(t::DataType, data::DocObj, fields) = doc_type!(t, current_module(), data, fields)

function doc(obj::Base.Callable, sig::Type = Union)
    isgeneric(obj) && sig !== Union && isempty(methods(obj, sig)) && return nothing
    results, groups = [], []
    for m in modules
        if haskey(meta(m), obj)
            docs = meta(m)[obj]
            if isa(docs, FuncDoc) || isa(docs, TypeDoc)
                push!(groups, docs)
                for msig in docs.order
                    if sig <: msig
                        push!(results, (msig, docs.meta[msig]))
                    end
                end
                if isempty(results) && docs.main !== nothing
                    push!(results, (Union{}, docs.main))
                end
            else
                push!(results, (Union{}, docs))
            end
        end
    end
    # If all method signatures are Union{} ( ⊥ ), concat all docstrings.
    if isempty(results)
        for group in groups
            append!(results, [group.meta[s] for s in reverse(group.order)])
        end
    else
        sort!(results, lt = (a, b) -> type_morespecific(first(a), first(b)))
        results = map(last, results)
    end
    catdoc(results)
end
doc(f::Base.Callable, args::Any...) = doc(f, Tuple{args...})

function typesummary(T::DataType)
    parts = UTF8String[
    """
    **Summary:**
    ```julia
    $(T.abstract ? "abstract" : T.mutable ? "type" : "immutable") $T <: $(super(T))
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
    DocObj(join(parts,'\n'), :Markdown, Symbol(""))
end

isfield(x) = isexpr(x, :.) &&
  (isa(x.args[1], Symbol) || isfield(x.args[1])) &&
  (isa(x.args[2], QuoteNode) || isexpr(x.args[2], :quote))

function fielddoc(T, k)
    for mod in modules
        if haskey(meta(mod), T) && isa(meta(mod)[T], TypeDoc) && haskey(meta(mod)[T].fields, k)
            return meta(mod)[T].fields[k]
        end
  end
  Text(sprint(io -> (print(io, "$T has fields: ");
                     print_joined(io, fieldnames(T), ", ", " and "))))
end

# Generic Callables

# Modules

function doc(m::Module)
    md = get_obj_meta(m)
    md === nothing || return md
    readme = Pkg.dir(string(m), "README.md")
    if isfile(readme)
        return DocObj(readall(readme), :Markdown, readme)
    end
end

# Keywords

const keywords = Dict{Symbol,DocObj}()

# Usage macros

function unblock(ex)
    isexpr(ex, :block) || return ex
    exs = filter(ex -> !(isa(ex, LineNumberNode) || isexpr(ex, :line)), ex.args)
    length(exs) == 1 || return ex
    return unblock(exs[1])
end

uncurly(ex) = isexpr(ex, :curly) ? ex.args[1] : ex

namify(ex::Expr) = isexpr(ex, :.) ? ex : namify(ex.args[1])
namify(ex::QuoteNode) = ex.value
namify(sy::Symbol) = sy

function doc_expr(str)
    if isa(str, AbstractString)
        parser = :Markdown
    else
        parser = :string
    end
    :(DocObj($(esc(str)), $(QuoteNode(parser)), @__FILE__))
end

function namedoc(meta, def, name)
    quote
        $(esc(def))
        Docs.doc!($(esc(name)), $(doc_expr(meta)))
        nothing
    end
end

function funcdoc(meta, def, def′)
    f = esc(namify(def′))
    quote
        $(esc(def))
        Docs.doc_function!($f, $(esc(signature(def′))), $(doc_expr(meta)))
        $f
    end
end

function typedoc(meta, def, def′)
    quote
        $(esc(def))
        Docs.doc_type!($(esc(namify(def′.args[2]))), $(doc_expr(meta)), $(field_meta(unblock(def′))))
        nothing
    end
end

function moddoc(meta, def, name)
    docex = :(@doc $meta $name)
    if def == nothing
        esc(:(eval($name, $(quot(docex)))))
    else
        def = unblock(def)
        block = def.args[3].args
        if !def.args[1]
            isempty(block) && error("empty baremodules are not documentable.")
            insert!(block, 2, :(import Base: call, @doc))
        end
        push!(block, docex)
        esc(Expr(:toplevel, def))
    end
end

function objdoc(meta, def)
    quote
        Docs.doc!($(esc(def)), $(doc_expr(meta)))
    end
end

function vardoc(meta, def, name)
    quote
        $(esc(def))
        Docs.doc!(Docs.Binding($(splitexpr(name)...)), $(doc_expr(meta)))
    end
end

multidoc(meta, objs) = quote $([:(@doc $(esc(meta)) $(esc(obj))) for obj in objs]...) end

doc"""
    @__doc__(ex)

Low-level macro used to mark expressions returned by a macro that should be documented. If
more than one expression is marked then the same docstring is applied to each expression.

    macro example(f)
        quote
            $(f)() = 0
            @__doc__ $(f)(x) = 1
            $(f)(x, y) = 2
        end |> esc
    end

`@__doc__` has no effect when a macro that uses it is not documented.
"""
:(Base.@__doc__)

function __doc__!(meta, def::Expr)
    if isexpr(def, :block, 2) && def.args[1] == symbol("#doc#")
        # Convert `Expr(:block, :#doc#, ...)` created by `@__doc__` to an `@doc`.
        def.head = :macrocall
        def.args = [symbol("@doc"), meta, def.args[end]]
        true
    else
        found = false
        for each in def.args
            found |= __doc__!(meta, each)
        end
        found
    end
end
__doc__!(meta, def) = false

fexpr(ex) = isexpr(ex, [:function, :stagedfunction, :(=)]) && isexpr(ex.args[1], :call)

function docm(meta, def, define = true)

    def′ = unblock(def)

    isexpr(def′, :quote) && isexpr(def′.args[1], :macrocall) &&
        return vardoc(meta, nothing, namify(def′.args[1]))

    ex = def # Save unexpanded expression for error reporting.
    def = macroexpand(def)
    def′ = unblock(def)

    define || (def = nothing)

    fexpr(def′)                ? funcdoc(meta, def, def′) :
    isexpr(def′, :function)    ? namedoc(meta, def, namify(def′)) :
    isexpr(def′, :call)        ? funcdoc(meta, nothing, def′) :
    isexpr(def′, :type)        ? typedoc(meta, def, def′) :
    isexpr(def′, :macro)       ?  vardoc(meta, def, symbol('@',namify(def′))) :
    isexpr(def′, :abstract)    ? namedoc(meta, def, namify(def′)) :
    isexpr(def′, :bitstype)    ? namedoc(meta, def, namify(def′.args[2])) :
    isexpr(def′, :typealias)   ?  vardoc(meta, def, namify(def′)) :
    isexpr(def′, :module)      ?  moddoc(meta, def, def′.args[2]) :
    isexpr(def′, [:(=), :const,
                 :global])     ?  vardoc(meta, def, namify(def′)) :
    isvar(def′)                ? objdoc(meta, def′) :
    isexpr(def′, :tuple)       ? multidoc(meta, def′.args) :
    __doc__!(meta, def′)       ? esc(def′) :

    # All other expressions are undocumentable and should be handled on a case-by-case basis
    # with `@__doc__`. Unbound string literals are also undocumentable since they cannot be
    # retrieved from the `__META__` `ObjectIdDict` without a reference to the string.
    isa(def′, Union{AbstractString, Expr}) ? docerror(ex) : objdoc(meta, def′)
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
    # implementation for Base.@doc
    # adds or returns documentation,
    # depending on the arguments
    if isexpr(ex, :->)
        # add documentation
        return docm(ex.args...)
    elseif isa(ex,Symbol) && haskey(keywords, ex)
        # return documentation for :keyword
        return QuoteNode(keywords[ex])
    elseif isexpr(ex, :call)
        # return documentation for call()
        return findmethod(ex)
    elseif isvar(ex)
        # return documentation for @var, var, and m.var
        return :(doc(Docs.Binding($(splitexpr(ex)...))))
    else
        # return documentation for anything else by first evaluating `ex`
        return :(doc($(esc(ex))))
    end
end

function findmethod(ex)
    f = esc(namify(ex.args[1]))
    :(doc($f, $(esc(signature(ex)))))
end

# Document display

const doc_renders = Dict{Symbol, Function}()

doc_renders[:catdoc] = (io::IO, mime::MIME, docs::DocObj) ->
    for doc in docs.content
        writemime(io, mime, doc)
    end

doc_renders[:string] = (io::IO, mime::MIME"text/plain", docs::DocObj) ->
    if isa(docs.content, AbstractString)
        println(io, docs.content)
    else
        writemime(io, mime, docs.content)
    end

function writemime(io::IO, mime::MIME"text/plain", docs::DocObj)
    get(doc_renders, docs.parser, doc_renders[:string])(io, mime, docs)
end

function writemime(io::IO, mime::MIME, docs::DocObj)
    get(doc_renders, docs.parser, doc_renders[:string])(io, mime, docs)
end

# Swap out the bootstrap macro with the real one
Base.DocBootstrap.setexpand!(docm)
Base.DocBootstrap.loaddocs()

include("utils.jl")

end
