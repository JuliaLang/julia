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
import Base.Meta: quot

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

"`doc(obj)`: Get the metadata associated with `obj`."
function doc(obj)
    for mod in modules
        haskey(meta(mod), obj) && return meta(mod)[obj]
    end
end

function doc(b::Binding)
    d = invoke(doc, Tuple{Any}, b)
    d == nothing ? doc(getfield(b.mod, b.var)) : d
end

# Function / Method support

function signature(expr::Expr)
    if isexpr(expr, :call)
        sig = :(Tuple{})
        for arg in expr.args[2:end]
            isexpr(arg, :parameters) && continue
            push!(sig.args, argtype(arg))
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
    order::Vector{Method}
    meta::ObjectIdDict
    source::ObjectIdDict
end

FuncDoc() = FuncDoc(nothing, [], ObjectIdDict(), ObjectIdDict())

function doc!(f::Function, data)
    fd = get!(meta(), f, FuncDoc())
    fd.main = data
end

function doc!(f::Function, m::Method, data, source)
    fd = get!(meta(), f, FuncDoc())
    isa(fd, FuncDoc) || error("Can't document a method when the function already has metadata")
    !haskey(fd.meta, m) && push!(fd.order, m)
    fd.meta[m] = data
    fd.source[m] = source
end

function doc(f::Function)
    docs = []
    for mod in modules
        if haskey(meta(mod), f)
            fd = meta(mod)[f]
            length(docs) == 0 && fd.main !== nothing && push!(docs, fd.main)
            if isa(fd, FuncDoc)
                for m in fd.order
                    push!(docs, fd.meta[m])
                end
            elseif length(docs) == 0
                return fd
            end
        end
    end
    return catdoc(docs...)
end

function doc(f::Function, m::Method)
    for mod in modules
        haskey(meta(mod), f) && isa(meta(mod)[f], FuncDoc) && haskey(meta(mod)[f].meta, m) &&
            return meta(mod)[f].meta[m]
    end
end

"""
`catdoc(xs...)`: Combine the documentation metadata `xs` into a single meta object.
"""
catdoc() = nothing
catdoc(xs...) = vcat(xs...)

# Type Documentation

isdoc(x) = isexpr(x, :string, AbstractString) ||
    (isexpr(x, :macrocall) && x.args[1] == symbol("@doc_str")) ||
    (isexpr(x, :call) && x.args[1] == Expr(:., Base.Markdown, QuoteNode(:doc_str)))

dict_expr(d) = :(Dict($([:($(Expr(:quote, f)) => $d) for (f, d) in d]...)))

function field_meta(def)
    meta = Dict()
    doc = nothing
    for l in def.args[3].args
        if isdoc(l)
            doc = mdify(l)
        elseif doc !== nothing && isexpr(l, Symbol, :(::))
            meta[namify(l)] = doc
            doc = nothing
        end
    end
    return dict_expr(meta)
end

type TypeDoc
    main
    fields::Dict{Symbol, Any}
    order::Vector{Method}
    meta::ObjectIdDict
end

TypeDoc() = TypeDoc(nothing, Dict(), [], ObjectIdDict())

function doc!(t::DataType, data, fields)
    td = get!(meta(), t, TypeDoc())
    td.main = data
    td.fields = fields
end

function doc!(f::DataType, m::Method, data, source)
    td = get!(meta(), f, TypeDoc())
    isa(td, TypeDoc) || error("Can't document a method when the type already has metadata")
    !haskey(td.meta, m) && push!(td.order, m)
    td.meta[m] = data
end

function doc(f::DataType)
    docs = []
    for mod in modules
        if haskey(meta(mod), f)
            fd = meta(mod)[f]
            if isa(fd, TypeDoc)
                length(docs) == 0 && fd.main !== nothing && push!(docs, fd.main)
                for m in fd.order
                    push!(docs, fd.meta[m])
                end
            elseif length(docs) == 0
                return fd
            end
        end
    end
    return catdoc(docs...)
end

isfield(x) = isexpr(x, :.) &&
  (isexpr(x.args[1], Symbol) || isfield(x.args[1])) &&
  isexpr(x.args[2], QuoteNode, :quote)

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

doc(f, ::Method) = doc(f)

# Modules

function doc(m::Module)
    md = invoke(doc, Tuple{Any}, m)
    md === nothing || return md
    readme = Pkg.dir(string(m), "README.md")
    if isfile(readme)
        return Markdown.parse_file(readme)
    end
end

# Keywords

const keywords = Dict{Symbol,Any}()

# Usage macros

isexpr(x::Expr) = true
isexpr(x) = false
isexpr(x::Expr, ts...) = x.head in ts
isexpr(x, ts...) = any(T->isa(T, Type) && isa(x, T), ts)

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

function mdify(ex)
    if isexpr(ex, AbstractString, :string)
        :(Markdown.parse($(esc(ex))))
    else
        esc(ex)
    end
end

function namedoc(meta, def, name)
    quote
        @init
        $(esc(def))
        doc!($(esc(name)), $(mdify(meta)))
        nothing
    end
end

function funcdoc(meta, def, def′)
    f = esc(namify(def′))
    m = :(methods($f, $(esc(signature(def′))))[end])
    quote
        @init
        $(esc(def))
        doc!($f, $m, $(mdify(meta)), $(esc(quot(def′))))
        $f
    end
end

function typedoc(meta, def, def′)
    quote
        @init
        $(esc(def))
        doc!($(esc(namify(def′.args[2]))), $(mdify(meta)), $(field_meta(unblock(def′))))
        nothing
    end
end

function moddoc(meta, def, name)
    docex = :(@doc $meta $name)
    def == nothing && return :(eval($name, $(quot(docex)))) |> esc
    push!(unblock(def).args[3].args, docex)
    return esc(Expr(:toplevel, def))
end

function objdoc(meta, def)
    quote
        @init
        doc!($(esc(def)), $(mdify(meta)))
    end
end

function vardoc(meta, def, name)
    quote
        @init
        $(esc(def))
        doc!(@var($(esc(name))), $(mdify(meta)))
    end
end

multidoc(meta, objs) = quote $([:(@doc $(esc(meta)) $(esc(obj))) for obj in objs]...) end

fexpr(ex) = isexpr(ex, :function, :stagedfunction, :(=)) && isexpr(ex.args[1], :call)

function docm(meta, def, define = true)
    def′ = unblock(def)

    isexpr(def′, :quote) && isexpr(def′.args[1], :macrocall) &&
        return vardoc(meta, nothing, namify(def′.args[1]))

    def = macroexpand(def)
    def′ = unblock(def)

    define || (def = nothing)

    fexpr(def′)                ? funcdoc(meta, def, def′) :
    isexpr(def′, :function)    ? namedoc(meta, def, namify(def′)) :
    isexpr(def′, :call)        ? funcdoc(meta, nothing, def′) :
    isexpr(def′, :type)        ? typedoc(meta, def, def′) :
    isexpr(def′, :macro)       ?  vardoc(meta, def, symbol("@", namify(def′))) :
    isexpr(def′, :abstract)    ? namedoc(meta, def, namify(def′)) :
    isexpr(def′, :bitstype)    ? namedoc(meta, def, def′.args[2]) :
    isexpr(def′, :typealias)   ?  vardoc(meta, def, namify(def′)) :
    isexpr(def′, :module)      ?  moddoc(meta, def, def′.args[2]) :
    isexpr(def′, :(=), :const,
                 :global)      ?  vardoc(meta, def, namify(def′)) :
    isvar(def′)                ? objdoc(meta, def′) :
    isexpr(def′, :tuple)       ? multidoc(meta, def′.args) :
    isa(def′, Expr)            ? error("Invalid doc expression $def′") :
    objdoc(meta, def′)
end

function docm(ex)
    isexpr(ex, :->)                        ? docm(ex.args...) :
    isa(ex,Symbol) && haskey(keywords, ex) ? keywords[ex] :
    isexpr(ex, :call)                      ? findmethod(ex) :
    isvar(ex)                              ? :(doc(@var($(esc(ex))))) :
    :(doc($(esc(ex))))
end

function findmethod(ex)
    f = esc(namify(ex.args[1]))
    if any(x -> isexpr(x, :(::)), ex.args)
        :(doc($f, methods($f, $(esc(signature(ex))))[1]))
    else
        :(doc($f, @which($(esc(ex)))))
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
