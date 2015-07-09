# This file is a part of Julia. License is MIT: http://julialang.org/license

module Docs

using Base.Meta
import Base.Markdown: @doc_str, MD

export doc

# Basic API / Storage

const modules = Module[]

const META′ = gensym("META")

@eval meta(mod) = mod.$META′

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

function doc!(obj, data)
    meta()[obj] = data
end

function doc(obj)
    for mod in modules
        haskey(meta(mod), obj) && return meta(mod)[obj]
    end
end

# Function / Method support

function newmethod(defs)
    keylen = -1
    key = nothing
    for def in defs
        length(def.sig.parameters) > keylen && (keylen = length(def.sig.parameters); key = def)
    end
    return key
end

function newmethod(funcs, f)
    applicable = Method[]
    for def in methods(f)
        (!haskey(funcs, def) || funcs[def] != def.func) && push!(applicable, def)
    end
    return newmethod(applicable)
end

def_dict(f) = [def => def.func for def in methods(f)]

function trackmethod(def)
    name = uncurly(unblock(def).args[1].args[1])
    f = esc(name)
    quote
        funcs = nothing
        if $(isexpr(name, Symbol)) && isdefined($(Expr(:quote, name))) && isgeneric($f)
            funcs = def_dict($f)
        end
        $(esc(def))
        if funcs !== nothing
            $f, newmethod(funcs, $f)
        else
            $f, newmethod(methods($f))
        end
    end
end

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
            length(docs) == 0 && fd.main != nothing && push!(docs, fd.main)
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

catdoc() = nothing
catdoc(xs...) = vcat(xs...)

# Type Documentation

isdoc(x) = isexpr(x, :string, AbstractString) ||
    (isexpr(x, :macrocall) && endswith(string(x.args[1]), "_str"))

dict_expr(d) = :(Dict($([:($(Expr(:quote, f)) => $d) for (f, d) in d]...)))

function field_meta(def)
    meta = Dict()
    doc = nothing
    for l in def.args[3].args
        if isdoc(l)
            doc = mdify(l)
        elseif doc != nothing && isexpr(l, Symbol, :(::))
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
                length(docs) == 0 && fd.main != nothing && push!(docs, fd.main)
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
    md == nothing || return md
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

function funcdoc(meta, def)
    quote
        @init
        f, m = $(trackmethod(def))
        doc!(f, m, $(mdify(meta)), $(esc(Expr(:quote, def))))
        f
    end
end

function typedoc(meta, def, name)
    quote
        @init
        $(esc(def))
        doc!($(esc(name)), $(mdify(meta)), $(field_meta(unblock(def))))
        nothing
    end
end

function objdoc(meta, def)
    quote
        @init
        f = $(esc(def))
        doc!(f, $(mdify(meta)))
        f
    end
end

fexpr(ex) = isexpr(ex, :function, :(=)) && isexpr(ex.args[1], :call)

function docm(meta, def)
    # Quote, Unblock and Macroexpand
    # * Always do macro expansion unless it's a quote (for consistency)
    # * Unblock before checking for Expr(:quote) to support `->` syntax
    # * Unblock after macro expansion to recognize structures of
    #   the generated AST
    def′ = unblock(def)
    if !isexpr(def′, :quote)
        def = macroexpand(def)
        def′ = unblock(def)
    elseif length(def′.args) == 1 && isexpr(def′.args[1], :macrocall)
        # Special case for documenting macros after definition with
        # `@doc "<doc string>" :@macro` or
        # `@doc "<doc string>" :(str_macro"")` syntax.
        #
        # Allow more general macrocall for now unless it causes confusion.
        return objdoc(meta, namify(def′.args[1]))
    end
    isexpr(def′, :macro) && return namedoc(meta, def, symbol("@", namify(def′)))
    isexpr(def′, :type) && return typedoc(meta, def, namify(def′.args[2]))
    isexpr(def′, :bitstype) && return namedoc(meta, def, def′.args[2])
    isexpr(def′, :abstract) && return namedoc(meta, def, namify(def′))
    isexpr(def′, :module) && return namedoc(meta, def, def′.args[2])
    fexpr(def′) && return funcdoc(meta, def)
    return objdoc(meta, def)
end

function docm(ex)
    isa(ex,Symbol) && haskey(keywords, ex) && return keywords[ex]
    isexpr(ex, :->) && return docm(ex.args...)
    isexpr(ex, :call) && return :(doc($(esc(ex.args[1])), @which $(esc(ex))))
    isexpr(ex, :macrocall) && (ex = namify(ex))
    :(doc($(esc(ex))))
end

# Not actually used; bootstrap version in bootstrap.jl

macro doc(args...)
    docm(args...)
end

# Swap out the bootstrap macro with the real one

Base.DocBootstrap.setexpand!(docm)

# Names are resolved relative to the DocBootstrap module, so
# inject the ones we need there.

eval(Base.DocBootstrap,
     :(import ..Docs: @init, doc!, doc, newmethod, def_dict, @doc_str))

# Metametadata

"""
The Docs module provides the `@doc` macro which can be used
to set and retreive documentation metadata for Julia objects.
Please see docs for the `@doc` macro for more info.
"""
Docs

"""
# Documentation

Functions, methods and types can be documented by placing a string before the
definition:

    \"""
    # The Foo Function
    `foo(x)`: Foo the living hell out of `x`.
    \"""
    foo(x) = ...

The `@doc` macro can be used directly to both set and retrieve documentation /
metadata. By default, documentation is written as Markdown, but any object can
be placed before the arrow. For example:

    @doc "blah" ->
    function foo() ...

The `->` is not required if the object is on the same line, e.g.

    @doc "foo" foo

# Documenting objects after they are defined
You can document an object after its definition by

    @doc "foo" function_to_doc
    @doc "bar" TypeToDoc

For functions, this currently only support documenting the whole function
Instead of a specific method. See Functions & Methods section below

For macros, the syntax is `@doc "macro doc" :(@Module.macro)` or
`@doc "macro doc" :(string_macro"")` for string macros. Without the quote `:()`
the expansion of the macro will be documented.

# Retrieving Documentation
You can retrieve docs for functions, macros and other objects as
follows:

    @doc foo
    @doc @time
    @doc md""

# Functions & Methods
Placing documentation before a method definition (e.g. `function foo()
...` or `foo() = ...`) will cause that specific method to be
documented, as opposed to the whole function. Method docs are
concatenated together in the order they were defined to provide docs
for the function.
"""
:@Base.DocBootstrap.doc

"`doc(obj)`: Get the doc metadata for `obj`."
doc

"""
`catdoc(xs...)`: Combine the documentation metadata `xs` into a single meta
object.
"""
catdoc

# Text / HTML objects

import Base: print, writemime

export HTML, @html_str

export HTML, Text

"""
`HTML(s)`: Create an object that renders `s` as html.

    HTML("<div>foo</div>")

You can also use a stream for large amounts of data:

    HTML() do io
      println(io, "<div>foo</div>")
    end
"""
type HTML{T}
    content::T
end

function HTML(xs...)
    HTML() do io
        for x in xs
            writemime(io, MIME"text/html"(), x)
        end
    end
end

writemime(io::IO, ::MIME"text/html", h::HTML) = print(io, h.content)
writemime(io::IO, ::MIME"text/html", h::HTML{Function}) = h.content(io)

"Create an `HTML` object from a literal string."
macro html_str(s)
    :(HTML($s))
end

function catdoc(xs::HTML...)
    HTML() do io
        for x in xs
            writemime(io, MIME"text/html"(), x)
        end
    end
end

export Text, @text_str

"""
`Text(s)`: Create an object that renders `s` as plain text.

    Text("foo")

You can also use a stream for large amounts of data:

    Text() do io
      println(io, "foo")
    end
"""
type Text{T}
    content::T
end

print(io::IO, t::Text) = print(io, t.content)
print(io::IO, t::Text{Function}) = t.content(io)
writemime(io::IO, ::MIME"text/plain", t::Text) = print(io, t)

@doc "Create a `Text` object from a literal string." ->
macro text_str(s)
    :(Text($s))
end

function catdoc(xs::Text...)
    Text() do io
        for x in xs
            writemime(io, MIME"text/plain"(), x)
        end
    end
end

# MD support

catdoc(md::MD...) = MD(md...)

# REPL help

function repl_search(io::IO, s)
    pre = "search:"
    print(io, pre)
    printmatches(io, s, completions(s), cols=Base.tty_size()[2]-length(pre))
    println(io, "\n")
end

repl_search(s) = repl_search(STDOUT, s)

function repl_corrections(io::IO, s)
    print(io, "Couldn't find ")
    Markdown.with_output_format(:cyan, io) do io
        println(io, s)
    end
    print_correction(io, s)
end

repl_corrections(s) = repl_corrections(STDOUT, s)

macro repl(ex)
    quote
        # Fuzzy Searching
        $(isexpr(ex, Symbol)) && repl_search($(string(ex)))
        if $(isa(ex, Symbol)) &&
                !(isdefined($(current_module()), $(Expr(:quote, ex))) ||
                  haskey(keywords, $(Expr(:quote, ex))))
            repl_corrections($(string(ex)))
        else
            if $(isfield(ex) ? :(isa($(esc(ex.args[1])), DataType)) : false)
                $(isfield(ex) ? :(fielddoc($(esc(ex.args[1])), $(ex.args[2]))) : nothing)
            else
                # Backwards-compatible with the previous help system, for now
                let doc = @doc $(esc(ex))
                    doc ≠ nothing ? doc : Base.Help.@help_ $(esc(ex))
                end
            end
        end
    end
end

# Search & Rescue
# Utilities for correcting user mistakes and (eventually)
# doing full documentation searches from the repl.

# Fuzzy Search Algorithm

function matchinds(needle, haystack; acronym = false)
    chars = collect(needle)
    is = Int[]
    lastc = '\0'
    for (i, char) in enumerate(haystack)
        isempty(chars) && break
        while chars[1] == ' ' shift!(chars) end # skip spaces
        if lowercase(char) == lowercase(chars[1]) && (!acronym || !isalpha(lastc))
            push!(is, i)
            shift!(chars)
        end
        lastc = char
    end
    return is
end

longer(x, y) = length(x) ≥ length(y) ? (x, true) : (y, false)

bestmatch(needle, haystack) =
    longer(matchinds(needle, haystack, acronym = true),
           matchinds(needle, haystack))

avgdistance(xs) =
    isempty(xs) ? 0 :
    (xs[end] - xs[1] - length(xs)+1)/length(xs)

function fuzzyscore(needle, haystack)
    score = 0.
    is, acro = bestmatch(needle, haystack)
    score += (acro?2:1)*length(is) # Matched characters
    score -= 2(length(needle)-length(is)) # Missing characters
    !acro && (score -= avgdistance(is)/10) # Contiguous
    !isempty(is) && (score -= mean(is)/100) # Closer to beginning
    return score
end

function fuzzysort(search, candidates)
    scores = map(cand -> (fuzzyscore(search, cand), -levenshtein(search, cand)), candidates)
    candidates[sortperm(scores)] |> reverse
end

# Levenshtein Distance

function levenshtein(s1, s2)
    a, b = collect(s1), collect(s2)
    m = length(a)
    n = length(b)
    d = Array(Int, m+1, n+1)

    d[1:m+1, 1] = 0:m
    d[1, 1:n+1] = 0:n

    for i = 1:m, j = 1:n
        d[i+1,j+1] = min(d[i  , j+1] + 1,
                         d[i+1, j  ] + 1,
                         d[i  , j  ] + (a[i] != b[j]))
    end

    return d[m+1, n+1]
end

function levsort(search, candidates)
    scores = map(cand -> (levenshtein(search, cand), -fuzzyscore(search, cand)), candidates)
    candidates = candidates[sortperm(scores)]
    i = 0
    for i = 1:length(candidates)
        levenshtein(search, candidates[i]) > 3 && break
    end
    return candidates[1:i]
end

# Result printing

function printmatch(io::IO, word, match)
    is, _ = bestmatch(word, match)
    Markdown.with_output_format(:fade, io) do io
        for (i, char) = enumerate(match)
            if i in is
                Markdown.with_output_format(print, :bold, io, char)
            else
                print(io, char)
            end
        end
    end
end

printmatch(args...) = printfuzzy(STDOUT, args...)

function printmatches(io::IO, word, matches; cols = Base.tty_size()[2])
    total = 0
    for match in matches
        total + length(match) + 1 > cols && break
        fuzzyscore(word, match) < 0 && break
        print(io, " ")
        printmatch(io, word, match)
        total += length(match) + 1
    end
end

printmatches(args...; cols = Base.tty_size()[2]) = printmatches(STDOUT, args..., cols = cols)

function print_joined_cols(io::IO, ss, delim = "", last = delim; cols = Base.tty_size()[2])
    i = 0
    total = 0
    for i = 1:length(ss)
        total += length(ss[i])
        total + max(i-2,0)*length(delim) + (i>1?1:0)*length(last) > cols && (i-=1; break)
    end
    print_joined(io, ss[1:i], delim, last)
end

print_joined_cols(args...; cols = Base.tty_size()[2]) = print_joined_cols(STDOUT, args...; cols=cols)

function print_correction(io, word)
    cors = levsort(word, accessible(current_module()))
    pre = "Perhaps you meant "
    print(io, pre)
    print_joined_cols(io, cors, ", ", " or "; cols = Base.tty_size()[2]-length(pre))
    println(io)
    return
end

print_correction(word) = print_correction(STDOUT, word)

# Completion data

const builtins = ["abstract", "baremodule", "begin", "bitstype", "break",
                  "catch", "ccall", "const", "continue", "do", "else",
                  "elseif", "end", "export", "finally", "for", "function",
                  "global", "if", "immutable", "import", "importall", "let",
                  "local", "macro", "module", "quote", "return", "try", "type",
                  "typealias", "using", "while"]

moduleusings(mod) = ccall(:jl_module_usings, Any, (Any,), mod)

filtervalid(names) = filter(x->!ismatch(r"#", x), map(string, names))

accessible(mod::Module) =
    [names(mod, true, true);
     map(names, moduleusings(mod))...;
     builtins] |> unique |> filtervalid

completions(name) = fuzzysort(name, accessible(current_module()))
completions(name::Symbol) = completions(string(name))

end
