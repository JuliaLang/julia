# This file is a part of Julia. License is MIT: https://julialang.org/license

## Code for searching and viewing documentation

using Markdown

using Base.Docs: catdoc, modules, DocStr, Binding, MultiDoc, keywords, isfield, namify, bindingexpr,
    defined, resolve, getdoc, meta, aliasof, signature

import Base.Docs: doc, formatdoc, parsedoc, apropos

using Base: with_output_color, mapany, isdeprecated, isexported

using Base.Filesystem: _readdirx

using InteractiveUtils: subtypes

using Unicode: normalize

## Help mode ##

# This is split into helpmode and _helpmode to easier unittest _helpmode
function helpmode(io::IO, line::AbstractString, mod::Module=Main)
    internal_accesses = Set{Pair{Module,Symbol}}()
    quote
        docs = $Markdown.insert_hlines($(REPL._helpmode(io, line, mod, internal_accesses)))
        $REPL.insert_internal_warning(docs, $internal_accesses)
    end
end
helpmode(line::AbstractString, mod::Module=Main) = helpmode(stdout, line, mod)

# A hack to make the line entered at the REPL available at trimdocs without
# passing the string through the entire mechanism.
const extended_help_on = Ref{Any}(nothing)

function _helpmode(io::IO, line::AbstractString, mod::Module=Main, internal_accesses::Union{Nothing, Set{Pair{Module,Symbol}}}=nothing)
    line = strip(line)
    ternary_operator_help = (line == "?" || line == "?:")
    if startswith(line, '?') && !ternary_operator_help
        line = line[2:end]
        extended_help_on[] = nothing
        brief = false
    else
        extended_help_on[] = line
        brief = true
    end
    # interpret anything starting with # or #= as asking for help on comments
    if startswith(line, "#")
        if startswith(line, "#=")
            line = "#="
        else
            line = "#"
        end
    end
    x = Meta.parse(line, raise = false, depwarn = false)
    assym = Symbol(line)
    expr =
        if haskey(keywords, assym) || Base.isoperator(assym) || isexpr(x, :error) ||
            isexpr(x, :invalid) || isexpr(x, :incomplete)
            # Docs for keywords must be treated separately since trying to parse a single
            # keyword such as `function` would throw a parse error due to the missing `end`.
            assym
        elseif isexpr(x, (:using, :import))
            (x::Expr).head
        else
            # Retrieving docs for macros requires us to make a distinction between the text
            # `@macroname` and `@macroname()`. These both parse the same, but are used by
            # the docsystem to return different results. The first returns all documentation
            # for `@macroname`, while the second returns *only* the docs for the 0-arg
            # definition if it exists.
            (isexpr(x, :macrocall, 1) && !endswith(line, "()")) ? quot(x) : x
        end
    # the following must call repl(io, expr) via the @repl macro
    # so that the resulting expressions are evaluated in the Base.Docs namespace
    :($REPL.@repl $io $expr $brief $mod $internal_accesses)
end
_helpmode(line::AbstractString, mod::Module=Main) = _helpmode(stdout, line, mod)

function formatdoc(d::DocStr)
    buffer = IOBuffer()
    for part in d.text
        formatdoc(buffer, d, part)
    end
    md = Markdown.MD(Any[Markdown.parse(seekstart(buffer))])
    assume_julia_code!(md)
end
@noinline formatdoc(buffer, d, part) = print(buffer, part)

function parsedoc(d::DocStr)
    if d.object === nothing
        md = formatdoc(d)
        md.meta[:module] = d.data[:module]
        md.meta[:path]   = d.data[:path]
        d.object = md
    end
    d.object
end

"""
    assume_julia_code!(doc::Markdown.MD) -> doc

Assume that code blocks with no language specified are Julia code.
"""
function assume_julia_code!(doc::Markdown.MD)
    assume_julia_code!(doc.content)
    doc
end

function assume_julia_code!(blocks::Vector)
    for (i, block) in enumerate(blocks)
        if block isa Markdown.Code && block.language == ""
            blocks[i] = Markdown.Code("julia", block.code)
        elseif block isa Vector || block isa Markdown.MD
            assume_julia_code!(block)
        end
    end
    blocks
end

## Trimming long help ("# Extended help")

struct Message  # For direct messages to the terminal
    msg    # AbstractString
    fmt    # keywords to `printstyled`
end
Message(msg) = Message(msg, ())

function Markdown.term(io::IO, msg::Message, columns)
    printstyled(io, msg.msg; msg.fmt...)
end

trimdocs(doc, brief::Bool) = doc

function trimdocs(md::Markdown.MD, brief::Bool)
    brief || return md
    md, trimmed = _trimdocs(md, brief)
    if trimmed
        line = extended_help_on[]
        line = isa(line, AbstractString) ? line : ""
        push!(md.content, Message("Extended help is available with `??$line`", (color=Base.info_color(), bold=true)))
    end
    return md
end

function _trimdocs(md::Markdown.MD, brief::Bool)
    content, trimmed = [], false
    for c in md.content
        if isa(c, Markdown.Header{1}) && isa(c.text, AbstractArray) && !isempty(c.text)
            item = c.text[1]
            if isa(item, AbstractString) &&
                lowercase(item) ∈ ("extended help",
                                   "extended documentation",
                                   "extended docs")
                trimmed = true
                break
            end
        end
        c, trm = _trimdocs(c, brief)
        trimmed |= trm
        push!(content, c)
    end
    return Markdown.MD(content, md.meta), trimmed
end

_trimdocs(md, brief::Bool) = md, false


is_tuple(expr) = false
is_tuple(expr::Expr) = expr.head == :tuple

struct Logged{F}
    f::F
    mod::Module
    collection::Set{Pair{Module,Symbol}}
end
function (la::Logged)(m::Module, s::Symbol)
    m !== la.mod && Base.isdefined(m, s) && !Base.ispublic(m, s) && push!(la.collection, m => s)
    la.f(m, s)
end
(la::Logged)(args...) = la.f(args...)

function log_nonpublic_access(expr::Expr, mod::Module, internal_access::Set{Pair{Module,Symbol}})
    if expr.head === :. && length(expr.args) == 2 && !is_tuple(expr.args[2])
        Expr(:call, Logged(getproperty, mod, internal_access), log_nonpublic_access.(expr.args, (mod,), (internal_access,))...)
    elseif expr.head === :call && expr.args[1] === Base.Docs.Binding
        Expr(:call, Logged(Base.Docs.Binding, mod, internal_access), log_nonpublic_access.(expr.args[2:end], (mod,), (internal_access,))...)
    else
        Expr(expr.head, log_nonpublic_access.(expr.args, (mod,), (internal_access,))...)
    end
end
log_nonpublic_access(expr, ::Module, _) = expr

function insert_internal_warning(md::Markdown.MD, internal_access::Set{Pair{Module,Symbol}})
    if !isempty(internal_access)
        items = Any[Any[Markdown.Paragraph(Any[Markdown.Code("", s)])] for s in sort!(["$mod.$sym" for (mod, sym) in internal_access])]
        admonition = Markdown.Admonition("warning", "Warning", Any[
            Markdown.Paragraph(Any["The following bindings may be internal; they may change or be removed in future versions:"]),
            Markdown.List(items, -1, false)])
        pushfirst!(md.content, admonition)
    end
    md
end
function insert_internal_warning(other, internal_access::Set{Pair{Module,Symbol}})
    # We don't know how to insert an internal symbol warning into non-markdown
    # content, so we don't.
    other
end

function doc(binding::Binding, sig::Type = Union{})
    if defined(binding)
        result = getdoc(resolve(binding), sig)
        result === nothing || return result
    end
    results, groups = DocStr[], MultiDoc[]
    # Lookup `binding` and `sig` for matches in all modules of the docsystem.
    for mod in modules
        dict = meta(mod; autoinit=false)
        isnothing(dict) && continue
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
        md = catdoc(mapany(parsedoc, results)...)
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

function lookup_doc(ex)
    if isa(ex, Expr) && ex.head !== :(.) && Base.isoperator(ex.head)
        # handle syntactic operators, e.g. +=, ::, .=
        ex = ex.head
    end
    if haskey(keywords, ex)
        return parsedoc(keywords[ex])
    elseif Meta.isexpr(ex, :incomplete)
        return :($(Markdown.md"No documentation found."))
    elseif !isa(ex, Expr) && !isa(ex, Symbol)
        return :($(doc)($(typeof)($(esc(ex)))))
    end
    if isa(ex, Symbol) && Base.isoperator(ex)
        str = string(ex)
        isdotted = startswith(str, ".")
        if endswith(str, "=") && Base.operator_precedence(ex) == Base.prec_assignment && ex !== :(:=)
            op = chop(str)
            eq = isdotted ? ".=" : "="
            return Markdown.parse("`x $op= y` is a synonym for `x $eq x $op y`")
        elseif isdotted && ex !== :(..)
            op = str[2:end]
            if op in ("&&", "||")
                return Markdown.parse("`x $ex y` broadcasts the boolean operator `$op` to `x` and `y`. See [`broadcast`](@ref).")
            else
                return Markdown.parse("`x $ex y` is akin to `broadcast($op, x, y)`. See [`broadcast`](@ref).")
            end
        end
    end
    binding = esc(bindingexpr(namify(ex)))
    if isexpr(ex, :call) || isexpr(ex, :macrocall) || isexpr(ex, :where)
        sig = esc(signature(ex))
        :($(doc)($binding, $sig))
    else
        :($(doc)($binding))
    end
end

# Object Summaries.
# =================

function summarize(binding::Binding, sig)
    io = IOBuffer()
    if defined(binding)
        binding_res = resolve(binding)
        if !isa(binding_res, Module)
            varstr = "$(binding.mod).$(binding.var)"
            if Base.ispublic(binding.mod, binding.var)
                println(io, "No documentation found for public binding `$varstr`.\n")
            else
                println(io, "No documentation found for private binding `$varstr`.\n")
            end
        end
        summarize(io, binding_res, binding)
    else
        println(io, "No documentation found.\n")
        quot = any(isspace, sprint(print, binding)) ? "'" : ""
        bpart = Base.lookup_binding_partition(Base.tls_world_age(), convert(Core.Binding, GlobalRef(binding.mod, binding.var)))
        if Base.binding_kind(bpart) === Base.PARTITION_KIND_GUARD
            println(io, "Binding ", quot, "`", binding, "`", quot, " does not exist.")
        else
            println(io, "Binding ", quot, "`", binding, "`", quot, " exists, but has not been assigned a value.")
        end
    end
    md = Markdown.parse(seekstart(io))
    # Save metadata in the generated markdown.
    md.meta[:results] = DocStr[]
    md.meta[:binding] = binding
    md.meta[:typesig] = sig
    return md
end

function summarize(io::IO, λ::Function, binding::Binding)
    kind = startswith(string(binding.var), '@') ? "macro" : "`Function`"
    println(io, "`", binding, "` is a ", kind, ".")
    println(io, "```\n", methods(λ), "\n```")
end

function summarize(io::IO, TT::Type, binding::Binding)
    println(io, "# Summary")
    T = Base.unwrap_unionall(TT)
    if T isa DataType
        println(io, "```")
        print(io,
            Base.isabstracttype(T) ? "abstract type " :
            Base.ismutabletype(T)  ? "mutable struct " :
            Base.isstructtype(T) ? "struct " :
            "primitive type ")
        supert = supertype(T)
        println(io, T)
        println(io, "```")
        if !Base.isabstracttype(T) && T.name !== Tuple.name && !isempty(fieldnames(T))
            println(io, "# Fields")
            println(io, "```")
            pad = maximum(length(string(f)) for f in fieldnames(T))
            for (f, t) in zip(fieldnames(T), fieldtypes(T))
                println(io, rpad(f, pad), " :: ", t)
            end
            println(io, "```")
        end
        subt = subtypes(TT)
        if !isempty(subt)
            println(io, "# Subtypes")
            println(io, "```")
            for t in subt
                println(io, Base.unwrap_unionall(t))
            end
            println(io, "```")
        end
        if supert != Any
            println(io, "# Supertype Hierarchy")
            println(io, "```")
            Base.show_supertypes(io, T)
            println(io)
            println(io, "```")
        end
    elseif T isa Union
        println(io, "`", binding, "` is of type `", typeof(TT), "`.\n")
        println(io, "# Union Composed of Types")
        for T1 in Base.uniontypes(T)
            println(io, " - `", Base.rewrap_unionall(T1, TT), "`")
        end
    else # unreachable?
        println(io, "`", binding, "` is of type `", typeof(TT), "`.\n")
    end
end

function find_readme(m::Module)::Union{String, Nothing}
    mpath = pathof(m)
    isnothing(mpath) && return nothing
    !isfile(mpath) && return nothing # modules in sysimage, where src files are omitted
    path = dirname(mpath)
    top_path = pkgdir(m)
    while true
        for entry in _readdirx(path; sort=true)
            isfile(entry) && (lowercase(entry.name) in ["readme.md", "readme"]) || continue
            return entry.path
        end
        path == top_path && break # go no further than pkgdir
        path = dirname(path) # work up through nested modules
    end
    return nothing
end
function summarize(io::IO, m::Module, binding::Binding; nlines::Int = 200)
    readme_path = find_readme(m)
    public = Base.ispublic(binding.mod, binding.var) ? "public" : "internal"
    if isnothing(readme_path)
        println(io, "No docstring or readme file found for $public module `$m`.\n")
    else
        println(io, "No docstring found for $public module `$m`.")
    end
    exports = filter!(!=(nameof(m)), names(m))
    if isempty(exports)
        println(io, "Module does not have any public names.")
    else
        println(io, "# Public names")
        print(io, "  `")
        join(io, exports, "`, `")
        println(io, "`\n")
    end
    if !isnothing(readme_path)
        readme_lines = readlines(readme_path)
        isempty(readme_lines) && return  # don't say we are going to print empty file
        println(io)
        println(io, "---")
        println(io, "_Package description from `$(basename(readme_path))`:_")
        for line in first(readme_lines, nlines)
            println(io, line)
        end
        length(readme_lines) > nlines && println(io, "\n[output truncated to first $nlines lines]")
    end
end

function summarize(io::IO, @nospecialize(T), binding::Binding)
    T = typeof(T)
    println(io, "`", binding, "` is of type `", T, "`.\n")
    summarize(io, T, binding)
end

# repl search and completions for help

# This type is returned from `accessible` and denotes a binding that is accessible within
# some context. It differs from `Base.Docs.Binding`, which is also used by the REPL, in
# that it doesn't track the defining module for a symbol unless the symbol is public but
# not exported, i.e. it's accessible but requires qualification. Using this type rather
# than `Base.Docs.Binding` simplifies things considerably, partially because REPL searching
# is based on `String`s, which this type stores, but `Base.Docs.Binding` stores a module
# and symbol and does not have any notion of the context from which the binding is accessed.
struct AccessibleBinding
    source::Union{String,Nothing}
    name::String
end

function AccessibleBinding(mod::Module, name::Symbol)
    m = isexported(mod, name) ? nothing : String(nameof(mod))
    return AccessibleBinding(m, String(name))
end
AccessibleBinding(name::Symbol) = AccessibleBinding(nothing, String(name))

function Base.show(io::IO, b::AccessibleBinding)
    b.source === nothing || print(io, b.source, '.')
    print(io, b.name)
end

quote_spaces(x) = any(isspace, x) ? "'" * x * "'" : x
quote_spaces(x::AccessibleBinding) = AccessibleBinding(x.source, quote_spaces(x.name))

function repl_search(io::IO, s::Union{Symbol,String}, mod::Module)
    pre = "search:"
    print(io, pre)
    printmatches(io, s, map(quote_spaces, doc_completions(s, mod)), cols = _displaysize(io)[2] - length(pre))
    println(io, "\n")
end

# TODO: document where this is used
repl_search(s, mod::Module) = repl_search(stdout, s, mod)

function repl_corrections(io::IO, s, mod::Module)
    print(io, "Couldn't find ")
    quot = any(isspace, s) ? "'" : ""
    print(io, quot)
    printstyled(io, s, color=:cyan)
    print(io, quot)
    if Base.identify_package(s) === nothing
        print(io, '\n')
    else
        print(io, ", but a loadable package with that name exists. If you are looking for the package docs load the package first.\n")
    end
    print_correction(io, s, mod)
end
repl_corrections(s) = repl_corrections(stdout, s)

# inverse of latex_symbols Dict, lazily created as needed
const symbols_latex = Dict{String,String}()
function symbol_latex(s::String)
    if isempty(symbols_latex)
        for (k,v) in Iterators.flatten((REPLCompletions.latex_symbols,
                                        REPLCompletions.emoji_symbols))
            symbols_latex[v] = k
        end

        # Overwrite with canonical mapping when a symbol has several completions (#39148)
        merge!(symbols_latex, REPLCompletions.symbols_latex_canonical)
    end

    return get(symbols_latex, s, "")
end
function repl_latex(io::IO, s0::String)
    # This has rampant `Core.Box` problems (#15276). Use the tricks of
    # https://docs.julialang.org/en/v1/manual/performance-tips/#man-performance-captured
    # We're changing some of the values so the `let` trick isn't applicable.
    s::String = s0
    latex::String = symbol_latex(s)
    if isempty(latex)
        # Decompose NFC-normalized identifier to match tab-completion
        # input if the first search came up empty.
        s = normalize(s, :NFD)
        latex = symbol_latex(s)
    end
    if !isempty(latex)
        print(io, "\"")
        printstyled(io, s, color=:cyan)
        print(io, "\" can be typed by ")
        printstyled(io, latex, "<tab>", color=:cyan)
        println(io, '\n')
    elseif any(c -> haskey(symbols_latex, string(c)), s)
        print(io, "\"")
        printstyled(io, s, color=:cyan)
        print(io, "\" can be typed by ")
        state::Char = '\0'
        with_output_color(:cyan, io) do io
            for c in s
                cstr = string(c)
                if haskey(symbols_latex, cstr)
                    latex = symbols_latex[cstr]
                    if length(latex) == 3 && latex[2] in ('^','_')
                        # coalesce runs of sub/superscripts
                        if state != latex[2]
                            '\0' != state && print(io, "<tab>")
                            print(io, latex[1:2])
                            state = latex[2]
                        end
                        print(io, latex[3])
                    else
                        if '\0' != state
                            print(io, "<tab>")
                            state = '\0'
                        end
                        print(io, latex, "<tab>")
                    end
                else
                    if '\0' != state
                        print(io, "<tab>")
                        state = '\0'
                    end
                    print(io, c)
                end
            end
            '\0' != state && print(io, "<tab>")
        end
        println(io, '\n')
    end
end
repl_latex(s::String) = repl_latex(stdout, s)

macro repl(ex, brief::Bool=false, mod::Module=Main) repl(ex; brief, mod) end
macro repl(io, ex, brief, mod, internal_accesses) repl(io, ex; brief, mod, internal_accesses) end

function repl(io::IO, s::Symbol; brief::Bool=true, mod::Module=Main, internal_accesses::Union{Nothing, Set{Pair{Module,Symbol}}}=nothing)
    str = string(s)
    quote
        repl_latex($io, $str)
        repl_search($io, $str, $mod)
        $(if !isdefined(mod, s) && !haskey(keywords, s) && !Base.isoperator(s)
               :(repl_corrections($io, $str, $mod))
          end)
        $(_repl(s, brief, mod, internal_accesses))
    end
end
isregex(x) = isexpr(x, :macrocall, 3) && x.args[1] === Symbol("@r_str") && !isempty(x.args[3])

repl(io::IO, ex::Expr; brief::Bool=true, mod::Module=Main, internal_accesses::Union{Nothing, Set{Pair{Module,Symbol}}}=nothing) = isregex(ex) ? :(apropos($io, $ex)) : _repl(ex, brief, mod, internal_accesses)
repl(io::IO, str::AbstractString; brief::Bool=true, mod::Module=Main, internal_accesses::Union{Nothing, Set{Pair{Module,Symbol}}}=nothing) = :(apropos($io, $str))
repl(io::IO, other; brief::Bool=true, mod::Module=Main, internal_accesses::Union{Nothing, Set{Pair{Module,Symbol}}}=nothing) = esc(:(@doc $other)) # TODO: track internal_accesses
#repl(io::IO, other) = lookup_doc(other) # TODO

repl(x; brief::Bool=true, mod::Module=Main) = repl(stdout, x; brief, mod)

function _repl(x, brief::Bool=true, mod::Module=Main, internal_accesses::Union{Nothing, Set{Pair{Module,Symbol}}}=nothing)
    if isexpr(x, :call)
        x = x::Expr
        # determine the types of the values
        kwargs = nothing
        pargs = Any[]
        for arg in x.args[2:end]
            if isexpr(arg, :parameters)
                kwargs = mapany(arg.args) do kwarg
                    if kwarg isa Symbol
                        kwarg = :($kwarg::Any)
                    elseif isexpr(kwarg, :kw)
                        lhs = kwarg.args[1]
                        rhs = kwarg.args[2]
                        if lhs isa Symbol
                            if rhs isa Symbol
                                kwarg.args[1] = :($lhs::(@isdefined($rhs) ? typeof($rhs) : Any))
                            else
                                kwarg.args[1] = :($lhs::typeof($rhs))
                            end
                        end
                    end
                    kwarg
                end
            elseif isexpr(arg, :kw)
                if kwargs === nothing
                    kwargs = Any[]
                end
                lhs = arg.args[1]
                rhs = arg.args[2]
                if lhs isa Symbol
                    if rhs isa Symbol
                        arg.args[1] = :($lhs::(@isdefined($rhs) ? typeof($rhs) : Any))
                    else
                        arg.args[1] = :($lhs::typeof($rhs))
                    end
                end
                push!(kwargs, arg)
            else
                if arg isa Symbol
                    arg = :($arg::(@isdefined($arg) ? typeof($arg) : Any))
                elseif !isexpr(arg, :(::))
                    arg = :(::typeof($arg))
                end
                push!(pargs, arg)
            end
        end
        if kwargs === nothing
            x.args = Any[x.args[1], pargs...]
        else
            x.args = Any[x.args[1], Expr(:parameters, kwargs...), pargs...]
        end
    end
    #docs = lookup_doc(x) # TODO
    docs = esc(:(@doc $x))
    docs = if isfield(x)
        quote
            if isa($(esc(x.args[1])), DataType)
                fielddoc($(esc(x.args[1])), $(esc(x.args[2])))
            else
                $docs
            end
        end
    else
        docs
    end
    docs = log_nonpublic_access(macroexpand(mod, docs), mod, internal_accesses)
    :(REPL.trimdocs($docs, $brief))
end

"""
    fielddoc(binding, field)

Return documentation for a particular `field` of a type if it exists.
"""
function fielddoc(binding::Binding, field::Symbol)
    for mod in modules
        dict = meta(mod; autoinit=false)
        isnothing(dict) && continue
        multidoc = get(dict, binding, nothing)
        if multidoc !== nothing
            structdoc = get(multidoc.docs, Union{}, nothing)
            if structdoc !== nothing
                fieldsdoc = get(structdoc.data, :fields, nothing)
                if fieldsdoc !== nothing
                    fielddoc = get(fieldsdoc, field, nothing)
                    if fielddoc !== nothing
                        return isa(fielddoc, Markdown.MD) ?
                            fielddoc : Markdown.parse(fielddoc)
                    end
                end
            end
        end
    end
    fs = fieldnames(resolve(binding))
    fields = isempty(fs) ? "no fields" : (length(fs) == 1 ? "field " : "fields ") *
                                          join(("`$f`" for f in fs), ", ", ", and ")
    Markdown.parse("`$(resolve(binding))` has $fields.")
end

# As with the additional `doc` methods, this converts an object to a `Binding` first.
fielddoc(object, field::Symbol) = fielddoc(aliasof(object, typeof(object)), field)


# Search & Rescue
# Utilities for correcting user mistakes and (eventually)
# doing full documentation searches from the repl.

# Fuzzy Search Algorithm

function matchinds(needle, haystack; acronym::Bool = false)
    chars = collect(needle)
    is = Int[]
    lastc = '\0'
    for (i, char) in enumerate(haystack)
        while !isempty(chars) && isspace(first(chars))
            popfirst!(chars) # skip spaces
        end
        isempty(chars) && break
        if lowercase(char) == lowercase(chars[1]) &&
           (!acronym || !isletter(lastc))
            push!(is, i)
            popfirst!(chars)
        end
        lastc = char
    end
    return is
end

matchinds(needle, (; name)::AccessibleBinding; acronym::Bool=false) =
    matchinds(needle, name; acronym)

longer(x, y) = length(x) ≥ length(y) ? (x, true) : (y, false)

bestmatch(needle, haystack) =
    longer(matchinds(needle, haystack, acronym = true),
           matchinds(needle, haystack))

# Optimal string distance: Counts the minimum number of insertions, deletions,
# transpositions or substitutions to go from one string to the other.
function string_distance(a::AbstractString, lena::Integer, b::AbstractString, lenb::Integer)
    if lena > lenb
        a, b = b, a
        lena, lenb = lenb, lena
    end
    start = 0
    for (i, j) in zip(a, b)
        if a == b
            start += 1
        else
            break
        end
    end
    start == lena && return lenb - start
    vzero = collect(1:(lenb - start))
    vone = similar(vzero)
    prev_a, prev_b = first(a), first(b)
    current = 0
    for (i, ai) in enumerate(a)
        i > start || (prev_a = ai; continue)
        left = i - start - 1
        current = i - start
        transition_next = 0
        for (j, bj) in enumerate(b)
            j > start || (prev_b = bj; continue)
            # No need to look beyond window of lower right diagonal
            above = current
            this_transition = transition_next
            transition_next = vone[j - start]
            vone[j - start] = current = left
            left = vzero[j - start]
            if ai != bj
                # Minimum between substitution, deletion and insertion
                current = min(current + 1, above + 1, left + 1)
                if i > start + 1 && j > start + 1 && ai == prev_b && prev_a == bj
                    current = min(current, (this_transition += 1))
                end
            end
            vzero[j - start] = current
            prev_b = bj
        end
        prev_a = ai
    end
    current
end

function fuzzyscore(needle::AbstractString, haystack::AbstractString)
    lena, lenb = length(needle), length(haystack)
    1 - (string_distance(needle, lena, haystack, lenb) / max(lena, lenb))
end

function fuzzyscore(needle::AbstractString, haystack::AccessibleBinding)
    score = fuzzyscore(needle, haystack.name)
    haystack.source === nothing && return score
    # Apply a "penalty" of half an edit if the comparator binding is public but not
    # exported so that exported/local names that exactly match the search query are
    # listed first
    penalty = 1 / (2 * max(length(needle), length(haystack.name)))
    return max(score - penalty, 0)
end

function fuzzysort(search::String, candidates::Vector{AccessibleBinding})
    scores = map(cand -> fuzzyscore(search, cand), candidates)
    candidates[sortperm(scores)] |> reverse
end

# Levenshtein Distance

function levenshtein(s1, s2)
    a, b = collect(s1), collect(s2)
    m = length(a)
    n = length(b)
    d = Matrix{Int}(undef, m+1, n+1)

    d[1:m+1, 1] = 0:m
    d[1, 1:n+1] = 0:n

    for i = 1:m, j = 1:n
        d[i+1,j+1] = min(d[i  , j+1] + 1,
                         d[i+1, j  ] + 1,
                         d[i  , j  ] + (a[i] != b[j]))
    end

    return d[m+1, n+1]
end

function levsort(search::String, candidates::Vector{AccessibleBinding})
    scores = map(candidates) do cand
        (Float64(levenshtein(search, cand.name)), -fuzzyscore(search, cand))
    end
    candidates = candidates[sortperm(scores)]
    i = 0
    for outer i = 1:length(candidates)
        levenshtein(search, candidates[i].name) > 3 && break
    end
    return candidates[1:i]
end

# Result printing

function printmatch(io::IO, word, match)
    is, _ = bestmatch(word, match)
    for (i, char) = enumerate(match)
        if i in is
            printstyled(io, char, bold=true)
        else
            print(io, char)
        end
    end
end

function printmatch(io::IO, word, match::AccessibleBinding)
    match.source === nothing || print(io, match.source, '.')
    printmatch(io, word, match.name)
end

function matchlength(x::AccessibleBinding)
    n = length(x.name)
    if x.source !== nothing
        n += length(x.source) + 1  # the +1 is for the `.` separator
    end
    return n
end
matchlength(x) = length(x)

function printmatches(io::IO, word, matches; cols::Int = _displaysize(io)[2])
    total = 0
    for match in matches
        ml = matchlength(match)
        total + ml + 1 > cols && break
        fuzzyscore(word, match) < 0.5 && break
        print(io, " ")
        printmatch(io, word, match)
        total += ml + 1
    end
end

printmatches(args...; cols::Int = _displaysize(stdout)[2]) = printmatches(stdout, args..., cols = cols)

function print_joined_cols(io::IO, ss::Vector{AccessibleBinding}, delim = "", last = delim; cols::Int = _displaysize(io)[2])
    i = 0
    total = 0
    for outer i = 1:length(ss)
        total += matchlength(ss[i])
        total + max(i-2,0)*length(delim) + (i>1 ? 1 : 0)*length(last) > cols && (i-=1; break)
    end
    join(io, ss[1:i], delim, last)
end

print_joined_cols(args...; cols::Int = _displaysize(stdout)[2]) = print_joined_cols(stdout, args...; cols=cols)

function print_correction(io::IO, word::String, mod::Module)
    cors = map(quote_spaces, levsort(word, accessible(mod)))
    pre = "Perhaps you meant "
    print(io, pre)
    print_joined_cols(io, cors, ", ", " or "; cols = _displaysize(io)[2] - length(pre))
    println(io)
    return
end

# TODO: document where this is used
print_correction(word, mod::Module) = print_correction(stdout, word, mod)

# Completion data

moduleusings(mod) = ccall(:jl_module_usings, Any, (Any,), mod)

function accessible(mod::Module)
    bindings = Set(AccessibleBinding(s) for s in names(mod; all=true, imported=true)
                   if !isdeprecated(mod, s))
    for used in moduleusings(mod)
        union!(bindings, (AccessibleBinding(used, s) for s in names(used)
                          if !isdeprecated(used, s)))
    end
    union!(bindings, (AccessibleBinding(k) for k in keys(Base.Docs.keywords)))
    filter!(b -> !occursin('#', b.name), bindings)
    return collect(bindings)
end

function doc_completions(name, mod::Module=Main)
    res = fuzzysort(name, accessible(mod))

    # to insert an entry like `raw""` for `"@raw_str"` in `res`
    ms = map(c -> match(r"^@(.*?)_str$", c.name), res)
    idxs = findall(!isnothing, ms)

    # avoid messing up the order while inserting
    for i in reverse!(idxs)
        c = only((ms[i]::AbstractMatch).captures)
        insert!(res, i, AccessibleBinding(res[i].source, "$(c)\"\""))
    end
    res
end
doc_completions(name::Symbol) = doc_completions(string(name), mod)


# Searching and apropos

# Docsearch simply returns true or false if an object contains the given needle
docsearch(haystack::AbstractString, needle) = occursin(needle, haystack)
docsearch(haystack::Symbol, needle) = docsearch(string(haystack), needle)
docsearch(::Nothing, needle) = false
function docsearch(haystack::Array, needle)
    for elt in haystack
        docsearch(elt, needle) && return true
    end
    false
end
function docsearch(haystack, needle)
    @warn "Unable to search documentation of type $(typeof(haystack))" maxlog=1
    false
end

## Searching specific documentation objects
function docsearch(haystack::MultiDoc, needle)
    for v in values(haystack.docs)
        docsearch(v, needle) && return true
    end
    false
end

function docsearch(haystack::DocStr, needle)
    docsearch(parsedoc(haystack), needle) && return true
    if haskey(haystack.data, :fields)
        for doc in values(haystack.data[:fields])
            docsearch(doc, needle) && return true
        end
    end
    false
end

## doc search

## Markdown search simply strips all markup and searches plain text version
docsearch(haystack::Markdown.MD, needle) = docsearch(stripmd(haystack.content), needle)

"""
    stripmd(x)

Strip all Markdown markup from x, leaving the result in plain text. Used
internally by apropos to make docstrings containing more than one markdown
element searchable.
"""
stripmd(@nospecialize x) = string(x) # for random objects interpolated into the docstring
stripmd(x::AbstractString) = x  # base case
stripmd(x::Nothing) = " "
stripmd(x::Vector) = string(map(stripmd, x)...)

stripmd(x::Markdown.BlockQuote) = "$(stripmd(x.content))"
stripmd(x::Markdown.Admonition) = "$(stripmd(x.content))"
stripmd(x::Markdown.Bold) = "$(stripmd(x.text))"
stripmd(x::Markdown.Code) = "$(stripmd(x.code))"
stripmd(x::Markdown.Header) = stripmd(x.text)
stripmd(x::Markdown.HorizontalRule) = " "
stripmd(x::Markdown.Image) = "$(stripmd(x.alt)) $(x.url)"
stripmd(x::Markdown.Italic) = "$(stripmd(x.text))"
stripmd(x::Markdown.LaTeX) = "$(x.formula)"
stripmd(x::Markdown.LineBreak) = " "
stripmd(x::Markdown.Link) = "$(stripmd(x.text)) $(x.url)"
stripmd(x::Markdown.List) = join(map(stripmd, x.items), " ")
stripmd(x::Markdown.MD) = join(map(stripmd, x.content), " ")
stripmd(x::Markdown.Paragraph) = stripmd(x.content)
stripmd(x::Markdown.Footnote) = "$(stripmd(x.id)) $(stripmd(x.text))"
stripmd(x::Markdown.Table) =
    join([join(map(stripmd, r), " ") for r in x.rows], " ")

apropos(string) = apropos(stdout, string)
apropos(io::IO, string) = apropos(io, Regex("\\Q$string", "i"))

function apropos(io::IO, needle::Regex)
    for mod in modules
        # Module doc might be in README.md instead of the META dict
        docsearch(doc(mod), needle) && println(io, mod)
        dict = meta(mod; autoinit=false)
        isnothing(dict) && continue
        for (k, v) in dict
            docsearch(v, needle) && println(io, k)
        end
    end
end
