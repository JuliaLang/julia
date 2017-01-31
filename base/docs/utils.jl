# This file is a part of Julia. License is MIT: http://julialang.org/license

# Text / HTML objects

import Base: print, show, ==, hash

export HTML, @html_str

export HTML, Text, apropos

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
            show(io, MIME"text/html"(), x)
        end
    end
end

show(io::IO, ::MIME"text/html", h::HTML) = print(io, h.content)
show{F <: Function}(io::IO, ::MIME"text/html", h::HTML{F}) = h.content(io)

"""
    @html_str -> Docs.HTML

Create an `HTML` object from a literal string.
"""
macro html_str(s)
    :(HTML($s))
end

function catdoc(xs::HTML...)
    HTML() do io
        for x in xs
            show(io, MIME"text/html"(), x)
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
print{F <: Function}(io::IO, t::Text{F}) = t.content(io)
show(io::IO, t::Text) = print(io, t)

=={T<:Union{HTML, Text}}(t1::T, t2::T) = t1.content == t2.content
hash{T<:Union{HTML, Text}}(t::T, h::UInt) = hash(T, hash(t.content, h))

"""
    @text_str -> Docs.Text

Create a `Text` object from a literal string.
"""
macro text_str(s)
    :(Text($s))
end

function catdoc(xs::Text...)
    Text() do io
        for x in xs
            show(io, MIME"text/plain"(), x)
        end
    end
end

# REPL help

function helpmode(io::IO, line::AbstractString)
    line = strip(line)
    expr =
        if haskey(keywords, Symbol(line))
            # Docs for keywords must be treated separately since trying to parse a single
            # keyword such as `function` would throw a parse error due to the missing `end`.
            Symbol(line)
        else
            x = Base.syntax_deprecation_warnings(false) do
                parse(line, raise = false)
            end
            # Retrieving docs for macros requires us to make a distinction between the text
            # `@macroname` and `@macroname()`. These both parse the same, but are used by
            # the docsystem to return different results. The first returns all documentation
            # for `@macroname`, while the second returns *only* the docs for the 0-arg
            # definition if it exists.
            (isexpr(x, :macrocall, 1) && !endswith(line, "()")) ? quot(x) : x
        end
    # the following must call repl(io, expr) via the @repl macro
    # so that the resulting expressions are evaluated in the Base.Docs namespace
    :(Base.Docs.@repl $io $expr)
end
helpmode(line::AbstractString) = helpmode(STDOUT, line)

function repl_search(io::IO, s)
    pre = "search:"
    print(io, pre)
    printmatches(io, s, completions(s), cols = displaysize(io)[2] - length(pre))
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

# inverse of latex_symbols Dict, lazily created as needed
const symbols_latex = Dict{String,String}()
function symbol_latex(s::String)
    if isempty(symbols_latex)
        for (k,v) in Base.REPLCompletions.latex_symbols
            symbols_latex[v] = k
        end
    end
    return get(symbols_latex, s, "")
end
function repl_latex(io::IO, s::String)
    latex = symbol_latex(s)
    if !isempty(latex)
        print(io, "\"")
        Markdown.with_output_format(:cyan, io) do io
            print(io, s)
        end
        print(io, "\" can be typed by ")
        Markdown.with_output_format(:cyan, io) do io
            print(io, latex, "<tab>")
        end
        println(io, '\n')
    elseif any(c -> haskey(symbols_latex, string(c)), s)
        print(io, "\"")
        Markdown.with_output_format(:cyan, io) do io
            print(io, s)
        end
        print(io, "\" can be typed by ")
        Markdown.with_output_format(:cyan, io) do io
            for c in s
                cstr = string(c)
                if haskey(symbols_latex, cstr)
                    print(io, symbols_latex[cstr], "<tab>")
                else
                    print(io, c)
                end
            end
        end
        println(io, '\n')
    end
end
repl_latex(s::String) = repl_latex(STDOUT, s)

macro repl(ex) repl(ex) end
macro repl(io, ex) repl(io, ex) end

function repl(io::IO, s::Symbol)
    str = string(s)
    quote
        repl_latex($io, $str)
        repl_search($io, $str)
        ($(isdefined(s) || haskey(keywords, s))) || repl_corrections($io, $str)
        $(_repl(s))
    end
end
isregex(x) = isexpr(x, :macrocall, 2) && x.args[1] === Symbol("@r_str") && !isempty(x.args[2])
repl(io::IO, ex::Expr) = isregex(ex) ? :(apropos($io, $ex)) : _repl(ex)
repl(io::IO, str::AbstractString) = :(apropos($io, $str))
repl(io::IO, other) = :(@doc $(esc(other)))

repl(x) = repl(STDOUT, x)

function _repl(x)
    if (isexpr(x, :call) && !any(isexpr(x, :(::)) for x in x.args))
        x.args[2:end] = [:(::typeof($arg)) for arg in x.args[2:end]]
    end
    docs = :(@doc $(esc(x)))
    if isfield(x)
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

# Levenshtein Distance

function levenshtein(s1, s2)
    a, b = collect(s1), collect(s2)
    m = length(a)
    n = length(b)
    d = Array{Int}(m+1, n+1)

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

function printmatches(io::IO, word, matches; cols = displaysize(io)[2])
    total = 0
    for match in matches
        total + length(match) + 1 > cols && break
        fuzzyscore(word, match) < 0 && break
        print(io, " ")
        printmatch(io, word, match)
        total += length(match) + 1
    end
end

printmatches(args...; cols = displaysize(STDOUT)[2]) = printmatches(STDOUT, args..., cols = cols)

function print_joined_cols(io::IO, ss, delim = "", last = delim; cols = displaysize(io)[2])
    i = 0
    total = 0
    for i = 1:length(ss)
        total += length(ss[i])
        total + max(i-2,0)*length(delim) + (i>1?1:0)*length(last) > cols && (i-=1; break)
    end
    join(io, ss[1:i], delim, last)
end

print_joined_cols(args...; cols = displaysize(STDOUT)[2]) = print_joined_cols(STDOUT, args...; cols=cols)

function print_correction(io, word)
    cors = levsort(word, accessible(current_module()))
    pre = "Perhaps you meant "
    print(io, pre)
    print_joined_cols(io, cors, ", ", " or "; cols = displaysize(io)[2] - length(pre))
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
    [filter!(s->Base.isdeprecated(mod, s), names(mod, true, true));
     map(names, moduleusings(mod))...;
     builtins] |> unique |> filtervalid

completions(name) = fuzzysort(name, accessible(current_module()))
completions(name::Symbol) = completions(string(name))


# Searching and apropos

# Docsearch simply returns true or false if an object contains the given needle
docsearch(haystack::AbstractString, needle) = !isempty(search(haystack, needle))
docsearch(haystack::Symbol, needle) = docsearch(string(haystack), needle)
docsearch(::Void, needle) = false
function docsearch(haystack::Array, needle)
    for elt in haystack
        docsearch(elt, needle) && return true
    end
    false
end
function docsearch(haystack, needle)
    Base.warn_once("unable to search documentation of type $(typeof(haystack))")
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

## Markdown search simply strips all markup and searches plain text version
docsearch(haystack::Markdown.MD, needle) =
    docsearch(stripmd(haystack.content), needle)

"""
    stripmd(x)

Strip all Markdown markup from x, leaving the result in plain text. Used
internally by apropos to make docstrings containing more than one markdown
element searchable.
"""
stripmd(x::AbstractString) = x  # base case
stripmd(x::Void) = " "
stripmd(x::Vector) = string(map(stripmd, x)...)
stripmd(x::Markdown.BlockQuote) = "$(stripmd(x.content))"
stripmd(x::Markdown.Admonition) = "$(stripmd(x.content))"
stripmd(x::Markdown.Bold) = "$(stripmd(x.text))"
stripmd(x::Markdown.Code) = "$(stripmd(x.code))"
stripmd{N}(x::Markdown.Header{N}) = stripmd(x.text)
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

# Apropos searches through all available documentation for some string or regex
"""
    apropos(string)

Search through all documentation for a string, ignoring case.
"""
apropos(string) = apropos(STDOUT, string)
apropos(io::IO, string) = apropos(io, Regex("\\Q$string", "i"))
function apropos(io::IO, needle::Regex)
    for mod in modules
        # Module doc might be in README.md instead of the META dict
        docsearch(doc(mod), needle) && println(io, mod)
        for (k, v) in meta(mod)
            docsearch(v, needle) && println(io, k)
        end
    end
end
