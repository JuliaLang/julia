# This file is a part of Julia. License is MIT: http://julialang.org/license

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

"Create a `Text` object from a literal string."
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
        # Fuzzy Searching
        $(isexpr(ex, Symbol)) && repl_search($(string(ex)))
        if $(isa(ex, Symbol)) &&
                !(isdefined($(current_module()), $(Expr(:quote, ex))) ||
                  haskey(keywords, $(Expr(:quote, ex))))
            repl_corrections($(string(ex)))
        else
            if $(isfield(ex) ? :(isa($(esc(ex.args[1])), DataType)) : false)
                $(isfield(ex) ? :(fielddoc($(esc(ex.args[1])), $(ex.args[2]))) : nothing)
            else
                $((isa(ex, Symbol) || isfield(ex)) ? :($doc($ex)) : Base.gen_call_with_extracted_types(doc, ex))
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

# Levenshtein Distance

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
