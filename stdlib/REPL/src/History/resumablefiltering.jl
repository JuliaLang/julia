# This file is a part of Julia. License is MIT: https://julialang.org/license

struct ConditionSet{S}
    words::Vector{SubString{S}}
    exacts::Vector{SubString{S}}
    negatives::Vector{SubString{S}}
    initialisms::Vector{SubString{S}}
    fuzzy::Vector{SubString{S}}
    regexps::Vector{SubString{S}}
    modes::Vector{SubString{S}}
end

ConditionSet{S}() where {S} = ConditionSet{S}([], [], [], [], [], [], [])

"""
    FILTER_SEPARATOR

Character used to separate multiple search conditions in a single query.
"""
const FILTER_SEPARATOR = ';'

"""
    FILTER_PREFIXES

List of single-character prefixes that set search modes.
"""
const FILTER_PREFIXES = ('!', '`', '=', '/', '~', '>')

"""
    FILTER_SHORTHELP_QUERY

The special single-character query that triggers display of `FILTER_SHORTHELP`.
"""
const FILTER_SHORTHELP_QUERY = "?"

"""
    FILTER_LONGHELP_QUERY

The special query that triggers display of `FILTER_LONGHELP`.
"""
const FILTER_LONGHELP_QUERY = "??"

"""
    FILTER_SHORTHELP

Annotated help text displayed when the user enters the help query (`$FILTER_SHORTHELP_QUERY`).
"""
const FILTER_SHORTHELP = S"""
 {bold,magenta:Interactive history search}

 Enter a search term at the prompt, and see matching candidates.
 A search term that is {italic:just} '{REPL_History_search_prefix:?}' brings up this help page.

 See more information on behaviour and keybindings with '{REPL_History_search_prefix:??}'.

 Different search modes are available via prefixes, as follows:
 {emphasis:•} {REPL_History_search_prefix:=} looks for exact matches
 {emphasis:•} {REPL_History_search_prefix:!} {italic:excludes} exact matches
 {emphasis:•} {REPL_History_search_prefix:/} performs a regexp search
 {emphasis:•} {REPL_History_search_prefix:~} looks for fuzzy matches
 {emphasis:•} {REPL_History_search_prefix:>} looks for a particular REPL mode
 {emphasis:•} {REPL_History_search_prefix:`} looks for an initialism (text with matching initials)

 You can also apply multiple restrictions with the separator '{REPL_History_search_separator:$FILTER_SEPARATOR}'.

 For example, {region:{REPL_History_search_prefix:/}^foo{REPL_History_search_separator:$FILTER_SEPARATOR}\
{REPL_History_search_prefix:`}bar{REPL_History_search_separator:$FILTER_SEPARATOR}\
{REPL_History_search_prefix:>}shell} will look for history entries that start with "{code:foo}",
 contains "{code:b... a... r...}", {italic:and} is a shell history entry.
"""

const FILTER_LONGHELP = S"""
 {bold,magenta:Interactive history search — behaviour and keybindings}

 Search your REPL history interactively by constructing filters.

 With no mode specified (see the basic help with '{REPL_History_search_prefix:?}'), entries are matched
 if they contain all of the words in the search string, in any order.

 If the entire search string is lowercase, the search is case-insensitive.

 If you want to include the filter separator '{REPL_History_search_separator:$FILTER_SEPARATOR}' in a query, or start
 a words filter with a prefix character, you may escape it with a backslash (e.g. {code:\\;}).

 Search results can be navigated with:
 {emphasis:•} {code:↑}, {code:Ctrl+P}, or {code:Ctrl+K} to move up
 {emphasis:•} {code:↓}, {code:Ctrl+N}, or {code:Ctrl+J} to move down
 {emphasis:•} {code:PageUp} or {code:Ctrl+B} to page up
 {emphasis:•} {code:PageDown} or {code:Ctrl+F} to page down
 {emphasis:•} {code:Alt+<} to jump to the first result
 {emphasis:•} {code:Alt+>} to jump to the last result

 Multiple search results can be selected with {code:Tab} and confirmed with {code:Enter}.
 You may use {code:Ctrl+S} to save selected entries to a file or the clipboard.

 To abort the search, use {code:Ctrl+C}, {code:Ctrl+D}, {code:Ctrl+G}, or {code:Esc Esc}.
"""

"""
    ConditionSet(spec::AbstractString) -> ConditionSet

Parse the raw search string `spec` into a `ConditionSet`.

Parsing is performed by splitting on unescaped `FILTER_SEPARATOR` and
dispatching each segment according to its leading prefix character.
"""
function ConditionSet(spec::S) where {S <: AbstractString}
    function addcond!(condset::ConditionSet, cond::SubString)
        isempty(cond) && return
        kind = first(cond)
        if kind ∈ ('!', '=', '`', '/', '>', '~')
            value = @view cond[2:end]
            if kind ∈ ('`', '>', '~')
                value = strip(value)
            elseif !all(isspace, value)
                value = if kind == '/'
                    rstrip(value)
                else # kind ∈ ('!', '=')
                    strip(value)
                end
            end
            isempty(value) && return
            if startswith(cond, '!')
                push!(condset.negatives, value)
            elseif startswith(cond, '=')
                push!(condset.exacts, value)
            elseif startswith(cond, '`')
                push!(condset.initialisms, value)
            elseif startswith(cond, '/')
                push!(condset.regexps, value)
            elseif startswith(cond, '>')
                push!(condset.modes, SubString(lowercase(value)))
            elseif startswith(cond, '~')
                push!(condset.fuzzy, value)
            end
        else
            if startswith(cond, '\\') && !(length(cond) > 1 && cond[2] == '\\')
                cond = @view cond[2:end]
            end
            push!(condset.words, strip(cond))
        end
        nothing
    end
    cset = ConditionSet{S}()
    pos = firstindex(spec)
    mark = pos
    lastind = lastindex(spec)
    escaped = false
    dropbytes = Int[]
    while pos <= lastind
        chr = spec[pos]
        if escaped
            chr == FILTER_SEPARATOR && push!(dropbytes, pos - mark)
            escaped = false
        elseif chr == '\\'
            escaped = true
        elseif chr == FILTER_SEPARATOR
            str = SubString(spec, mark:pos - 1)
            if !isempty(dropbytes)
                str = SubString(convert(S, String(deleteat!(collect(codeunits(str)), dropbytes))))
                empty!(dropbytes)
            end
            addcond!(cset, lstrip(str))
            mark = pos + 1
        end
        pos = nextind(spec, pos)
    end
    if mark <= lastind
        str = SubString(spec, mark:pos - 1)
        if !isempty(dropbytes)
            str = SubString(convert(S, String(deleteat!(collect(codeunits(str)), dropbytes))))
        end
        addcond!(cset, lstrip(SubString(spec, mark:lastind)))
    end
    cset
end

"""
    ismorestrict(a::ConditionSet, b::ConditionSet) -> Bool

Whether `a` is at least as restrictive as `b`, across all conditions.
"""
function ismorestrict(a::ConditionSet, b::ConditionSet)
    length(a.fuzzy) == length(b.fuzzy) &&
        all(splat(==), zip(a.fuzzy, b.fuzzy)) || return false
    length(a.regexps) == length(b.regexps) &&
        all(splat(==), zip(a.regexps, b.regexps)) || return false
    length(a.modes) == length(b.modes) &&
        all(splat(==), zip(a.modes, b.modes)) || return false
    length(a.exacts) >= length(b.exacts) &&
        all(splat(occursin), zip(b.exacts, a.exacts)) || return false
    length(a.words) >= length(b.words) &&
        all(splat(occursin), zip(b.words, a.words)) || return false
    length(a.negatives) >= length(b.negatives) &&
        all(splat(occursin), zip(a.negatives, b.negatives)) || return false
    length(a.initialisms) >= length(b.initialisms) &&
        all(splat(occursin), zip(b.initialisms, a.initialisms)) || return false
    true
end

struct FilterSpec
    exacts::Vector{String}
    negatives::Vector{String}
    regexps::Vector{Regex}
    modes::Vector{Symbol}
end

FilterSpec() = FilterSpec([], [], [], [])

function FilterSpec(cset::ConditionSet)
    spec = FilterSpec([], [], [], [])
    for term in cset.exacts
        push!(spec.exacts, String(term))
    end
    for words in cset.words
        casesensitive = any(isuppercase, words)
        for word in eachsplit(words)
            if casesensitive
                push!(spec.exacts, String(word))
            else
                push!(spec.regexps, Regex(string("\\Q", word, "\\E"), "i"))
            end
        end
    end
    for term in cset.negatives
        push!(spec.negatives, String(term))
    end
    for rx in cset.regexps
        try
            push!(spec.regexps, Regex(rx))
        catch _
            # Regex error, skip
        end
    end
    for itlsm in cset.initialisms
        rx = Regex(join((string("(?:(?:\\b|_+)(?:\\Q", ltr, "\\E|\\Q", uppercase(ltr),
                                "\\E)\\w+|\\p{Ll}\\Q", uppercase(ltr), "\\E)")
                         for ltr in itlsm), "[\\W_]*?"))
        push!(spec.regexps, rx)
    end
    for fuzz in cset.fuzzy
        for word in eachsplit(fuzz)
            rx = Regex(join((string("\\Q", ltr, "\\E") for ltr in word), "[^\\s\"#%&()*+,\\-\\/:;<=>?@[\\]^`{|}~]*?"),
                       ifelse(any(isuppercase, fuzz), "", "i"))
            push!(spec.regexps, rx)
        end
    end
    for mode in cset.modes
        push!(spec.modes, Symbol(mode))
    end
    spec
end


"""
    filterchunkrev!(out, candidates, spec, seen, idx; maxtime, maxresults) -> Int

Incrementally filter `candidates[1:idx]` in reverse order.

Pushes matches onto `out` until either `maxtime` is exceeded or `maxresults`
collected, then returns the new resume index. Only unique entries (by mode and content)
are added to avoid showing duplicate history items.
"""
function filterchunkrev!(out::Vector{HistEntry}, candidates::DenseVector{HistEntry},
                         spec::FilterSpec, seen::Set{Tuple{Symbol,String}}, idx::Int = length(candidates);
                         maxtime::Float64 = Inf, maxresults::Int = length(candidates))
    batchsize = clamp(length(candidates) ÷ 512, 10, 1000)
    for batch in Iterators.partition(idx:-1:1, batchsize)
        time() > maxtime && break
        for outer idx in batch
            entry = candidates[idx]
            if (entry.mode, entry.content) ∈ seen
                continue
            end
            if !isempty(spec.modes)
                entry.mode ∈ spec.modes || continue
            end
            matchfail = false
            for text in spec.exacts
                if !occursin(text, entry.content)
                    matchfail = true
                    break
                end
            end
            matchfail && continue
            for text in spec.negatives
                if occursin(text, entry.content)
                    matchfail = true
                    break
                end
            end
            matchfail && continue
            for rx in spec.regexps
                if !occursin(rx, entry.content)
                    matchfail = true
                    break
                end
            end
            matchfail && continue
            push!(seen, (entry.mode, entry.content))
            pushfirst!(out, entry)
            length(out) == maxresults && break
        end
    end
    max(0, idx - 1)
end

"""
    matchregions(spec::FilterSpec, candidate::AbstractString) -> Vector{UnitRange{Int}}

Find all matching character ranges in `candidate` for `spec`.

Collects exact-substring and regex matches, then returns them
sorted by start index (and longer matches first).
"""
function matchregions(spec::FilterSpec, candidate::AbstractString)
    matches = UnitRange{Int}[]
    for text in spec.exacts
        append!(matches, findall(text, candidate))
    end
    for rx in spec.regexps
        for (; match) in eachmatch(rx, candidate)
            push!(matches, 1+match.offset:thisind(candidate, match.offset + match.ncodeunits))
        end
    end
    sort!(matches, by = m -> (first(m), -last(m)))
    # Combine adjacent matches separated by a single space
    for (i, match) in enumerate(matches)
        i == length(matches) && break
        nextmatch = matches[i + 1]
        if last(match) + 1 == first(nextmatch) - 1 && candidate[last(match)+1] == ' '
            matches[i] = first(match):last(nextmatch)
            matches[i+1] = last(nextmatch)+1:last(nextmatch)
        end
    end
    filter!(!isempty, matches)
end
