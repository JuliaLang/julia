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
    FILTER_HELP_QUERY

The special single-character query that triggers display of `FILTER_HELPSTRING`.
"""
const FILTER_HELP_QUERY = "?"

"""
    FILTER_HELPSTRING

Annotated help text displayed when the user enters the help query (`$FILTER_HELP_QUERY`).
"""
const FILTER_HELPSTRING = S"""
 {bold,magenta:Interactive history search}

 Enter a search term at the prompt, and see matching candidates.
 A search term that is {italic:just} '{REPL_history_search_prefix:?}' brings up this help page.

 Different search modes are available via prefixes, as follows:
 {emphasis:•} {REPL_history_search_prefix:=} looks for exact matches
 {emphasis:•} {REPL_history_search_prefix:!} {italic:excludes} exact matches
 {emphasis:•} {REPL_history_search_prefix:/} performs a regexp search
 {emphasis:•} {REPL_history_search_prefix:~} looks for fuzzy matches
 {emphasis:•} {REPL_history_search_prefix:>} looks for a particular REPL mode
 {emphasis:•} {REPL_history_search_prefix:`} looks for an initialism

 You can also apply multiple restrictions with the \
separator '{REPL_history_search_separator:$FILTER_SEPARATOR}',
 for example, {region:{REPL_history_search_prefix:^}foo{REPL_history_search_separator:$FILTER_SEPARATOR}\
{REPL_history_search_prefix:`}bar{REPL_history_search_separator:$FILTER_SEPARATOR}\
{REPL_history_search_prefix:>}shell} will look for history entries that start with "{code:foo}",
 contains "{code:b... a... r...}", {italic:and} is a shell history entry.

 A literal '{code:$FILTER_SEPARATOR}' (or any other character) can be escaped by being prefixed with a backslash, as {code:\\;}.
"""

"""
    ConditionSet(spec::AbstractString) -> ConditionSet

Parse the raw search string `spec` into a `ConditionSet`.

Parsing is performed by splitting on unescaped `FILTER_SEPARATOR` and
dispatching each segment according to its leading prefix character.
"""
function ConditionSet(spec::S) where {S <: AbstractString}
    function addcond!(condset::ConditionSet, cond::SubString)
        if startswith(cond, '!')
            push!(condset.negatives, strip(@view cond[2:end]))
        elseif startswith(cond, '=')
            push!(condset.exacts, lstrip(@view cond[2:end]))
        elseif startswith(cond, '`')
            push!(condset.initialisms, strip(@view cond[2:end]))
        elseif startswith(cond, '/')
            push!(condset.regexps, @view cond[2:end])
        elseif startswith(cond, '>')
            push!(condset.modes, strip(@view cond[2:end]))
        elseif startswith(cond, '~')
            push!(condset.fuzzy, strip(@view cond[2:end]))
        else
            if startswith(cond, '\\') && !(length(cond) > 1 && cond[2] == '\\')
                cond = @view cond[2:end]
            end
            push!(condset.words, cond)
        end
    end
    cset = ConditionSet{S}()
    pos = firstindex(spec)
    mark = pos
    lastind = lastindex(spec)
    escaped = false
    while pos <= lastind
        chr = spec[pos]
        if escaped
        elseif chr == '\\'
            escaped = true
        elseif chr == FILTER_SEPARATOR
            addcond!(cset, lstrip(SubString(spec, mark:pos - 1)))
            mark = pos + 1
        end
        pos = nextind(spec, pos)
    end
    if mark <= lastind
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
    for words in cset.words, word in eachsplit(words)
        if any(isuppercase, word)
            push!(spec.exacts, String(word))
        else
            push!(spec.regexps, Regex(string("\\Q", word, "\\E"), "i"))
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
        rx = Regex(join((string("(?:\\b(?:\\Q", ltr, "\\E|\\Q", uppercase(ltr),
                                "\\E)\\w+|\\p{Ll}\\Q", uppercase(ltr), "\\E)")
                         for ltr in itlsm), "\\W*?"))
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
    filterchunkrev!(out, candidates, spec; idx, maxtime, maxresults) -> Int

Incrementally filter `candidates[1:idx]` in reverse order.

Pushes matches onto `out` until either `maxtime` is exceeded or `maxresults`
collected, then returns the new resume index.
"""
function filterchunkrev!(out::Vector{HistEntry}, candidates::DenseVector{HistEntry},
                         spec::FilterSpec, idx::Int = length(candidates);
                         maxtime::Float64 = Inf, maxresults::Int = length(candidates))
    batchsize = clamp(length(candidates) ÷ 512, 10, 1000)
    for batch in Iterators.partition(idx:-1:1, batchsize)
        time() > maxtime && break
        for outer idx in batch
            entry = candidates[idx]
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
    filter!(!isempty, matches)
    sort!(matches, by = m -> (first(m), -last(m)))
end
