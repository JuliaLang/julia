# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    runsearch() -> (; mode::Union{Symbol, Nothing}, text::String, index::Int)

Launch the interactive REPL history search interface.

Spawns prompt and display tasks, waits for user confirm or abort,
and returns the final selection (if any).
"""
function runsearch(histfile::HistoryFile, term, prefix::String = "\e[90m")
    update!(histfile)
    events = Channel{Symbol}(Inf)
    pspec = create_prompt(events, term, prefix)
    ptask = @spawn runprompt!(pspec, events)
    dtask = @spawn run_display!(pspec, events, histfile.records)
    wait(ptask)
    fullselection(fetch(dtask))
end

"""
    fullselection(state::SelectorState) -> (; mode::Symbol, text::String, index::Int)

Gather all selected and hovered entries and return them as joined text.
The `index` field is the index of the latest (most recent) selected entry,
suitable for setting `hist.cur_idx` after search.
"""
function fullselection(state::SelectorState)
    text = IOBuffer()
    entries = copy(state.selection.gathered)
    for act in state.selection.active
        push!(entries, state.candidates[act])
    end
    if isempty(entries) && state.hover ∈ axes(state.candidates, 1)
        push!(entries, state.candidates[end-state.hover+1])
    end
    sort!(entries, by = e -> e.index)
    mainmode = if !isempty(entries) first(entries).mode end
    mainindex = if !isempty(entries) Int(last(entries).index) else 0 end
    join(text, Iterators.map(e -> e.content, entries), '\n')
    (mode = mainmode, text = String(take!(text)), index = mainindex)
end

"""
    run_display!((; term,pstate), events::Channel{Symbol}, hist::Vector{HistEntry})

Drive the display event loop until confirm or abort.

Listens for navigation, edits, save, and abort events, re-filters history
incrementally, and re-renders via `redisplay_all`.
"""
function run_display!((; term, pstate), events::Channel{Symbol}, hist::Vector{HistEntry})
    # Output-related variables
    out = term.out_stream
    outsize = displaysize(out)
    buf = IOContext(IOBuffer(), out)
    # Main state variables
    state = SelectorState(outsize, "", FilterSpec(), hist)
    # Filter state
    filter_idx = 0
    filter_seen = Dict{Tuple{Symbol,String}, Int}()
    # Precompute duplicate counts from the full history (independent of query)
    hist_dupcounts = Dict{Tuple{Symbol,String}, Int}()
    for entry in hist
        key = (entry.mode, entry.content)
        hist_dupcounts[key] = get(hist_dupcounts, key, 0) + 1
    end
    # Expand/collapse state for duplicate entries
    expanded_key::Union{Nothing, Tuple{Symbol,String}} = nothing
    expanded_entries = HistEntry[]
    expanded_hover = 0
    # Active duplicate counts for display (nothing when no filter, hist_dupcounts when filtering)
    active_dupcounts::Union{Nothing, Dict{Tuple{Symbol,String}, Int}} = nothing
    # Candidate cache
    cands_cache = Pair{ConditionSet{String}, Vector{HistEntry}}[]
    cands_cachestate = zero(UInt8)
    cands_current = HistEntry[]
    cands_cond = ConditionSet{String}()
    cands_temp = HistEntry[]
    redisplay_all(out, EMPTY_STATE, state, pstate; buf)
    # Event loop
    while true
        event = @lock events if !isempty(events) take!(events) end
        if isnothing(event)
        elseif event === :abort
            print(out, "\e[1G\e[J")
            return EMPTY_STATE
        elseif event === :confirm
            print(out, "\e[1G\e[J")
            if expanded_hover > 0 && expanded_hover <= length(expanded_entries)
                entry = expanded_entries[expanded_hover]
                return SelectorState(state.area, state.query, state.filter,
                    [entry], 0, (active = Int[], gathered = HistEntry[]), 1)
            end
            return state
        elseif event === :clear
            print(out, "\e[H\e[2J")
            redisplay_all(out, EMPTY_STATE, state, pstate; buf,
                          dupcounts=active_dupcounts, expanded_key, expanded_entries, expanded_hover)
            continue
        elseif event === :redraw
            print(out, "\e[1G\e[J")
            redisplay_all(out, EMPTY_STATE, state, pstate; buf,
                          dupcounts=active_dupcounts, expanded_key, expanded_entries, expanded_hover)
            continue
        elseif event ∈ (:up, :down, :pageup, :pagedown)
            # Handle navigation within expanded entries
            if !isnothing(expanded_key) && expanded_hover > 0
                if event ∈ (:up, :pageup)
                    expanded_hover -= 1
                    # expanded_hover=0 returns to the main entry
                elseif expanded_hover < length(expanded_entries)
                    expanded_hover += 1
                else
                    # Past the last expanded entry, exit and move down
                    expanded_hover = 0
                    prevstate, state = state, movehover(state, false, event === :pagedown)
                end
                redisplay_all(out, EMPTY_STATE, state, pstate; buf,
                              dupcounts=active_dupcounts, expanded_key, expanded_entries, expanded_hover)
                continue
            end
            # Check if we should enter expanded entries (on main entry, pressing down)
            if !isnothing(expanded_key) && event ∈ (:down, :pagedown) && !isempty(expanded_entries)
                hov = gethover(state)
                if !isnothing(hov) && (hov.mode, hov.content) == expanded_key
                    expanded_hover = 1
                    redisplay_all(out, EMPTY_STATE, state, pstate; buf,
                                  dupcounts=active_dupcounts, expanded_key, expanded_entries, expanded_hover)
                    continue
                end
            end
            # Normal navigation
            prevstate, state = state, movehover(state, event ∈ (:up, :pageup), event ∈ (:pageup, :pagedown))
            @lock events begin
                nextevent = if !isempty(events) first(events.data) end
                while nextevent ∈ (:up, :down, :pageup, :pagedown)
                    take!(events)
                    state = movehover(state, nextevent ∈ (:up, :pageup), event ∈ (:pageup, :pagedown))
                    nextevent = if !isempty(events) first(events.data) end
                end
            end
            redisplay_all(out, prevstate, state, pstate; buf,
                          dupcounts=active_dupcounts, expanded_key, expanded_entries, expanded_hover)
            continue
        elseif event === :jumpfirst
            prevstate = state
            state = SelectorState(
                state.area, state.query, state.filter, state.candidates,
                length(state.candidates) - componentrows(state).candidates,
                state.selection, length(state.candidates))
            redisplay_all(out, prevstate, state, pstate; buf,
                          dupcounts=active_dupcounts, expanded_key, expanded_entries, expanded_hover)
            continue
        elseif event === :jumplast
            prevstate = state
            state = SelectorState(
                state.area, state.query, state.filter, state.candidates,
                0, state.selection, 1)
            redisplay_all(out, prevstate, state, pstate; buf,
                          dupcounts=active_dupcounts, expanded_key, expanded_entries, expanded_hover)
            continue
        elseif event === :tab
            prevstate, state = state, toggleselection(state)
            redisplay_all(out, prevstate, state, pstate; buf,
                          dupcounts=active_dupcounts, expanded_key, expanded_entries, expanded_hover)
            continue
        elseif event === :expand
            # Only expand when dedup is active (non-empty query with filter)
            if isnothing(active_dupcounts) || filter_idx != 0
                continue
            end
            hovered = gethover(state)
            if isnothing(hovered) || !isempty(state.selection.gathered)
                continue
            end
            ekey = (hovered.mode, hovered.content)
            dc = get(hist_dupcounts, ekey, 1)
            if dc <= 1
                continue
            end
            if expanded_key == ekey
                # Collapse: already expanded, toggle off
                expanded_key = nothing
                empty!(expanded_entries)
            else
                # Expand: find all instances in hist, excluding the most recent
                expanded_key = ekey
                empty!(expanded_entries)
                for entry in hist
                    if entry.mode == ekey[1] && entry.content == ekey[2]
                        push!(expanded_entries, entry)
                    end
                end
                if !isempty(expanded_entries)
                    _, max_idx = findmax(e -> e.index, expanded_entries)
                    deleteat!(expanded_entries, max_idx)
                end
            end
            expanded_hover = 0
            redisplay_all(out, EMPTY_STATE, state, pstate; buf,
                          dupcounts=active_dupcounts, expanded_key, expanded_entries, expanded_hover)
            continue
        elseif event === :edit
            @lock events begin
                while !isempty(events) && first(events.data) === :edit
                    take!(events)
                end
            end
            query = REPL.LineEdit.input_string(pstate)
            if query === state.query
                redisplay_all(out, state, state, pstate; buf,
                              dupcounts=active_dupcounts, expanded_key, expanded_entries, expanded_hover)
                continue
            end
            # Reset expanded state on query change
            expanded_key = nothing
            empty!(expanded_entries)
            expanded_hover = 0
            # Determine the conditions/filter spec
            cands_cond = ConditionSet(query)
            filter_spec = FilterSpec(cands_cond)
            # Construct a provisional new state
            prevstate, state = state, SelectorState(
                outsize, query, filter_spec, HistEntry[], state.selection.gathered)
            # Gather selected candidates
            if !isempty(prevstate.selection.active)
                for act in prevstate.selection.active
                    push!(state.selection.gathered, prevstate.candidates[act])
                end
                sort!(state.selection.gathered, by = e -> e.index)
                state = SelectorState(
                    state.area, state.query, state.filter, state.candidates,
                    -min(length(state.selection.gathered), state.area.height ÷ 8),
                    state.selection, 1)
            end
            # Show help?
            if query ∈ (FILTER_SHORTHELP_QUERY,FILTER_LONGHELP_QUERY)
                redisplay_all(out, prevstate, state, pstate; buf,
                          dupcounts=active_dupcounts, expanded_key, expanded_entries, expanded_hover)
                continue
            end
            # Parse the conditions and find a good candidate list
            cands_current = hist
            for (cond, cands) in Iterators.reverse(cands_cache)
                if ismorestrict(cands_cond, cond)
                    cands_current = cands
                    break
                end
            end
            # Start filtering candidates
            # Only deduplicate when user has entered a search query with conditions.
            # When browsing with no filter (empty query), show all history including duplicates.
            if isempty(filter_spec.exacts) && isempty(filter_spec.negatives) &&
               isempty(filter_spec.regexps) && isempty(filter_spec.modes)
                # No filtering needed, just copy all candidates
                append!(state.candidates, cands_current)
                filter_idx = 0
                active_dupcounts = nothing
            else
                # Filtering needed, deduplicate results and show ×N badges
                active_dupcounts = hist_dupcounts
                empty!(filter_seen)
                filter_idx = filterchunkrev!(
                    state, cands_current, filter_seen;
                    maxtime = time() + 0.01,
                    maxresults = outsize[1])
            end
            if filter_idx == 0
                cands_cachestate = addcache!(
                    cands_cache, cands_cachestate, cands_cond => state.candidates)
            end
            redisplay_all(out, prevstate, state, pstate; buf,
                          dupcounts=active_dupcounts, expanded_key, expanded_entries, expanded_hover)
            continue
        elseif event === :copy
            content = strip(fullselection(state).text)
            isempty(content) || saveclipboard(term.out_stream, content)
            return EMPTY_STATE
        elseif event === :filesave
            content = strip(fullselection(state).text)
            isempty(content) || savefile(term, content)
            return EMPTY_STATE
        else
            error("Unknown event: $event")
        end
        if displaysize(out) != outsize
            outsize = displaysize(out)
            prevstate, state = state, SelectorState(
                outsize, state.query, state.filter, state.candidates,
                state.scroll, state.selection, state.hover)
            redisplay_all(out, prevstate, state, pstate; buf,
                          dupcounts=active_dupcounts, expanded_key, expanded_entries, expanded_hover)
        elseif filter_idx != 0
            append!(empty!(cands_temp), state.candidates)
            prevstate = SelectorState(
                state.area, state.query, state.filter, cands_temp,
                state.scroll, state.selection, state.hover)
            filter_idx = filterchunkrev!(
                state, cands_current, filter_seen, filter_idx;
                maxtime = time() + 0.01)
            if filter_idx == 0
                cands_cachestate = addcache!(
                    cands_cache, cands_cachestate, cands_cond => state.candidates)
            end
            # If there are now new candidates in the view, update
            length(state.candidates) != length(prevstate.candidates) &&
                length(prevstate.candidates) - state.hover < outsize[1] &&
                redisplay_all(out, prevstate, state, pstate; buf,
                          dupcounts=active_dupcounts, expanded_key, expanded_entries, expanded_hover)
        elseif isnothing(event)
            yield()
            sleep(0.01)
        end
    end
end

function filterchunkrev!(state::SelectorState, candidates::DenseVector{HistEntry},
                         seen::Dict{Tuple{Symbol,String}, Int}, idx::Int = length(candidates);
                         maxtime::Float64 = Inf, maxresults::Int = length(candidates))
    oldlen = length(state.candidates)
    idx = filterchunkrev!(state.candidates, candidates, state.filter, seen, idx;
                          maxtime = maxtime, maxresults = maxresults)
    newlen = length(state.candidates)
    newcands = view(state.candidates, (oldlen + 1):newlen)
    gfound = Int[]
    for (i, g) in enumerate(state.selection.gathered)
        cind = searchsorted(newcands, g, by = e -> e.index)
        isempty(cind) && continue
        push!(state.selection.active, oldlen + first(cind))
        push!(gfound, i)
    end
    isempty(gfound) || deleteat!(state.selection.gathered, gfound)
    idx
end

"""
    movehover(state::SelectorState, backwards::Bool, page::Bool)

Move the hover cursor in `state` by one row or one page.

The direction and size of the move is determined by `backwards` and `page`.
"""
function movehover(state::SelectorState, backwards::Bool, page::Bool)
    candrows = componentrows(state).candidates
    shift = ifelse(backwards, 1, -1) * ifelse(page, max(1, candrows - 1), 1)
    # We need to adjust for the existence of the gathered selection,
    # and the division line depending on whether it will still be
    # visible after the move.
    if !isempty(state.selection.gathered) && state.scroll < 0 &&
        state.hover + shift + state.scroll <= candrows
        candrows -= 1
        shift -= page
    end
    ngathered = length(state.selection.gathered)
    if page && state.scroll < 0 && state.hover < shift
        shift -= min(-state.scroll, ngathered) - 2 * (state.hover == -1)
    end
    newhover = state.hover + shift
    # This looks a little funky because we want to produce a particular
    # behaviour when crossing between the active and gathered selection, namely
    # we want to ensure it always takes an explicit step to go from one section
    # to another and skip over 0 as an invalid position.
    newhover = if sign(newhover) == sign(state.hover) || (abs(state.hover) == 1 && newhover != 0)
        clamp(newhover, -ngathered + iszero(ngathered), max(1, length(state.candidates)))
    elseif ngathered == 0
        1
    elseif newhover == 0
        -sign(state.hover)
    else
        sign(state.hover)
    end
    newscroll = clamp(state.scroll,
                      max(-ngathered, newhover - candrows + (ngathered >= candrows)),
                      newhover - (newhover >= 0))
    SelectorState(
        state.area, state.query, state.filter, state.candidates,
        newscroll, state.selection, newhover)
end

"""
    toggleselection(state::SelectorState)

Vary the selection of the current candidate (selected by hover) in `state`.
"""
function toggleselection(state::SelectorState)
    newselection = if state.hover > 0
        hoveridx = length(state.candidates) - state.hover + 1
        hoveridx ∈ axes(state.candidates, 1) || return state
        activecopy = copy(state.selection.active)
        selsearch = searchsorted(activecopy, hoveridx)
        if isempty(selsearch)
            insert!(activecopy, first(selsearch), hoveridx)
        else
            elt = activecopy[selsearch]
            gidx = findfirst(==(elt), state.selection.gathered)
            isnothing(gidx) || deleteat!(state.selection.gathered, gidx)
            deleteat!(activecopy, first(selsearch))
        end
        (active = activecopy, gathered = state.selection.gathered)
    elseif state.hover < 0
        -state.hover ∈ axes(state.selection.gathered, 1) || return state
        gatheredcopy = copy(state.selection.gathered)
        deleteat!(gatheredcopy, -state.hover)
        (active = state.selection.active, gathered = gatheredcopy)
    else
        return state
    end
    newstate = SelectorState(
        state.area, state.query, state.filter, state.candidates,
        state.scroll, newselection, state.hover)
    movehover(newstate, false, false)
end

"""
    addcache!(cache::Vector{T}, state::Unsigned, new::T)

Add `new` to the log-structured `cache` according to `state`.

The lifetime of `new` is exponentially decaying, it has a `1` in `2^(k-1)`
chance of reaching the `k`-th position in the cache.

The cache can hold as many items as the number of bits in `state` (e.g. 8 for `UInt8`).
"""
function addcache!(cache::Vector{T}, state::Unsigned, new::T) where {T}
    maxsize = sizeof(state) * 8
    nextstate = state + one(state)
    shift = state ⊻ nextstate
    uninitialised = maxsize - length(cache)
    if Base.leading_zeros(nextstate) < uninitialised
        push!(cache, new)
        return nextstate
    end
    for b in 1:(maxsize - 1)
        iszero(shift & (0x1 << (maxsize - b))) && continue
        cache[b - uninitialised] = cache[b - uninitialised + 1]
    end
    cache[end] = new
    nextstate
end

"""
    savefile(term::Base.Terminals.TTYTerminal, content::AbstractString)

Prompt the user to save `content` to a file path, and record the action.
"""
function savefile(term::Base.Terminals.TTYTerminal, content::AbstractString)
    out = term.out_stream
    nlines = count('\n', content) + 1
    print(out, S"\e[1G\e[2K{grey,bold:history>} {bold,emphasis:save file: }")
    filename = try
        readline(term.in_stream)
    catch err
        if err isa InterruptException
            ""
        else
            rethrow()
        end
    end
    if isempty(filename)
        println(out, S"\e[F\e[2K{light,grey:{bold:history>} {red:×} History selection aborted}\n")
        return
    end
    open(filename, "w") do io
        seekend(io)
        if !iszero(position(io))
            seek(io, position(io) - 1)
            lastchar = read(io, UInt8)
            seekend(io)
            lastchar == UInt8('\n') || write(io, '\n')
        end
        write(io, content, '\n')
    end
    println(out, S"\e[F\e[2K{grey,bold:history>} {shadow:Wrote $nlines selected \
                   $(ifelse(nlines == 1, \"line\", \"lines\")) to {underline,link=$(abspath(filename)):$filename}}\n")
end

"""
    saveclipboard(term::Base.Terminals.TTYTerminal, content::AbstractString)

Save `content` to the clipboard and record the action.
"""
function saveclipboard(msgio::IO, content::AbstractString)
    nlines = count('\n', content) + 1
    clipboard(content)
    println(msgio, S"\e[1G\e[2K{grey,bold:history>} {shadow:Copied $nlines \
                     $(ifelse(nlines == 1, \"line\", \"lines\")) to clipboard}\n")
end
